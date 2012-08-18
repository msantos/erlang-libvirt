%% Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%% 
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%% 
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% Serial console
%%
%% Handling the serial console is a hack right now.
%%
%%  * the receive buffer is polled every recv_timeout milliseconds
%%
%%  * if there is no data available, flush the console by sending the
%%    flush control character
%%
%%  * the caller must disable echo mode (stty -echo) or the input
%%    is returned to as part of the received data
%%
%%  * the caller must disable the shell prompt or it will be returned
%%    as part of the received data
%%
%%  * the caller must reset the mode/prompt at the end of the session
%%
%% It's not possible to do all of the above automatically because we
%% don't know what the user is running on the serial console, e.g., could
%% be a login prompt, an editor session, ...
%%
-module(vert_console).
-behaviour(gen_server).

-export([
        open/1, open/2,
        close/1,

        write/2,
        send/2,
        recv/2,
        flush/1,

        mode/2
    ]).
-export([poll/2]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        uri = "qemu:///system", % libvirt connection URI
        stream,                 % libvirt stream descriptor
        domain,                 % libvirt domain descriptor
        devname = <<>>,         % VM console device
        flush = <<16#0f>>,      % flush control sequence (^O),
                                %   <<>> to disable
        recbuf = 16#ffff,       % receive buffer size
        recv_timeout = 100,     % poll the receive buffer,
                                %   0 to disable polling
        eol = true,             % accumulate buffer until eol is seen
        pid,                    % PID of controlling process

        tref
    }).

-define(PROPLIST_TO_RECORD(Record),
            fun(Proplist) ->
                Fields = record_info(fields, Record),
                [Tag| Values] = tuple_to_list(#Record{}),
                Defaults = lists:zip(Fields, Values),
                L = lists:map(fun ({K,V}) -> proplists:get_value(K, Proplist, V) end, Defaults),
                list_to_tuple([Tag|L])
            end).


%%--------------------------------------------------------------------
%%% API
%%--------------------------------------------------------------------
open(Host) ->
    open(Host, []).

open(Host, Opt) ->
    start_link(Host, Opt).

close(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, close).

write(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    gen_server:call(Ref, {write, Data}).

send(Ref, Data) when is_pid(Ref), is_list(Data) ->
    send(Ref, list_to_binary(Data));
send(Ref, Data) when is_pid(Ref), is_binary(Data) ->
    write(Ref, <<Data/binary, $\n>>).

recv(Ref, Timeout) when is_pid(Ref), Timeout > 0 ->
    recv_loop(Ref, Timeout, []).

recv_loop(Ref, Timeout, Data) ->
    receive
        {vert_console, Ref, Buf} ->
            recv_loop(Ref, Timeout, [Buf|Data])
    after
        Timeout ->
            {ok, list_to_binary(lists:reverse(Data))}
    end.

flush(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, flush).

mode(Ref, raw) when is_pid(Ref) ->
    send(Ref, "stty -echo"),
    send(Ref, "PS1=");
mode(Ref, sane) when is_pid(Ref) ->
    send(Ref, "stty sane").

start_link(Host, Opt) ->
    Pid = self(),
    gen_server:start_link(?MODULE, [Pid, Host, options(Opt)], []).


%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Pid, Host,
      #state{
        uri = URI
      } = Opt]) ->
    process_flag(trap_exit, true),

    {ok, Connect} = vert:virConnectOpen(URI),
    {ok, Domain} = vert:virDomainLookupByName(Connect, Host),
    {ok, Stream} = vert:virStreamNew(Connect),
    ok = vert:virDomainOpenConsole(Domain, Stream),

    State = Opt#state{
        stream = Stream,
        domain = Domain,
        pid = Pid
    },

    {ok, Tref} = poll_init(State),

    {ok, State#state{tref = Tref}}.

handle_call({write, Data}, _From, #state{stream = Stream} = State) ->
    Size = byte_size(Data),
    Reply = case vert:virStreamSend(Stream, Data) of
        {ok, Size} -> ok;
        {ok, N} -> {ok, N}; % partial write
        Error -> Error
    end,
    {reply, Reply, State};

handle_call(flush, _From, #state{stream = Stream, flush = Flush} = State) ->
    Reply = vert:virStreamSend(Stream, Flush),
    {reply, Reply, State};

handle_call(close, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({vert_console, Data}, #state{pid = Pid} = State) ->
    Pid ! {vert_console, self(), Data},
    {noreply, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([wtf, Info]),
    {noreply, State}.

terminate(_Reason, #state{stream = Stream, tref = Tref}) ->
    case Tref of
        undefined -> ok;
        _ -> timer:cancel(Tref)
    end,
    vert:virStreamFinish(Stream),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
options(Opt) ->
    Fun = ?PROPLIST_TO_RECORD(state),
    Fun(Opt).

poll_init(#state{recv_timeout = 0}) ->
    {ok, undefined};
poll_init(#state{recv_timeout = Timeout} = State) ->
    Self = self(),
    timer:apply_interval(
        Timeout,
        ?MODULE,
        poll,
        [Self, State]).

poll(Pid, #state{
                stream = Stream,
                recbuf = N,
                flush = Flush
                }) ->
        case vert:virStreamRecv(Stream, N) of
            {error, eagain} ->
                flush_tty(Stream, Flush);
            {ok, Buf} ->
                Pid ! {vert_console, Buf}
        end.

flush_tty(_Stream, <<>>) ->
    ok;
flush_tty(Stream, Flush) ->
    {ok, _} = vert:virStreamSend(Stream, Flush),
    ok.
