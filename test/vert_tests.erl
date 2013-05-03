%% Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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
-module(vert_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("vert.hrl").

connect_test() ->
    {ok, Connect} = vert:virConnectOpen("qemu:///system"),

    {ok, Cap} = vert:virConnectGetCapabilities(Connect),

    error_logger:info_report([{capabilities, Cap}]),

    {ok, #node_info{} = Info} = vert:virNodeGetInfo(Connect),

    error_logger:info_report([{node_info, Info}]),

    ok.

create_suspend_resume_destroy_test() ->
    {ok, Connect, Domain} = create(),

    Active = vert:virConnectNumOfDomains(Connect),

    error_logger:info_report([{active_domains, Active}]),

    true = Active > 0,

    {ok, DomainIDs} = vert:virConnectListDomains(Connect),

    error_logger:info_report([{domains, DomainIDs}]),

    Info = info(Domain),

    error_logger:info_report([{domain_info, Info}]),

    ok = vert:virDomainSuspend(Domain),

    error_logger:info_report({domain, suspended}),

    ok = vert:virDomainResume(Domain),

    error_logger:info_report({domain, resumed}),

    ok = vert:virDomainDestroy(Domain),

    error_logger:info_report({domain, destroyed}),

    {error,"Domain not found: no domain with matching id 31337"} =
        vert:virDomainLookupByID(Connect, 31337).

console_test_() ->
    {timeout, 60, [{?LINE, fun() -> console_input_output() end}]}.

console_input_output() ->
    % use testvm
    {ok, _Connect, Domain} = create(false),

    [{name, Name}, {info, _Info}] = info(Domain),

    {ok, Ref} = vert_console:open(Name),

    error_logger:info_report([{boot, waiting_for_keypress}]),

    {ok, <<"\rPress", _/binary>>} = vert_console:read(Ref),

    error_logger:info_report([{boot, got_keypress}]),

    % Choose this console
    vert_console:send(Ref, ""),

    error_logger:info_report([{boot, sent_keypress}]),

    wait_for_prompt(Ref),

    ok = vert_console:send(Ref, "uname -a"),
    {ok, Res} = vert_console:recv(Ref, 1000),

    error_logger:info_report([{console, Res}]),

    ok = vert:virDomainDestroy(Domain).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
info(Domain) ->
    {ok, Name} = vert:virDomainGetName(Domain),
    {ok, Info} = vert:virDomainGetInfo(Domain),

    [{name, Name}, {info, Info}].

create() ->
    Cfg = os:getenv("VERT_DOMAIN_XML"),
    create(Cfg).

create(Cfg) ->
    Path = case Cfg of
        false ->
            filename:join([
                filename:dirname(code:which(vert)),
                "..",
                "priv",
                "example.xml"
            ]);
        _ -> Cfg
    end,

    {ok, Connect} = vert:virConnectOpen("qemu:///system"),
    {ok, XML} = file:read_file(Path),

    {ok, Domain} = vert:virDomainDefineXML(Connect, XML),
    ok = vert:virDomainCreate(Domain),

    {ok, Connect, Domain}.

wait_for_prompt(Ref) ->
    {ok, Out} = vert_console:read(Ref),
    error_logger:info_report([{dmesg, Out}]),

    case binary:match(Out, [<<"press">>]) of
        nomatch ->
            wait_for_prompt(Ref);
        _ ->
            error_logger:info_report([{pressing, enter_key}]),
            % wildly pound the enter key like an overcaffeinated sysadmin
            ok = vert_console:write(Ref, <<"\r\n\r\n">>),
            wait_for_prompt_1(Ref)
    end.

wait_for_prompt_1(Ref) ->
    {ok, Out} = vert_console:read(Ref),
    error_logger:info_report([{dmesg, Out}]),

    case binary:match(Out, [<<"# ">>]) of
        nomatch -> wait_for_prompt_1(Ref);
        _ ->
            error_logger:info_report([{got, prompt}]),
            ok
    end.
