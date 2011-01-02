%% Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
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
-module(vert).
-include("vert.hrl").

-export([
        open/1,
        close/1,
        get/2,
        set/2,

        free/1,
        create/2,

        save/2,
        restore/2
    ]).


-on_load(on_load/0).

on_load() ->
    erlang:load_nif(niflib(), []).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------

%%
%% Connections
%%

open({connect, Name}) when is_list(Name) ->
    connect_open(Name, ?VERT_CONNECT_OPEN);
open({connect, Name, read}) when is_list(Name) ->
    connect_open(Name, ?VERT_CONNECT_OPEN_READONLY);
open({connect, Name, auth}) when is_list(Name) ->
    % connect_open_readonly(Name, ?VERT_CONNECT_OPEN_AUTH);
    erlang:error(not_implemented).

close({connect, Ref, Bin}) when is_reference(Ref) ->
    close({connect, Bin});
close({connect, Bin}) ->
    connect_close(Bin).

get({Ptr, Ref, Bin}, Type) when ( Ptr == connect orelse Ptr == domain ),
is_reference(Ref), ( is_atom(Type) orelse is_tuple(Type) ) ->
    ?MODULE:get({Ptr, Bin}, Type);
get({connect, Bin}, capabilities) ->
    connect_get_capabilities(Bin);
get({connect, Bin}, cellsfreemem) ->
    {ok, #node_info{nodes = Nodes}} = get({connect, Bin}, info),
    connect_get_cellsfreememory(Bin, Nodes);
get({connect, Bin}, encrypted) ->
    connect_is_encrypted(Bin);
get({connect, Bin}, secure) ->
    connect_is_secure(Bin);
get({connect, Bin}, freemem) ->
    connect_get_freememory(Bin);
get({connect, Bin}, hostname) ->
    connect_get_hostname(Bin);

%% struct _virNodeInfo {
%%     char model[32];     /* string indicating the CPU model */
%%     unsigned long memory;/* memory size in kilobytes */
%%     unsigned int cpus;  /* the number of active CPUs */
%%     unsigned int mhz;   /* expected CPU frequency */
%%     unsigned int nodes; /* the number of NUMA cell, 1 for uniform mem access */
%%     unsigned int sockets;/* number of CPU socket per node */
%%     unsigned int cores; /* number of core per socket */
%%     unsigned int threads;/* number of threads per core */
%% };
get({connect, Bin}, info) ->
    Long = erlang:system_info(wordsize),
    case connect_get_info(Bin) of
        {ok, <<
            Model:32/native-bytes,
            Memory:Long/native-unsigned-integer-unit:8,
            Cpus:32/native,
            Mhz:32/native,
            Nodes:32/native,
            Sockets:32/native,
            Cores:32/native,
            Threads:32/native
        >>} ->
        {ok, #node_info{
                model = Model,
                memory = Memory,
                cpus = Cpus,
                mhz = Mhz,
                nodes = Nodes,
                sockets = Sockets,
                cores = Cores,
                threads = Threads
            }};
        Error ->
            Error
    end;
get({connect, Bin}, libversion) ->
    case connect_get_libversion(Bin) of
        {ok, Version} ->
            {ok, version(Version)};
        Err ->
            Err
    end;
get({connect, Bin}, version) ->
    case connect_get_version(Bin) of
        {ok, Version} ->
            {ok, version(Version)};
        Err ->
            Err
    end;
get({connect, Bin}, maxvcpus) ->
    connect_get_maxvcpus(Bin, []);
get({connect, Bin}, {maxvcpus, Type}) when is_list(Type) ->
    connect_get_maxvcpus(Bin, Type);

%% struct _virSecurityModel {
%%  char model[VIR_SECURITY_MODEL_BUFLEN];      /* security model string */
%%  char doi[VIR_SECURITY_DOI_BUFLEN];          /* domain of interpetation */
%% }
get({connect, Bin}, secmodel) ->
    case connect_get_securitymodel(Bin) of
        {ok, <<
            Model:?VIR_SECURITY_MODEL_BUFLEN/native-bytes,
            Doi:?VIR_SECURITY_DOI_BUFLEN/native-bytes
            >>} ->
            {ok, #security_model{
                    model = Model,
                    doi = Doi
                }};
        Err ->
            Err
    end;

get({connect, Bin}, num_domains) ->
    ?MODULE:get({connect, Bin}, {num_domains, active});
get({connect, Bin}, {num_domains, active}) ->
    connect_get_numdomains(Bin, ?VERT_DOMAIN_LIST_ACTIVE);
get({connect, Bin}, {num_domains, inactive}) ->
    connect_get_numdomains(Bin, ?VERT_DOMAIN_LIST_INACTIVE);

get({connect, Bin}, type) ->
    connect_get_type(Bin);
get({connect, Bin}, uri) ->
    connect_get_uri(Bin);

get({connect, Bin}, {domain, {id, ID}}) when is_integer(ID) ->
    domain_lookup(Bin, ?VERT_DOMAIN_LOOKUP_BY_ID, ID);
get({connect, Bin}, {domain, {name, Name}}) when is_list(Name) ->
    domain_lookup(Bin, ?VERT_DOMAIN_LOOKUP_BY_NAME, Name);
get({connect, Bin}, {domain, {uuid, UUID}}) when is_list(UUID) ->
    domain_lookup(Bin, ?VERT_DOMAIN_LOOKUP_BY_UUID, UUID);
get({connect, Bin}, {domain, {uuid, UUID}}) when is_binary(UUID) ->
    domain_lookup(Bin, ?VERT_DOMAIN_LOOKUP_BY_RAWUUID, UUID);

get({connect, Bin}, domains) ->
    ?MODULE:get({connect, Bin}, {domains, active});
get({connect, Bin}, {domains, active}) ->
    case connect_get_numdomains(Bin, ?VERT_DOMAIN_LIST_ACTIVE) of
        {ok, 0} -> [];
        {ok, Max} -> domain_list(Bin, ?VERT_DOMAIN_LIST_ACTIVE, Max);
        Err -> Err
    end;
get({connect, Bin}, {domains, inactive}) ->
    case connect_get_numdomains(Bin, ?VERT_DOMAIN_LIST_INACTIVE) of
        {ok, 0} -> [];
        {ok, Max} -> domain_list(Bin, ?VERT_DOMAIN_LIST_INACTIVE, Max);
        Err -> Err
    end;

%%
%% domain
%%

%% struct virDomainInfo{
%%     unsigned char   state
%%     unsigned long   maxMem
%%     unsigned long   memory
%%     unsigned short  nrVirtCpu
%%     unsigned long long  cpuTime
%% }
get({domain, Bin}, info) ->
    Long = erlang:system_info(wordsize),
    case domain_get_info(Bin) of
        {ok, <<
            State:8,
            MaxMem:Long/native-unsigned-integer-unit:8,
            Memory:Long/native-unsigned-integer-unit:8,
            NrVirtCpu:2/native-unsigned-integer-unit:8,
            CpuTime:8/native-unsigned-integer-unit:8,
            _WTF/binary
            >>} ->
            #domain_info{
                state = state({domain, State}),
                maxmem = MaxMem,
                memory = Memory,
                nrvirtcpu = NrVirtCpu,
                cputime = CpuTime
            };
        Err ->
            Err
    end.


set({Ptr, Ref, Bin}, Type) when ( Ptr == connect orelse Ptr == domain ),
is_reference(Ref), ( is_atom(Type) orelse is_tuple(Type) ) ->
    set({Ptr, Bin}, Type);
set({domain, Dom}, autostart) ->
    set({domain, Dom}, {autostart, true});
set({domain, Dom}, {autostart, true}) ->
    domain_set_autostart(Dom, 1);
set({domain, Dom}, {autostart, false}) ->
    domain_set_autostart(Dom, 0).

%%
%% Domain
%%
free({domain, Ref, Dom}) when is_reference(Ref) ->
    domain_free(Dom).

create(Connect, {transient, Cfg}) ->
    create(Connect, {transient, Cfg, []});
create({connect, Ref, Conn}, {transient, Cfg, Flags}) when is_reference(Ref),
    is_list(Cfg), is_list(Flags) ->
    Bits = lists:foldl(fun(N, X) -> flags(N) bor X end, 0, Flags),
    domain_create(Conn, ?VERT_DOMAIN_CREATE_TRANSIENT, Cfg, Bits);
create({connect, Ref, Conn}, {persistent, Cfg}) when is_reference(Ref), is_list(Cfg) ->
    domain_create(Conn, ?VERT_DOMAIN_CREATE_PERSISTENT, Cfg, 0).

save({domain, Ref, Bin}, File) when is_reference(Ref), is_list(File) ->
    domain_save(Bin, File).

restore({connect, Ref, Bin}, File) when is_reference(Ref), is_list(File) ->
    domain_restore(Bin, File).


%%-------------------------------------------------------------------------
%%% NIF stubs
%%-------------------------------------------------------------------------
connect_open(_,_) ->
    erlang:error(not_implemented).

connect_get_capabilities(_) ->
    erlang:error(not_implemented).
connect_get_freememory(_) ->
    erlang:error(not_implemented).
connect_get_hostname(_) ->
    erlang:error(not_implemented).
connect_get_info(_) ->
    erlang:error(not_implemented).
connect_get_securitymodel(_) ->
    erlang:error(not_implemented).
connect_get_type(_) ->
    erlang:error(not_implemented).
connect_get_uri(_) ->
    erlang:error(not_implemented).
connect_get_libversion(_) ->
    erlang:error(not_implemented).
connect_get_version(_) ->
    erlang:error(not_implemented).

connect_is_encrypted(_) ->
    erlang:error(not_implemented).
connect_is_secure(_) ->
    erlang:error(not_implemented).

connect_get_cellsfreememory(_,_) ->
    erlang:error(not_implemented).
connect_get_maxvcpus(_,_) ->
    erlang:error(not_implemented).
connect_get_numdomains(_,_) ->
    erlang:error(not_implemented).

connect_close(_) ->
    erlang:error(not_implemented).

domain_get_info(_) ->
    erlang:error(not_implemented).

domain_list(_,_,_) ->
    erlang:error(not_implemented).
domain_lookup(_,_,_) ->
    erlang:error(not_implemented).
domain_free(_) ->
    erlang:error(not_implemented).
domain_create(_,_,_,_) ->
    erlang:error(not_implemented).

domain_save(_,_) ->
    erlang:error(not_implemented).
domain_restore(_,_) ->
    erlang:error(not_implemented).

domain_set_autostart(_,_) ->
    erlang:error(not_implemented).


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
version(Version) when is_integer(Version) ->
    Major = Version div 1000000,
    Minor = Version rem 1000000 div 1000,
    Release = Version rem 1000000 rem 1000,
    {Major, Minor, Release}.

flags(paused) -> ?VIR_DOMAIN_START_PAUSED.

state({domain, ?VIR_DOMAIN_NOSTATE}) -> undefined;
state({domain, ?VIR_DOMAIN_RUNNING}) -> running;
state({domain, ?VIR_DOMAIN_BLOCKED}) -> blocked;
state({domain, ?VIR_DOMAIN_PAUSED}) -> paused;
state({domain, ?VIR_DOMAIN_SHUTDOWN}) -> shutdown;
state({domain, ?VIR_DOMAIN_SHUTOFF}) -> shutoff;
state({domain, ?VIR_DOMAIN_CRASHED}) -> crashed;

state({domain, undefined}) -> ?VIR_DOMAIN_NOSTATE;
state({domain, running}) -> ?VIR_DOMAIN_RUNNING;
state({domain, blocked}) -> ?VIR_DOMAIN_BLOCKED;
state({domain, paused}) -> ?VIR_DOMAIN_PAUSED;
state({domain, shutdown}) -> ?VIR_DOMAIN_SHUTDOWN;
state({domain, shutoff}) -> ?VIR_DOMAIN_SHUTOFF;
state({domain, crashed}) -> ?VIR_DOMAIN_CRASHED.


privdir(File) ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..",
        "priv",
        File
    ]).

niflib() ->
    privdir(?MODULE).

