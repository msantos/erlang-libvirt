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

close(#connect{res = Res}) ->
    connect_close(Res).

get(#connect{res = Res}, capabilities) ->
    connect_get_capabilities(Res);
get(#connect{} = Res, cellsfreemem) ->
    {ok, #node_info{nodes = Nodes}} = ?MODULE:get(Res, info),
    connect_get_cellsfreememory(Res, Nodes);
get(#connect{res = Res}, encrypted) ->
    connect_is_encrypted(Res);
get(#connect{res = Res}, secure) ->
    connect_is_secure(Res);
get(#connect{res = Res}, freemem) ->
    connect_get_freememory(Res);
get(#connect{res = Res}, hostname) ->
    connect_get_hostname(Res);

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
get(#connect{res = Res}, info) ->
    Long = erlang:system_info(wordsize),
    case connect_get_info(Res) of
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
get(#connect{res = Res}, libversion) ->
    case connect_get_libversion(Res) of
        {ok, Version} ->
            {ok, version(Version)};
        Err ->
            Err
    end;
get(#connect{res = Res}, version) ->
    case connect_get_version(Res) of
        {ok, Version} ->
            {ok, version(Version)};
        Err ->
            Err
    end;
get(#connect{res = Res}, maxvcpus) ->
    connect_get_maxvcpus(Res, []);
get(#connect{res = Res}, {maxvcpus, Type}) when is_list(Type) ->
    connect_get_maxvcpus(Res, Type);

%% struct _virSecurityModel {
%%  char model[VIR_SECURITY_MODEL_BUFLEN];      /* security model string */
%%  char doi[VIR_SECURITY_DOI_BUFLEN];          /* domain of interpetation */
%% }
get(#connect{res = Res}, secmodel) ->
    case connect_get_securitymodel(Res) of
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


get(#connect{res = Res}, type) ->
    connect_get_type(Res);
get(#connect{res = Res}, uri) ->
    connect_get_uri(Res);

get(#connect{res = Res}, {domain, {id, ID}}) when is_integer(ID) ->
    domain_lookup(Res, ?VERT_DOMAIN_LOOKUP_BY_ID, ID);
get(#connect{res = Res}, {domain, {name, Name}}) when is_list(Name) ->
    domain_lookup(Res, ?VERT_DOMAIN_LOOKUP_BY_NAME, Name);
get(#connect{res = Res}, {domain, {uuid, UUID}}) when is_list(UUID) ->
    domain_lookup(Res, ?VERT_DOMAIN_LOOKUP_BY_UUID, UUID);
get(#connect{res = Res}, {domain, {uuid, UUID}}) when is_binary(UUID) ->
    domain_lookup(Res, ?VERT_DOMAIN_LOOKUP_BY_RAWUUID, UUID);

get(#connect{} = Res, Type) when is_atom(Type) ->
    ?MODULE:get(Res, {Type, active});

get(#connect{res = Res}, {Type, num_active}) ->
    connect_get_numactive(Res, resource(Type));
get(#connect{res = Res}, {Type, num_inactive}) ->
    connect_get_numinactive(Res, resource(Type));

get(#connect{res = Res}, {Type, active}) ->
    Resource = resource(Type),
    case connect_get_numactive(Res, Resource) of
        {ok, 0} -> [];
        {ok, Max} -> connect_get_listactive(Res, Resource, Max);
        Err -> Err
    end;
get(#connect{res = Res}, {Type, inactive}) ->
    Resource = resource(Type),
    case connect_get_numactive(Res, Resource) of
        {ok, 0} -> [];
        {ok, Max} -> connect_get_listinactive(Res, Resource, Max);
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
get(#domain{res = Res}, info) ->
    Long = erlang:system_info(wordsize),
    case domain_get_info(Res) of
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


set(Resource, autostart) ->
    set(Resource, {autostart, true});
set(#domain{res = Res}, {autostart, true}) ->
    domain_set_autostart(Res, 1);
set(#domain{res = Res}, {autostart, false}) ->
    domain_set_autostart(Res, 0).

%%
%% Domain
%%
free(#domain{res = Res}) ->
    domain_free(Res).

create(Connect, {transient, Cfg}) ->
    create(Connect, {transient, Cfg, []});
create(#connect{res = Res}, {transient, Cfg, Flags}) when is_list(Cfg), is_list(Flags) ->
    Bits = lists:foldl(fun(N, X) -> flags(N) bor X end, 0, Flags),
    domain_create(Res, ?VERT_DOMAIN_CREATE_TRANSIENT, Cfg, Bits);
create(#connect{res = Res}, {persistent, Cfg}) when is_list(Cfg) ->
    domain_create(Res, ?VERT_DOMAIN_CREATE_PERSISTENT, Cfg, 0).

save(#domain{res = Res}, File) when is_list(File) ->
    domain_save(Res, File).

restore(#connect{res = Res}, File) when is_list(File) ->
    domain_restore(Res, File).


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
connect_get_numactive(_,_) ->
    erlang:error(not_implemented).
connect_get_numinactive(_,_) ->
    erlang:error(not_implemented).

connect_get_listactive(_,_,_) ->
    erlang:error(not_implemented).
connect_get_listinactive(_,_,_) ->
    erlang:error(not_implemented).

connect_close(_) ->
    erlang:error(not_implemented).

domain_get_info(_) ->
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

resource(domains) -> ?VERT_RES_DOMAINS;
resource(interfaces) -> ?VERT_RES_INTERFACES;
resource(networks) -> ?VERT_RES_NETWORKS;
resource(storagepools) -> ?VERT_RES_STORAGEPOOLS;
resource(filters) -> ?VERT_RES_FILTERS;
resource(secrets) -> ?VERT_RES_SECRETS.

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

