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
-module(vert_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("vert.hrl").

connect_test() ->
    {ok, Connect} = vert:virConnectOpen("qemu:///system"),

    {ok, _Cap} = vert:virConnectGetCapabilities(Connect),

    {ok, #node_info{}} = vert:virNodeGetInfo(Connect),

    ok.

create_test() ->
    Cfg = os:getenv("VERT_DOMAIN_XML"),

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
    ok = vert:virDomainCreate(Domain, 0),

    Active = vert:virConnectNumOfDomains(Connect),

    true = Active > 0, 

    {ok, _DomainIDs} = vert:virConnectListDomains(Connect),

    Info = info(Domain),

    error_logger:info_report([{domain_info, Info}]),

    ok = vert:virDomainSuspend(Domain),

    ok = vert:virDomainResume(Domain),

    ok = vert:virDomainDestroy(Domain),

    {error,"Domain not found: no domain with matching id 31337"} =
        vert:virDomainLookupByID(Connect, 31337).

info(Domain) ->
    {ok, Name} = vert:virDomainGetName(Domain),
    {ok, Info} = vert:virDomainGetInfo(Domain),

    [{name, Name}, {info, Info}].
