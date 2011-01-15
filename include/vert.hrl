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

%% virConnectPtr resource
-record(resource, {
        type,
        ref,
        res
    }).

%% virDomainPtr resource
-record(domain, {
        ref,
        res
    }).

%% virInterfacePtr resource
-record(interface, {
        ref,
        res
    }).

%% virNetworkPtr resource
-record(network, {
        ref,
        res
    }).

%% virStoragePoolPtr resource
-record(storagepool, {
        ref,
        res
    }).

%% virNWFilterPtr resource
-record(filter, {
        ref,
        res
    }).

%% virSecretPtr resource
-record(secret, {
        ref,
        res
    }).

%% virNodeInfo
-record(node_info, {
        model = <<>>,
        memory = 0,
        cpus = 0,
        mhz = 0,
        nodes = 0,
        sockets = 0,
        cores = 0,
        threads = 0
    }).

%% virSecurityModel
-define(VIR_SECURITY_MODEL_BUFLEN, (256 + 1)).
-define(VIR_SECURITY_DOI_BUFLEN, (256 + 1)).

-record(security_model, {
        model = <<>>,
        doi = <<>>
    }).

%% virDomainGetInfo
-record(domain_info, {
        state = 0,
        maxmem = 0,
        memory = 0,
        nrvirtcpu = 0,
        cputime = 0
    }).


%%
%% vert
%%

%% Resource types
-define(VERT_RES_CONNECT, 0).
-define(VERT_RES_DOMAIN, 1).
-define(VERT_RES_INTERFACE, 2).
-define(VERT_RES_NETWORK, 3).
-define(VERT_RES_STORAGEPOOL, 4).
-define(VERT_RES_FILTER, 5).
-define(VERT_RES_SECRET, 6).

%% connect_open/2
-define(VERT_CONNECT_OPEN, 0).
-define(VERT_CONNECT_OPEN_READONLY, 1).
-define(VERT_CONNECT_OPEN_AUTH, 2).

%% domain_lookup/3
-define(VERT_ATTR_ID, 0).
-define(VERT_ATTR_NAME, 1).
-define(VERT_ATTR_UUID, 2).
-define(VERT_ATTR_RAWUUID, 3).
-define(VERT_ATTR_MAC, 4).
-define(VERT_ATTR_DESC, 5).
-define(VERT_ATTR_INFO, 6).
-define(VERT_ATTR_AUTOSTART, 7).
-define(VERT_ATTR_BLOCKINFO, 8).
-define(VERT_ATTR_CONNECT, 9).
-define(VERT_ATTR_JOBINFO, 10).
-define(VERT_ATTR_MAXMEMORY, 11).
-define(VERT_ATTR_MAXVCPUS, 12).
-define(VERT_ATTR_MEMORYPARAMETERS, 13).
-define(VERT_ATTR_OSTYPE, 14).
-define(VERT_ATTR_SCHEDULERPARAMETERS, 15).
-define(VERT_ATTR_SCHEDULERTYPE, 16).
-define(VERT_ATTR_SECURITYLABEL, 17).
-define(VERT_ATTR_VCPUS, 18).
-define(VERT_ATTR_VCPUSFLAGS, 19).
-define(VERT_ATTR_CAPABILITIES, 20).
-define(VERT_ATTR_HOSTNAME, 21).
-define(VERT_ATTR_LIBVERSION, 22).
-define(VERT_ATTR_FREEMEMORY, 23).
-define(VERT_ATTR_CELLSFREEMEMORY, 24).
-define(VERT_ATTR_TYPE, 25).
-define(VERT_ATTR_VERSION, 26).
-define(VERT_ATTR_URI, 27).
-define(VERT_ATTR_ENCRYPTED, 28).
-define(VERT_ATTR_SECURE, 29).
-define(VERT_ATTR_SECURITYMODEL, 30).

%% domain_create

% types
-define(VERT_DOMAIN_CREATE_TRANSIENT, 0).
-define(VERT_DOMAIN_CREATE_PERSISTENT, 1).

% flags
-define(VIR_DOMAIN_NONE, 0).            % Default behavior
-define(VIR_DOMAIN_START_PAUSED, 1).    % Launch guest in paused state

% domain_info/1 
-define(VIR_DOMAIN_NOSTATE, 0).     % no state
-define(VIR_DOMAIN_RUNNING, 1).     % the domain is running
-define(VIR_DOMAIN_BLOCKED, 2).     % the domain is blocked on resource
-define(VIR_DOMAIN_PAUSED, 3).      % the domain is paused by user
-define(VIR_DOMAIN_SHUTDOWN, 4).    % the domain is being shut down
-define(VIR_DOMAIN_SHUTOFF, 5).     % the domain is shut off
-define(VIR_DOMAIN_CRASHED, 6).     % the domain is crashed

