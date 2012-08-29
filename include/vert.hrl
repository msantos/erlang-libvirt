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

%% virStorageVolPtr resource
-record(storagevol, {
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

%% virStreamPtr resource
-record(stream, {
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

%% virStoragePoolInfo
-record(storagepool_info, {
        state = 0,
        capacity = 0,
        allocation = 0,
        available = 0
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

-define(VIR_UUID_BUFLEN, 16).
-define(HOST_NAME_MAX, 255).
-define(MAXPATHLEN, 1024).

% virKeycodeSet
-define(VIR_KEYCODE_SET_LINUX, 0).
-define(VIR_KEYCODE_SET_XT, 1).
-define(VIR_KEYCODE_SET_ATSET1, 2).
-define(VIR_KEYCODE_SET_ATSET2, 3).
-define(VIR_KEYCODE_SET_ATSET3, 4).
-define(VIR_KEYCODE_SET_OSX, 5).
-define(VIR_KEYCODE_SET_XT_KBD, 6).
-define(VIR_KEYCODE_SET_USB, 7).
-define(VIR_KEYCODE_SET_WIN32, 8).
-define(VIR_KEYCODE_SET_RFB, 9).
-define(VIR_KEYCODE_SET_LAST, 10).

-define(VIR_STREAM_NONBLOCK, 1).
