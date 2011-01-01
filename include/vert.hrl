%% Copyright (c) 2010, Michael Santos <michael.santos@gmail.com>
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


%%
%% vert
%%

%% connect_open/2
-define(VERT_CONNECT_OPEN, 0).
-define(VERT_CONNECT_OPEN_READONLY, 1).
-define(VERT_CONNECT_OPEN_AUTH, 2).

%% domain_lookup/3
-define(VERT_DOMAIN_LOOKUP_BY_ID, 0).
-define(VERT_DOMAIN_LOOKUP_BY_NAME, 1).
-define(VERT_DOMAIN_LOOKUP_BY_UUID, 2).
-define(VERT_DOMAIN_LOOKUP_BY_RAWUUID, 3).

%% domain_list/2
-define(VERT_DOMAIN_LIST_ACTIVE, 0).
-define(VERT_DOMAIN_LIST_INACTIVE, 1).

%% domain create

% types
-define(VERT_DOMAIN_CREATE_TRANSIENT, 0).
-define(VERT_DOMAIN_CREATE_PERSISTENT, 1).

% flags
-define(VIR_DOMAIN_NONE, 0).            % Default behavior
-define(VIR_DOMAIN_START_PAUSED, 1).    % Launch guest in paused state

