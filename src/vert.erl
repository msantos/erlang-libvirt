%% Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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

-define(UINT16(N), N:2/native-unsigned-integer-unit:8).
-define(UINT32(N), N:4/native-unsigned-integer-unit:8).
-define(UINT64(N), N:8/native-unsigned-integer-unit:8).

-define(INT16(N), N:2/native-integer-unit:8).
-define(INT32(N), N:4/native-integer-unit:8).
-define(INT64(N), N:8/native-integer-unit:8).

-export([
    lookup/2
    ]).
-export([
        virNodeGetCellsFreeMemory/2,
        virNodeGetFreeMemory/1,
        virNodeGetInfo/1,
        virNodeGetSecurityModel/1,

        virNetworkCreate/1,
        virNetworkDefineXML/2,
        virNetworkDestroy/1,
        virNetworkGetAutostart/1,
        virNetworkGetBridgeName/1,
        virNetworkGetName/1,
        virNetworkGetUUID/1,
        virNetworkGetUUIDString/1,
        virNetworkGetXMLDesc/1, virNetworkGetXMLDesc/2,
        virNetworkIsActive/1,
        virNetworkIsPersistent/1,
        virNetworkLookupByName/2,
        virNetworkLookupByUUID/2,
        virNetworkLookupByUUIDString/2,
        virNetworkSetAutostart/2,
        virNetworkUndefine/1,

        virNWFilterDefineXML/2,
        virNWFilterGetName/1,
        virNWFilterGetUUID/1,
        virNWFilterGetUUIDString/1,
        virNWFilterGetXMLDesc/1, virNWFilterGetXMLDesc/2,
        virNWFilterLookupByName/2,
        virNWFilterLookupByUUID/2,
        virNWFilterLookupByUUIDString/2,
        virNWFilterUndefine/1,

        virInterfaceCreate/1,
        virInterfaceDefineXML/2,
        virInterfaceDestroy/1,
        virInterfaceGetMACString/1,
        virInterfaceGetName/1,
        virInterfaceGetXMLDesc/1, virInterfaceGetXMLDesc/2,
        virInterfaceIsActive/1,
        virInterfaceLookupByMACString/2,
        virInterfaceLookupByName/2,
        virInterfaceUndefine/1,

        virDomainCreate/1, virDomainCreate/2,
        virDomainDefineXML/2,
        virDomainDestroy/1,
        virDomainGetAutostart/1,
        virDomainGetBlockInfo/2,
        virDomainGetID/1,
        virDomainGetInfo/1,
        virDomainGetJobInfo/1,
        virDomainGetMaxMemory/1,
        virDomainGetMaxVcpus/1,
        virDomainGetMemoryParameters/1,
        virDomainGetName/1,
        virDomainGetOSType/1,
        virDomainGetSchedulerParameters/1,
        virDomainGetSchedulerType/1,
        virDomainGetSecurityLabel/1,
        virDomainGetUUID/1,
        virDomainGetUUIDString/1,
        virDomainGetXMLDesc/2,
        virDomainIsActive/1,
        virDomainIsPersistent/1,
        virDomainLookupByID/2,
        virDomainLookupByName/2,
        virDomainLookupByUUID/2,
        virDomainLookupByUUIDString/2,
        virDomainOpenConsole/2, virDomainOpenConsole/3,
        virDomainRestore/2,
        virDomainResume/1,
        virDomainSave/2,
        virDomainScreenshot/3, virDomainScreenshot/4,
        virDomainSendKey/4, virDomainSendKey/5,
        virDomainSetAutostart/2,
        virDomainShutdown/1,
        virDomainSuspend/1,
        virDomainUndefine/1,

        virConnectFindStoragePoolSources/3, virConnectFindStoragePoolSources/4,
        virConnectGetCapabilities/1,
        virConnectGetHostname/1,
        virConnectGetLibVersion/1,
        virConnectGetMaxVcpus/2,
        virConnectGetType/1,
        virConnectGetURI/1,
        virConnectGetVersion/1,
        virConnectIsEncrypted/1,
        virConnectIsSecure/1,
        virConnectListDefinedDomains/1, virConnectListDefinedDomains/2,
        virConnectListDefinedInterfaces/1, virConnectListDefinedInterfaces/2,
        virConnectListDefinedNetworks/1, virConnectListDefinedNetworks/2,
        virConnectListDefinedStoragePools/1, virConnectListDefinedStoragePools/2,
        virConnectListDomains/1, virConnectListDomains/2,
        virConnectListInterfaces/1, virConnectListInterfaces/2,
        virConnectListNetworks/1, virConnectListNetworks/2,
        virConnectListNWFilters/1, virConnectListNWFilters/2,
        virConnectListSecrets/1, virConnectListSecrets/2,
        virConnectListStoragePools/1, virConnectListStoragePools/2,
        virConnectNumOfDefinedDomains/1,
        virConnectNumOfDefinedInterfaces/1,
        virConnectNumOfDefinedNetworks/1,
        virConnectNumOfDefinedStoragePools/1,
        virConnectNumOfDomains/1,
        virConnectNumOfInterfaces/1,
        virConnectNumOfNetworks/1,
        virConnectNumOfNWFilters/1,
        virConnectNumOfSecrets/1,
        virConnectNumOfStoragePools/1,
        virConnectOpen/1,
        virConnectOpenReadOnly/1,

        virSecretDefineXML/2,
        virSecretGetUsageID/1,
        virSecretGetUsageType/1,
        virSecretGetUUID/1,
        virSecretGetUUIDString/1,
        virSecretGetValue/1, virSecretGetValue/2,
        virSecretGetXMLDesc/1, virSecretGetXMLDesc/2,
        virSecretLookupByUsage/3,
        virSecretLookupByUUID/2,
        virSecretLookupByUUIDString/2,
        virSecretSetValue/2,
        virSecretUndefine/1,

        virStoragePoolBuild/1, virStoragePoolBuild/2,
        virStoragePoolCreate/1, virStoragePoolCreate/2,
        virStoragePoolCreateXML/3,
        virStoragePoolDefineXML/2, virStoragePoolDefineXML/3,
        virStoragePoolDelete/2,
        virStoragePoolDestroy/1,
        virStoragePoolGetAutostart/1,
        virStoragePoolGetInfo/1,
        virStoragePoolGetName/1,
        virStoragePoolGetUUID/1,
        virStoragePoolGetUUIDString/1,
        virStoragePoolGetXMLDesc/2,
        virStoragePoolIsActive/1,
        virStoragePoolIsPersistent/1,
        virStoragePoolListVolumes/1, virStoragePoolListVolumes/2,
        virStoragePoolNumOfVolumes/1,
        virStoragePoolLookupByName/2,
        virStoragePoolLookupByUUID/2,
        virStoragePoolLookupByUUIDString/2,
        virStoragePoolLookupByVolume/1,
        virStoragePoolRefresh/1, virStoragePoolRefresh/2,
        virStoragePoolSetAutostart/2,
        virStoragePoolUndefine/1,

        virStorageVolCreateXML/2, virStorageVolCreateXML/3,
        virStorageVolCreateXMLFrom/3, virStorageVolCreateXMLFrom/4,
        virStorageVolDelete/1, virStorageVolDelete/2,
        virStorageVolDownload/4, virStorageVolDownload/5,
        virStorageVolGetInfo/1,
        virStorageVolGetKey/1,
        virStorageVolGetName/1,
        virStorageVolGetPath/1,
        virStorageVolGetXMLDesc/1, virStorageVolGetXMLDesc/2,
        virStorageVolLookupByKey/2,
        virStorageVolLookupByName/2,
        virStorageVolLookupByPath/2,
        virStorageVolResize/2, virStorageVolResize/3,
        virStorageVolUpload/4, virStorageVolUpload/5,
        virStorageVolWipe/1, virStorageVolWipe/2,
        virStorageVolWipePattern/2, virStorageVolWipePattern/3,

        virStreamAbort/1,
        virStreamFinish/1,
        virStreamNew/1, virStreamNew/2,
        virStreamRecv/2,
        virStreamSend/2

    ]).
%-export([
%        cast/2, cast/3, cast/4,
%        call/2, call/3
%    ]).

-on_load(on_load/0).

on_load() ->
    case erlang:system_info(smp_support) of
        true ->
            erlang:load_nif(niflib(), []);
        false ->
            {error, "Requires smp support (-smp flag to enable)"}
    end.


%%-------------------------------------------------------------------------
%%% Stream
%%-------------------------------------------------------------------------

%virStreamSendAll(Stream, Handler, Opaque) ->
virStreamSend(#resource{type = stream, res = Res}, Data) ->
    call(virStreamSend, [Res, Data]).
%virStreamRecvAll(Stream, Handler, Opaque) ->
virStreamRecv(#resource{type = stream, res = Res}, Nbytes) ->
    call(virStreamRecv, [Res, Nbytes]).

virStreamNew(#resource{type = connect, res = Res}) ->
    call(virStreamNew, [Res, ?VIR_STREAM_NONBLOCK]).
virStreamNew(#resource{type = connect, res = Res}, Flags) ->
    call(virStreamNew, [Res, Flags]).

virStreamFinish(#resource{type = stream, res = Res}) ->
    call(virStreamFinish, [Res]).

%virStreamEventUpdateCallback(Stream, Events) ->
%virStreamEventRemoveCallback(Stream) ->
%virStreamEventAddCallback(Stream, Events, Cb, Opaque, Ff) ->

virStreamAbort(#resource{type = stream, res = Res}) ->
    call(virStreamAbort, [Res]).


%%-------------------------------------------------------------------------
%%% Storage Volume
%%-------------------------------------------------------------------------

virStorageVolWipe(Vol) ->
    virStorageVolWipe(Vol, 0).
virStorageVolWipe(#resource{type = storagevol, res = Res}, Flags) ->
    call(virStorageVolWipe, [Res, Flags]).

virStorageVolWipePattern(Vol, Alg) ->
    virStorageVolWipePattern(Vol, Alg, 0).
virStorageVolWipePattern(#resource{type = storagevol, res = Res}, Alg, Flags) ->
    call(virStorageVolWipePattern, [Res, Alg, Flags]).

virStorageVolResize(Res, Capacity) ->
    virStorageVolResize(Res, Capacity, 0).
virStorageVolResize(#resource{type = storagevol, res = Res},
                    Capacity, Flags) ->
    call(virStorageVolResize, [Res, Capacity, Flags]).

virStorageVolDownload(Res, Stream, Offset, Length) ->
    virStorageVolDownload(Res, Stream, Offset, Length, 0).
virStorageVolDownload(#resource{type = storagevol, res = Res},
                    #resource{type = stream, res = Stream},
                    Offset, Length, Flags) ->
    call(virStorageVolDownload, [Res, Stream, Offset, Length, Flags]).

virStorageVolUpload(Res, Stream, Offset, Length) ->
    virStorageVolUpload(Res, Stream, Offset, Length, 0).
virStorageVolUpload(#resource{type = storagevol, res = Res},
                    #resource{type = stream, res = Stream},
                    Offset, Length, Flags) ->
    call(virStorageVolUpload, [Res, Stream, Offset, Length, Flags]).

virStorageVolLookupByPath(#resource{type = connect, res = Res}, Path) ->
    call(virStorageVolLookupByPath, [Res, Path]).

virStorageVolLookupByName(#resource{type = storagepool, res = Res}, Name) ->
    call(virStorageVolLookupByName, [Res, Name]).

virStorageVolLookupByKey(#resource{type = connect, res = Res}, Key) ->
    call(virStorageVolLookupByKey, [Res, Key]).

virStorageVolGetXMLDesc(Vol) ->
    virStorageVolGetXMLDesc(Vol, 0).
virStorageVolGetXMLDesc(#resource{type = storagevol, res = Res}, Flags) ->
    call(virStorageVolGetXMLDesc, [Res, Flags]).

virStorageVolGetPath(#resource{type = storagevol, res = Res}) ->
    call(virStorageVolGetPath, [Res]).

virStorageVolGetName(#resource{type = storagevol, res = Res}) ->
    call(virStorageVolGetName, [Res]).

virStorageVolGetKey(#resource{type = storagevol, res = Res}) ->
    call(virStorageVolGetKey, [Res]).

virStorageVolGetInfo(#resource{type = storagevol, res = Res}) ->
    Pad = wordalign(4)*8,
    case call(virStorageVolGetInfo, [Res]) of
        {ok, <<?INT32(Type),            % virStorageVolType flags
               0:Pad,
               ?UINT64(Capacity),       % Logical size bytes
               ?UINT64(Allocation)      % Current allocation bytes
             >>} ->
            {ok, #storagevol_info{
                    type = Type,
                    capacity = Capacity,
                    allocation = Allocation
                    }};
        Error ->
            Error
    end.

virStorageVolDelete(Res) ->
    virStorageVolDelete(Res, 0).
virStorageVolDelete(#resource{type = storagevol, res = Res}, Flags) ->
    call(virStorageVolDelete, [Res, Flags]).

virStorageVolCreateXMLFrom(Pool, XML, Clonevol) ->
    virStorageVolCreateXMLFrom(Pool, XML, Clonevol, 0).
virStorageVolCreateXMLFrom(#resource{type = storagepool, res = Res},
                           XML, #resource{type = storagevol, res = Vol}, Flags) ->
    call(virStorageVolCreateXMLFrom, [Res, XML, Vol, Flags]).

virStorageVolCreateXML(Res, XML) ->
    virStorageVolCreateXML(Res, XML, 0).
virStorageVolCreateXML(#resource{type = storagepool, res = Res}, XML, Flags) ->
    call(virStorageVolCreateXML, [Res, XML, Flags]).


%%-------------------------------------------------------------------------
%%% Storage Pool
%%-------------------------------------------------------------------------

virStoragePoolUndefine(#resource{type = storagepool, res = Res}) ->
    ok(call(virStoragePoolUndefine, [Res])).

virStoragePoolSetAutostart(#resource{type = storagepool, res = Res}, Autostart) ->
    call(virStoragePoolSetAutostart, [Res, Autostart]).

virStoragePoolRefresh(Res) ->
    virStoragePoolRefresh(Res, 0).
virStoragePoolRefresh(#resource{type = storagepool, res = Res}, Flags) ->
    call(virStoragePoolRefresh, [Res, Flags]).

virStoragePoolNumOfVolumes(#resource{type = storagepool, res = Res}) ->
    call(virStoragePoolNumOfVolumes, [Res]).

virStoragePoolLookupByVolume(#resource{type = storagevol, res = Res}) ->
    call(virStoragePoolLookupByVolume, [Res]).

virStoragePoolLookupByUUIDString(#resource{type = connect, res = Res}, UUID) ->
    call(virStoragePoolLookupByUUIDString, [Res, UUID]).
virStoragePoolLookupByUUID(#resource{type = connect, res = Res}, UUID) ->
    call(virStoragePoolLookupByUUID, [Res, UUID]).
virStoragePoolLookupByName(#resource{type = connect, res = Res}, Name) ->
    call(virStoragePoolLookupByName, [Res, Name]).

virStoragePoolListVolumes(Res) ->
    {ok, Maxnames} = virStoragePoolNumOfVolumes(Res),
    virStoragePoolListVolumes(Res, Maxnames).
virStoragePoolListVolumes(#resource{type = storagepool}, 0) ->
    {ok, []};
virStoragePoolListVolumes(#resource{type = storagepool, res = Res}, Maxnames) ->
    call(virStoragePoolListVolumes, [Res, Maxnames]).

virStoragePoolIsPersistent(#resource{type = storagepool, res = Res}) ->
    bool(call(virStoragePoolIsPersistent, [Res])).

virStoragePoolIsActive(#resource{type = storagepool, res = Res}) ->
    bool(call(virStoragePoolIsActive, [Res])).

virStoragePoolGetXMLDesc(#resource{type = storagepool, res = Res}, Flags) ->
    call(virStoragePoolGetXMLDesc, [Res, Flags]).

virStoragePoolGetUUIDString(#resource{type = storagepool, res = Res}) ->
    call(virStoragePoolGetUUIDString, [Res]).

virStoragePoolGetUUID(#resource{type = storagepool, res = Res}) ->
    call(virStoragePoolGetUUID, [Res]).

virStoragePoolGetName(#resource{type = storagepool, res = Res}) ->
    call(virStoragePoolGetName, [Res]).

virStoragePoolGetInfo(#resource{type = storagepool, res = Res}) ->
    Pad = wordalign(4)*8,
    case call(virStoragePoolGetInfo, [Res]) of
        {ok, <<?INT32(State),       % virStoragePoolState flags
               0:Pad,
               ?UINT64(Capacity),   % Logical size bytes
               ?UINT64(Allocation), % Current allocation bytes
               ?UINT64(Available)   % Remaining free space bytes
             >>} ->
            {ok, #storagepool_info{
                    state = State,
                    capacity = Capacity,
                    allocation = Allocation,
                    available = Available
                    }};
        {error, _} = Error ->
            Error
    end.

virStoragePoolGetAutostart(#resource{type = storagepool, res = Res}) ->
    call(virStoragePoolGetAutostart, [Res]).

virStoragePoolDestroy(#resource{type = storagepool, res = Res}) ->
    ok(call(virStoragePoolDestroy, [Res])).

virStoragePoolDelete(#resource{type = storagepool, res = Res}, Flags) ->
    call(virStoragePoolDelete, [Res, Flags]).

virStoragePoolDefineXML(Res, XML) ->
    virStoragePoolDefineXML(Res, XML, 0).
virStoragePoolDefineXML(#resource{type = connect, res = Res}, XML, Flags) ->
    call(virStoragePoolDefineXML, [Res, XML, Flags]).

virStoragePoolCreateXML(#resource{type = storagepool, res = Res}, XML, Flags) ->
    call(virStoragePoolCreateXML, [Res, XML, Flags]).

virStoragePoolCreate(Res) ->
    virStoragePoolCreate(Res, 0).
virStoragePoolCreate(#resource{type = storagepool, res = Res}, Flags) ->
    call(virStoragePoolCreate, [Res, Flags]).

virStoragePoolBuild(Res) ->
    virStoragePoolBuild(Res, 0).
virStoragePoolBuild(#resource{type = storagepool, res = Res}, Flags) ->
    call(virStoragePoolBuild, [Res, Flags]).


%%-------------------------------------------------------------------------
%%% Secret
%%-------------------------------------------------------------------------

virSecretUndefine(#resource{type = secret, res = Res}) ->
    call(virSecretUndefine, [Res]).

virSecretSetValue(Res, Value) ->
    virSecretSetValue(Res, Value, 0).
virSecretSetValue(#resource{type = secret, res = Res}, Value, Flags) ->
    call(virSecretSetValue, [Res, Value, Flags]).

virSecretLookupByUsage(#resource{type = connect, res = Res}, UsageType, UsageID) ->
    call(virSecretLookupByUsage, [Res, UsageType, UsageID]).

virSecretLookupByUUIDString(#resource{type = connect, res = Res}, UUID) ->
    call(virSecretLookupByUUIDString, [Res, UUID]).

virSecretLookupByUUID(#resource{type = connect, res = Res}, UUID) ->
    call(virSecretLookupByUUID, [Res, UUID]).

virSecretGetXMLDesc(Secret) ->
    virSecretGetXMLDesc(Secret, 0).
virSecretGetXMLDesc(#resource{type = secret, res = Res}, Flags) ->
    call(virSecretGetXMLDesc, [Res, Flags]).

virSecretGetValue(Secret) ->
    virSecretGetValue(Secret, 0).
virSecretGetValue(#resource{type = secret, res = Res}, Flags) ->
    call(virSecretGetValue, [Res, Flags]).

virSecretGetUsageType(#resource{type = secret, res = Res}) ->
    call(virSecretGetUsageType, [Res]).

virSecretGetUsageID(#resource{type = secret, res = Res}) ->
    call(virSecretGetUsageID, [Res]).

virSecretGetUUIDString(#resource{type = secret, res = Res}) ->
    call(virSecretGetUUIDString, [Res]).

virSecretGetUUID(#resource{type = secret, res = Res}) ->
    call(virSecretGetUUID, [Res]).

virSecretDefineXML(Res, XML) ->
    virSecretDefineXML(Res, XML, 0).
virSecretDefineXML(#resource{type = connect, res = Res}, XML, Flags) ->
    call(virSecretDefineXML, [Res, XML, Flags]).


%%-------------------------------------------------------------------------
%%% Node
%%-------------------------------------------------------------------------

%virNodeNumOfDevices(Conn, Cap, Flags) ->
%virNodeListDevices(Conn, Cap, Names, Maxnames, Flags) ->

%% struct _virSecurityModel {
%%  char model[VIR_SECURITY_MODEL_BUFLEN];      /* security model string */
%%  char doi[VIR_SECURITY_DOI_BUFLEN];          /* domain of interpetation */
%% }
virNodeGetSecurityModel(#resource{type = connect, res = Res}) ->
    case call(virNodeGetSecurityModel, [Res]) of
        {ok, <<
            Model:?VIR_SECURITY_MODEL_BUFLEN/native-bytes,
            Doi:?VIR_SECURITY_DOI_BUFLEN/native-bytes
            >>} ->
            {ok, #security_model{
                    model = Model,
                    doi = Doi
                }};
        Error ->
            Error
    end.

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
virNodeGetInfo(#resource{type = connect, res = Res}) ->
    Long = erlang:system_info({wordsize, external}),
    case call(virNodeGetInfo, [Res]) of
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
    end.

virNodeGetFreeMemory(#resource{type = connect, res = Res}) ->
    call(virNodeGetFreeMemory, [Res]).

virNodeGetCellsFreeMemory(#resource{type = connect, res = Res}, MaxCells) ->
    call(virNodeGetFreeMemory, [Res, MaxCells]).


%%-------------------------------------------------------------------------
%%% Node Device
%%-------------------------------------------------------------------------

%virNodeDeviceReset(Dev) ->
%virNodeDeviceReAttach(Dev) ->
%virNodeDeviceNumOfCaps(Dev) ->
%virNodeDeviceLookupByName(Conn, Name) ->
%virNodeDeviceListCaps(Dev, Names, Maxnames) ->
%virNodeDeviceGetXMLDesc(Dev, Flags) ->
%virNodeDeviceGetParent(Dev) ->
%virNodeDeviceGetName(Dev) ->
%virNodeDeviceFree(Dev) ->
%virNodeDeviceDettach(Dev) ->
%virNodeDeviceDestroy(Dev) ->
%virNodeDeviceCreateXML(Conn, XmlDesc, Flags) ->


%%-------------------------------------------------------------------------
%%% Network
%%-------------------------------------------------------------------------

virNetworkUndefine(#resource{type = network, res = Res}) ->
    call(virNetworkUndefine, [Res]).

virNetworkSetAutostart(#resource{type = network, res = Res}, Autostart)
   when is_integer(Autostart) ->
    call(virNetworkSetAutostart, [Res, Autostart]).

virNetworkLookupByUUIDString(#resource{type = connect, res = Res}, Uuidstr)
    when is_binary(Uuidstr); is_list(Uuidstr) ->
    call(virNetworkLookupByUUIDString, [Res, Uuidstr]).

virNetworkLookupByUUID(#resource{type = connect, res = Res}, Uuid) when is_binary(Uuid);
    is_list(Uuid) ->
    call(virNetworkLookupByUUID, [Res, Uuid]).

virNetworkLookupByName(#resource{type = connect, res = Res}, Name) ->
    call(virNetworkLookupByName, [Res, Name]).

virNetworkIsPersistent(#resource{type = network, res = Res}) ->
    bool(call(virNetworkIsPersistent, [Res])).

virNetworkIsActive(#resource{type = network, res = Res}) ->
    bool(call(virNetworkIsActive, [Res])).

virNetworkGetXMLDesc(Res) ->
    virNetworkGetXMLDesc(Res, 0).
virNetworkGetXMLDesc(#resource{type = network, res = Res}, Flags)
    when is_integer(Flags) ->
    call(virNetworkGetXMLDesc, [Res, Flags]).

virNetworkGetUUIDString(#resource{type = network, res = Res}) ->
    call(virNetworkGetUUIDString, [Res]).

virNetworkGetUUID(#resource{type = network, res = Res}) ->
    call(virNetworkGetUUID, [Res]).

virNetworkGetName(#resource{type = network, res = Res}) ->
    call(virNetworkGetName, [Res]).

%virNetworkGetConnect(#resource{type = network, res = Res}) ->

virNetworkGetBridgeName(#resource{type = network, res = Res}) ->
    call(virNetworkGetBridgeName, [Res]).

virNetworkGetAutostart(#resource{type = network, res = Res}) ->
    call(virNetworkGetAutostart, [Res]).

virNetworkDestroy(#resource{type = network, res = Res}) ->
    ok(call(virNetworkDestroy, [Res])).

virNetworkDefineXML(#resource{type = connect, res = Res}, Xml) ->
    call(virNetworkDefineXML, [Res, Xml]).

%virNetworkCreateXML(Conn, XmlDesc) ->

virNetworkCreate(#resource{type = network, res = Res}) ->
    ok(call(virNetworkCreate, [Res])).

%%-------------------------------------------------------------------------
%%% NWFilter
%%-------------------------------------------------------------------------
virNWFilterUndefine(#resource{type = nwfilter, res = Res}) ->
    ok(call(virNWFilterUndefine, [Res])).

virNWFilterLookupByUUIDString(#resource{type = connect, res = Res}, Uuidstr)
    when is_binary(Uuidstr); is_list(Uuidstr) ->
    call(virNWFilterLookupByUUIDString, [Res, Uuidstr]).

virNWFilterLookupByUUID(#resource{type = connect, res = Res}, Uuid) ->
    call(virNWFilterLookupByUUID, [Res, Uuid]).

virNWFilterLookupByName(#resource{type = connect, res = Res}, Name) ->
    call(virNWFilterLookupByName, [Res, Name]).

virNWFilterGetXMLDesc(#resource{type = nwfilter, res = Res}) ->
    call(virNWFilterGetXMLDesc, [Res, 0]).
virNWFilterGetXMLDesc(#resource{type = nwfilter, res = Res}, Flags) ->
    call(virNWFilterGetXMLDesc, [Res, Flags]).

virNWFilterGetUUIDString(#resource{type = nwfilter, res = Res}) ->
    call(virNWFilterGetUUIDString, [Res]).

virNWFilterGetUUID(#resource{type = nwfilter, res = Res}) ->
    call(virNWFilterGetUUID, [Res]).

virNWFilterGetName(#resource{type = nwfilter, res = Res}) ->
    call(virNWFilterGetName, [Res]).

virNWFilterDefineXML(#resource{type = connect, res = Res}, Xml) ->
    call(virNWFilterDefineXML, [Res, Xml]).


%%-------------------------------------------------------------------------
%%% Interface
%%-------------------------------------------------------------------------
virInterfaceUndefine(#resource{type = interface, res = Res}) ->
    bool(call(virInterfaceUndefine, [Res])).

virInterfaceLookupByName(#resource{type = connect, res = Res}, Name)
    when is_list(Name); length(Name) < 128 ->
    call(virInterfaceLookupByName, [Res, Name]).

virInterfaceLookupByMACString(#resource{type = connect, res = Res}, Macstr)
    when is_list(Macstr); length(Macstr) =< 18 ->
    call(virInterfaceLookupByMACString, [Res, Macstr]).

virInterfaceIsActive(#resource{type = interface, res = Res}) ->
    bool(call(virInterfaceIsActive, [Res])).

virInterfaceGetXMLDesc(#resource{type = interface, res = Res}) ->
    virInterfaceGetXMLDesc(Res, 0).
virInterfaceGetXMLDesc(#resource{type = interface, res = Res}, Flags)
    when is_integer(Flags) ->
    call(virInterfaceGetXMLDesc, [Res, Flags]).

virInterfaceGetName(#resource{type = interface, res = Res}) ->
    call(virInterfaceGetName, [Res]).

virInterfaceGetMACString(#resource{type = interface, res = Res}) ->
    call(virInterfaceGetMACString, [Res]).

%virInterfaceGetConnect(#resource{type = interface, res = Res}) ->
%    call(virInterfaceGetConnect, [Res]).

virInterfaceDestroy(#resource{type = interface, res = Res}) ->
    ok(call(virInterfaceDestroy, [Res])).

virInterfaceDefineXML(#resource{type = connect, res = Res}, Xml)
    when is_binary(Xml); is_list(Xml) ->
    call(virInterfaceDefineXML, [Res, Xml]).

virInterfaceCreate(#resource{type = interface, res = Res}) ->
    ok(call(virInterfaceCreate, [Res])).


%%-------------------------------------------------------------------------
%%% Domain
%%-------------------------------------------------------------------------

virDomainUndefine(#resource{type = domain, res = Res}) ->
    ok(call(virDomainUndefine, [Res])).

virDomainSuspend(#resource{type = domain, res = Res}) ->
    ok(call(virDomainSuspend, [Res])).

virDomainShutdown(#resource{type = domain, res = Res}) ->
    ok(call(virDomainShutdown, [Res])).

%virDomainSetVcpus(#resource{type = domain, res = Res}, Nvcpus) ->
%virDomainSetSchedulerParameters(#resource{type = domain, res = Res}, Params, Nparams) ->
%virDomainSetMemory(#resource{type = domain, res = Res}, Memory) ->
%virDomainSetMaxMemory(#resource{type = domain, res = Res}, Memory) ->

virDomainSetAutostart(Res, Autostart) when Autostart =:= true ->
    virDomainSetAutostart(Res, 1);
virDomainSetAutostart(Res, Autostart) when Autostart =:= false ->
    virDomainSetAutostart(Res, 0);
virDomainSetAutostart(#resource{type = domain, res = Res}, Autostart) when is_integer(Autostart) ->
    call(virDomainSetAutostart, [Res, Autostart]).

virDomainSave(#resource{type = domain, res = Res}, To) when is_list(To) ->
    call(virDomainSave, [Res, To]).

virDomainResume(#resource{type = domain, res = Res}) ->
    ok(call(virDomainResume, [Res])).

virDomainRestore(#resource{type = connect, res = Res}, From) ->
    call(virDomainRestore, [Res, From]).

%virDomainReboot(#resource{type = domain, res = Res}, Flags) ->
%virDomainPinVcpu(#resource{type = domain, res = Res}, Vcpu, Cpumap, Maplen) ->
%virDomainMigrateToURI(#resource{type = domain, res = Res}, Duri, Flags, Dname, Bandwidth) ->
%virDomainMigrate(#resource{type = domain, res = Res}, Dconn, Flags, Dname, Uri, Bandwidth) ->
%virDomainMemoryStats(#resource{type = domain, res = Res}, Stats, Nr_stats, Flags) ->
%virDomainMemoryPeek(#resource{type = domain, res = Res}, Start, Size, Buffer, Flags) ->

virDomainLookupByUUIDString(#resource{type = connect, res = Res}, UUID) ->
    call(virDomainLookupByUUIDString, [Res, UUID]).

virDomainLookupByUUID(Res, Uuid) when is_list(Uuid) ->
    virDomainLookupByUUID(Res, list_to_binary(Uuid));
virDomainLookupByUUID(#resource{type = connect, res = Res}, Uuid)
    when byte_size(Uuid) == ?VIR_UUID_BUFLEN  ->
    call(virDomainLookupByUUID, [Res, Uuid]).

virDomainLookupByName(Res, Name) when is_list(Name) ->
    virDomainLookupByName(Res, list_to_binary(Name));
virDomainLookupByName(#resource{type = connect, res = Res}, Name)
    when byte_size(Name) < ?HOST_NAME_MAX ->
    call(virDomainLookupByName, [Res, Name]).

virDomainLookupByID(#resource{type = connect, res = Res}, Id) when is_integer(Id) ->
    call(virDomainLookupByID, [Res, Id]).

virDomainIsPersistent(#resource{type = domain, res = Res}) ->
    bool(call(virDomainIsPersistent, [Res])).
virDomainIsActive(#resource{type = domain, res = Res}) ->
    bool(call(virDomainIsActive, [Res])).
%virDomainInterfaceStats(#resource{type = domain, res = Res}, Path, Stats, Size) ->

virDomainGetXMLDesc(#resource{type = domain, res = Res}, Flags) ->
    call(virDomainGetXMLDesc, [Res, Flags]).

%virDomainGetVcpus(#resource{type = domain, res = Res}, Info, Maxinfo, Cpumaps, Maplen) ->

virDomainGetUUIDString(#resource{type = domain, res = Res}) ->
    call(virDomainGetUUIDString, [Res]).

virDomainGetUUID(#resource{type = domain, res = Res}) ->
    call(virDomainGetUUID, [Res]).

virDomainGetSecurityLabel(#resource{type = domain, res = Res}) ->
    call(virDomainGetSecurityLabel, [Res]).

virDomainGetSchedulerType(#resource{type = domain, res = Res}) ->
    call(virDomainGetSchedulerType, [Res]).

virDomainGetSchedulerParameters(#resource{type = domain, res = Res}) ->
    call(virDomainGetSchedulerParameters, [Res]).

virDomainGetOSType(#resource{type = domain, res = Res}) ->
    call(virDomainGetOSType, [Res]).

virDomainGetName(#resource{type = domain, res = Res}) ->
    call(virDomainGetName, [Res]).

virDomainGetMaxVcpus(#resource{type = domain, res = Res}) ->
    call(virDomainGetMaxVcpus, [Res]).

virDomainGetMemoryParameters(#resource{type = domain, res = Res}) ->
    call(virDomainGetMemoryParameters, [Res]).

virDomainGetMaxMemory(#resource{type = domain, res = Res}) ->
    call(virDomainGetMaxMemory, [Res]).

%% struct virDomainInfo{
%%     unsigned char   state
%%     unsigned long   maxMem (Kb)
%%     unsigned long   memory (Kb)
%%     unsigned short  nrVirtCpu
%%     unsigned long long  cpuTime (nanoseconds)
%% }
virDomainGetInfo(#resource{type = domain, res = Res}) ->
    Long = erlang:system_info({wordsize, external}),
    case call(virDomainGetInfo, [Res]) of
        {ok, <<
            State:8, % _Pad:24,
            MaxMem:Long/native-unsigned-integer-unit:8,
            Memory:Long/native-unsigned-integer-unit:8,
            NrVirtCpu:2/native-unsigned-integer-unit:8, % _Pad1:16,
            CpuTime:8/native-unsigned-integer-unit:8
            >>} ->
            {ok, #domain_info{
                state = state({domain, State}),
                maxmem = MaxMem,
                memory = Memory,
                nrvirtcpu = NrVirtCpu,
                cputime = CpuTime
            }};
        Err ->
            Err
    end.

virDomainGetID(#resource{type = domain, res = Res}) ->
    call(virDomainGetID, [Res]).

virDomainGetJobInfo(#resource{type = domain, res = Res}) ->
    call(virDomainGetJobInfo, [Res]).

%virDomainGetConnect(#resource{type = domain, res = Res}) ->

virDomainGetBlockInfo(Res, Path) when is_list(Path) ->
    virDomainGetBlockInfo(Res, list_to_binary(Path));
virDomainGetBlockInfo(#resource{type = domain, res = Res}, Path)
    when byte_size(Path) < ?MAXPATHLEN ->
    call(virDomainGetBlockInfo, [Res, Path]).

virDomainGetAutostart(#resource{type = domain, res = Res}) ->
    call(virDomainGetAutostart, [Res]).

%virDomainDetachDevice(#resource{type = domain, res = Res}, Xml) ->

virDomainDestroy(#resource{type = domain, res = Res}) ->
    ok(call(virDomainDestroy, [Res])).

virDomainDefineXML(#resource{type = connect, res = Res}, Xml) when is_binary(Xml); is_list(Xml) ->
    call(virDomainDefineXML, [Res, Xml]).

%virDomainCreateXML(Conn, XmlDesc, Flags) ->

virDomainCreate(Domain) ->
    virDomainCreate(Domain, 0).
virDomainCreate(#resource{type = domain, res = Res}, Flags) when is_integer(Flags) ->
    ok(call(virDomainCreate, [Res, Flags])).

%virDomainCoreDump(#resource{type = domain, res = Res}, To, Flags) ->
%virDomainBlockStats(#resource{type = domain, res = Res}, Path, Stats, Size) ->
%virDomainBlockPeek(#resource{type = domain, res = Res}, Path, Offset, Size, Buffer, Flags) ->
%virDomainAttachDevice(#resource{type = domain, res = Res}, Xml) ->

virDomainOpenConsole(Domain, Stream) ->
    virDomainOpenConsole(Domain, <<>>, Stream).
virDomainOpenConsole(#resource{type = domain, res = Res}, Devname,
                     #resource{type = stream, res = Stream}) ->
    call(virDomainOpenConsole, [Res, Devname, Stream]).

virDomainScreenshot(Domain, Stream, Screen) ->
    virDomainScreenshot(Domain, Stream, Screen, 0).
virDomainScreenshot(#resource{type = domain, res = Res},
                    #resource{type = stream, res = Stream}, Screen, Flags) ->
    call(virDomainScreenshot, [Res, Stream, Screen, Flags]).

virDomainSendKey(Domain, Codeset, Holdtime, Keycodes) ->
    virDomainSendKey(Domain, Codeset, Holdtime, Keycodes, 0).
virDomainSendKey(Domain, Codeset, Holdtime, Keycodes, Flags) when is_atom(Codeset) ->
    virDomainSendKey(Domain, keycode(Codeset), Holdtime, Keycodes, Flags);
virDomainSendKey(#resource{type = domain, res = Res}, Codeset,
                    Holdtime, Keycodes, Flags) when is_integer(Codeset), is_list(Keycodes) ->
    call(virDomainSendKey, [Res, Codeset, Holdtime, Keycodes, Flags]).

%%-------------------------------------------------------------------------
%%% Connect
%%-------------------------------------------------------------------------

virConnectOpen(Name) when is_list(Name) ->
    virConnectOpen(list_to_binary(Name));
virConnectOpen(Name) when byte_size(Name) < ?HOST_NAME_MAX ->
    call(virConnectOpen, [Name]).

virConnectOpenReadOnly(Name) when is_list(Name) ->
    call(virConnectOpenReadOnly, [Name]);
virConnectOpenReadOnly(Name) when byte_size(Name) < ?HOST_NAME_MAX ->
    call(virConnectOpenReadOnly, [Name]).

%virConnectOpenAuth(Name, Auth, Flags) ->

virConnectNumOfStoragePools(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfStoragePools, [Res]).

virConnectNumOfSecrets(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfSecrets, [Res]).

virConnectNumOfNetworks(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfNetworks, [Res]).

virConnectNumOfNWFilters(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfNWFilters, [Res]).

virConnectNumOfInterfaces(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfInterfaces, [Res]).

virConnectNumOfDomains(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfDomains, [Res]).

virConnectNumOfDefinedStoragePools(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfDefinedStoragePools, [Res]).

virConnectNumOfDefinedNetworks(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfDefinedNetworks, [Res]).

virConnectNumOfDefinedInterfaces(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfDefinedInterfaces, [Res]).

virConnectNumOfDefinedDomains(#resource{type = connect, res = Res}) ->
    call(virConnectNumOfDefinedDomains, [Res]).

virConnectListStoragePools(Res) ->
    {ok, Maxnames} = virConnectNumOfStoragePools(Res),
    virConnectListStoragePools(Res, Maxnames).
virConnectListStoragePools(_, 0) ->
    {ok, []};
virConnectListStoragePools(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListStoragePools, [Res, Maxnames]).

virConnectListSecrets(Res) ->
    {ok, Maxuuids} = virConnectNumOfSecrets(Res),
    virConnectListSecrets(Res, Maxuuids).
virConnectListSecrets(_, 0) ->
    {ok, []};
virConnectListSecrets(#resource{type = connect, res = Res}, Maxuuids) when is_integer(Maxuuids) ->
    call(virConnectListSecrets, [Res, Maxuuids]).

virConnectListNetworks(Res) ->
    {ok, Maxnames} = virConnectNumOfNetworks(Res),
    virConnectListNetworks(Res, Maxnames).
virConnectListNetworks(_, 0) ->
    {ok, []};
virConnectListNetworks(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListNetworks, [Res, Maxnames]).

virConnectListNWFilters(Res) ->
    {ok, Maxnames} = virConnectNumOfNWFilters(Res),
    virConnectListNWFilters(Res, Maxnames).
virConnectListNWFilters(_, 0) ->
    {ok, []};
virConnectListNWFilters(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListNWFilters, [Res, Maxnames]).

virConnectListInterfaces(Res) ->
    {ok, Maxnames} = virConnectNumOfInterfaces(Res),
    virConnectListInterfaces(Res, Maxnames).
virConnectListInterfaces(_, 0) ->
    {ok, []};
virConnectListInterfaces(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListInterfaces, [Res, Maxnames]).

virConnectListDomains(Res) ->
    {ok, Maxids} = virConnectNumOfDomains(Res),
    virConnectListDomains(Res, Maxids).
virConnectListDomains(_, 0) ->
    {ok, []};
virConnectListDomains(#resource{type = connect, res = Res}, Maxids) when is_integer(Maxids) ->
    call(virConnectListDomains, [Res, Maxids]).

virConnectListDefinedStoragePools(Res) ->
    {ok, Maxnames} = virConnectNumOfDefinedStoragePools(Res),
    virConnectListDefinedStoragePools(Res, Maxnames).
virConnectListDefinedStoragePools(_, 0) ->
    {ok, []};
virConnectListDefinedStoragePools(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListDefinedStoragePools, [Res, Maxnames]).

virConnectListDefinedNetworks(Res) ->
    {ok, Maxnames} = virConnectNumOfDefinedNetworks(Res),
    virConnectListDefinedNetworks(Res, Maxnames).
virConnectListDefinedNetworks(_, 0) ->
    {ok, []};
virConnectListDefinedNetworks(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListDefinedNetworks, [Res, Maxnames]).

virConnectListDefinedInterfaces(Res) ->
    {ok, Maxnames} = virConnectNumOfDefinedInterfaces(Res),
    virConnectListDefinedInterfaces(Res, Maxnames).
virConnectListDefinedInterfaces(_, 0) ->
    {ok, []};
virConnectListDefinedInterfaces(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListDefinedInterfaces, [Res, Maxnames]).

virConnectListDefinedDomains(Res) ->
    {ok, Maxnames} = virConnectNumOfDefinedDomains(Res),
    virConnectListDefinedDomains(Res, Maxnames).
virConnectListDefinedDomains(_, 0) ->
    {ok, []};
virConnectListDefinedDomains(#resource{type = connect, res = Res}, Maxnames) when is_integer(Maxnames) ->
    call(virConnectListDefinedDomains, [Res, Maxnames]).

virConnectIsSecure(#resource{type = connect, res = Res}) ->
    bool(call(virConnectIsSecure, [Res])).

virConnectIsEncrypted(#resource{type = connect, res = Res}) ->
    bool(call(virConnectIsEncrypted, [Res])).

virConnectGetVersion(#resource{type = connect, res = Res}) ->
    case call(virConnectGetVersion, [Res]) of
        {ok, Version} ->
            {ok, version(Version)};
        Err ->
            Err
    end.

virConnectGetURI(#resource{type = connect, res = Res}) ->
    call(virConnectGetURI, [Res]).

virConnectGetType(#resource{type = connect, res = Res}) ->
    call(virConnectGetType, [Res]).

virConnectGetMaxVcpus(Res, Type) when is_list(Type) ->
    virConnectGetMaxVcpus(Res, list_to_binary(Type));
virConnectGetMaxVcpus(#resource{type = connect, res = Res}, Type)
    when byte_size(Type) < 1024 ->
    call(virConnectGetMaxVcpus, [Res, Type]).

virConnectGetLibVersion(#resource{type = connect, res = Res}) ->
    case call(virConnectGetLibVersion, [Res]) of
        {ok, Version} ->
            {ok, version(Version)};
        Error ->
            Error
    end.

virConnectGetHostname(#resource{type = connect, res = Res}) ->
    call(virConnectGetHostname, [Res]).

virConnectGetCapabilities(#resource{type = connect, res = Res}) ->
    call(virConnectGetCapabilities, [Res]).

virConnectFindStoragePoolSources(Conn, Type, SrcSpec) ->
    virConnectFindStoragePoolSources(Conn, Type, SrcSpec, 0).
virConnectFindStoragePoolSources(#resource{type = connect, res = Res}, Type, SrcSpec, Flags) ->
    call(virConnectFindStoragePoolSources, [Res, Type, SrcSpec, Flags]).

%virConnectDomainXMLToNative(Conn, NativeFormat, DomainXml, Flags) ->
%virConnectDomainXMLFromNative(Conn, NativeFormat, NativeConfig, Flags) ->

%virConnectCompareCPU(Conn, XmlDesc, Flags) ->

%virGetVersion(LibVer, Type, TypeVer) ->

%%-------------------------------------------------------------------------
%%% Not for use in language bindings
%%-------------------------------------------------------------------------
%#%virDomainRef(Domain)
%#%virNetworkRef(Network)
%#%virNodeDeviceRef(Dev)
%#%virResetError(Err)
%#%virResetLastError()
%#%virSaveLastError()
%#%virSecretRef(Secret)
%#%virStoragePoolRef(Pool)
%#%virStorageVolRef(Vol)
%#%virStreamRef(Stream)

%#%virCopyLastError(To)
%#%virDefaultErrorFunc(Err)
%#%virEventRegisterImpl(AddHandle, UpdateHandle, RemoveHandle, AddTimeout, UpdateTimeout, RemoveTimeout)
%#%virFreeError(Err)
%#%virGetLastError()
%#%virInitialize()
%#%virInterfaceRef(Iface)

%#%virConnectDomainEventRegister(Conn, Cb, Opaque, Freecb)
%#%virConnectDomainEventDeregister(Conn, Cb)

%#%virConnSetErrorFunc(Conn, UserData, Handler)
%#%virConnResetLastError(Conn)
%#%virConnGetLastError(Conn)
%#%virConnCopyLastError(Conn, To)
%#%virSetErrorFunc(UserData, Handler)

%#%virNetworkFree(Network)
%#%virInterfaceFree(Iface)
%#%virDomainFree(#resource{type = domain, res = Res})
%#%virConnectRef(Conn)
%#%virDomainCreateLinux(Conn, XmlDesc, Flags)

% Called by virConnectPtr dtor
%#%virConnectClose(#resource{type = connect, res = Res})

%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------

% Lookup a resource
lookup(Connect, {domain, Name}) ->
    Fun = [ fun() -> vert:virDomainLookupByID(Connect, list_to_integer(Name)) end,
            fun() -> vert:virDomainLookupByUUIDString(Connect, Name) end,
            fun() -> vert:virDomainLookupByName(Connect, Name) end ],
    lookup_1(Fun);
lookup(Connect, {network, Name}) ->
    Fun = [ fun() -> vert:virNetworkLookupByUUIDString(Connect, Name) end,
            fun() -> vert:virNetworkLookupByName(Connect, Name) end ],
    lookup_1(Fun);
lookup(Connect, {nwfilter, Name}) ->
    Fun = [ fun() -> vert:virNWFilterLookupByUUIDString(Connect, Name) end,
            fun() -> vert:virNWFilterLookupByName(Connect, Name) end ],
    lookup_1(Fun);
lookup(Connect, {secret, Name}) ->
    Fun = [ fun() -> vert:virSecretLookupByUUIDString(Connect, Name) end ],
    lookup_1(Fun);
lookup(Connect, {storagepool, Name}) ->
    Fun = [ fun() -> vert:virStoragePoolLookupByUUIDString(Connect, Name) end,
            fun() -> vert:virStoragePoolLookupByName(Connect, Name) end ],
    lookup_1(Fun);
lookup(Connect, {storagevol, Name}) ->
    Fun = [ fun() -> vert:virStorageVolLookupByKey(Connect, Name) end,
            fun() -> vert:virStorageVolLookupByPath(Connect, Name) end ],
    lookup_1(Fun).

lookup_1(Fun)  ->
    lookup_1(Fun, []).
lookup_1([], [{error, Error}|_]) ->
    {error, Error};
lookup_1([Fun|Tail], Acc) ->
    try Fun() of
        {ok, Res} ->
            {ok, Res};
        {error, Error} ->
            lookup_1(Tail, [{error, Error}|Acc])
    catch
        _:_ ->
            lookup_1(Tail, Acc)
    end.

%%-------------------------------------------------------------------------
%%% NIF stubs
%%-------------------------------------------------------------------------
cast_2(Fun, [Arg1]) ->
    cast(Fun, Arg1);
cast_2(Fun, [Arg1, Arg2]) ->
    cast(Fun, Arg1, Arg2);
cast_2(Fun, [Arg1, Arg2, Arg3]) ->
    cast(Fun, Arg1, Arg2, Arg3);
cast_2(Fun, [Arg1, Arg2, Arg3, Arg4]) ->
    cast(Fun, Arg1, Arg2, Arg3, Arg4);
cast_2(Fun, [Arg1, Arg2, Arg3, Arg4, Arg5]) ->
    cast(Fun, Arg1, Arg2, Arg3, Arg4, Arg5).

cast(_,_) ->
    erlang:error(not_implemented).
cast(_,_,_) ->
    erlang:error(not_implemented).
cast(_,_,_,_) ->
    erlang:error(not_implemented).
cast(_,_,_,_,_) ->
    erlang:error(not_implemented).
cast(_,_,_,_,_,_) ->
    erlang:error(not_implemented).

%%-------------------------------------------------------------------------
%%% Blocking API
%%-------------------------------------------------------------------------
call(Fun, Arg) ->
    % Result is returned from the NIF
    % function, so not a tagged tuple
    case cast_2(Fun, Arg) of
        ok ->
            block();
        Error ->
            Error
    end.

block() ->
    % Message sent by the background thread
    receive
        {vert, Response} ->
            Response
    end.


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
version(Version) when is_integer(Version) ->
    Major = Version div 1000000,
    Minor = Version rem 1000000 div 1000,
    Release = Version rem 1000000 rem 1000,
    {Major, Minor, Release}.

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

keycode(linux) -> ?VIR_KEYCODE_SET_LINUX;
keycode(xt) -> ?VIR_KEYCODE_SET_XT;
keycode(atset1) -> ?VIR_KEYCODE_SET_ATSET1;
keycode(atset2) -> ?VIR_KEYCODE_SET_ATSET2;
keycode(atset3) -> ?VIR_KEYCODE_SET_ATSET3;
keycode(osx) -> ?VIR_KEYCODE_SET_OSX;
keycode(xt_kbd) -> ?VIR_KEYCODE_SET_XT_KBD;
keycode(usb) -> ?VIR_KEYCODE_SET_USB;
keycode(win32) -> ?VIR_KEYCODE_SET_WIN32;
keycode(rfb) -> ?VIR_KEYCODE_SET_RFB;
keycode(last) -> ?VIR_KEYCODE_SET_LAST.

bool({ok, 0}) -> false;
bool({ok, 1}) -> true.

ok({ok, _}) -> ok;
ok(Error) -> Error.

progname_ebin() ->
    filename:join([
        filename:dirname(code:which(?MODULE)),
        "..", "priv", ?MODULE
    ]).

progname_priv() ->
    case application:get_env(?MODULE, port_executable) of
        {ok, Executable} -> Executable;
        undefined -> filename:join([
                        code:priv_dir(?MODULE),
                        ?MODULE
                     ])
    end.

niflib() ->
    case code:priv_dir(?MODULE) of
        {error, bad_name} -> progname_ebin();
        _ -> progname_priv()
    end.

wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.
