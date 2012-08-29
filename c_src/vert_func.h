/* Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

ErlNifFunc vert_funcs[] = {
    /* connect */
    {"virConnectFindStoragePoolSources", 3, vert_virConnectFindStoragePoolSources},
    {"virConnectGetCapabilities", 1, vert_virConnectGetCapabilities},
    {"virConnectGetHostname", 1, vert_virConnectGetHostname},
    {"virConnectGetLibVersion", 1, vert_virConnectGetLibVersion},
    {"virConnectGetMaxVcpus", 2, vert_virConnectGetMaxVcpus},
    {"virConnectGetType", 1, vert_virConnectGetType},
    {"virConnectGetURI", 1, vert_virConnectGetURI},
    {"virConnectGetVersion", 1, vert_virConnectGetVersion},
    {"virConnectIsEncrypted", 1, vert_virConnectIsEncrypted},
    {"virConnectIsSecure", 1, vert_virConnectIsSecure},
    {"virConnectListDefinedDomains", 2, vert_virConnectListDefinedDomains},
    {"virConnectListDefinedInterfaces", 2, vert_virConnectListDefinedInterfaces},
    {"virConnectListDefinedNetworks", 2, vert_virConnectListDefinedNetworks},
    {"virConnectListDefinedStoragePools", 2, vert_virConnectListDefinedStoragePools},
    {"virConnectListDomains", 2, vert_virConnectListDomains},
    {"virConnectListInterfaces", 2, vert_virConnectListInterfaces},
    {"virConnectListNetworks", 2, vert_virConnectListNetworks},
    {"virConnectListNWFilters", 2, vert_virConnectListNWFilters},
    {"virConnectListSecrets", 2, vert_virConnectListSecrets},
    {"virConnectListStoragePools", 2, vert_virConnectListStoragePools},
    {"virConnectNumOfDefinedDomains", 1, vert_virConnectNumOfDefinedDomains},
    {"virConnectNumOfDefinedInterfaces", 2, vert_virConnectNumOfDefinedInterfaces},
    {"virConnectNumOfDefinedNetworks", 1, vert_virConnectNumOfDefinedNetworks},
    {"virConnectNumOfDefinedStoragePools", 1, vert_virConnectNumOfDefinedStoragePools},
    {"virConnectNumOfDomains", 1, vert_virConnectNumOfDomains},
    {"virConnectNumOfInterfaces", 1, vert_virConnectNumOfInterfaces},
    {"virConnectNumOfNetworks", 1, vert_virConnectNumOfNetworks},
    {"virConnectNumOfNWFilters", 1, vert_virConnectNumOfNWFilters},
    {"virConnectNumOfSecrets", 1, vert_virConnectNumOfSecrets},
    {"virConnectNumOfStoragePools", 1, vert_virConnectNumOfStoragePools},
    {"virConnectOpen", 1, vert_virConnectOpen},
    {"virConnectOpenReadOnly", 1, vert_virConnectOpenReadOnly},
    {"virNodeGetCellsFreeMemory", 2, vert_virNodeGetCellsFreeMemory},
    {"virNodeGetFreeMemory", 1, vert_virNodeGetFreeMemory},
    {"virNodeGetInfo", 1, vert_virNodeGetInfo},
    {"virNodeGetSecurityModel", 1, vert_virNodeGetSecurityModel},

    /* domain */
    {"virDomainCreate", 2, vert_virDomainCreate},
    {"virDomainDefineXML", 2, vert_virDomainDefineXML},
    {"virDomainDestroy", 1, vert_virDomainDestroy},
    {"virDomainGetAutostart", 1, vert_virDomainGetAutostart},
    {"virDomainGetBlockInfo", 2, vert_virDomainGetBlockInfo},
    {"virDomainGetID", 1, vert_virDomainGetID},
    {"virDomainGetInfo", 1, vert_virDomainGetInfo},
    {"virDomainGetJobInfo", 1, vert_virDomainGetJobInfo},
    {"virDomainGetMaxMemory", 1, vert_virDomainGetMaxMemory},
    {"virDomainGetMaxVcpus", 1, vert_virDomainGetMaxVcpus},
    {"virDomainGetMemoryParameters", 1, vert_virDomainGetMemoryParameters},
    {"virDomainGetName", 1, vert_virDomainGetName},
    {"virDomainGetOSType", 1, vert_virDomainGetOSType},
    {"virDomainGetSchedulerParameters", 1, vert_virDomainGetSchedulerParameters},
    {"virDomainGetSchedulerType", 1, vert_virDomainGetSchedulerType},
    {"virDomainGetSecurityLabel", 1, vert_virDomainGetSecurityLabel},
    {"virDomainGetUUID", 1, vert_virDomainGetUUID},
    {"virDomainGetUUIDString", 1, vert_virDomainGetUUIDString},
    {"virDomainGetXMLDesc", 2, vert_virDomainGetXMLDesc},
    {"virDomainLookupByID", 2, vert_virDomainLookupByID},
    {"virDomainLookupByName", 2, vert_virDomainLookupByName},
    {"virDomainLookupByUUID", 2, vert_virDomainLookupByUUID},
    {"virDomainLookupByUUIDString", 2, vert_virDomainLookupByUUIDString},
    {"virDomainOpenConsole", 3, vert_virDomainOpenConsole},
    {"virDomainRestore", 2, vert_virDomainRestore},
    {"virDomainResume", 1, vert_virDomainResume},
    {"virDomainSave", 2, vert_virDomainSave},
    {"virDomainScreenshot", 4, vert_virDomainScreenshot},
    {"virDomainSendKey", 5, vert_virDomainSendKey},
    {"virDomainSetAutostart", 2, vert_virDomainSetAutostart},
    {"virDomainShutdown", 1, vert_virDomainShutdown},
    {"virDomainSuspend", 1, vert_virDomainSuspend},
    {"virDomainUndefine", 1, vert_virDomainUndefine},

    /* interface */
    {"virInterfaceCreate", 1, vert_virInterfaceCreate},
    {"virInterfaceDefineXML", 2, vert_virInterfaceDefineXML},
    {"virInterfaceDestroy", 1, vert_virInterfaceDestroy},
    {"virInterfaceGetMACString", 1, vert_virInterfaceGetMACString},
    {"virInterfaceGetName", 1, vert_virInterfaceGetName},
    {"virInterfaceGetXMLDesc", 1, vert_virInterfaceGetXMLDesc},
    {"virInterfaceLookupByMACString", 2, vert_virInterfaceLookupByMACString},
    {"virInterfaceLookupByName", 2, vert_virInterfaceLookupByName},
    {"virInterfaceUndefine", 1, vert_virInterfaceUndefine},

    /* network */
    {"virNetworkCreate", 1, vert_virNetworkCreate},
    {"virNetworkDefineXML", 2, vert_virNetworkDefineXML},
    {"virNetworkDestroy", 1, vert_virNetworkDestroy},
    {"virNetworkGetAutostart", 1, vert_virNetworkGetAutostart},
    {"virNetworkGetAutostart", 1, vert_virNetworkGetAutostart},
    {"virNetworkGetBridgeName", 1, vert_virNetworkGetBridgeName},
    {"virNetworkGetName", 1, vert_virNetworkGetName},
    {"virNetworkGetUUID", 1, vert_virNetworkGetUUID},
    {"virNetworkGetUUIDString", 1, vert_virNetworkGetUUIDString},
    {"virNetworkGetXMLDesc", 2, vert_virNetworkGetXMLDesc},
    {"virNetworkIsPersistent", 1, vert_virNetworkIsPersistent},
    {"virNetworkLookupByName", 2, vert_virNetworkLookupByName},
    {"virNetworkLookupByUUID", 2, vert_virNetworkLookupByUUID},
    {"virNetworkLookupByUUIDString", 2, vert_virNetworkLookupByUUIDString},
    {"virNetworkSetAutostart", 2, vert_virNetworkSetAutostart},
    {"virNetworkUndefine", 1, vert_virNetworkUndefine},

    /* nwfilter */
    {"virNWFilterDefineXML", 2, vert_virNWFilterDefineXML},
    {"virNWFilterGetName", 1, vert_virNWFilterGetName},
    {"virNWFilterGetUUID", 1, vert_virNWFilterGetUUID},
    {"virNWFilterGetUUIDString", 1, vert_virNWFilterGetUUIDString},
    {"virNWFilterGetXMLDesc", 2, vert_virNWFilterGetXMLDesc},
    {"virNWFilterLookupByName", 2, vert_virNWFilterLookupByName},
    {"virNWFilterLookupByUUID", 2, vert_virNWFilterLookupByUUID},
    {"virNWFilterLookupByUUIDString", 2, vert_virNWFilterLookupByUUIDString},
    {"virNWFilterUndefine", 1, vert_virNWFilterUndefine},

    /* storagepool */
    {"virStoragePoolBuild", 2, vert_virStoragePoolBuild},
    {"virStoragePoolCreate", 2, vert_virStoragePoolCreate},
    {"virStoragePoolCreateXML", 3, vert_virStoragePoolCreateXML},
    {"virStoragePoolDefineXML", 3, vert_virStoragePoolDefineXML},
    {"virStoragePoolDelete", 2, vert_virStoragePoolDelete},
    {"virStoragePoolDestroy", 1, vert_virStoragePoolDestroy},
    {"virStoragePoolGetAutostart", 1, vert_virStoragePoolGetAutostart},
    {"virStoragePoolGetInfo", 1, vert_virStoragePoolGetInfo},
    {"virStoragePoolGetName", 1, vert_virStoragePoolGetName},
    {"virStoragePoolGetUUID", 1, vert_virStoragePoolGetUUID},
    {"virStoragePoolGetUUIDString", 1, vert_virStoragePoolGetUUIDString},
    {"virStoragePoolGetXMLDesc", 2, vert_virStoragePoolGetXMLDesc},
    {"virStoragePoolIsActive", 1, vert_virStoragePoolIsActive},
    {"virStoragePoolIsPersistent", 1, vert_virStoragePoolIsPersistent},
    {"virStoragePoolListVolumes", 2, vert_virStoragePoolListVolumes},
    {"virStoragePoolLookupByName", 2, vert_virStoragePoolLookupByName},
    {"virStoragePoolLookupByUUID", 2, vert_virStoragePoolLookupByUUID},
    {"virStoragePoolLookupByUUIDString", 2, vert_virStoragePoolLookupByUUIDString},
    {"virStoragePoolLookupByVolume", 1, vert_virStoragePoolLookupByVolume},
    {"virStoragePoolNumOfVolumes", 1, vert_virStoragePoolNumOfVolumes},
    {"virStoragePoolRefresh", 2, vert_virStoragePoolRefresh},
    {"virStoragePoolSetAutostart", 2, vert_virStoragePoolSetAutostart},
    {"virStoragePoolUndefine", 1, vert_virStoragePoolUndefine},

    /* storagevol */
    {"virStorageVolCreateXML", 3, vert_virStorageVolCreateXML},
    {"virStorageVolCreateXMLFrom", 4, vert_virStorageVolCreateXMLFrom},
    {"virStorageVolDelete", 2, vert_virStorageVolDelete},
    {"virStorageVolDownload", 5, vert_virStorageVolDownload},
    {"virStorageVolGetInfo", 1, vert_virStorageVolGetInfo},
    {"virStorageVolGetKey", 1, vert_virStorageVolGetKey},
    {"virStorageVolGetName", 1, vert_virStorageVolGetName},
    {"virStorageVolGetPath", 1, vert_virStorageVolGetPath},
    {"virStorageVolGetXMLDesc", 2, vert_virStorageVolGetXMLDesc},
    {"virStorageVolLookupByKey", 2, vert_virStorageVolLookupByKey},
    {"virStorageVolLookupByName", 2, vert_virStorageVolLookupByName},
    {"virStorageVolLookupByPath", 2, vert_virStorageVolLookupByPath},
    {"virStorageVolResize", 3, vert_virStorageVolResize},
    {"virStorageVolUpload", 5, vert_virStorageVolUpload},
    {"virStorageVolWipe", 2, vert_virStorageVolWipe},
    {"virStorageVolWipePattern", 3, vert_virStorageVolWipePattern},

    /* stream */
    {"virStreamAbort", 1, vert_virStreamAbort},
    {"virStreamFinish", 1, vert_virStreamFinish},
    {"virStreamNew", 2, vert_virStreamNew},
    {"virStreamRecv", 2, vert_virStreamRecv},
    {"virStreamSend", 2, vert_virStreamSend},

    {NULL, 0, NULL}
};
