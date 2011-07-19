/* Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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
    {"virConnectOpen", 0, vert_virConnectOpen},
    {"virConnectOpenReadOnly", 0, vert_virConnectOpenReadOnly},
    {"virConnectClose", 0, vert_virConnectClose},
    {"virConnectGetCapabilities", 0, vert_virConnectGetCapabilities},
    {"virConnectGetHostname", 0, vert_virConnectGetHostname},
    {"virConnectGetLibVersion", 0, vert_virConnectGetLibVersion},
    {"virConnectGetMaxVcpus", 0, vert_virConnectGetMaxVcpus},
    {"virNodeGetFreeMemory", 0, vert_virNodeGetFreeMemory},
    {"virNodeGetInfo", 0, vert_virNodeGetInfo},
    {"virNodeGetCellsFreeMemory", 0, vert_virNodeGetCellsFreeMemory},
    {"virConnectGetType", 0, vert_virConnectGetType},
    {"virConnectGetVersion", 0, vert_virConnectGetVersion},
    {"virConnectGetURI", 0, vert_virConnectGetURI},
    {"virConnectIsEncrypted", 0, vert_virConnectIsEncrypted},
    {"virConnectIsSecure", 0, vert_virConnectIsSecure},
    {"virNodeGetSecurityModel", 0, vert_virNodeGetSecurityModel},
    {"virConnectNumOfDomains", 0, vert_virConnectNumOfDomains},
    {"virConnectNumOfInterfaces", 0, vert_virConnectNumOfInterfaces},
    {"virConnectNumOfNetworks", 0, vert_virConnectNumOfNetworks},
    {"virConnectNumOfStoragePools", 0, vert_virConnectNumOfStoragePools},
    {"virConnectNumOfSecrets", 0, vert_virConnectNumOfSecrets},
    {"virConnectNumOfNWFilters", 0, vert_virConnectNumOfNWFilters},
    {"virConnectNumOfDefinedDomains", 0, vert_virConnectNumOfDefinedDomains},
    {"virConnectNumOfDefinedInterfaces", 0, vert_virConnectNumOfDefinedInterfaces},
    {"virConnectNumOfDefinedNetworks", 0, vert_virConnectNumOfDefinedNetworks},
    {"virConnectNumOfDefinedStoragePools", 0, vert_virConnectNumOfDefinedStoragePools},
    {"virConnectListDomains", 0, vert_virConnectListDomains},
    {"virConnectListInterfaces", 0, vert_virConnectListInterfaces},
    {"virConnectListNetworks", 0, vert_virConnectListNetworks},
    {"virConnectListNWFilters", 0, vert_virConnectListNWFilters},
    {"virConnectListSecrets", 0, vert_virConnectListSecrets},
    {"virConnectListStoragePools", 0, vert_virConnectListStoragePools},
    {"virConnectListDefinedDomains", 0, vert_virConnectListDefinedDomains},
    {"virConnectListDefinedInterfaces", 0, vert_virConnectListDefinedInterfaces},
    {"virConnectListDefinedNetworks", 0, vert_virConnectListDefinedNetworks},
    {"virConnectListDefinedStoragePools", 0, vert_virConnectListDefinedStoragePools},

    /* domain */
    {"virDomainLookupByID", 0, vert_virDomainLookupByID},
    {"virDomainLookupByName", 0, vert_virDomainLookupByName},
    {"virDomainLookupByUUID", 0, vert_virDomainLookupByUUID},
    {"virDomainGetAutostart", 0, vert_virDomainGetAutostart},
    {"virDomainGetBlockInfo", 0, vert_virDomainGetBlockInfo},
    {"virDomainGetID", 0, vert_virDomainGetID},
    {"virDomainGetInfo", 0, vert_virDomainGetInfo},
    {"virDomainGetJobInfo", 0, vert_virDomainGetJobInfo},
    {"virDomainGetMaxMemory", 0, vert_virDomainGetMaxMemory},
    {"virDomainGetMaxVcpus", 0, vert_virDomainGetMaxVcpus},
    {"virDomainGetMemoryParameters", 0, vert_virDomainGetMemoryParameters},
    {"virDomainGetName", 0, vert_virDomainGetName},
    {"virDomainGetOSType", 0, vert_virDomainGetOSType},
    {"virDomainGetSchedulerParameters", 0, vert_virDomainGetSchedulerParameters},
    {"virDomainGetSchedulerType", 0, vert_virDomainGetSchedulerType},
    {"virDomainGetSecurityLabel", 0, vert_virDomainGetSecurityLabel},
    {"virDomainGetUUID", 0, vert_virDomainGetUUID},
    {"virDomainGetUUIDString", 0, vert_virDomainGetUUIDString},
    {"virDomainGetUUIDString", 0, vert_virDomainGetUUIDString},
    {"virDomainGetXMLDesc", 0, vert_virDomainGetXMLDesc},
    {"virDomainSave", 0, vert_virDomainSave},
    {"virDomainRestore", 0, vert_virDomainRestore},
    {"virDomainSetAutostart", 0, vert_virDomainSetAutostart},
    {"virDomainShutdown", 0, vert_virDomainShutdown},
    {"virDomainSuspend", 0, vert_virDomainSuspend},
    {"virDomainResume", 0, vert_virDomainResume},
    {"virDomainDefineXML", 0, vert_virDomainDefineXML},
    {"virDomainUndefine", 0, vert_virDomainUndefine},
    {"virDomainCreate", 0, vert_virDomainCreate},
    {"virDomainDestroy", 0, vert_virDomainDestroy},

    /* interface */
    {"virInterfaceLookupByName", 0, vert_virInterfaceLookupByName},
    {"virInterfaceLookupByMACString", 0, vert_virInterfaceLookupByMACString},
    {"virInterfaceGetName", 0, vert_virInterfaceGetName},
    {"virInterfaceGetMACString", 0, vert_virInterfaceGetMACString},
    {"virInterfaceGetXMLDesc", 0, vert_virInterfaceGetXMLDesc},
    {"virInterfaceDefineXML", 0, vert_virInterfaceDefineXML},
    {"virInterfaceUndefine", 0, vert_virInterfaceUndefine},
    {"virInterfaceCreate", 0, vert_virInterfaceCreate},
    {"virInterfaceDestroy", 0, vert_virInterfaceDestroy},

    /* network */
    {"virNetworkLookupByName", 0, vert_virNetworkLookupByName},
    {"virNetworkLookupByUUID", 0, vert_virNetworkLookupByUUID},
    {"virNetworkLookupByUUIDString", 0, vert_virNetworkLookupByUUIDString},
    {"virNetworkGetAutostart", 0, vert_virNetworkGetAutostart},
    {"virNetworkGetBridgeName", 0, vert_virNetworkGetBridgeName},
    {"virNetworkGetName", 0, vert_virNetworkGetName},
    {"virNetworkGetUUID", 0, vert_virNetworkGetUUID},
    {"virNetworkGetXMLDesc", 0, vert_virNetworkGetXMLDesc},
    {"virNetworkIsPersistent", 0, vert_virNetworkIsPersistent},
    {"virNetworkDefineXML", 0, vert_virNetworkDefineXML},
    {"virNetworkUndefine", 0, vert_virNetworkUndefine},
    {"virNetworkCreate", 0, vert_virNetworkCreate},
    {"virNetworkDestroy", 0, vert_virNetworkDestroy},

    {NULL, 0, NULL}
};
