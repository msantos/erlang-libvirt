/* Copyright (c) 2010-2011, Michael Santos <michael.santos@gmail.com>
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
#include <libvirt/libvirt.h>
#include <libvirt/virterror.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include "erl_nif.h"

#include "vert.h"


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_enomem;
static ERL_NIF_TERM atom_resource;
static ERL_NIF_TERM atom_connect;
static ERL_NIF_TERM atom_domain;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *err);

void error(void *userData, virErrorPtr error);
static ERL_NIF_TERM verterr(ErlNifEnv *env);

void connection_cleanup(ErlNifEnv *env, void *obj);
void domain_cleanup(ErlNifEnv *env, void *obj);
void interface_cleanup(ErlNifEnv *env, void *obj);
void network_cleanup(ErlNifEnv *env, void *obj);
void storagepool_cleanup(ErlNifEnv *env, void *obj);
#if THIS_VERSION_SUPPORTS_FILTER
void filter_cleanup(ErlNifEnv *env, void *obj);
#endif
void secret_cleanup(ErlNifEnv *env, void *obj);

static ErlNifResourceType *LIBVIRT_CONNECT_RESOURCE;
static ErlNifResourceType *LIBVIRT_DOMAIN_RESOURCE;
static ErlNifResourceType *LIBVIRT_INTERFACE_RESOURCE;
static ErlNifResourceType *LIBVIRT_NETWORK_RESOURCE;
static ErlNifResourceType *LIBVIRT_STORAGEPOOL_RESOURCE;
#if THIS_VERSION_SUPPORTS_FILTER
static ErlNifResourceType *LIBVIRT_FILTER_RESOURCE;
#endif
static ErlNifResourceType *LIBVIRT_SECRET_RESOURCE;


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_enomem = enif_make_atom(env, "enomem");
    atom_resource = enif_make_atom(env, "resource");
    atom_connect = enif_make_atom(env, "connect");
    atom_domain = enif_make_atom(env, "domain");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");

    if ( (LIBVIRT_CONNECT_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_connect_resource", connection_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if ( (LIBVIRT_DOMAIN_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_domain_resource", domain_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if ( (LIBVIRT_INTERFACE_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_interface_resource", interface_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if ( (LIBVIRT_NETWORK_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_network_resource", network_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if ( (LIBVIRT_STORAGEPOOL_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_storagepool_resource", storagepool_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

#if THIS_VERSION_SUPPORTS_FILTER
    if ( (LIBVIRT_FILTER_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_filter_resource", filter_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;
#endif

    if ( (LIBVIRT_SECRET_RESOURCE = enif_open_resource_type(env, NULL,
            "libvirt_secret_resource", secret_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if (virInitialize() != 0)
        return -2;

    /* XXX Disable error messges to stderr
     * XXX Probably should send the errors to a mailbox
     * */
//    virSetErrorFunc(NULL, error);
    virSetErrorFunc(NULL, NULL);

    return 0;
}

    void
unload(ErlNifEnv *env, void *priv_data)
{
}  

/* 0: const char *name, 1: type */
    static ERL_NIF_TERM
nif_virConnectOpen(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char name[1024]; /* XXX what should be the size of this? */
    int type = VERT_CONNECT_OPEN;

    virConnectPtr *cp = NULL;
    ERL_NIF_TERM res = {0};


    (void)memset(name, '\0', sizeof(name));

    /* If the string is truncated, return badarg
     * If the string is empty or has the wrong encoding, consider it to be NULL
     *  This assumes that enif_get_string() does not modify the buffer
     * If the string has a length, ok
     */
    if (enif_get_string(env, argv[0], name, sizeof(name), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    cp = enif_alloc_resource(LIBVIRT_CONNECT_RESOURCE, sizeof(virConnectPtr));

    if (cp == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_CONNECT_OPEN:
            *cp = virConnectOpen( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_READONLY:
            *cp = virConnectOpenReadOnly( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_AUTH:
            return enif_make_badarg(env);

        default:
            return enif_make_badarg(env);
    }

    if (*cp == NULL) {
        enif_release_resource(cp);
        return verterr(env);
    }

    /* XXX disable logging to stderr */
    virConnSetErrorFunc(*cp, NULL, NULL);

    res = enif_make_resource(env, cp);
    enif_release_resource(cp);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_connect,
            enif_make_ref(env), res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectClose(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    ERL_NIF_TERM res = atom_ok;

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (virConnectClose(*cp) != 0)
        res = verterr(env);

    cp = NULL;

    return res;
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetCapabilities(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    char *res = NULL;
    ERL_NIF_TERM capabilities = {0};

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetCapabilities(*cp);

    if (res == NULL)
        return verterr(env);

    capabilities = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple2(env,
            atom_ok,
            capabilities);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetHostname(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    char *res = NULL;
    ERL_NIF_TERM hostname = {0};

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetHostname(*cp);

    if (res == NULL)
        return verterr(env);

    hostname = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple2(env,
            atom_ok,
            hostname);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetLibVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    int res = -1;
    unsigned long version = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetLibVersion(*cp, &version);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_ulong(env, version));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetMaxVcpus(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    int res = -1;
    char type[1024];


    (void)memset(type, '\0', sizeof(type));

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], type, sizeof(type), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    res = virConnectGetMaxVcpus(*cp, (type[0] == '\0' ? NULL : type));

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    u_int64_t res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virNodeGetFreeMemory(*cp);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_uint64(env, res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    virNodeInfo info;
    ErlNifBinary buf = {0};
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virNodeGetInfo(*cp, &info);

    if (res == -1)
        return verterr(env);

    if (!enif_alloc_binary(sizeof(virNodeInfo), &buf))
        return atom_enomem;

    (void)memcpy(buf.data, &info, buf.size);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: virConnectPtr, 1: int max */
    static ERL_NIF_TERM
nif_virNodeGetCellsFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int max = 0;

    u_int64_t *mem = NULL;
    int res = -1;
    int i = 0;

    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &max) || max <= 0)
        return enif_make_badarg(env);

    mem = calloc(max, sizeof(u_int64_t));

    if (mem == NULL)
        return atom_enomem;

    res = virNodeGetCellsFreeMemory(*cp, mem, 0, max);

    if (res == -1)
        return verterr(env);

    list = enif_make_list(env, 0);
    for (i = 0; i < res; i++)
        list = enif_make_list_cell(env, enif_make_int(env, mem[i]), list);

    free(mem);

    return enif_make_tuple2(env,
            atom_ok,
            list);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetType(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    const char *res = NULL;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetType(*cp);

    if (res == NULL)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_string(env, res, ERL_NIF_LATIN1));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    int res = -1;
    unsigned long version = 0;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetVersion(*cp, &version);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_ulong(env, version));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetURI(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    char *res = NULL;
    ERL_NIF_TERM uri = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectGetURI(*cp);

    if (res == NULL)
        return verterr(env);

    uri = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple2(env,
            atom_ok,
            uri);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectIsEncrypted(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectIsEncrypted(*cp);

    if (res == -1)
        return verterr(env);

    return (res == 1 ? atom_true : atom_false);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectIsSecure(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virConnectIsSecure(*cp);

    if (res == -1)
        return verterr(env);

    return (res == 1 ? atom_true : atom_false);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetSecurityModel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;

    virSecurityModel sec;
    ErlNifBinary buf = {0};
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    res = virNodeGetSecurityModel(*cp, &sec);

    if (res == -1)
        return verterr(env);

    if (!enif_alloc_binary(sizeof(virSecurityModel), &buf))
        return atom_enomem;

    (void)memcpy(buf.data, &sec, buf.size);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: virConnectPtr, 1: int type */
    static ERL_NIF_TERM
nif_ConnectNumActive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_RES_DOMAIN;

    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectNumOfDomains(*cp);
            break;
        case VERT_RES_INTERFACE:
            res = virConnectNumOfInterfaces(*cp);
            break;
        case VERT_RES_NETWORK:
            res = virConnectNumOfNetworks(*cp);
            break;
        case VERT_RES_STORAGEPOOL:
            res = virConnectNumOfStoragePools(*cp);
            break;
        case VERT_RES_SECRET:
            res = virConnectNumOfSecrets(*cp);
            break;
        case VERT_RES_FILTER:
#ifdef THIS_VERSION_SUPPORTS_FILTER
            res = virConnectNumOfNWFilters(*cp);
#else
            res = 0;
#endif
            break;
        default:
            return enif_make_badarg(env);
    }

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, res));
}

/* 0: virConnectPtr, 1: int type */
    static ERL_NIF_TERM
nif_ConnectNumInactive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_RES_DOMAIN;

    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectNumOfDefinedDomains(*cp);
            break;
        case VERT_RES_INTERFACE:
            res = virConnectNumOfDefinedInterfaces(*cp);
            break;
        case VERT_RES_NETWORK:
            res = virConnectNumOfDefinedNetworks(*cp);
            break;
        case VERT_RES_STORAGEPOOL:
            res = virConnectNumOfDefinedStoragePools(*cp);
            break;
        default:
            return enif_make_badarg(env);
    }

    if (res == -1)
        return verterr(env);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, res));
}


/**
 ** Domain operations
 **/

/* 0: virConnectPtr, 1: int type 2: int | char* */
    static ERL_NIF_TERM
nif_DomainLookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    virDomainPtr *dp = NULL;
    int type = VERT_LOOKUP_BY_ID;

    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    dp = enif_alloc_resource(LIBVIRT_DOMAIN_RESOURCE, sizeof(virDomainPtr));

    if (dp == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_LOOKUP_BY_ID: {
                int id = 0;

                if (!enif_get_int(env, argv[2], &id))
                    return enif_make_badarg(env);
                *dp = virDomainLookupByID(*cp, id);
            }
            break;

        case VERT_LOOKUP_BY_NAME: {
                char name[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *dp = virDomainLookupByName(*cp, name);
            }
            break;

        case VERT_LOOKUP_BY_UUID: {
                char uuid[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], uuid, sizeof(uuid), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *dp = virDomainLookupByUUID(*cp, (const unsigned char *)uuid);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*dp == NULL) {
        enif_release_resource(dp);
        return verterr(env);
    }

    res = enif_make_resource(env, dp);
    enif_release_resource(dp);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: virDomainPtr, 1: int type */
    static ERL_NIF_TERM
nif_ResourceFree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int type = VERT_RES_DOMAIN;

    ERL_NIF_TERM res = atom_ok;


    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_RES_DOMAIN: {
            virDomainPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virDomainFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        case VERT_RES_INTERFACE: {
            virInterfacePtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virInterfaceFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        case VERT_RES_NETWORK: {
            virNetworkPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_NETWORK_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virNetworkFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;

        case VERT_RES_STORAGEPOOL: {
            virStoragePoolPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_STORAGEPOOL_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virStoragePoolFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        case VERT_RES_FILTER: {
#if THIS_VERSION_SUPPORTS_FILTER
            virNWFilterPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_FILTER_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virNWFilterFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
#endif
            }
            break;
        case VERT_RES_SECRET: {
            virSecretPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_SECRET_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virSecretFree(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        default:
            return enif_make_badarg(env);

    }

    return res;
}

/* 0: virDomainPtr, 1: int type */
    static ERL_NIF_TERM
nif_ResourceDestroy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    int type = VERT_RES_DOMAIN;

    ERL_NIF_TERM res = atom_ok;


    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_RES_DOMAIN: {
            virDomainPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virDomainDestroy(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        case VERT_RES_INTERFACE: {
            virInterfacePtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virInterfaceDestroy(*p, 0) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        case VERT_RES_NETWORK: {
            virNetworkPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_NETWORK_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virNetworkDestroy(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;

        case VERT_RES_STORAGEPOOL: {
            virStoragePoolPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_STORAGEPOOL_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            if (virStoragePoolDestroy(*p) != 0)
                res = verterr(env);
            *p = NULL;
            }
            break;
        default:
            return enif_make_badarg(env);

    }

    return res;
}

/* 0: virConnectPtr, 1: int type, 2: maxdomains */
    static ERL_NIF_TERM
nif_ConnectGetListActive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_RES_DOMAIN;
    int max = 0;

    int i = 0;
    int res = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[2], &max) || max <= 0)
        return enif_make_badarg(env);

    if (type != VERT_RES_DOMAIN) {
        names = calloc(max, sizeof(char *));

        if (names == NULL)
            return atom_enomem;
    }

    list = enif_make_list(env, 0);

    switch (type) {
        case VERT_RES_DOMAIN: {
            int *domains = NULL;

            domains = calloc(max, sizeof(int));

            if (domains == NULL)
                return atom_enomem;

            res = virConnectListDomains(*cp, domains, max);

            if (res == -1)
                return verterr(env);

            for (i = 0; i < res; i++)
                list = enif_make_list_cell(env,
                        enif_make_int(env, domains[i]),
                        list);

            free(domains);

            return enif_make_tuple2(env,
                atom_ok,
                list);
            }
            break;

        case VERT_RES_INTERFACE:
            res = virConnectListInterfaces(*cp, names, max);
            break;
            
        case VERT_RES_NETWORK:
            res = virConnectListNetworks(*cp, names, max);
            break;

        case VERT_RES_FILTER:
#if THIS_VERSION_SUPPORTS_FILTER
            res = virConnectListNWFilters(*cp, names, max);
#else
            res = 0;
#endif
            break;

        case VERT_RES_SECRET:
            res = virConnectListSecrets(*cp, names, max);
            break;

        case VERT_RES_STORAGEPOOL:
            res = virConnectListStoragePools(*cp, names, max);
            break;

        default:
            return enif_make_badarg(env);
    }

    if (res == -1)
        return verterr(env);

    for (i = 0; i < res; i++)
        list = enif_make_list_cell(env,
            enif_make_string(env, names[i], ERL_NIF_LATIN1),
            list);

    if (names)
        free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

/* 0: virConnectPtr, 1: int type, 2: maxdomains */
    static ERL_NIF_TERM
nif_ConnectGetListInactive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_RES_DOMAIN;
    int max= 0;

    int i = 0;
    int res = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[2], &max) || max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));

    if (names == NULL)
        return atom_enomem;

    list = enif_make_list(env, 0);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectListDefinedDomains(*cp, names, max);
            break;

        case VERT_RES_INTERFACE:
            res = virConnectListDefinedInterfaces(*cp, names, max);
            break;

        case VERT_RES_NETWORK:
            res = virConnectListDefinedNetworks(*cp, names, max);
            break;
            
        case VERT_RES_STORAGEPOOL:
            res = virConnectListDefinedNetworks(*cp, names, max);
            break;

        default:
            return enif_make_badarg(env);
    }

    if (res == -1)
        return verterr(env);

    for (i = 0; i < res; i++)
        list = enif_make_list_cell(env,
        enif_make_string(env, names[i], ERL_NIF_LATIN1),
        list);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

/* 0: virConnectPtr, 1: int type 2: char *, 3: int flags */
    static ERL_NIF_TERM
nif_virDomainCreate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_DOMAIN_CREATE_TRANSIENT;
    char cfg[8192]; /* XXX size ??? this is XML after all */
    int flags = 0;

    virDomainPtr *dp = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    /* XXX use binary */
    if (enif_get_string(env, argv[2], cfg, sizeof(cfg), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[3], &flags))
        return enif_make_badarg(env);

    dp = enif_alloc_resource(LIBVIRT_DOMAIN_RESOURCE, sizeof(virDomainPtr));

    if (dp == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_DOMAIN_CREATE_TRANSIENT:
            *dp = virDomainCreateXML(*cp, cfg, flags);
            break;

        case VERT_DOMAIN_CREATE_PERSISTENT:
            *dp = virDomainDefineXML(*cp, cfg);

            if (virDomainCreate(*dp) < 0) {
                res = verterr(env);
                (void)virDomainFree(*dp);
                enif_release_resource(dp);
                return res;
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*dp == NULL) {
        enif_release_resource(dp);
        return verterr(env);
    }

    res = enif_make_resource(env, dp);
    enif_release_resource(dp);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: virDomainPtr */
    static ERL_NIF_TERM
nif_virDomainGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dp = NULL;

    virDomainInfo info = {0};
    int res = -1;

    ErlNifBinary buf = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    res = virDomainGetInfo(*dp, &info);

    if (res != 0)
        return verterr(env);

    if (!enif_alloc_binary(sizeof(virDomainInfo), &buf))
        return atom_enomem;

    (void)memcpy(buf.data, &info, buf.size);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: virDomainPtr, 1: char* */
    static ERL_NIF_TERM
nif_virDomainSave(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dp = NULL;
    char file[MAXPATHLEN];

    int res = -1;

    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], file, sizeof(file), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    res = virDomainSave(*dp, file);

    if (res != 0)
        return verterr(env);

    return atom_ok;
}

/* 0: virDomainPtr, 1: char* */
    static ERL_NIF_TERM
nif_virDomainRestore(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    char file[MAXPATHLEN];

    int res = -1;

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], file, sizeof(file), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    res = virDomainRestore(*cp, file);

    if (res != 0)
        return verterr(env);

    return atom_ok;
}

/* 0: virDomainPtr, 1: int flag */
    static ERL_NIF_TERM
nif_virDomainSetAutostart(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dp = NULL;
    int flags = 0;

    int res = -1;

    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &flags))
        return enif_make_badarg(env);

    res = virDomainSetAutostart(*dp, flags);

    if (res != 0)
        return verterr(env);

    return atom_ok;
}

/* 0: virDomainPtr */
    static ERL_NIF_TERM
nif_virDomainShutdown(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dp = NULL;

    int res = -1;

    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    res = virDomainShutdown(*dp);

    if (res != 0)
        return verterr(env);

    return atom_ok;
}


/* Interfaces */

/* 0: virConnectPtr, 1: int type 2: int | char* */
    static ERL_NIF_TERM
nif_InterfaceLookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_LOOKUP_BY_NAME;

    virInterfacePtr *ifp = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    ifp = enif_alloc_resource(LIBVIRT_INTERFACE_RESOURCE, sizeof(virInterfacePtr));

    if (ifp == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_LOOKUP_BY_NAME: {
                char name[1024]; /* XXX max interface length ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *ifp = virInterfaceLookupByName(*cp, name);
            }
            break;

        case VERT_LOOKUP_BY_MAC: {
                char mac[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], mac, sizeof(mac), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *ifp = virInterfaceLookupByMACString(*cp, mac);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*ifp == NULL) {
        enif_release_resource(ifp);
        return verterr(env);
    }

    res = enif_make_resource(env, ifp);
    enif_release_resource(ifp);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: virConnectPtr, 1: int type */
    static ERL_NIF_TERM
nif_InterfaceGet(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virInterfacePtr *ifp = NULL;
    int type = VERT_LOOKUP_BY_NAME;

    const char *res = NULL;


    if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&ifp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_LOOKUP_BY_NAME:
            res = virInterfaceGetName(*ifp);
            break;

        case VERT_LOOKUP_BY_MAC:
            res = virInterfaceGetMACString(*ifp);
            break;

        case VERT_LOOKUP_BY_DESC:
            res = virInterfaceGetXMLDesc(*ifp, 0);
            break;

        default:
            return enif_make_badarg(env);
    }

    if (res == NULL) {
        enif_release_resource(ifp);
        return verterr(env);
    }

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, res, ERL_NIF_LATIN1));
}


/*
 * Utility functions
 */

    static ERL_NIF_TERM
verterr(ErlNifEnv *env)
{
    ERL_NIF_TERM res = {0};
    virErrorPtr err = {0};


    err = virSaveLastError();
    res = error_tuple(env, err->message);
    virFreeError(err);

    return res;
}


    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, char *err)
{
    return enif_make_tuple2(env,
            atom_error,
            enif_make_string(env, err, ERL_NIF_LATIN1));
}  


/*
 * Callbacks
 */
    void
connection_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: connection=%p/%p\n", *p, p);
    if (*p)
        (void)virConnectClose(*p);
}

    void
domain_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: domain=%p/%p\n", *p, p);
    if (*p)
        (void)virDomainFree(*p);
}

    void
interface_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: interface=%p/%p\n", *p, p);
    if (*p)
        (void)virInterfaceFree(*p);
}

    void
network_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: network=%p/%p\n", *p, p);
    if (*p)
        (void)virNetworkFree(*p);
}

    void
storagepool_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: storagepool=%p/%p\n", *p, p);
    if (*p)
        (void)virStoragePoolFree(*p);
}

#if THIS_VERSION_SUPPORTS_FILTER
    void
filter_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: filter=%p/%p\n", *p, p);
    if (*p)
        (void)virNWFilterFree(*p);
}
#endif

    void
secret_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    (void)fprintf(stderr, "cleanup: secret=%p/%p\n", *p, p);
    if (*p)
        (void)virSecretFree(*p);
}


    void
error (void *userData, virErrorPtr error)
{
}


static ErlNifFunc nif_funcs[] = {
    /* connect */
    {"connect_open", 2, nif_virConnectOpen},
    {"connect_close", 1, nif_virConnectClose},

    {"connect_get_capabilities", 1, nif_virConnectGetCapabilities},
    {"connect_get_hostname", 1, nif_virConnectGetHostname},
    {"connect_get_libversion", 1, nif_virConnectGetLibVersion},
    {"connect_get_maxvcpus", 2, nif_virConnectGetMaxVcpus},
    {"connect_get_freememory", 1, nif_virNodeGetFreeMemory},
    {"connect_get_info", 1, nif_virNodeGetInfo},
    {"connect_get_cellsfreememory", 2, nif_virNodeGetCellsFreeMemory},
    {"connect_get_type", 1, nif_virConnectGetType},
    {"connect_get_version", 1, nif_virConnectGetVersion},
    {"connect_get_uri", 1, nif_virConnectGetURI},
    {"connect_get_securitymodel", 1, nif_virNodeGetSecurityModel},

    {"connect_get_numactive", 2, nif_ConnectNumActive},
    {"connect_get_numinactive", 2, nif_ConnectNumInactive},
    {"connect_get_listactive", 3, nif_ConnectGetListActive},
    {"connect_get_listinactive", 3, nif_ConnectGetListInactive},

    {"connect_is_encrypted", 1, nif_virConnectIsEncrypted},
    {"connect_is_secure", 1, nif_virConnectIsSecure},


    /* domain */
    {"domain_lookup", 3, nif_DomainLookup},
    {"domain_get_info", 1, nif_virDomainGetInfo},

    {"domain_create", 4, nif_virDomainCreate},
    {"domain_save", 2, nif_virDomainSave},
    {"domain_restore", 2, nif_virDomainRestore},
    {"domain_shutdown", 1, nif_virDomainShutdown},

    {"domain_set_autostart", 2, nif_virDomainSetAutostart},

    /* interfaces */
    {"interface_lookup", 3, nif_InterfaceLookup},
    {"interface_get", 2, nif_InterfaceGet},

    {"resource_free", 2, nif_ResourceFree},
    {"resource_destroy", 2, nif_ResourceDestroy},
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)

