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
static ERL_NIF_TERM bincopy(ErlNifEnv *env, void *src, size_t len);

void null_logger(void *userData, virErrorPtr error);
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
    virSetErrorFunc(NULL, null_logger);

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

    ISNULL(cp);

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

    VERTERR(virConnectClose(*cp) != 0);

    cp = NULL;

    return res;
}

/* 0: virConnectPtr, 1: type */
    static ERL_NIF_TERM
nif_ConnectGet(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_ATTR_CAPABILITIES: {
            char *cap = NULL;

            cap = virConnectGetCapabilities(*cp);

            VERTERR(cap == NULL);

            term = enif_make_tuple2(env, atom_ok,
                    enif_make_string(env, cap, ERL_NIF_LATIN1));
            free(cap);
            }
            break;

        case VERT_ATTR_HOSTNAME: {
            char *hostname = NULL;

            hostname = virConnectGetHostname(*cp);

            VERTERR(hostname == NULL);

            term = enif_make_tuple2(env, atom_ok,
                    enif_make_string(env, hostname, ERL_NIF_LATIN1));

            free(hostname);
            }
            break;

        case VERT_ATTR_LIBVERSION: {
            unsigned long version = -1;

            VERTERR(virConnectGetLibVersion(*cp, &version) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_ulong(env, version));
            }
            break;

        case VERT_ATTR_MAXVCPUS: {
            char type[1024];
            int max = -1;

            (void)memset(type, '\0', sizeof(type));

            if (enif_get_string(env, argv[2], type, sizeof(type), ERL_NIF_LATIN1) < 0)
                return enif_make_badarg(env);

            max = virConnectGetMaxVcpus(*cp, (type[0] == '\0' ? NULL : type));

            VERTERR(max < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_int(env, max));
            }
            break;

        case VERT_ATTR_FREEMEMORY: {
            u_int64_t mem = 0;

            mem = virNodeGetFreeMemory(*cp);

            VERTERR(mem == 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_uint64(env, mem));
            }
            break;

        case VERT_ATTR_INFO: {
            virNodeInfo info;
            ERL_NIF_TERM buf = {0};

            VERTERR(virNodeGetInfo(*cp, &info) < 0);
            buf = bincopy(env, &info, sizeof(virNodeInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env,
                atom_ok, buf);
            }
            break;

        case VERT_ATTR_CELLSFREEMEMORY: {
            int max = 0;
            u_int64_t *mem = NULL;
            ERL_NIF_TERM list = {0};
            int i = 0;

            mem = calloc(max, sizeof(u_int64_t));
            ISNULL(mem);

            if (!enif_get_int(env, argv[2], &max) || max <= 0)
                return enif_make_badarg(env);

            VERTERR(virNodeGetCellsFreeMemory(*cp, mem, 0, max) < 0);

            list = enif_make_list(env, 0);
            for (i = 0; i < max; i++)
                list = enif_make_list_cell(env, enif_make_int(env, mem[i]), list);

            free(mem);

            term = enif_make_tuple2(env,
                atom_ok,
                list);
            }
            break;

        case VERT_ATTR_TYPE: {
            const char *type = NULL;

            type = virConnectGetType(*cp);

            VERTERR(type == NULL);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_string(env, type, ERL_NIF_LATIN1));
            }
            break;

        case VERT_ATTR_VERSION: {
            unsigned long version = 0;

            VERTERR(virConnectGetVersion(*cp, &version) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_ulong(env, version));
            }
            break;

        case VERT_ATTR_URI: {
            char *uri = NULL;

            uri = virConnectGetURI(*cp);

            VERTERR(uri == NULL);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_string(env, uri, ERL_NIF_LATIN1));

            free(uri);
            }
            break;

        case VERT_ATTR_ENCRYPTED: {
            int res = -1;

            res = virConnectIsEncrypted(*cp);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_SECURE: {
            int res = -1;

            res = virConnectIsSecure(*cp);

            VERTERR(res == -1);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_SECURITYMODEL: {
            virSecurityModel model;
            ERL_NIF_TERM buf = {0};

            VERTERR(virNodeGetSecurityModel(*cp, &model) < 0);
            buf = bincopy(env, &model, sizeof(virSecurityModel));
            NOMEM(buf);

            term = enif_make_tuple2(env,
                atom_ok, buf);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    return term;
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

    VERTERR(res == -1);

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

    VERTERR(res == -1);

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
    int type = VERT_ATTR_ID;

    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    dp = enif_alloc_resource(LIBVIRT_DOMAIN_RESOURCE, sizeof(virDomainPtr));
    ISNULL(dp);

    switch (type) {
        case VERT_ATTR_ID: {
                int id = 0;

                if (!enif_get_int(env, argv[2], &id))
                    return enif_make_badarg(env);
                *dp = virDomainLookupByID(*cp, id);
            }
            break;

        case VERT_ATTR_NAME: {
                char name[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *dp = virDomainLookupByName(*cp, name);
            }
            break;

        case VERT_ATTR_UUID: {
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
            VERTERR(virDomainFree(*p) != 0);
            *p = NULL;
            }
            break;
        case VERT_RES_INTERFACE: {
            virInterfacePtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virInterfaceFree(*p) != 0);
            *p = NULL;
            }
            break;
        case VERT_RES_NETWORK: {
            virNetworkPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_NETWORK_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virNetworkFree(*p) != 0);
            *p = NULL;
            }
            break;

        case VERT_RES_STORAGEPOOL: {
            virStoragePoolPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_STORAGEPOOL_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virStoragePoolFree(*p) != 0);
            *p = NULL;
            }
            break;
        case VERT_RES_FILTER: {
#if THIS_VERSION_SUPPORTS_FILTER
            virNWFilterPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_FILTER_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virNWFilterFree(*p) != 0);
            *p = NULL;
#endif
            }
            break;
        case VERT_RES_SECRET: {
            virSecretPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_SECRET_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virSecretFree(*p) != 0);
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
            VERTERR(virDomainDestroy(*p) != 0);
            *p = NULL;
            }
            break;
        case VERT_RES_INTERFACE: {
            virInterfacePtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virInterfaceDestroy(*p, 0) != 0);
            *p = NULL;
            }
            break;
        case VERT_RES_NETWORK: {
            virNetworkPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_NETWORK_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virNetworkDestroy(*p) != 0);
            *p = NULL;
            }
            break;

        case VERT_RES_STORAGEPOOL: {
            virStoragePoolPtr *p = NULL;
            if (!enif_get_resource(env, argv[0], LIBVIRT_STORAGEPOOL_RESOURCE, (void **)&p))
                return enif_make_badarg(env);
            VERTERR(virStoragePoolDestroy(*p) != 0);
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
        ISNULL(names);
    }

    list = enif_make_list(env, 0);

    switch (type) {
        case VERT_RES_DOMAIN: {
            int *domains = NULL;

            domains = calloc(max, sizeof(int));

            ISNULL(domains);

            res = virConnectListDomains(*cp, domains, max);

            VERTERR(res == -1);

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

    VERTERR(res == -1);

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

    ISNULL(names);

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

    VERTERR(res == -1);

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

    ISNULL(dp);

    switch (type) {
        case VERT_DOMAIN_CREATE_TRANSIENT:
            *dp = virDomainCreateXML(*cp, cfg, flags);
            break;

        case VERT_DOMAIN_CREATE_PERSISTENT:
            *dp = virDomainDefineXML(*cp, cfg);

            if (virDomainCreate(*dp) < 0) {
                res = verterr(env);
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

/* 0: virDomainPtr, 1: type */
    static ERL_NIF_TERM
nif_DomainGet(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dp = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_ATTR_AUTOSTART: {
            int autostart = 0;

            VERTERR(virDomainGetAutostart(*dp, &autostart) < 0);

            term = (autostart ? atom_true : atom_false);
            }
            break;

#ifdef HAVE_VIRDOMAINGETBLOCKINFO
        case VERT_ATTR_BLOCKINFO: {
            char path[MAXPATHLEN];
            virDomainInfo info = {0};
            ERL_NIF_TERM buf = {0};

            if (argc != 3 || !enif_get_string(env, argv[2], path, sizeof(path), ERL_NIF_LATIN1))
                return enif_make_badarg(env);

            VERTERR(virDomainGetBlockInfo(*dp, path, &info, 0) < 0);
            buf = bincopy(env, &info, sizeof(virDomainInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;
#endif

        case VERT_ATTR_CONNECT: {
            virConnectPtr *cp = NULL;
            ERL_NIF_TERM res = {0};

            cp = enif_alloc_resource(LIBVIRT_CONNECT_RESOURCE, sizeof(virConnectPtr));

            ISNULL(cp);

            *cp = virDomainGetConnect(*dp);

            VERTERR(*cp == NULL);

            res = enif_make_resource(env, cp);
            enif_release_resource(cp);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple4(env,
                    atom_resource,
                    atom_connect,
                    enif_make_ref(env), res));
            }
            break;

        case VERT_ATTR_ID: {
            unsigned int id = 0;

            id = virDomainGetID(*dp);

            VERTERR(id < 0);

            term = enif_make_uint(env, id);
            }
            break;

        case VERT_ATTR_INFO: {
            virDomainInfo info = {0};
            ERL_NIF_TERM buf = {0};
            
            VERTERR(virDomainGetInfo(*dp, &info) < 0);

            buf = bincopy(env, &info, sizeof(virDomainInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

#ifdef HAVE_VIRDOMAINGETJOBINFO
        case VERT_ATTR_JOBINFO: {
            virDomainJobInfo info = {0};
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetJobInfo(*dp, &info) < 0);
            buf = bincopy(env, &info, sizeof(virDomainInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;
#endif

        case VERT_ATTR_MAXMEMORY: {
            unsigned long mem = 0;

            mem = virDomainGetMaxMemory(*dp);    // XXX can also be NULL for domain0
            VERTERR(mem == 0);

            term = enif_make_ulong(env, mem);
            }
            break;

        case VERT_ATTR_MAXVCPUS: {
            int max = -1;

            max = virDomainGetMaxVcpus(*dp);

            VERTERR(max < 0);

            term = enif_make_int(env, max);
            }
            break;

#ifdef HAVE_VIRDOMAINGETMEMORYPARAMETERS
        case VERT_ATTR_MEMORYPARAMETERS: {
            int n = 0;
            ErlNifBinary buf = {0};

            VERTERR( (virDomainGetMemoryParameters(*dp, NULL, &n, 0) < 0) || n == 0);
            
            if (!enif_alloc_binary(sizeof(virMemoryParameter)*n, &buf))
                return atom_enomem;

            VERTERR(virDomainGetMemoryParameters(*dp, buf.data, &n, 0) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple4(env,
                    enif_make_atom(env, "parameter"),
                    erl_make_binary(env, &buf),
                    erl_make_int(env, n)
                    ));
            }
            break;
#endif

        case VERT_ATTR_NAME: {
            const char *name = NULL;

            name = virDomainGetName(*dp);

            VERTERR(*dp == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_OSTYPE: {
            char *type = NULL;  /* should be freed */

            type = virDomainGetOSType(*dp);

            VERTERR(type == NULL);

            term = enif_make_string(env, type, ERL_NIF_LATIN1);
            free(type);
            }
            break;
            
        case VERT_ATTR_SCHEDULERPARAMETERS: {
            virSchedParameter params;
            int n = 0;
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetSchedulerParameters(*dp, &params, &n) < 0);

            buf = bincopy(env, &params, sizeof(virSchedParameter));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_SCHEDULERTYPE: {
            char *type = NULL;
            int n = 0;
            ERL_NIF_TERM buf = {0};

            type = virDomainGetSchedulerType(*dp, &n);

            VERTERR(type == NULL);
            buf = bincopy(env, type, strlen(type)+1);
            NOMEM(buf);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple3(env,
                    enif_make_atom(env, "parameter"),
                    buf,
                    enif_make_int(env, n)
                    ));

            free(type);
            }
            break;

        case VERT_ATTR_SECURITYLABEL: {
            virSecurityLabel label;
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetSecurityLabel(*dp, &label) < 0);
            buf = bincopy(env, &label, sizeof(virSecurityLabel));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_RAWUUID: {
            unsigned char uuid[VIR_UUID_BUFLEN];
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetUUID(*dp, uuid) < 0);
            buf = bincopy(env, &uuid, sizeof(VIR_UUID_BUFLEN));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_UUID: {
            char uuid[VIR_UUID_STRING_BUFLEN];

            VERTERR(virDomainGetUUIDString(*dp, uuid) < 0);

            term = enif_make_tuple2(env, atom_ok,
                enif_make_string(env, uuid, ERL_NIF_LATIN1));
            }
            break;

/*
        case VERT_ATTR_VCPUS: {
            virVcpuInfo info = {0};
            int max = 0;

            VERTERR(virDomainGetVcpus(*dp, &info, maxinfo, cpumaps, int maplen) < 0);

            }
            break;

        case VERT_ATTR_VCPUSFLAGS: {
            }
            break;
*/

        case VERT_ATTR_DESC: {
            char *desc = NULL;
            int flags = 0;

            if (argc != 3 || !enif_get_int(env, argv[1], &type))
                return enif_make_badarg(env);

            desc = virDomainGetXMLDesc(*dp, flags);

            VERTERR(desc == NULL);

            term = enif_make_tuple2(env, atom_ok,
                enif_make_string(env, desc, ERL_NIF_LATIN1));

            free(desc);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    return term;
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

    VERTERR(res != 0);

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

    VERTERR(res != 0);

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

    VERTERR(res != 0);

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

    VERTERR(res != 0);

    return atom_ok;
}


/* Interfaces */

/* 0: virConnectPtr, 1: int type 2: int | char* */
    static ERL_NIF_TERM
nif_InterfaceLookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_ATTR_NAME;

    virInterfacePtr *ifp = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    ifp = enif_alloc_resource(LIBVIRT_INTERFACE_RESOURCE, sizeof(virInterfacePtr));

    ISNULL(ifp);

    switch (type) {
        case VERT_ATTR_NAME: {
                char name[1024]; /* XXX max interface length ??? */

                if (argc != 3 || enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *ifp = virInterfaceLookupByName(*cp, name);
            }
            break;

        case VERT_ATTR_MAC: {
                char mac[1024]; /* XXX max size ??? */

                if (argc != 3 || enif_get_string(env, argv[2], mac, sizeof(mac), ERL_NIF_LATIN1) < 1)
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
    int type = VERT_ATTR_NAME;

    const char *res = NULL;


    if (!enif_get_resource(env, argv[0], LIBVIRT_INTERFACE_RESOURCE, (void **)&ifp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_ATTR_NAME:
            res = virInterfaceGetName(*ifp);
            break;

        case VERT_ATTR_MAC:
            res = virInterfaceGetMACString(*ifp);
            break;

        case VERT_ATTR_DESC:
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


/* Network */

/* 0: virConnectPtr, 1: int type 2: int | char* */
    static ERL_NIF_TERM
nif_NetworkLookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *cp = NULL;
    int type = VERT_ATTR_NAME;

    virNetworkPtr *np = NULL;
    ERL_NIF_TERM res = {0};


    if (argc != 3)
        return enif_make_badarg(env);

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&cp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    np = enif_alloc_resource(LIBVIRT_NETWORK_RESOURCE, sizeof(virNetworkPtr));

    ISNULL(np);

    switch (type) {
        case VERT_ATTR_NAME: {
                char name[1024]; /* XXX max interface length ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *np = virNetworkLookupByName(*cp, name);
            }
            break;

        case VERT_ATTR_RAWUUID: {
                ErlNifBinary buf = {0};

                if (!enif_inspect_iolist_as_binary(env, argv[2], &buf))
                    return enif_make_badarg(env);

                *np = virNetworkLookupByUUID(*cp, buf.data);
            }
            break;

        case VERT_ATTR_UUID: {
                ErlNifBinary buf = {0};

                if (!enif_inspect_iolist_as_binary(env, argv[2], &buf))
                    return enif_make_badarg(env);

                *np = virNetworkLookupByUUIDString(*cp, (const char *)buf.data);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*np == NULL) {
        enif_release_resource(np);
        return verterr(env);
    }

    res = enif_make_resource(env, np);
    enif_release_resource(np);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: virDomainPtr, 1: type */
    static ERL_NIF_TERM
nif_NetworkGet(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virNetworkPtr *np = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_NETWORK_RESOURCE, (void **)&np))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_ATTR_AUTOSTART: {
            int autostart = 0;

            VERTERR(virNetworkGetAutostart(*np, &autostart) < 0);

            term = (autostart ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_BRIDGENAME: {
            char *name = NULL;

            name = virNetworkGetBridgeName(*np);

            VERTERR(name == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            free(name);
            }
            break;

        case VERT_ATTR_NAME: {
            const char *name = NULL;

            name = virNetworkGetName(*np);

            VERTERR(name == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_RAWUUID: {
            unsigned char uuid[VIR_UUID_BUFLEN];

            VERTERR(virNetworkGetUUID(*np, uuid) < 0);
            term = bincopy(env, uuid, sizeof(uuid));
            NOMEM(term);
            }
            break;

        case VERT_ATTR_UUID: {
            char uuid[VIR_UUID_STRING_BUFLEN];

            VERTERR (virNetworkGetUUIDString(*np, uuid) < 0);

            term = enif_make_string(env, uuid, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_DESC: {
            char *desc = NULL;
            int flags = 0;

            if (argc != 3 || !enif_get_int(env, argv[2], &flags))
                return enif_make_badarg(env);

            desc = virNetworkGetXMLDesc(*np, flags);

            VERTERR(desc == NULL);

            term = enif_make_string(env, desc, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_ACTIVE: {
            int res = -1;

            res = virNetworkIsPersistent(*np);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_PERSISTENT: {
            int res = -1;

            res = virNetworkIsPersistent(*np);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    return term;
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

    static ERL_NIF_TERM
bincopy(ErlNifEnv *env, void *src, size_t len)
{
    ErlNifBinary buf = {0};

    if (!enif_alloc_binary(len, &buf))
        return atom_enomem;

    (void)memcpy(buf.data, src, buf.size);

    return enif_make_binary(env, &buf);
}


/*
 * Callbacks
 */
    void
connection_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virConnectClose(*p);
}

    void
domain_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virDomainFree(*p);
}

    void
interface_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virInterfaceFree(*p);
}

    void
network_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virNetworkFree(*p);
}

    void
storagepool_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virStoragePoolFree(*p);
}

#if THIS_VERSION_SUPPORTS_FILTER
    void
filter_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virNWFilterFree(*p);
}
#endif

    void
secret_cleanup(ErlNifEnv *env, void *obj)
{
    void **p = obj;

    if (*p)
        (void)virSecretFree(*p);
}


    void
null_logger(void *userData, virErrorPtr error)
{
}


static ErlNifFunc nif_funcs[] = {
    /* connect */
    {"connect_open", 2, nif_virConnectOpen},
    {"connect_close", 1, nif_virConnectClose},

    {"connect_get", 2, nif_ConnectGet},
    {"connect_get", 3, nif_ConnectGet},

    {"connect_get_numactive", 2, nif_ConnectNumActive},
    {"connect_get_numinactive", 2, nif_ConnectNumInactive},
    {"connect_get_listactive", 3, nif_ConnectGetListActive},
    {"connect_get_listinactive", 3, nif_ConnectGetListInactive},

    /* domain */
    {"domain_lookup", 3, nif_DomainLookup},

    {"domain_get", 2, nif_DomainGet},
    {"domain_get", 3, nif_DomainGet},

    {"domain_create", 4, nif_virDomainCreate},
    {"domain_save", 2, nif_virDomainSave},
    {"domain_restore", 2, nif_virDomainRestore},
    {"domain_shutdown", 1, nif_virDomainShutdown},

    {"domain_set_autostart", 2, nif_virDomainSetAutostart},

    /* interface */
    {"interface_lookup", 3, nif_InterfaceLookup},
    {"interface_get", 2, nif_InterfaceGet},

    /* network */
    {"network_get", 2, nif_NetworkGet},
    {"network_lookup", 2, nif_NetworkLookup},

    {"resource_free", 2, nif_ResourceFree},
    {"resource_destroy", 2, nif_ResourceDestroy},
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)

