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
#include "vert.h"
#include "vert_util.h"
#include "vert_domain.h"
#include "vert_interface.h"
#include "vert_network.h"


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_undefined = enif_make_atom(env, "undefined");
    atom_enomem = enif_make_atom(env, "enomem");
    atom_resource = enif_make_atom(env, "resource");
    atom_connect = enif_make_atom(env, "connect");
    atom_domain = enif_make_atom(env, "domain");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");

    if ( (NIF_VERT_RESOURCE = enif_open_resource_type(env, NULL,
            "vert_resource", vert_cleanup,
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

    VERT_RESOURCE *vp = NULL;
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

    RESALLOC(vp, VERT_RES_CONNECT);

    switch (type) {
        case VERT_CONNECT_OPEN:
            vp->res = virConnectOpen( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_READONLY:
            vp->res = virConnectOpenReadOnly( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_AUTH:
            return enif_make_badarg(env);

        default:
            return enif_make_badarg(env);
    }

    if (vp->res == NULL) {
        enif_release_resource(vp);
        return verterr(env);
    }

    /* XXX disable logging to stderr */
    virConnSetErrorFunc(vp->res, NULL, NULL);

    res = enif_make_resource(env, vp);
    enif_release_resource(vp);

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
    VERT_RESOURCE *vp = NULL;

    ERL_NIF_TERM res = atom_ok;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);
    VERTERR(virConnectClose(vp->res) == -1);
    vp->res = NULL;

    return res;
}

/* 0: virConnectPtr, 1: type */
    static ERL_NIF_TERM
nif_ConnectGet(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);

    switch (type) {
        case VERT_ATTR_CAPABILITIES: {
            char *cap = NULL;

            cap = virConnectGetCapabilities(vp->res);

            VERTERR(cap == NULL);

            term = enif_make_tuple2(env, atom_ok,
                    enif_make_string(env, cap, ERL_NIF_LATIN1));
            free(cap);
            }
            break;

        case VERT_ATTR_HOSTNAME: {
            char *hostname = NULL;

            hostname = virConnectGetHostname(vp->res);

            VERTERR(hostname == NULL);

            term = enif_make_tuple2(env, atom_ok,
                    enif_make_string(env, hostname, ERL_NIF_LATIN1));

            free(hostname);
            }
            break;

        case VERT_ATTR_LIBVERSION: {
            unsigned long version = -1;

            VERTERR(virConnectGetLibVersion(vp->res, &version) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_ulong(env, version));
            }
            break;

        case VERT_ATTR_MAXVCPUS: {
            char name[1024];
            int max = -1;

            (void)memset(name, '\0', sizeof(name));

            if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 0)
                return enif_make_badarg(env);

            max = virConnectGetMaxVcpus(vp->res, (name[0] == '\0' ? NULL : name));

            VERTERR(max < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_int(env, max));
            }
            break;

        case VERT_ATTR_FREEMEMORY: {
            u_int64_t mem = 0;

            mem = virNodeGetFreeMemory(vp->res);

            VERTERR(mem == 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_uint64(env, mem));
            }
            break;

        case VERT_ATTR_INFO: {
            virNodeInfo info;
            ERL_NIF_TERM buf = {0};

            VERTERR(virNodeGetInfo(vp->res, &info) < 0);
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

            VERTERR(virNodeGetCellsFreeMemory(vp->res, mem, 0, max) < 0);

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
            const char *name = NULL;

            name = virConnectGetType(vp->res);

            VERTERR(name == NULL);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_string(env, name, ERL_NIF_LATIN1));
            }
            break;

        case VERT_ATTR_VERSION: {
            unsigned long version = 0;

            VERTERR(virConnectGetVersion(vp->res, &version) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_ulong(env, version));
            }
            break;

        case VERT_ATTR_URI: {
            char *uri = NULL;

            uri = virConnectGetURI(vp->res);

            VERTERR(uri == NULL);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_string(env, uri, ERL_NIF_LATIN1));

            free(uri);
            }
            break;

        case VERT_ATTR_ENCRYPTED: {
            int res = -1;

            res = virConnectIsEncrypted(vp->res);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_SECURE: {
            int res = -1;

            res = virConnectIsSecure(vp->res);

            VERTERR(res == -1);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_SECURITYMODEL: {
            virSecurityModel model;
            ERL_NIF_TERM buf = {0};

            VERTERR(virNodeGetSecurityModel(vp->res, &model) < 0);
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
    VERT_RESOURCE *vp = NULL;
    int type = 0;

    int res = -1;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectNumOfDomains(vp->res);
            break;
        case VERT_RES_INTERFACE:
            res = virConnectNumOfInterfaces(vp->res);
            break;
        case VERT_RES_NETWORK:
            res = virConnectNumOfNetworks(vp->res);
            break;
        case VERT_RES_STORAGEPOOL:
            res = virConnectNumOfStoragePools(vp->res);
            break;
        case VERT_RES_SECRET:
            res = virConnectNumOfSecrets(vp->res);
            break;
        case VERT_RES_FILTER:
#ifdef THIS_VERSION_SUPPORTS_FILTER
            res = virConnectNumOfNWFilters(vp->res);
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

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_ConnectNumInactive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = 0;

    int res = -1;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectNumOfDefinedDomains(vp->res);
            break;
        case VERT_RES_INTERFACE:
            res = virConnectNumOfDefinedInterfaces(vp->res);
            break;
        case VERT_RES_NETWORK:
            res = virConnectNumOfDefinedNetworks(vp->res);
            break;
        case VERT_RES_STORAGEPOOL:
            res = virConnectNumOfDefinedStoragePools(vp->res);
            break;
        default:
            return enif_make_badarg(env);
    }

    VERTERR(res == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, res));
}


/* 0: VERT_RESOURCE */
    static ERL_NIF_TERM
nif_ResourceFree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    switch (vp->type) {
        case VERT_RES_DOMAIN:
            VERTERR(virDomainFree(vp->res) != 0);
            break;
        case VERT_RES_INTERFACE:
            VERTERR(virInterfaceFree(vp->res) != 0);
            break;
        case VERT_RES_NETWORK:
            VERTERR(virNetworkFree(vp->res) != 0);
            break;
        case VERT_RES_STORAGEPOOL:
            VERTERR(virStoragePoolFree(vp->res) != 0);
            break;
        case VERT_RES_FILTER:
#if THIS_VERSION_SUPPORTS_FILTER
            VERTERR(virNWFilterFree(vp->res) != 0);
#endif
            break;
        case VERT_RES_SECRET:
            VERTERR(virSecretFree(vp->res) != 0);
            break;
        default:
            return enif_make_badarg(env);

    }

    vp->res = NULL;

    return atom_ok;
}

/* 0: VERT_RESOURCE */
    static ERL_NIF_TERM
nif_ResourceDestroy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    switch (vp->type) {
        case VERT_RES_DOMAIN:
            VERTERR(virDomainDestroy(vp->res) != 0);
            break;
        case VERT_RES_INTERFACE:
            VERTERR(virInterfaceDestroy(vp->res, 0) != 0);
            break;
        case VERT_RES_NETWORK:
            VERTERR(virNetworkDestroy(vp->res) != 0);
            break;
        case VERT_RES_STORAGEPOOL:
            VERTERR(virStoragePoolDestroy(vp->res) != 0);
            break;
        default:
            return enif_make_badarg(env);
    }

    vp->res = NULL;

    return atom_ok;
}

/* 0: VERT_RESOURCE, 1: int type 2: int maxdomains */
    static ERL_NIF_TERM
nif_ConnectGetListActive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = 0;
    int max = 0;

    int i = 0;
    int res = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[2], &max) || max <= 0)
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);

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

            res = virConnectListDomains(vp->res, domains, max);

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
            res = virConnectListInterfaces(vp->res, names, max);
            break;
            
        case VERT_RES_NETWORK:
            res = virConnectListNetworks(vp->res, names, max);
            break;

        case VERT_RES_FILTER:
#if THIS_VERSION_SUPPORTS_FILTER
            res = virConnectListNWFilters(vp->res, names, max);
#else
            res = 0;
#endif
            break;

        case VERT_RES_SECRET:
            res = virConnectListSecrets(vp->res, names, max);
            break;

        case VERT_RES_STORAGEPOOL:
            res = virConnectListStoragePools(vp->res, names, max);
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

/* 0: VERT_RESOURCE, 1: int type, 2: int maxdomains */
    static ERL_NIF_TERM
nif_ConnectGetListInactive(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = 0;
    int max= 0;

    int i = 0;
    int res = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &max) || max <= 0)
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);

    names = calloc(max, sizeof(char *));

    ISNULL(names);

    list = enif_make_list(env, 0);

    switch (type) {
        case VERT_RES_DOMAIN:
            res = virConnectListDefinedDomains(vp->res, names, max);
            break;

        case VERT_RES_INTERFACE:
            res = virConnectListDefinedInterfaces(vp->res, names, max);
            break;

        case VERT_RES_NETWORK:
            res = virConnectListDefinedNetworks(vp->res, names, max);
            break;
            
        case VERT_RES_STORAGEPOOL:
            res = virConnectListDefinedNetworks(vp->res, names, max);
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
    {"domain_lookup", 3, vert_domain_lookup},

    {"domain_get", 2, vert_domain_get},
    {"domain_get", 3, vert_domain_get},

    {"domain_create", 4, vert_domain_create},
    {"domain_save", 2, vert_domain_save},
    {"domain_restore", 2, vert_domain_restore},
    {"domain_shutdown", 1, vert_domain_shutdown},

    {"domain_set_autostart", 2, vert_domain_autostart},

    /* interface */
    {"interface_lookup", 3, vert_interface_lookup},
    {"interface_get", 2, vert_interface_get},

    /* network */
    {"network_get", 2, vert_network_get},
    {"network_lookup", 3, vert_network_lookup},

    {"resource_free", 1, nif_ResourceFree},
    {"resource_destroy", 1, nif_ResourceDestroy},
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)

