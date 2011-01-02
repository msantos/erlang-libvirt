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
#include "erl_nif.h"

#include "vert.h"


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_enomem;
static ERL_NIF_TERM atom_connect;
static ERL_NIF_TERM atom_domain;
static ERL_NIF_TERM atom_true;
static ERL_NIF_TERM atom_false;

static ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *err);

void error(void *userData, virErrorPtr error);
static ERL_NIF_TERM verterr(ErlNifEnv *env);

void connection_cleanup(ErlNifEnv *env, void *obj);
void domain_cleanup(ErlNifEnv *env, void *obj);

static ErlNifResourceType *LIBVIRT_CONNECT_RESOURCE;
static ErlNifResourceType *LIBVIRT_DOMAIN_RESOURCE;


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_enomem = enif_make_atom(env, "enomem");
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

    virConnectPtr *conn = NULL;
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

    conn = enif_alloc_resource(LIBVIRT_CONNECT_RESOURCE, sizeof(virConnectPtr));

    if (conn == NULL)
        return atom_error;

    switch (type) {
        case VERT_CONNECT_OPEN:
            *conn = virConnectOpen( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_READONLY:
            *conn = virConnectOpenReadOnly( (name[0] == '\0' ? NULL : name));
            break;

        case VERT_CONNECT_OPEN_AUTH:
            return enif_make_badarg(env);

        default:
            return enif_make_badarg(env);
    }

    if (*conn == NULL) {
        enif_release_resource(conn);
        return verterr(env);
    }

    /* XXX disable logging to stderr */
    virConnSetErrorFunc(*conn, NULL, NULL);

    res = enif_make_resource(env, conn);
    enif_release_resource(conn);

    return enif_make_tuple(env, 2,
        atom_ok,
        enif_make_tuple(env, 3,
            atom_connect,
            enif_make_ref(env), res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectClose(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    ERL_NIF_TERM res = atom_ok;

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (virConnectClose(*conn) != 0)
        res = verterr(env);

    conn = NULL;

    return res;
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetCapabilities(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    char *res = NULL;
    ERL_NIF_TERM capabilities = {0};

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetCapabilities(*conn);

    if (res == NULL)
        return verterr(env);

    capabilities = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple(env, 2,
            atom_ok,
            capabilities);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetHostname(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    char *res = NULL;
    ERL_NIF_TERM hostname = {0};

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetHostname(*conn);

    if (res == NULL)
        return verterr(env);

    hostname = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple(env, 2,
            atom_ok,
            hostname);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetLibVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int res = -1;
    unsigned long version = -1;

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetLibVersion(*conn, &version);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_ulong(env, version));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetMaxVcpus(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int res = -1;
    char type[1024];


    (void)memset(type, '\0', sizeof(type));

    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], type, sizeof(type), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    res = virConnectGetMaxVcpus(*conn, (type[0] == '\0' ? NULL : type));

    if (res == -1)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_int(env, res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    u_int64_t res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virNodeGetFreeMemory(*conn);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_uint64(env, res));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    virNodeInfo info;
    ErlNifBinary buf = {0};
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virNodeGetInfo(*conn, &info);

    if (res == -1)
        return verterr(env);

    if (!enif_alloc_binary(sizeof(virNodeInfo), &buf))
        return atom_enomem;

    (void)memcpy(buf.data, &info, buf.size);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: virConnectPtr, 1: int maxnodes */
    static ERL_NIF_TERM
nif_virNodeGetCellsFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int maxnodes = 0;
    u_int64_t *mem = NULL;
    int res = -1;
    int i = 0;

    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &maxnodes) || maxnodes <= 0)
        return enif_make_badarg(env);

    mem = calloc(maxnodes, sizeof(u_int64_t));

    if (mem == NULL)
        return atom_enomem;

    res = virNodeGetCellsFreeMemory(*conn, mem, 0, maxnodes);

    if (res == -1)
        return verterr(env);

    list = enif_make_list(env, 0);
    for (i = 0; i < res; i++)
        list = enif_make_list_cell(env, enif_make_int(env, mem[i]), list);

    free(mem);

    return enif_make_tuple(env, 2,
            atom_ok,
            list);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetType(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    const char *res = NULL;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetType(*conn);

    if (res == NULL)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_string(env, res, ERL_NIF_LATIN1));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int res = -1;
    unsigned long version = 0;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetVersion(*conn, &version);

    if (res == -1)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_ulong(env, version));
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectGetURI(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    char *res = NULL;
    ERL_NIF_TERM uri = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectGetURI(*conn);

    if (res == NULL)
        return verterr(env);

    uri = enif_make_string(env, res, ERL_NIF_LATIN1);
    free(res);

    return enif_make_tuple(env, 2,
            atom_ok,
            uri);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectIsEncrypted(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectIsEncrypted(*conn);

    if (res == -1)
        return verterr(env);

    return (res == 1 ? atom_true : atom_false);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virConnectIsSecure(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virConnectIsSecure(*conn);

    if (res == -1)
        return verterr(env);

    return (res == 1 ? atom_true : atom_false);
}

/* 0: virConnectPtr */
    static ERL_NIF_TERM
nif_virNodeGetSecurityModel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    virSecurityModel sec;
    ErlNifBinary buf = {0};
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    res = virNodeGetSecurityModel(*conn, &sec);

    if (res == -1)
        return verterr(env);

    if (!enif_alloc_binary(sizeof(virSecurityModel), &buf))
        return atom_enomem;

    (void)memcpy(buf.data, &sec, buf.size);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_binary(env, &buf));
}

/* 0: virConnectPtr, 1: int type */
    static ERL_NIF_TERM
nif_virConnectNumOfDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int type = VERT_DOMAIN_LIST_ACTIVE;
    int res = -1;


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    switch (type) {
        case VERT_DOMAIN_LIST_ACTIVE:
            res = virConnectNumOfDomains(*conn);
            break;
        case VERT_DOMAIN_LIST_INACTIVE:
            res = virConnectNumOfDefinedDomains(*conn);
            break;
        default:
            return enif_make_badarg(env);
    }

    if (res == -1)
        return verterr(env);

    return enif_make_tuple(env, 2,
            atom_ok,
            enif_make_int(env, res));
}


/**
 ** Domain operations
 **/

/* 0: virConnectPtr, 1: int type 2: int | char* */
    static ERL_NIF_TERM
nif_virDomainLookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    virDomainPtr *dom = NULL;
    int type = VERT_DOMAIN_LOOKUP_BY_ID;

    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    dom = enif_alloc_resource(LIBVIRT_DOMAIN_RESOURCE, sizeof(virDomainPtr));

    if (dom == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_DOMAIN_LOOKUP_BY_ID: {
                int id = 0;

                if (!enif_get_int(env, argv[2], &id))
                    return enif_make_badarg(env);
                *dom = virDomainLookupByID(*conn, id);
            }
            break;

        case VERT_DOMAIN_LOOKUP_BY_NAME: {
                char name[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *dom = virDomainLookupByName(*conn, name);
            }
            break;

        case VERT_DOMAIN_LOOKUP_BY_UUID: {
                char uuid[1024]; /* XXX max size ??? */

                if (enif_get_string(env, argv[2], uuid, sizeof(uuid), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                *dom = virDomainLookupByUUID(*conn, (const unsigned char *)uuid);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*dom == NULL) {
        enif_release_resource(dom);
        return verterr(env);
    }

    res = enif_make_resource(env, dom);
    enif_release_resource(dom);

    return enif_make_tuple(env, 2,
        atom_ok,
        enif_make_tuple(env, 3,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: virDomainPtr */
    static ERL_NIF_TERM
nif_virDomainFree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virDomainPtr *dom = NULL;
    ERL_NIF_TERM res = atom_ok;


    if (!enif_get_resource(env, argv[0], LIBVIRT_DOMAIN_RESOURCE, (void **)&dom))
        return enif_make_badarg(env);

    if (virDomainFree(*dom) != 0)
        res = verterr(env);

    *dom = NULL;

    return res;
}

/* 0: virConnectPtr, 1: int type, 2: maxdomains */
    static ERL_NIF_TERM
nif_virDomainList(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int type = VERT_DOMAIN_LIST_ACTIVE;
    int maxdomains = 0;

    int i = 0;
    int res = -1;

    ERL_NIF_TERM list = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[2], &maxdomains) || maxdomains <= 0)
        return enif_make_badarg(env);

    list = enif_make_list(env, 0);

    switch (type) {
        case VERT_DOMAIN_LIST_ACTIVE: {
            int *domains = NULL;

            domains = calloc(maxdomains, sizeof(int));

            if (domains == NULL)
                return atom_enomem;

            res = virConnectListDomains(*conn, domains, maxdomains);

            if (res == -1)
                return verterr(env);

            for (i = 0; i < res; i++)
                list = enif_make_list_cell(env,
                        enif_make_int(env, domains[i]),
                        list);

            free(domains);
            }
            break;

        case VERT_DOMAIN_LIST_INACTIVE: {
            char **domains = NULL;

            domains = calloc(maxdomains, sizeof(char *));

            if (domains == NULL)
                return atom_enomem;

            res = virConnectListDefinedDomains(*conn, domains, maxdomains);

            if (res == -1)
                return verterr(env);

            for (i = 0; i < res; i++)
                list = enif_make_list_cell(env,
                        enif_make_string(env, domains[i], ERL_NIF_LATIN1),
                        list);

            free(domains);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    return enif_make_tuple(env, 2,
        atom_ok,
        list);
}

/* 0: virConnectPtr, 1: int type 2: char *, 3: int flags */
    static ERL_NIF_TERM
nif_virDomainCreate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    virConnectPtr *conn = NULL;
    int type = VERT_DOMAIN_CREATE_TRANSIENT;
    char cfg[8192]; /* XXX size ??? this is XML after all */
    int flags = 0;

    virDomainPtr *dom = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], LIBVIRT_CONNECT_RESOURCE, (void **)&conn))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    /* XXX use binary */
    if (enif_get_string(env, argv[2], cfg, sizeof(cfg), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[3], &flags))
        return enif_make_badarg(env);

    dom = enif_alloc_resource(LIBVIRT_DOMAIN_RESOURCE, sizeof(virDomainPtr));

    if (dom == NULL)
        return atom_enomem;

    switch (type) {
        case VERT_DOMAIN_CREATE_TRANSIENT:
            *dom = virDomainCreateXML(*conn, cfg, flags);
            break;

        case VERT_DOMAIN_CREATE_PERSISTENT:
            *dom = virDomainDefineXML(*conn, cfg);

            if (virDomainCreate(*dom) < 0) {
                res = verterr(env);
                (void)virDomainFree(*dom);
                enif_release_resource(dom);
                return res;
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (*dom == NULL) {
        enif_release_resource(dom);
        return verterr(env);
    }

    res = enif_make_resource(env, dom);
    enif_release_resource(dom);

    return enif_make_tuple(env, 2,
        atom_ok,
        enif_make_tuple(env, 3,
            atom_domain,
            enif_make_ref(env), res));
}


/*
 * Utility functions
 */

    static ERL_NIF_TERM
verterr(ErlNifEnv *env)
{
    ERL_NIF_TERM res = {0};
    virErrorPtr err = {0};

    /*
    virCopyLastError(&err);
    errmsg = error_tuple(env, err.message);
    virResetError(&err);
    return errmsg;
    */

    err = virSaveLastError();
    res = error_tuple(env, err->message);
    virFreeError(err);

    return res;
}


    static ERL_NIF_TERM
error_tuple(ErlNifEnv *env, char *err)
{
    return enif_make_tuple(env, 2,
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
    {"connect_get_numdomains", 2, nif_virConnectNumOfDomains},

    {"connect_is_encrypted", 1, nif_virConnectIsEncrypted},
    {"connect_is_secure", 1, nif_virConnectIsSecure},

    /* domain */
    {"domain_lookup", 3, nif_virDomainLookup},
    {"domain_free", 1, nif_virDomainFree},
    {"domain_list", 3, nif_virDomainList},

    {"domain_create", 4, nif_virDomainCreate},

};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)

