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
#include "vert_connect.h"


/* If the string is truncated, return badarg
 * If the string is empty or has the wrong encoding, consider it to be NULL
 *  This assumes that enif_get_string() does not modify the buffer
 * If the string has a length, ok
 */
    ERL_NIF_TERM
vert_virConnectOpen(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char name[HOST_NAME_MAX] = {0};

    VERT_RESOURCE *vp = NULL;


    VERT_GET_STRING(0, name, sizeof(name));

    RESOURCE_ALLOC(vp, VERT_RES_CONNECT, NULL);

    vp->res = virConnectOpen( (name[0] == '\0' ? NULL : name));

    CHECK_VIRPTR_NULL(vp);

    /* Disable logging to stderr */
    virConnSetErrorFunc(vp->res, NULL, NULL);

    VERT_RET_RESOURCE(vp, atom_connect);
}

    ERL_NIF_TERM
vert_virConnectOpenReadOnly(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    char name[HOST_NAME_MAX] = {0};

    VERT_RESOURCE *vp = NULL;


    VERT_GET_STRING(0, name, sizeof(name));

    RESOURCE_ALLOC(vp, VERT_RES_CONNECT, NULL);

    vp->res = virConnectOpenReadOnly( (name[0] == '\0' ? NULL : name));

    CHECK_VIRPTR_NULL(vp);

    /* Disable logging to stderr */
    virConnSetErrorFunc(vp->res, NULL, NULL);

    VERT_RET_RESOURCE(vp, atom_connect);
}

/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_virConnectClose(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERTERR(virConnectClose(vp->res) == -1);

    vp->res = NULL;

    return atom_ok;
}

    ERL_NIF_TERM
vert_virConnectGetCapabilities(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char *cap = NULL;

    ERL_NIF_TERM term = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    cap = virConnectGetCapabilities(vp->res);

    VERTERR(cap == NULL);

    term = enif_make_tuple2(env, atom_ok,
        enif_make_string(env, cap, ERL_NIF_LATIN1));

    free(cap);

    return term;
}

    ERL_NIF_TERM
vert_virConnectGetHostname(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char *hostname = NULL;

    ERL_NIF_TERM term = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    hostname = virConnectGetHostname(vp->res);

    VERTERR(hostname == NULL);

    term = enif_make_tuple2(env, atom_ok,
        enif_make_string(env, hostname, ERL_NIF_LATIN1));

    free(hostname);

    return term;
}

    ERL_NIF_TERM
vert_virConnectGetLibVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    unsigned long version = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERTERR(virConnectGetLibVersion(vp->res, &version) < 0);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_ulong(env, version));
}

    ERL_NIF_TERM
vert_virConnectGetMaxVcpus(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char type[1024] = {0};
    int max = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_STRING(1, type, sizeof(type));

    max = virConnectGetMaxVcpus(vp->res, (type[0] == '\0' ? NULL : type));
    VERTERR(max < 0);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_int(env, max));
}

    ERL_NIF_TERM
vert_virNodeGetFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    u_int64_t mem = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    mem = virNodeGetFreeMemory(vp->res);
    VERTERR(mem == 0);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_uint64(env, mem));
}

    ERL_NIF_TERM
vert_virNodeGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    virNodeInfo info;
    ERL_NIF_TERM buf = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERTERR(virNodeGetInfo(vp->res, &info) < 0);

    BINCOPY(buf, &info, sizeof(virNodeInfo));

    return enif_make_tuple2(env,
        atom_ok, buf);
}

    ERL_NIF_TERM
vert_virNodeGetCellsFreeMemory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    u_int64_t *mem = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    mem = calloc(max, sizeof(u_int64_t));
    ISNULL(mem);

    VERTERR(virNodeGetCellsFreeMemory(vp->res, mem, 0, max) < 0);

    VERT_COPY_LIST(list, mem, max);

    free(mem);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectGetType(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    const char *name = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    name = virConnectGetType(vp->res);
    VERTERR(name == NULL);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, name, ERL_NIF_LATIN1));
}

    ERL_NIF_TERM
vert_virConnectGetVersion(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    unsigned long version = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERTERR(virConnectGetVersion(vp->res, &version) < 0);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_ulong(env, version));
}

    ERL_NIF_TERM
vert_virConnectGetURI(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char *uri = NULL;

    ERL_NIF_TERM term = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    uri = virConnectGetURI(vp->res);

    VERTERR(uri == NULL);

    term = enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, uri, ERL_NIF_LATIN1));

    free(uri);

    return term;
}

    ERL_NIF_TERM
vert_virConnectIsEncrypted(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectIsEncrypted(vp->res);

    VERTERR(n == -1);

    return (n == 1 ? atom_true : atom_false);
}

    ERL_NIF_TERM
vert_virConnectIsSecure(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectIsSecure(vp->res);

    VERTERR(n == -1);

    return (n == 1 ? atom_true : atom_false);
}

    ERL_NIF_TERM
vert_virNodeGetSecurityModel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    virSecurityModel model;
    ERL_NIF_TERM buf = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERTERR(virNodeGetSecurityModel(vp->res, &model) < 0);
    BINCOPY(buf, &model, sizeof(virSecurityModel));

    return enif_make_tuple2(env,
        atom_ok, buf);
}

    ERL_NIF_TERM
vert_virConnectNumOfDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfDomains(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfInterfaces(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfInterfaces(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfNetworks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfNetworks(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfStoragePools(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfStoragePools(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfSecrets(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfSecrets(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfNWFilters(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#ifdef HAVE_VIRCONNECTNUMOFNWFILTERS
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfNWFilters(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
#else
    return error_tuple(env, atom_unsupported);
#endif
}

    ERL_NIF_TERM
vert_virConnectNumOfDefinedDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfDefinedDomains(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfDefinedInterfaces(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfDefinedInterfaces(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfDefinedNetworks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfDefinedNetworks(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectNumOfDefinedStoragePools(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int n = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    n = virConnectNumOfDefinedStoragePools(vp->res);

    VERTERR(n == -1);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virConnectListDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    int *domains = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};

    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    domains = calloc(max, sizeof(int));
    ISNULL(domains);

    n = virConnectListDomains(vp->res, domains, max);

    VERTERR(n == -1);

    VERT_COPY_LIST(list, domains, n);

    free(domains);

    return enif_make_tuple2(env,
            atom_ok,
            list);
}

    ERL_NIF_TERM
vert_virConnectListInterfaces(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    char **names = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListInterfaces(vp->res, names, max);

    VERTERR(n == -1);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListNetworks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    char **names = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListNetworks(vp->res, names, max);

    VERTERR(n == -1);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListNWFilters(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
#if HAVE_VIRCONNECTlISTNWfILTERS
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    char **names = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListNWFilters(vp->res, names, max);

    VERTERR(n == -1);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
#else
    return error_tuple(env, atom_unsupported);
#endif
}

    ERL_NIF_TERM
vert_virConnectListSecrets(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    char **names = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListSecrets(vp->res, names, max);

    VERTERR(n == -1);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListStoragePools(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    char **names = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListStoragePools(vp->res, names, max);

    VERTERR(n == -1);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListDefinedDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;

    int n = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListDefinedDomains(vp->res, names, max);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListDefinedInterfaces(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;

    int n = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListDefinedInterfaces(vp->res, names, max);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListDefinedNetworks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;

    int n = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListDefinedNetworks(vp->res, names, max);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}

    ERL_NIF_TERM
vert_virConnectListDefinedStoragePools(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;

    int n = -1;

    char **names = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return enif_make_badarg(env);

    names = calloc(max, sizeof(char *));
    ISNULL(names);

    n = virConnectListDefinedStoragePools(vp->res, names, max);

    VERT_COPY_STRING(list, names, n);

    free(names);

    return enif_make_tuple2(env,
        atom_ok,
        list);
}
