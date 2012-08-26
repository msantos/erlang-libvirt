/* Copyright (c) 2010-2012, Michael Santos <michael.santos@gmail.com>
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


VERT_FUN_INT_RES(virConnectIsEncrypted, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectIsSecure, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfDomains, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfInterfaces, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfNetworks, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfStoragePools, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfSecrets, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfDefinedDomains, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfDefinedInterfaces, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfDefinedNetworks, VERT_RES_CONNECT)
VERT_FUN_INT_RES(virConnectNumOfDefinedStoragePools, VERT_RES_CONNECT)
#ifdef HAVE_NWFILTER
VERT_FUN_INT_RES(virConnectNumOfNWFilters, VERT_RES_CONNECT)
#else
VERT_FUN_UNSUPPORTED(virConnectNumOfNWFilters)
#endif

VERT_FUN_GETNAME(virConnectGetType, VERT_RES_CONNECT)

VERT_FUN_CHARP_RES(virConnectGetCapabilities, VERT_RES_CONNECT)
VERT_FUN_CHARP_RES(virConnectGetHostname, VERT_RES_CONNECT)
VERT_FUN_CHARP_RES(virConnectGetURI, VERT_RES_CONNECT)

VERT_FUN_ULONG_RES(virConnectGetLibVersion, VERT_RES_CONNECT)
VERT_FUN_ULONG_RES(virConnectGetVersion, VERT_RES_CONNECT)

VERT_FUN_INT_RES_CHARPP_INT(virConnectListInterfaces, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListNetworks, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListSecrets, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListStoragePools, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListDefinedDomains, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListDefinedInterfaces, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListDefinedNetworks, VERT_RES_CONNECT)
VERT_FUN_INT_RES_CHARPP_INT(virConnectListDefinedStoragePools, VERT_RES_CONNECT)

#if HAVE_NWFILTER
VERT_FUN_INT_RES_CHARPP_INT(virConnectListNWFilters, VERT_RES_CONNECT)
#else
VERT_FUN_UNSUPPORTED(virConnectListNWFilters)
#endif


/* If the string is truncated, return badarg
 * If the string is empty or has the wrong encoding, consider it to be NULL
 *  This assumes that enif_get_string() does not modify the buffer
 * If the string has a length, ok
 */
    ERL_NIF_TERM
vert_virConnectOpen(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return vert_connect_res_charp(env, argv, virConnectOpen);
}

    ERL_NIF_TERM
vert_virConnectOpenReadOnly(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return vert_connect_res_charp(env, argv, virConnectOpenReadOnly);
}

    ERL_NIF_TERM
vert_virConnectGetMaxVcpus(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary buf = {0};
    int max = -1;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, buf);

    VERT_BIN_APPEND_NULL(buf);

    max = virConnectGetMaxVcpus(vp->res,
            (buf.data[0] == '\0' ? NULL : (char *)buf.data));
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
    unsigned long long *mem = NULL;
    ERL_NIF_TERM list = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    mem = calloc(max, sizeof(unsigned long long));
    ISNULL(mem);

    VERTERR(virNodeGetCellsFreeMemory(vp->res, mem, 0, max) < 0);

    VERT_COPY_LIST(list, mem, max);

    free(mem);

    return enif_make_tuple2(env,
        atom_ok,
        list);
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
vert_virConnectListDomains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int max = 0;
    int *array = NULL;

    int n = -1;
    ERL_NIF_TERM list = {0};

    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, max);

    if (max <= 0)
        return error_tuple(env, atom_badarg);

    array = calloc(max, sizeof(int));
    ISNULL(array);

    n = virConnectListDomains(vp->res, array, max);

    VERTERR(n < 0);

    VERT_COPY_LIST(list, array, n);

    free(array);

    return enif_make_tuple2(env,
            atom_ok,
            list);
}


/*
 * Internal functions
 */
    ERL_NIF_TERM
vert_connect_res_charp(
        ErlNifEnv *env,
        const ERL_NIF_TERM argv[],
        virConnectPtr (*fp)(const char *))
{
    ErlNifBinary buf = {0};

    VERT_RESOURCE *vp = NULL;


    VERT_GET_IOLIST(0, buf);

    VERT_BIN_APPEND_NULL(buf);

    VERT_RES_ALLOC(vp, VERT_RES_CONNECT, NULL);

    vp->res = fp( (buf.data[0] == '\0' ? NULL : (char *)buf.data));

    if (vp->res == NULL) {
        enif_release_resource(vp);
        return verterr(env);
    }

    /* Disable logging to stderr */
    virConnSetErrorFunc(vp->res, NULL, NULL);

    return vert_make_resource(env, vp, atom_connect);
}
