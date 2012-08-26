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
#include <libvirt/libvirt.h>
#include <libvirt/virterror.h>
#include <stdio.h>
#include <string.h>
#include <sys/param.h>
#include <net/if.h>
#include "erl_nif.h"


ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_badarg;
ERL_NIF_TERM atom_undefined;
ERL_NIF_TERM atom_unsupported;
ERL_NIF_TERM atom_eagain;
ERL_NIF_TERM atom_enomem;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_resource;
ERL_NIF_TERM atom_connect;
ERL_NIF_TERM atom_domain;
ERL_NIF_TERM atom_interface;
ERL_NIF_TERM atom_network;
ERL_NIF_TERM atom_nwfilter;
ERL_NIF_TERM atom_stream;
ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_false;
ERL_NIF_TERM atom_vert;


#define VERTERR(x) do { \
    if ((x)) return verterr(env); \
} while (0)

#define ISNULL(x) do { \
    if ((x) == NULL) \
        return error_tuple(env, atom_enomem); \
} while (0)

#define BINCOPY(dst, src, size) do { \
    dst = bincopy(env, src, size); \
    if ((dst) == atom_enomem) \
        return error_tuple(env, atom_enomem); \
} while (0)


/* NIF resource */
ErlNifResourceType *NIF_VERT_RESOURCE;

typedef struct _vert_resource {
    int type;
    void *res;
    virConnectPtr conn;
} VERT_RESOURCE;

/* resource types */
enum {
    VERT_RES_CONNECT = 0,
    VERT_RES_DOMAIN,
    VERT_RES_INTERFACE,
    VERT_RES_NETWORK,
    VERT_RES_NWFILTER,
    VERT_RES_STORAGEPOOL,
    VERT_RES_FILTER,
    VERT_RES_SECRET,
    VERT_RES_STREAM,
};

#define VERT_RES_ALLOC(var,vtype,initial) do { \
    var = enif_alloc_resource(NIF_VERT_RESOURCE, sizeof(VERT_RESOURCE)); \
    ISNULL(var); \
    (var)->type = vtype; \
    (var)->res = NULL; \
    (var)->conn = (initial); \
} while (0)

#define VERT_GET_RESOURCE(index, var, vtype) do { \
    if (!enif_get_resource(env, argv[index], NIF_VERT_RESOURCE, (void **)&var) || \
        ((var)->type != (vtype))) \
        return error_tuple(env, atom_badarg); \
} while (0)

#define VERT_GET_INT(index, var) do { \
    if (enif_get_int(env, argv[(index)], &var) < 0) \
        return error_tuple(env, atom_badarg); \
} while (0)

#define VERT_GET_UINT(index, var) do { \
    if (enif_get_uint(env, argv[(index)], &var) < 0) \
        return error_tuple(env, atom_badarg); \
} while (0)

#define VERT_GET_ULONG(index, var) do { \
    if (enif_get_ulong(env, argv[(index)], &var) < 0) \
        return error_tuple(env, atom_badarg); \
} while (0)

#define VERT_GET_IOLIST(index, var) do { \
    if (!enif_inspect_iolist_as_binary(env, argv[(index)], &var)) \
        return error_tuple(env, atom_badarg); \
} while (0)

#define VERT_COPY_LIST(dst, src, size) do { \
    int i = 0; \
    dst = enif_make_list(env, 0); \
        for (i = 0; i < size; i++) \
            dst = enif_make_list_cell(env, \
                    enif_make_int(env, src[i]), dst); \
} while (0)

#define VERT_COPY_STRING(dst, src, size) do { \
    int i = 0; \
    dst = enif_make_list(env, 0); \
    for (i = 0; i < size; i++) \
        dst = enif_make_list_cell(env, \
            enif_make_string(env, src[i], ERL_NIF_LATIN1), \
            dst); \
} while (0)

#define VERT_BIN_APPEND_NULL(bin) do { \
    if (!enif_realloc_binary(&bin, bin.size+1)) \
        return error_tuple(env, atom_enomem); \
    bin.data[bin.size-1] = '\0'; \
} while (0)

#define VERT_FUN_INT_RES(fun, type) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
 \
    VERT_RESOURCE *vp = NULL; \
    int n = -1; \
 \
    VERT_GET_RESOURCE(0, vp, type); \
 \
    n = fun(vp->res); \
 \
    if (n < 0) return verterr(env); \
 \
    return enif_make_tuple2(env, \
            atom_ok, \
            enif_make_int(env, n)); \
}

#define VERT_FUN_CCHARP_RES(fun, type) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    VERT_RESOURCE *vp = NULL; \
    const char *p = NULL; \
 \
    VERT_GET_RESOURCE(0, vp, type); \
 \
    p = fun(vp->res); \
 \
    VERTERR(p == NULL); \
 \
    return  enif_make_tuple2(env, atom_ok, \
        enif_make_string(env, p, ERL_NIF_LATIN1)); \
}

#define VERT_FUN_CHARP_RES(fun, type) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    VERT_RESOURCE *vp = NULL; \
    char *p = NULL; \
 \
    ERL_NIF_TERM term = {0}; \
 \
    VERT_GET_RESOURCE(0, vp, type); \
 \
    p = fun(vp->res); \
 \
    VERTERR(p == NULL); \
 \
    term = enif_make_tuple2(env, atom_ok, \
    enif_make_string(env, p, ERL_NIF_LATIN1)); \
 \
    free(p); \
 \
    return term; \
}

#define VERT_FUN_ULONG_RES(fun, type) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    VERT_RESOURCE *vp = NULL; \
    unsigned long ul = -1; \
 \
    VERT_GET_RESOURCE(0, vp, type); \
 \
    VERTERR(fun(vp->res, &ul) < 0); \
 \
    return enif_make_tuple2(env, \
        atom_ok, enif_make_ulong(env, ul)); \
}

#define VERT_FUN_INT_RES_CHARPP_INT(fun, type) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    VERT_RESOURCE *vp = NULL; \
    int max = 0; \
    char **names = NULL; \
 \
    int n = -1; \
    ERL_NIF_TERM list = {0}; \
 \
    VERT_GET_RESOURCE(0, vp, type); \
    VERT_GET_INT(1, max); \
 \
    if (max <= 0) \
        return error_tuple(env, atom_badarg); \
 \
    names = calloc(max, sizeof(char *)); \
    ISNULL(names); \
 \
    n = fun(vp->res, names, max); \
 \
    VERTERR(n < 0); \
 \
    VERT_COPY_STRING(list, names, n); \
 \
    free(names); \
 \
    return enif_make_tuple2(env, \
        atom_ok, list); \
}

#define VERT_FUN_DEFINEXML(fun, type, tag) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    VERT_RESOURCE *vp = NULL; \
    ErlNifBinary xml = {0}; \
 \
    VERT_RESOURCE *rp = NULL; \
 \
    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT); \
    VERT_GET_IOLIST(1, xml); \
 \
    VERT_BIN_APPEND_NULL(xml); \
 \
    VERT_RES_ALLOC(rp, type, vp->res); \
 \
    rp->res = fun(vp->res, (const char *)xml.data); \
 \
    if (rp->res == NULL) { \
        enif_release_resource(rp); \
        return verterr(env); \
    } \
 \
    return vert_make_resource(env, rp, tag); \
}

#define VERT_FUN_UNSUPPORTED(fun) \
    ERL_NIF_TERM \
vert_##fun(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    return error_tuple(env, atom_unsupported); \
}
