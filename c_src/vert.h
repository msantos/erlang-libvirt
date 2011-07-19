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
#include <net/if.h>
#include "erl_nif.h"


ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_undefined;
ERL_NIF_TERM atom_unsupported;
ERL_NIF_TERM atom_enomem;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_resource;
ERL_NIF_TERM atom_connect;
ERL_NIF_TERM atom_domain;
ERL_NIF_TERM atom_interface;
ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_false;


#define VERTERR(x) do { \
    if ((x)) return verterr(env); \
} while (0)

#define ISNULL(x) do { \
    if ((x) == NULL) return atom_enomem; \
} while (0)

#define CHECK_VIRPTR_NULL(var) do { \
    if (var->res == NULL) { \
        enif_release_resource(var); \
        return verterr(env); \
    } \
} while (0)

#define BINCOPY(dst, src, size) do { \
    dst = bincopy(env, src, size); \
    if ((dst) == atom_enomem) \
        return atom_enomem; \
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
    VERT_RES_STORAGEPOOL,
    VERT_RES_FILTER,
    VERT_RES_SECRET,
};

#define RESOURCE_ALLOC(var,vtype,initial) do { \
    var = enif_alloc_resource(NIF_VERT_RESOURCE, sizeof(VERT_RESOURCE)); \
    ISNULL(var); \
    (var)->type = vtype; \
    (var)->res = NULL; \
    (var)->conn = (initial); \
} while (0)

#define VERT_GET_RESOURCE(index, var, vtype) do { \
    if (!enif_get_resource(env, argv[index], NIF_VERT_RESOURCE, (void **)&var) || \
        ((var)->type != (vtype))) \
        return enif_make_badarg(env); \
} while (0)

#define VERT_GET_RESOURCE1(index, var) do { \
    if (!enif_get_resource(env, argv[index], NIF_VERT_RESOURCE, (void **)&var)) \
        return enif_make_badarg(env); \
} while (0)

#define VERT_GET_STRING(index, var, size) do { \
    if (enif_get_string(env, argv[(index)], var, size, ERL_NIF_LATIN1) < 1) \
        return enif_make_badarg(env); \
} while (0)

#define VERT_GET_INT(index, var) do { \
    if (enif_get_int(env, argv[(index)], &var) < 0) \
        return enif_make_badarg(env); \
} while (0)

#define VERT_GET_IOLIST(index, var) do { \
    if (!enif_inspect_iolist_as_binary(env, argv[(index)], &var)) \
        return enif_make_badarg(env); \
} while (0)

#define VERT_RET_RESOURCE(var, atom) do { \
    ERL_NIF_TERM res = {0}; \
    if (var->res == NULL) { \
        enif_release_resource(var); \
        return verterr(env); \
    } \
    res = enif_make_resource(env, var); \
    enif_release_resource(var); \
    return vert_make_resource(env, atom, res); \
} while (0)

/* nif_virDomainLookup */
enum {
    VERT_CONNECT_OPEN = 0,
    VERT_CONNECT_OPEN_READONLY,
    VERT_CONNECT_OPEN_AUTH
};

enum {
    VERT_ATTR_ID = 0,
    VERT_ATTR_NAME,
    VERT_ATTR_UUID,
    VERT_ATTR_RAWUUID,
    VERT_ATTR_MAC,
    VERT_ATTR_DESC,
    VERT_ATTR_INFO,
    VERT_ATTR_AUTOSTART,
    VERT_ATTR_BLOCKINFO,
    VERT_ATTR_CONNECT,
    VERT_ATTR_JOBINFO,
    VERT_ATTR_MAXMEMORY,
    VERT_ATTR_MAXVCPUS,
    VERT_ATTR_MEMORYPARAMETERS,
    VERT_ATTR_OSTYPE,
    VERT_ATTR_SCHEDULERPARAMETERS,
    VERT_ATTR_SCHEDULERTYPE,
    VERT_ATTR_SECURITYLABEL,
    VERT_ATTR_VCPUS,
    VERT_ATTR_VCPUSFLAGS,
    VERT_ATTR_CAPABILITIES,
    VERT_ATTR_HOSTNAME,
    VERT_ATTR_LIBVERSION,
    VERT_ATTR_FREEMEMORY,
    VERT_ATTR_CELLSFREEMEMORY,
    VERT_ATTR_TYPE,
    VERT_ATTR_VERSION,
    VERT_ATTR_URI,
    VERT_ATTR_ENCRYPTED,
    VERT_ATTR_SECURE,
    VERT_ATTR_SECURITYMODEL,
    VERT_ATTR_BRIDGENAME,
    VERT_ATTR_ACTIVE,
    VERT_ATTR_PERSISTENT,
};


