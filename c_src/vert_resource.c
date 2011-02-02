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
#include "vert_resource.h"


/* 0: VERT_RESOURCE, 1: int type, 2: char * */
    ERL_NIF_TERM
vert_resource_define(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = 0;
    ErlNifBinary cfg;

    VERT_RESOURCE *rp = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[2], &cfg))
        return enif_make_badarg(env);

    /* NULL terminate the string */
    if (!enif_realloc_binary(&cfg, cfg.size+1))
        return atom_enomem;

    cfg.data[cfg.size-1] = '\0';

    RESTYPE(vp, VERT_RES_CONNECT);
    RESALLOC(rp, type, vp->res);

    switch (type) {
        case VERT_RES_DOMAIN:
            rp->res = virDomainDefineXML(vp->res, (char *)cfg.data);
            break;
        case VERT_RES_INTERFACE:
            rp->res = virInterfaceDefineXML(vp->res, (char *)cfg.data, 0);
            break;
        case VERT_RES_NETWORK:
            rp->res = virNetworkDefineXML(vp->res, (char *)cfg.data);
            break;
        case VERT_RES_STORAGEPOOL:
            rp->res = virStoragePoolDefineXML(vp->res, (char *)cfg.data, 0);
            break;
#ifdef HAVE_NWFILTER
        case VERT_RES_FILTER:
            rp->res = virNWFilterDefineXML(vp->res, (char *)cfg.data);
            break;
#endif
        case VERT_RES_SECRET:
            rp->res = virSecretDefineXML(vp->res, (char *)cfg.data, 0);
            break;
        default:
            return error_tuple(env, atom_unsupported);
    }

    if (rp->res == NULL) {
        enif_release_resource(rp);
        return verterr(env);
    }

    res = enif_make_resource(env, rp);
    enif_release_resource(rp);

    return vert_make_resource(env, atom_domain, res);
}


/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_resource_undefine(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *rp = NULL;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&rp))
        return enif_make_badarg(env);

    switch (rp->type) {
        case VERT_RES_DOMAIN:
            VERTERR(virDomainUndefine(rp->res) == -1); /* Domain is still running */
            break;
        case VERT_RES_INTERFACE:
            VERTERR(virInterfaceUndefine(rp->res) == -1);
            VERTERR(virInterfaceFree(rp->res) == -1);
            break;
        case VERT_RES_NETWORK:
            VERTERR(virNetworkUndefine(rp->res) == -1); /* Network is still running */
            break;
        case VERT_RES_STORAGEPOOL:
            VERTERR(virStoragePoolUndefine(rp->res) == -1); /* XXX need to free */
            break;
#ifdef HAVE_FILTER
        case VERT_RES_FILTER:
            VERTERR(virNWFilterUndefine(rp->res) == -1);
            VERTERR(virNWFilterFree(rp->res) == -1);
            break;
#endif
        case VERT_RES_SECRET:
            VERTERR(virSecretUndefine(rp->res) == -1);
            VERTERR(virSecretFree(rp->res) == -1);
            break;
        default:
            return error_tuple(env, atom_unsupported);
    }

    return atom_ok;
}


/* 0: VERT_RESOURCE, 1: VERT_RESOURCE, 2: uint32 flags */
    ERL_NIF_TERM
vert_resource_create(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *rp = NULL;
    int flags = 0;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&rp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &flags))
        return enif_make_badarg(env);

    switch (rp->type) {
        case VERT_RES_DOMAIN:
            /*
            VERTERR(virDomainCreateWithFlags(dp->res, flags) == -1);
            */
            VERTERR(virDomainCreate(rp->res) == -1);
            break;
        case VERT_RES_INTERFACE:
            VERTERR(virInterfaceCreate(rp->res, 0) == -1);
            break;
        case VERT_RES_NETWORK:
            VERTERR(virNetworkCreate(rp->res) == -1);
            break;
        case VERT_RES_STORAGEPOOL:
            VERTERR(virStoragePoolCreate(rp->res, 0) == -1);
            break;
#if 0
        case VERT_RES_FILTER:
            break;
        case VERT_RES_SECRET:
            break;
#endif
        default:
            return error_tuple(env, atom_unsupported);
    }

    return atom_ok;
}


/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_resource_destroy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    switch (vp->type) {
        case VERT_RES_DOMAIN:
            VERTERR(virDomainDestroy(vp->res) != 0);
            VERTERR(virDomainFree(vp->res) != 0);
            break;
        case VERT_RES_INTERFACE:
            VERTERR(virInterfaceDestroy(vp->res, 0) != 0);
            VERTERR(virInterfaceFree(vp->res) != 0);
            break;
        case VERT_RES_NETWORK:
            VERTERR(virNetworkDestroy(vp->res) != 0);
            VERTERR(virNetworkFree(vp->res) != 0);
            break;
        case VERT_RES_STORAGEPOOL:
            VERTERR(virStoragePoolDestroy(vp->res) != 0);
            VERTERR(virStoragePoolFree(vp->res) != 0);
            break;
        default:
            return error_tuple(env, atom_unsupported);
    }

    vp->res = NULL;

    return atom_ok;
}

