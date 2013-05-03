/* Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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

VERT_FUN_SETFLAG(virStorageVolDelete, VERT_RES_STORAGEVOL)
VERT_FUN_GETNAME(virStorageVolGetKey, VERT_RES_STORAGEVOL)
VERT_FUN_GETNAME(virStorageVolGetName, VERT_RES_STORAGEVOL)
VERT_FUN_GETXMLDESC(virStorageVolGetXMLDesc, VERT_RES_STORAGEVOL)
VERT_FUN_LOOKUPBYNAME(virStorageVolLookupByKey, VERT_RES_STORAGEVOL, atom_storagevol)
VERT_FUN_LOOKUPBYNAME(virStorageVolLookupByPath, VERT_RES_STORAGEVOL, atom_storagevol)
VERT_FUN_SETFLAG(virStorageVolWipe, VERT_RES_STORAGEVOL)

    ERL_NIF_TERM
vert_virStorageVolCreateXML(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *pp = NULL;
    ErlNifBinary xml = {0};
    u_int32_t flags = 0;

    VERT_RESOURCE *vp = NULL;


    VERT_GET_RESOURCE(0, pp, VERT_RES_STORAGEPOOL);
    VERT_GET_IOLIST(1, xml);
    VERT_GET_UINT(2, flags);

    VERT_BIN_APPEND_NULL(xml);

    VERT_RES_ALLOC(vp, VERT_RES_STORAGEVOL, pp->conn);

    vp->res = virStorageVolCreateXML(pp->res, (char *)xml.data, flags);

    if (vp->res == NULL) {
        enif_release_resource(vp);
        return verterr(env);
    }

    return vert_make_resource(env, vp, atom_storagevol);
}

    ERL_NIF_TERM
vert_virStorageVolCreateXMLFrom(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *pp = NULL;
    ErlNifBinary xml = {0};
    VERT_RESOURCE *vp1 = NULL;
    u_int32_t flags = 0;

    VERT_RESOURCE *vp2 = NULL;

    VERT_GET_RESOURCE(0, pp, VERT_RES_STORAGEPOOL);
    VERT_GET_IOLIST(1, xml);
    VERT_GET_RESOURCE(2, vp1, VERT_RES_STORAGEVOL);
    VERT_GET_UINT(3, flags);

    VERT_BIN_APPEND_NULL(xml);

    VERT_RES_ALLOC(vp2, VERT_RES_STORAGEVOL, pp->conn);

    vp2->res = virStorageVolCreateXMLFrom(pp->res, (char *)xml.data, vp1->res, flags);

    if (vp2->res == NULL) {
        enif_release_resource(vp2);
        return verterr(env);
    }

    return vert_make_resource(env, vp2, atom_storagevol);
}

    ERL_NIF_TERM
vert_virStorageVolDownload(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *svp = NULL;
    VERT_RESOURCE *stp = NULL;
    u_int64_t offset = 0;
    u_int64_t length = 0;
    u_int32_t flags = 0;


    VERT_GET_RESOURCE(0, svp, VERT_RES_STORAGEVOL);
    VERT_GET_RESOURCE(1, stp, VERT_RES_STREAM);
    VERT_GET_UINT64(2, offset);
    VERT_GET_UINT64(3, length);
    VERT_GET_UINT(4, flags);

    VERTERR(virStorageVolDownload(svp->res, stp->res, offset,
                length, flags) < 0);

    return atom_ok;
}

    ERL_NIF_TERM
vert_virStorageVolGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    ErlNifBinary info = {0};


    VERT_GET_RESOURCE(0, sp, VERT_RES_STORAGEVOL);

    if (!enif_alloc_binary(sizeof(virStorageVolInfo), &info))
        return error_tuple(env, atom_enomem);

    VERTERR(virStorageVolGetInfo(sp->res, (virStorageVolInfoPtr)info.data) < 0);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &info));
}

    ERL_NIF_TERM
vert_virStorageVolGetPath(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    char *path = NULL;

    ERL_NIF_TERM term = {0};


    VERT_GET_RESOURCE(0, sp, VERT_RES_STORAGEVOL);

    path = virStorageVolGetPath(sp->res);
    VERTERR(path == NULL);

    term = enif_make_tuple2(env, atom_ok,
            enif_make_string(env, path, ERL_NIF_LATIN1));

    free(path);

    return term;
}

    ERL_NIF_TERM
vert_virStorageVolLookupByName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *pp = NULL;
    ErlNifBinary name = {0};

    VERT_RESOURCE *vp = NULL;

    VERT_GET_RESOURCE(0, pp, VERT_RES_STORAGEPOOL);
    VERT_GET_IOLIST(1, name);

    VERT_BIN_APPEND_NULL(name);

    VERT_RES_ALLOC(vp, VERT_RES_STORAGEVOL, pp->conn);

    vp->res = virStorageVolLookupByName(pp->res, (char *)name.data);

    if (vp->res == NULL) {
        enif_release_resource(vp);
        return verterr(env);
    }

    return vert_make_resource(env, vp, atom_storagevol);
}

#ifdef HAVE_VIRSTORAGEVOLRESIZE
    ERL_NIF_TERM
vert_virStorageVolResize(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    u_int64_t capacity = 0;
    u_int32_t flags = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_STORAGEVOL);
    VERT_GET_UINT64(1, capacity);
    VERT_GET_UINT(2, flags);

    VERTERR(virStorageVolResize(vp->res, capacity, flags) < 0);

    return atom_ok;
}
#else
VERT_FUN_UNSUPPORTED(virStorageVolResize);
#endif

    ERL_NIF_TERM
vert_virStorageVolUpload(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    VERT_RESOURCE *stp = NULL;
    u_int64_t offset = 0;
    u_int64_t length = 0;
    u_int32_t flags = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_STORAGEVOL);
    VERT_GET_RESOURCE(1, stp, VERT_RES_STREAM);
    VERT_GET_UINT64(2, offset);
    VERT_GET_UINT64(3, length);
    VERT_GET_UINT(4, flags);

    VERTERR(virStorageVolUpload(vp->res, stp->res, offset, length, flags) < 0);

    return atom_ok;
}

#ifdef HAVE_VIRSTORAGEVOLWIPEPATTERN
    ERL_NIF_TERM
vert_virStorageVolWipePattern(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    u_int32_t alg = 0;
    u_int32_t flags = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_STORAGEVOL);
    VERT_GET_UINT(1, alg);
    VERT_GET_UINT(2, flags);

    VERTERR(virStorageWipePattern(vp->res, alg, flags) < 0);

    return atom_ok;
}
#else
VERT_FUN_UNSUPPORTED(virStorageVolWipePattern);
#endif
