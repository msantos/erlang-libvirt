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

VERT_FUN_SETFLAG(virStoragePoolBuild, VERT_RES_STORAGEPOOL)
VERT_FUN_SETFLAG(virStoragePoolCreate, VERT_RES_STORAGEPOOL)
VERT_FUN_SETFLAG(virStoragePoolDelete, VERT_RES_STORAGEPOOL)

VERT_FUN_CREATEXML(virStoragePoolCreateXML, VERT_RES_STORAGEPOOL, atom_storagepool)
VERT_FUN_CREATEXML(virStoragePoolDefineXML, VERT_RES_STORAGEPOOL, atom_storagepool)

VERT_FUN_INT_RES(virStoragePoolDestroy, VERT_RES_STORAGEPOOL)
VERT_FUN_GETAUTOSTART(virStoragePoolGetAutostart, VERT_RES_STORAGEPOOL)
VERT_FUN_GETNAME(virStoragePoolGetName, VERT_RES_STORAGEPOOL)
VERT_FUN_GETUUID(virStoragePoolGetUUID, VERT_RES_STORAGEPOOL)
VERT_FUN_GETUUIDSTRING(virStoragePoolGetUUIDString, VERT_RES_STORAGEPOOL)
VERT_FUN_GETXMLDESC(virStoragePoolGetXMLDesc, VERT_RES_STORAGEPOOL)
VERT_FUN_INT_RES(virStoragePoolIsActive, VERT_RES_STORAGEPOOL)
VERT_FUN_INT_RES(virStoragePoolIsPersistent, VERT_RES_STORAGEPOOL)

VERT_FUN_INT_RES_CHARPP_INT(virStoragePoolListVolumes, VERT_RES_STORAGEPOOL)
VERT_FUN_INT_RES(virStoragePoolNumOfVolumes, VERT_RES_STORAGEPOOL)
VERT_FUN_LOOKUPBYNAME(virStoragePoolLookupByName, VERT_RES_STORAGEPOOL, atom_storagepool)
VERT_FUN_LOOKUPBYNAME(virStoragePoolLookupByUUIDString, VERT_RES_STORAGEPOOL, atom_storagepool)
VERT_FUN_LOOKUPBYUUID(virStoragePoolLookupByUUID, VERT_RES_STORAGEPOOL, atom_storagepool)
VERT_FUN_SETFLAG(virStoragePoolRefresh, VERT_RES_STORAGEPOOL)
VERT_FUN_SETFLAG(virStoragePoolSetAutostart, VERT_RES_STORAGEPOOL)
VERT_FUN_INT_RES(virStoragePoolUndefine, VERT_RES_STORAGEPOOL)

    ERL_NIF_TERM
vert_virStoragePoolGetInfo(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    ErlNifBinary info = {0};


    VERT_GET_RESOURCE(0, sp, VERT_RES_STORAGEPOOL);

    if (!enif_alloc_binary(sizeof(virStoragePoolInfo), &info))
        return error_tuple(env, atom_enomem);

    VERTERR(virStoragePoolGetInfo(sp->res, (virStoragePoolInfoPtr)info.data) < 0);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_binary(env, &info));
}

    ERL_NIF_TERM
vert_virStoragePoolLookupByVolume(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    VERT_RESOURCE *pp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_STORAGEVOL);

    VERT_RES_ALLOC(pp, VERT_RES_STORAGEPOOL, vp->conn);

    pp->res = virStoragePoolLookupByVolume(vp->res);

    if (pp->res == NULL) {
        enif_release_resource(pp);
        return verterr(env);
    }

    return vert_make_resource(env, pp, atom_storagepool);
}
