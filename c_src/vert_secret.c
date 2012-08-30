/* Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
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


VERT_FUN_GETUUID(virSecretGetUUID, VERT_RES_SECRET)
VERT_FUN_GETUUIDSTRING(virSecretGetUUIDString, VERT_RES_SECRET)
VERT_FUN_GETNAME(virSecretGetUsageID, VERT_RES_SECRET)
VERT_FUN_INT_RES(virSecretGetUsageType, VERT_RES_SECRET)
VERT_FUN_GETXMLDESC(virSecretGetXMLDesc, VERT_RES_SECRET)
VERT_FUN_LOOKUPBYUUID(virSecretLookupByUUID, VERT_RES_SECRET, atom_secret)
VERT_FUN_LOOKUPBYNAME(virSecretLookupByUUIDString, VERT_RES_SECRET, atom_secret)
VERT_FUN_INT_RES(virSecretUndefine, VERT_RES_SECRET)


    ERL_NIF_TERM
vert_virSecretDefineXML(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary xml = {0};
    u_int32_t flags = 0;

    VERT_RESOURCE *sp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, xml);
    VERT_GET_UINT(2, flags);

    VERT_BIN_APPEND_NULL(xml);

    VERT_RES_ALLOC(sp, VERT_RES_SECRET, vp->conn);

    sp->res = virSecretDefineXML(vp->res, (char *)xml.data, flags);

    if (sp->res == NULL) {
        enif_release_resource(sp);
        return verterr(env);
    }

    return vert_make_resource(env, sp, atom_secret);
}  

    ERL_NIF_TERM
vert_virSecretGetValue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    u_int32_t flags = 0;
    size_t value_size = 0;

    unsigned char *secret = NULL;
    ERL_NIF_TERM buf = {0};


    VERT_GET_RESOURCE(0, sp, VERT_RES_SECRET);
    VERT_GET_UINT(1, flags);

    secret = virSecretGetValue(sp->res, &value_size, flags);
    VERTERR(secret == NULL);

    BINCOPY(buf, secret, value_size);

    free(secret);

    return enif_make_tuple2(env,
            atom_ok,
            buf);
}

    ERL_NIF_TERM
vert_virSecretLookupByUsage(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int usageType = 0;
    ErlNifBinary usageID = {0};

    VERT_RESOURCE *sp = NULL;

    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_INT(1, usageType);
    VERT_GET_IOLIST(2, usageID);

    VERT_BIN_APPEND_NULL(usageID);

    VERT_RES_ALLOC(sp, VERT_RES_SECRET, vp->res);

    sp->res = virSecretLookupByUsage(vp->res, usageType, (char *)usageID.data);

    if (sp->res == NULL) {
        enif_release_resource(vp);
        return verterr(env);
    }

    return vert_make_resource(env, sp, atom_secret);
}

    ERL_NIF_TERM
vert_virSecretSetValue(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    ErlNifBinary value = {0};
    u_int32_t flags = 0;


    VERT_GET_RESOURCE(0, sp, VERT_RES_SECRET);
    VERT_GET_IOLIST(1, value);
    VERT_GET_UINT(2, flags);

    VERTERR(virSecretSetValue(sp->res, value.data, value.size, flags) < 0);

    return atom_ok;
}
