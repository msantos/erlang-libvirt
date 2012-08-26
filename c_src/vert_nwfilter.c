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
#include "vert_nwfilter.h"

#ifdef HAVE_NWFILTER
VERT_FUN_INT_RES(virNWFilterUndefine, VERT_RES_NWFILTER)
VERT_FUN_DEFINEXML(virNWFilterDefineXML, VERT_RES_NWFILTER, atom_nwfilter)
VERT_FUN_GETNAME(virNWFilterGetName, VERT_RES_NWFILTER)
VERT_FUN_GETUUID(virNWFilterGetUUID, VERT_RES_NWFILTER)
VERT_FUN_GETUUIDSTRING(virNWFilterGetUUIDString, VERT_RES_NWFILTER)
VERT_FUN_GETXMLDESC(virNWFilterGetXMLDesc, VERT_RES_NWFILTER)

    ERL_NIF_TERM
vert_virNWFilterLookupByName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary name = {0};

    VERT_RESOURCE *np = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, name);

    VERT_BIN_APPEND_NULL(name);

    VERT_RES_ALLOC(np, VERT_RES_NWFILTER, vp->res);

    np->res = virNWFilterLookupByName(vp->res, (char *)name.data);

    if (np->res == NULL) {
        enif_release_resource(np);
        return verterr(env);
    }

    return vert_make_resource(env, np, atom_nwfilter);
}

    ERL_NIF_TERM
vert_virNWFilterLookupByUUID(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary uuid = {0};

    VERT_RESOURCE *np = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, uuid);

    VERT_BIN_APPEND_NULL(uuid);

    VERT_RES_ALLOC(np, VERT_RES_NWFILTER, vp->res);

    np->res = virNWFilterLookupByUUID(vp->res, uuid.data);

    if (np->res == NULL) {
        enif_release_resource(np);
        return verterr(env);
    }

    return vert_make_resource(env, np, atom_nwfilter);
}

    ERL_NIF_TERM
vert_virNWFilterLookupByUUIDString(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary uuid = {0};

    VERT_RESOURCE *np = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, uuid);

    VERT_BIN_APPEND_NULL(uuid);

    VERT_RES_ALLOC(np, VERT_RES_NWFILTER, vp->res);

    np->res = virNWFilterLookupByUUIDString(vp->res, (char *)uuid.data);

    if (np->res == NULL) {
        enif_release_resource(np);
        return verterr(env);
    }

    return vert_make_resource(env, np, atom_nwfilter);
}
#else
VERT_FUN_UNSUPPORTED(virNWFilterUndefine)
VERT_FUN_UNSUPPORTED(virNWFilterGetXMLDesc)
VERT_FUN_UNSUPPORTED(virNWFilterDefineXML)
VERT_FUN_UNSUPPORTED(virNWFilterGetName)
VERT_FUN_UNSUPPORTED(virNWFilterGetUUID)
VERT_FUN_UNSUPPORTED(virNWFilterGetUUIDString)
VERT_FUN_UNSUPPORTED(virNWFilterLookupByName)
VERT_FUN_UNSUPPORTED(virNWFilterLookupByUUID)
VERT_FUN_UNSUPPORTED(virNWFilterLookupByUUIDString)
#endif
