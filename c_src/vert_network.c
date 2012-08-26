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


VERT_FUN_INT_RES(virNetworkCreate, VERT_RES_NETWORK)
VERT_FUN_INT_RES(virNetworkDestroy, VERT_RES_NETWORK)
VERT_FUN_INT_RES(virNetworkIsPersistent, VERT_RES_NETWORK)
VERT_FUN_INT_RES(virNetworkUndefine, VERT_RES_NETWORK)

VERT_FUN_DEFINEXML(virNetworkDefineXML, VERT_RES_NETWORK, atom_network)

VERT_FUN_GETAUTOSTART(virNetworkGetAutostart, VERT_RES_NETWORK)
VERT_FUN_GETNAME(virNetworkGetName, VERT_RES_NETWORK)
VERT_FUN_GETUUID(virNetworkGetUUID, VERT_RES_NETWORK)
VERT_FUN_GETUUIDSTRING(virNetworkGetUUIDString, VERT_RES_NETWORK)
VERT_FUN_GETXMLDESC(virNetworkGetXMLDesc, VERT_RES_NETWORK)
VERT_FUN_LOOKUPBYNAME(virNetworkLookupByName, VERT_RES_NETWORK, atom_network)
VERT_FUN_LOOKUPBYNAME(virNetworkLookupByUUIDString, VERT_RES_NETWORK, atom_network)
VERT_FUN_LOOKUPBYUUID(virNetworkLookupByUUID, VERT_RES_NETWORK, atom_network)
VERT_FUN_SETFLAG(virNetworkSetAutostart, VERT_RES_NETWORK)

    ERL_NIF_TERM
vert_virNetworkGetBridgeName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *np = NULL;
    char *name = NULL;

    ERL_NIF_TERM term = {0};


    VERT_GET_RESOURCE(0, np, VERT_RES_NETWORK);

    name = virNetworkGetBridgeName(np->res);
    VERTERR(name == NULL);

    term = enif_make_string(env, name, ERL_NIF_LATIN1);
    free(name);

    return enif_make_tuple2(env,
        atom_ok,
        term);
}
