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

ERL_NIF_TERM vert_interface_int_res_uint(
        ErlNifEnv *env,
        const ERL_NIF_TERM argv[],
        int (*fp)(virInterfacePtr, unsigned int)
        );

VERT_FUN_INT_RES(virInterfaceUndefine, VERT_RES_INTERFACE)

VERT_FUN_GETNAME(virInterfaceGetMACString, VERT_RES_INTERFACE)
VERT_FUN_GETNAME(virInterfaceGetName, VERT_RES_INTERFACE)
VERT_FUN_GETXMLDESC(virInterfaceGetXMLDesc, VERT_RES_INTERFACE)
VERT_FUN_LOOKUPBYNAME(virInterfaceLookupByName, VERT_RES_INTERFACE, atom_interface)
VERT_FUN_LOOKUPBYNAME(virInterfaceLookupByMACString, VERT_RES_INTERFACE, atom_interface)

    ERL_NIF_TERM
vert_virInterfaceDefineXML(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary cfg;

    VERT_RESOURCE *ifp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, cfg);

    VERT_BIN_APPEND_NULL(cfg);

    VERT_RES_ALLOC(ifp, VERT_RES_INTERFACE, vp->res);

    ifp->res = virInterfaceDefineXML(vp->res, (char *)cfg.data, 0);

    if (ifp->res == NULL) {
        enif_release_resource(ifp);
        return verterr(env);
    }

    return vert_make_resource(env, ifp, atom_interface);
}


    ERL_NIF_TERM
vert_virInterfaceCreate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return vert_interface_int_res_uint(env, argv, virInterfaceCreate);
}

    ERL_NIF_TERM
vert_virInterfaceDestroy(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    return vert_interface_int_res_uint(env, argv, virInterfaceDestroy);
}

/*
 * Internal functions
 */
    ERL_NIF_TERM
vert_interface_int_res_uint(
        ErlNifEnv *env,
        const ERL_NIF_TERM argv[],
        int (*fp)(virInterfacePtr, unsigned int))
{
    VERT_RESOURCE *ifp = NULL;
    int flags = 0;

    int n = 0;


    VERT_GET_RESOURCE(0, ifp, VERT_RES_INTERFACE);
    VERT_GET_INT(1, flags);

    n = fp(ifp->res, (unsigned int)flags);

    VERTERR(n < 0);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_int(env, n));
}
