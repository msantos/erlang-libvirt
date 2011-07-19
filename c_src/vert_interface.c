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
#include "vert_interface.h"


    ERL_NIF_TERM
vert_virInterfaceLookupByName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char name[IFNAMSIZ];

    VERT_RESOURCE *ifp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_STRING(1, name, sizeof(name));

    RESOURCE_ALLOC(ifp, VERT_RES_INTERFACE, vp->res);

    ifp->res = virInterfaceLookupByName(vp->res, name);

    VERT_RET_RESOURCE(ifp, atom_interface);
}

    ERL_NIF_TERM
vert_virInterfaceLookupByMACString(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char mac[18]; /* aa:bb:cc:00:11:22\0 */

    VERT_RESOURCE *ifp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_STRING(1, mac, sizeof(mac));

    RESOURCE_ALLOC(ifp, VERT_RES_INTERFACE, vp->res);

    ifp->res = virInterfaceLookupByMACString(vp->res, mac);

    VERT_RET_RESOURCE(ifp, atom_interface);
}

    ERL_NIF_TERM
vert_virInterfaceGetName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *ifp = NULL;
    const char *res = NULL;


    VERT_GET_RESOURCE(0, ifp, VERT_RES_INTERFACE);

    res = virInterfaceGetName(ifp->res);

    VERTERR(res == NULL);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, res, ERL_NIF_LATIN1));
}

    ERL_NIF_TERM
vert_virInterfaceGetMACString(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *ifp = NULL;
    const char *res = NULL;


    VERT_GET_RESOURCE(0, ifp, VERT_RES_INTERFACE);

    res = virInterfaceGetMACString(ifp->res);

    VERTERR(res == NULL);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, res, ERL_NIF_LATIN1));
}

    ERL_NIF_TERM
vert_virInterfaceGetXMLDesc(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *ifp = NULL;
    const char *res = NULL;


    VERT_GET_RESOURCE(0, ifp, VERT_RES_INTERFACE);

    res = virInterfaceGetXMLDesc(ifp->res, 0);

    VERTERR(res == NULL);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, res, ERL_NIF_LATIN1));
}
