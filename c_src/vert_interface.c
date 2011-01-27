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


/* 0: VERT_RESOURCE, 1: int type 2: int | char* */
    ERL_NIF_TERM
vert_interface_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = VERT_ATTR_NAME;

    VERT_RESOURCE *ifp = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);
    RESALLOC(ifp, VERT_RES_INTERFACE);
    ifp->conn = vp->res;

    switch (type) {
        case VERT_ATTR_NAME: {
                char name[1024]; /* XXX max interface length ??? */

                if (argc != 3 || enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                ifp->res = virInterfaceLookupByName(vp->res, name);
            }
            break;

        case VERT_ATTR_MAC: {
                char mac[1024]; /* XXX max size ??? */

                if (argc != 3 || enif_get_string(env, argv[2], mac, sizeof(mac), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                ifp->res = virInterfaceLookupByMACString(vp->res, mac);
            }
            break;

        default:
            return enif_make_badarg(env);
    }

    if (ifp->res == NULL) {
        enif_release_resource(ifp);
        return verterr(env);
    }

    res = enif_make_resource(env, ifp);
    enif_release_resource(ifp);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: VERT_RESOURCE, 1: int type */
    ERL_NIF_TERM
vert_interface_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *ifp = NULL;
    int type = VERT_ATTR_NAME;

    const char *res = NULL;


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&ifp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(ifp, VERT_RES_INTERFACE);

    switch (type) {
        case VERT_ATTR_NAME:
            res = virInterfaceGetName(ifp->res);
            break;

        case VERT_ATTR_MAC:
            res = virInterfaceGetMACString(ifp->res);
            break;

        case VERT_ATTR_DESC:
            res = virInterfaceGetXMLDesc(ifp->res, 0);
            break;

        case VERT_ATTR_CONNECT: {
            VERT_RESOURCE *cp = NULL;
            ERL_NIF_TERM res = {0};

            RESALLOC(cp, VERT_RES_CONNECT);
            cp->res = ifp->conn;
            res = enif_make_resource(env, cp);
            enif_release_resource(cp);

            res = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple4(env,
                atom_resource,
                atom_connect,
                enif_make_ref(env), res));
        }
        break;

        default:
            return enif_make_badarg(env);
    }

    if (res == NULL) {
        enif_release_resource(ifp);
        return verterr(env);
    }

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_string(env, res, ERL_NIF_LATIN1));
}


