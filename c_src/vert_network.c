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
#include "vert_network.h"


/* 0: VERT_RESOURCE, 1: int type 2: int | char* */
    ERL_NIF_TERM
vert_network_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    int type = VERT_ATTR_NAME;

    VERT_RESOURCE *np = NULL;
    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(vp, VERT_RES_CONNECT);
    RESALLOC(np, VERT_RES_NETWORK, vp->res);

    switch (type) {
        case VERT_ATTR_NAME: {
                char name[1024]; /* XXX max interface length ??? */

                if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                    return enif_make_badarg(env);

                np->res = virNetworkLookupByName(vp->res, name);
            }
            break;

        case VERT_ATTR_RAWUUID: {
                ErlNifBinary buf = {0};

                if (!enif_inspect_iolist_as_binary(env, argv[2], &buf))
                    return enif_make_badarg(env);

                np->res = virNetworkLookupByUUID(vp->res, buf.data);
            }
            break;

        case VERT_ATTR_UUID: {
                ErlNifBinary buf = {0};

                if (!enif_inspect_iolist_as_binary(env, argv[2], &buf))
                    return enif_make_badarg(env);

                np->res = virNetworkLookupByUUIDString(vp->res, (const char *)buf.data);
            }
            break;

        default:
            return enif_make_tuple2(env,
                atom_error, atom_unsupported);
    }

    if (np->res == NULL) {
        enif_release_resource(np);
        return verterr(env);
    }

    res = enif_make_resource(env, np);
    enif_release_resource(np);

    return enif_make_tuple2(env,
        atom_ok,
        enif_make_tuple4(env,
            atom_resource,
            atom_domain,
            enif_make_ref(env), res));
}

/* 0: VERT_RESOURCE, 1: type */
    ERL_NIF_TERM
vert_network_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *np = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&np))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    RESTYPE(np, VERT_RES_NETWORK);

    switch (type) {
        case VERT_ATTR_AUTOSTART: {
            int autostart = 0;

            VERTERR(virNetworkGetAutostart(np->res, &autostart) < 0);

            term = (autostart ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_BRIDGENAME: {
            char *name = NULL;

            name = virNetworkGetBridgeName(np->res);

            VERTERR(name == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            free(name);
            }
            break;

        case VERT_ATTR_NAME: {
            const char *name = NULL;

            name = virNetworkGetName(np->res);

            VERTERR(name == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_RAWUUID: {
            unsigned char uuid[VIR_UUID_BUFLEN];

            VERTERR(virNetworkGetUUID(np->res, uuid) < 0);
            term = bincopy(env, uuid, sizeof(uuid));
            NOMEM(term);
            }
            break;

        case VERT_ATTR_UUID: {
            char uuid[VIR_UUID_STRING_BUFLEN];

            VERTERR (virNetworkGetUUIDString(np->res, uuid) < 0);

            term = enif_make_string(env, uuid, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_DESC: {
            char *desc = NULL;
            int flags = 0;

            if (argc != 3 || !enif_get_int(env, argv[2], &flags))
                return enif_make_badarg(env);

            desc = virNetworkGetXMLDesc(np->res, flags);

            VERTERR(desc == NULL);

            term = enif_make_string(env, desc, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_ACTIVE: {
            int res = -1;

            res = virNetworkIsPersistent(np->res);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_PERSISTENT: {
            int res = -1;

            res = virNetworkIsPersistent(np->res);

            VERTERR(res < 0);

            term = (res == 1 ? atom_true : atom_false);
            }
            break;

        case VERT_ATTR_CONNECT: {
            VERT_RESOURCE *cp = NULL;
            ERL_NIF_TERM res = {0};

            RESALLOC(cp, VERT_RES_CONNECT, NULL);
            cp->res = np->conn;
            res = enif_make_resource(env, cp);
            enif_release_resource(cp);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple4(env,
                atom_resource,
                atom_connect,
                enif_make_ref(env), res));
            }
            break;

        default:
            return enif_make_tuple2(env,
                atom_error, atom_unsupported);
    }

    return term;
}

