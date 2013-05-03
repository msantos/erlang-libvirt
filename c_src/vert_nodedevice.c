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


VERT_FUN_CREATEXML(virNodeDeviceCreateXML, VERT_RES_NODEDEVICE, atom_nodedevice)
VERT_FUN_INT_RES(virNodeDeviceDestroy, VERT_RES_NODEDEVICE)
VERT_FUN_INT_RES(virNodeDeviceDettach, VERT_RES_NODEDEVICE)
VERT_FUN_INT_RES(virNodeDeviceNumOfCaps, VERT_RES_NODEDEVICE)
VERT_FUN_INT_RES(virNodeDeviceReAttach, VERT_RES_NODEDEVICE)
VERT_FUN_INT_RES(virNodeDeviceReset, VERT_RES_NODEDEVICE)
VERT_FUN_GETNAME(virNodeDeviceGetName, VERT_RES_NODEDEVICE)
VERT_FUN_GETNAME(virNodeDeviceGetParent, VERT_RES_NODEDEVICE)
VERT_FUN_GETXMLDESC(virNodeDeviceGetXMLDesc, VERT_RES_NODEDEVICE)
VERT_FUN_LOOKUPBYNAME(virNodeDeviceLookupByName, VERT_RES_NODEDEVICE, atom_nodedevice)

    ERL_NIF_TERM
vert_virNodeNumOfDevices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary cap = {0};
    u_int32_t flags = 0;

    int n = 0;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, cap);
    VERT_GET_UINT(2, flags);

    if (cap.size > 0)
        VERT_BIN_APPEND_NULL(cap);

    n = virNodeNumOfDevices(vp->res,
            (cap.size == 0 ? NULL : (char *)cap.data),
            flags);

    VERTERR(n < 0);

    return enif_make_tuple2(env,
            atom_ok,
            enif_make_int(env, n));
}

    ERL_NIF_TERM
vert_virNodeListDevices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    ErlNifBinary cap = {0};
    char **names = NULL;
    int maxnames = 0;
    u_int32_t flags = 0;

    int n = 0;
    int rv = 0;
    ERL_NIF_TERM dev = {0};


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);
    VERT_GET_IOLIST(1, cap);
    VERT_GET_INT(2, maxnames);
    VERT_GET_UINT(3, flags);

    if (cap.size > 0)
        VERT_BIN_APPEND_NULL(cap);

    names = calloc(maxnames, sizeof(char *));

    if (names == NULL)
        return error_tuple(env, atom_enomem);

    rv = virNodeListDevices(vp->res,
            (cap.size == 0 ? NULL : (char *)cap.data),
            names, maxnames, flags);

    if (rv < 0)
        goto ERR;

    dev = enif_make_list(env, 0);
    for (n = 0; n < rv; n++) {
        dev = enif_make_list_cell(env,
                enif_make_string(env, names[n], ERL_NIF_LATIN1),
                dev);
    }

    free(names);

    return enif_make_tuple2(env,
            atom_ok,
            dev);

ERR:
    free(names);
    return verterr(env);
}

    ERL_NIF_TERM
vert_virNodeDeviceListCaps(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;
    char **names = NULL;
    int maxnames = 0;

    int n = 0;
    int rv = 0;
    ERL_NIF_TERM cap = {0};


    VERT_GET_RESOURCE(0, dp, VERT_RES_NODEDEVICE);
    VERT_GET_INT(1, maxnames);

    names = calloc(maxnames, sizeof(char *));

    if (names == NULL)
        return error_tuple(env, atom_enomem);

    rv = virNodeDeviceListCaps(dp->res, names, maxnames);

    if (rv < 0)
        goto ERR;

    cap = enif_make_list(env, 0);
    for (n = 0; n < rv; n++) {
        cap = enif_make_list_cell(env,
                enif_make_string(env, names[n], ERL_NIF_LATIN1),
                cap);
    }

    free(names);

    return enif_make_tuple2(env,
            atom_ok,
            cap);

ERR:
    free(names);
    return verterr(env);
}
