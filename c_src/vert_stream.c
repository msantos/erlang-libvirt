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
#include "vert_stream.h"

VERT_FUN_INT_RES(virStreamAbort, VERT_RES_STREAM);
VERT_FUN_INT_RES(virStreamFinish, VERT_RES_STREAM);

    ERL_NIF_TERM
vert_virStreamNew(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    VERT_RESOURCE *sp = NULL;


    VERT_GET_RESOURCE(0, vp, VERT_RES_CONNECT);

    VERT_RES_ALLOC(sp, VERT_RES_STREAM, vp->res);

    sp->res = virStreamNew(vp->res,  VIR_STREAM_NONBLOCK);

    if (sp->res == NULL) {
        enif_release_resource(sp);
        return verterr(env);
    }

    return vert_make_resource(env, sp, atom_stream);
}

    ERL_NIF_TERM
vert_virStreamSend(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    ErlNifBinary buf = {0};

    int rv = 0;

    VERT_GET_RESOURCE(0, sp, VERT_RES_STREAM);
    VERT_GET_IOLIST(1, buf);

    rv = virStreamSend(sp->res, (char *)buf.data, buf.size);

    if (rv == -2)
        return error_tuple(env, atom_eagain);

    if (rv < 0)
        return verterr(env);

    return enif_make_tuple2(env, atom_ok, enif_make_int(env, rv));
}

    ERL_NIF_TERM
vert_virStreamRecv(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *sp = NULL;
    size_t nbytes = 0;

    ErlNifBinary buf = {0};
    int rv = 0;


    VERT_GET_RESOURCE(0, sp, VERT_RES_STREAM);
    VERT_GET_ULONG(1, nbytes);

    if (!enif_alloc_binary(nbytes, &buf))
        return error_tuple(env, atom_enomem);

    rv = virStreamRecv(sp->res, (char *)buf.data, buf.size);

    if (rv == -2)
        return error_tuple(env, atom_eagain);

    if (rv < 0)
        return verterr(env);

    if (!enif_realloc_binary(&buf, rv))
        return error_tuple(env, atom_enomem);

    return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &buf));
}
