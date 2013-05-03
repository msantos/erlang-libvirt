/* Copyright (c) 2010-2013, Michael Santos <michael.santos@gmail.com>
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
#include "vert_funcs.h"
#include "vert_func.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>


#define VERT_READ   0
#define VERT_WRITE  1

#define MAX_ATOM_LEN 255

typedef struct _vert_state {
    ErlNifTid tid;
    int fd[2];
} VERT_STATE;

typedef struct _vert_cast {
    ErlNifPid pid;
    char name[MAX_ATOM_LEN+1];
    int argc;
    ERL_NIF_TERM *argv;
} VERT_CAST;

void *vert_loop(void *arg);


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
    VERT_STATE *state = NULL;
    int flags = 0;


    state = enif_alloc(sizeof(VERT_STATE));

    if (state == NULL)
        return -1;

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_badarg = enif_make_atom(env, "badarg");
    atom_undefined = enif_make_atom(env, "undefined");
    atom_unsupported = enif_make_atom(env, "unsupported");
    atom_enomem = enif_make_atom(env, "enomem");
    atom_eagain = enif_make_atom(env, "eagain");
    atom_resource = enif_make_atom(env, "resource");
    atom_connect = enif_make_atom(env, "connect");
    atom_domain = enif_make_atom(env, "domain");
    atom_interface = enif_make_atom(env, "interface");
    atom_network = enif_make_atom(env, "network");
    atom_nodedevice = enif_make_atom(env, "nodedevice");
    atom_nwfilter = enif_make_atom(env, "nwfilter");
    atom_secret = enif_make_atom(env, "secret");
    atom_storagepool = enif_make_atom(env, "storagepool");
    atom_storagevol = enif_make_atom(env, "storagevol");
    atom_stream = enif_make_atom(env, "stream");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");
    atom_vert = enif_make_atom(env, "vert");

    if ( (NIF_VERT_RESOURCE = enif_open_resource_type(env, NULL,
            "vert_resource", vert_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if (virInitialize() != 0)
        return -2;

    /* Disable error messges to stderr */
    virSetErrorFunc(NULL, null_logger);

    /* Create a thread for blocking libvirt operations */
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, state->fd) < 0)
        return -1;

    /* Set the write socket (request from Erlang) to non-blocking.
     * The read end (read by the thread calling into libvirt) is
     * blocking by default */
    flags = fcntl(state->fd[VERT_WRITE], F_GETFL, 0);
    if (fcntl(state->fd[VERT_WRITE], F_SETFL, flags | O_NONBLOCK) < 0)
        return -1;

    if (enif_thread_create("vert_loop", &state->tid, vert_loop, state, NULL) != 0)
        return -1;

    *priv_data = state;

    return 0;
}

    void *
vert_loop(void *arg)
{
    VERT_STATE *state = arg;
    VERT_CAST cmd;
    ErlNifEnv *env = NULL;

    fd_set rfds;
    int n = 0;
    ERL_NIF_TERM res = {0};

    ErlNifFunc *fp = NULL;

    env = enif_alloc_env();

    if (env == NULL)
        goto ERR;

    for ( ; ; ) {
        FD_ZERO(&rfds);
        FD_SET(state->fd[VERT_READ], &rfds);

        n = select(state->fd[VERT_READ]+1, &rfds, NULL, NULL, NULL);

        if (n < 0) {
            switch (errno) {
                case EAGAIN:
                case EINTR:
                    continue;
                default:
                    goto ERR;
            }
        }

        if (read(state->fd[VERT_READ], &cmd, sizeof(cmd)) != sizeof(cmd))
            goto ERR;

        for (fp = vert_funcs; fp->name != NULL; fp++) {
            if ( (strcmp(fp->name, cmd.name) == 0) &&
                    (fp->arity == cmd.argc)) {
                res = (*fp->fptr)(env, cmd.argc, cmd.argv);
                break;
            }
        }

        if (fp->name == NULL)
            res = error_tuple(env, atom_unsupported);

        (void)enif_send(
                NULL,
                &cmd.pid,
                env,
                enif_make_tuple2(env, atom_vert, res)
                );

        enif_free(cmd.argv);
        enif_clear_env(env);
    }

ERR:
    if (env)
        enif_free_env(env);

    return NULL;
}


    ERL_NIF_TERM
vert_cast(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_STATE *state = NULL;
    VERT_CAST cmd = {{0}};
    ERL_NIF_TERM res = {0};


    state = enif_priv_data(env);

    if (!enif_get_atom(env, argv[0], cmd.name, sizeof(cmd.name), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    cmd.argv = enif_alloc(sizeof(ERL_NIF_TERM) * (argc-1));

    if (cmd.argv == NULL)
        return error_tuple(env, atom_enomem);

    (void)enif_self(env, &cmd.pid);
    (void)memcpy(cmd.argv, &argv[1], sizeof(ERL_NIF_TERM) * (argc-1));
    cmd.argc = argc-1;

    if (write(state->fd[VERT_WRITE], &cmd, sizeof(VERT_CAST)) < 0) {
        res = error_errno(env, errno);
        enif_free(cmd.argv);
        goto OUT;
    }

    res = atom_ok;

OUT:
    return res;
}


static ErlNifFunc nif_funcs[] = {
    {"cast", 2, vert_cast},
    {"cast", 3, vert_cast},
    {"cast", 4, vert_cast},
    {"cast", 5, vert_cast},
    {"cast", 6, vert_cast},
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, NULL)
