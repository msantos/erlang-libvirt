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
#include "vert_connect.h"
#include "vert_domain.h"
#include "vert_interface.h"
#include "vert_network.h"
#include "vert_resource.h"

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
    ErlNifPid *pid;
    ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    int argc;
    void *argv;
} VERT_CAST;

void *vert_loop(void *arg);

static ErlNifFunc vert_funcs[] = {
    /* connect */
    {"connect_open", 2, vert_connect_open},
    {"connect_close", 1, vert_connect_close},

    {"connect_get", 2, vert_connect_get},
    {"connect_get", 3, vert_connect_get},

    {"connect_get_numactive", 2, vert_connect_numactive},
    {"connect_get_numinactive", 2, vert_connect_numinactive},
    {"connect_get_listactive", 3, vert_connect_listactive},
    {"connect_get_listinactive", 3, vert_connect_listinactive},

    /* domain */
    {"domain_lookup", 3, vert_domain_lookup},

    {"domain_get", 2, vert_domain_get},
    {"domain_get", 3, vert_domain_get},

    {"domain_save", 2, vert_domain_save},
    {"domain_restore", 2, vert_domain_restore},
    {"domain_shutdown", 1, vert_domain_shutdown},
    {"domain_suspend", 1, vert_domain_suspend},
    {"domain_resume", 1, vert_domain_resume},

    {"domain_set_autostart", 2, vert_domain_autostart},

    /* interface */
    {"interface_lookup", 3, vert_interface_lookup},
    {"interface_get", 2, vert_interface_get},

    /* network */
    {"network_get", 2, vert_network_get},
    {"network_lookup", 3, vert_network_lookup},

    /* all resource types */
    {"resource_define", 3, vert_resource_define},
    {"resource_undefine", 1, vert_resource_undefine},
    {"resource_create", 2, vert_resource_create},
    {"resource_destroy", 1, vert_resource_destroy},

    {NULL, 0, NULL}
};


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
    atom_undefined = enif_make_atom(env, "undefined");
    atom_unsupported = enif_make_atom(env, "unsupported");
    atom_enomem = enif_make_atom(env, "enomem");
    atom_resource = enif_make_atom(env, "resource");
    atom_connect = enif_make_atom(env, "connect");
    atom_domain = enif_make_atom(env, "domain");
    atom_true = enif_make_atom(env, "true");
    atom_false = enif_make_atom(env, "false");

    if ( (NIF_VERT_RESOURCE = enif_open_resource_type(env, NULL,
            "vert_resource", vert_cleanup,
            ERL_NIF_RT_CREATE, NULL)) == NULL)
        return -1;

    if (virInitialize() != 0)
        return -2;

    /* XXX Disable error messges to stderr
     * XXX Probably should send the errors to a mailbox
     * */
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


    void
unload(ErlNifEnv *env, void *priv_data)
{
}  


    void *
vert_loop(void *arg)
{
    VERT_STATE *state = arg;
    VERT_CAST *cmd = NULL;
    ErlNifEnv *env = NULL;
    ERL_NIF_TERM res = {0};

    fd_set rfds;
    ssize_t n = 0;


    env = enif_alloc_env();
    cmd = enif_alloc(sizeof(VERT_CAST));

    if ( (env == NULL) || (cmd == NULL))
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

        if (read(state->fd[VERT_READ], cmd, sizeof(VERT_CAST)) < 0)
            goto ERR;

        res = (*cmd->fptr)(env, cmd->argc, (ERL_NIF_TERM *)cmd->argv);

        (void)enif_send(NULL, cmd->pid, env, res);

        enif_free(cmd->pid);
        enif_free(cmd->argv);

        enif_clear_env(env);
    }

ERR:
    if (cmd)
        enif_free(cmd);
    if (env)
        enif_free_env(env);

    return NULL;
}


    ERL_NIF_TERM
vert_cast(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_STATE *state = NULL;
    VERT_CAST *cmd = NULL;
    char buf[MAX_ATOM_LEN+1];
    int i = 0;
    int found = 0;


    state = enif_priv_data(env);

    if (!enif_get_atom(env, argv[0], buf, sizeof(buf), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    for (i = 0; vert_funcs[i].name != NULL; i++) {
        if ( (strcmp(vert_funcs[i].name, buf) == 0) &&
                (vert_funcs[i].arity == argc-1)) {
            found = 1;
            break;
        }
    }

    if (found == 0)
        return enif_make_badarg(env);

    cmd = enif_alloc(sizeof(VERT_CAST));
    cmd->argv = enif_alloc(sizeof(ERL_NIF_TERM) * (argc-1));
    cmd->pid = enif_alloc(sizeof(ErlNifPid));

    if ( (cmd == NULL) || (cmd->argv == NULL) || (cmd->pid == NULL))
        return error_tuple(env, atom_enomem);

    (void)enif_self(env, cmd->pid);
    cmd->fptr = vert_funcs[i].fptr;
    (void)memcpy(cmd->argv, &argv[1], sizeof(ERL_NIF_TERM) * (argc-1));
    cmd->argc = argc-1;

    if (write(state->fd[VERT_WRITE], cmd, sizeof(VERT_CAST)) < 0)
        return error_errno(env, errno);

    enif_free(cmd);

    return atom_ok;
}


static ErlNifFunc nif_funcs[] = {
    {"cast", 2, vert_cast},
    {"cast", 3, vert_cast},
    {"cast", 4, vert_cast},
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)
