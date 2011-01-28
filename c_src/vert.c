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


    static int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
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

    return 0;
}


    void
unload(ErlNifEnv *env, void *priv_data)
{
}  


static ErlNifFunc nif_funcs[] = {
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
};

ERL_NIF_INIT(vert, nif_funcs, load, NULL, NULL, unload)

