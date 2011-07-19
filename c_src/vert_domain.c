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
#include "vert_domain.h"


/* 0: virConnectPtr, 1: int type 2: int | char* */
    ERL_NIF_TERM
vert_domain_lookup(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    VERT_RESOURCE *dp = NULL;
    int type = VERT_ATTR_ID;

    ERL_NIF_TERM res = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    CHECK_RESOURCE_TYPE(vp, VERT_RES_CONNECT);
    RESOURCE_ALLOC(dp, VERT_RES_DOMAIN, vp->res);

    switch (type) {
        case VERT_ATTR_ID: {
            int id = 0;

            if (!enif_get_int(env, argv[2], &id))
                return enif_make_badarg(env);
            dp->res = virDomainLookupByID(vp->res, id);
            }
            break;

        case VERT_ATTR_NAME: {
            char name[HOST_NAME_MAX];

            if (enif_get_string(env, argv[2], name, sizeof(name), ERL_NIF_LATIN1) < 1)
                return enif_make_badarg(env);

            dp->res = virDomainLookupByName(vp->res, name);
            }
            break;

        case VERT_ATTR_UUID: {
            char uuid[VIR_UUID_BUFLEN];

            if (enif_get_string(env, argv[2], uuid, sizeof(uuid), ERL_NIF_LATIN1) < 1)
                return enif_make_badarg(env);

            dp->res = virDomainLookupByUUID(vp->res, (const unsigned char *)uuid);
            }
            break;

        default:
            return error_tuple(env, atom_unsupported);
    }

    if (dp->res == NULL) {
        enif_release_resource(dp);
        return verterr(env);
    }

    res = enif_make_resource(env, dp);
    enif_release_resource(dp);

    return vert_make_resource(env, atom_domain, res);
}


/* 0: VERT_RESOURCE, 1: type */
    ERL_NIF_TERM
vert_domain_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;
    int type = 0;

    ERL_NIF_TERM term = {0};


    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &type))
        return enif_make_badarg(env);

    CHECK_RESOURCE_TYPE(dp, VERT_RES_DOMAIN);

    switch (type) {
        case VERT_ATTR_AUTOSTART: {
            int autostart = 0;

            VERTERR(virDomainGetAutostart(dp->res, &autostart) < 0);

            term = (autostart ? atom_true : atom_false);
            }
            break;

#ifdef HAVE_VIRDOMAINGETBLOCKINFO
        case VERT_ATTR_BLOCKINFO: {
            char path[MAXPATHLEN];
            virDomainInfo info = {0};
            ERL_NIF_TERM buf = {0};

            if (argc != 3 || !enif_get_string(env, argv[2], path, sizeof(path), ERL_NIF_LATIN1))
                return enif_make_badarg(env);

            VERTERR(virDomainGetBlockInfo(dp->res, path, &info, 0) < 0);
            buf = bincopy(env, &info, sizeof(virDomainInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;
#endif

        case VERT_ATTR_CONNECT: {
            VERT_RESOURCE *cp = NULL;
            ERL_NIF_TERM res = {0};

            RESOURCE_ALLOC(cp, VERT_RES_CONNECT, NULL);
            cp->res = dp->conn;
            res = enif_make_resource(env, cp);
            enif_release_resource(cp);

            term = vert_make_resource(env, atom_connect, res);
            }
            break;

        case VERT_ATTR_ID: {
            unsigned int id = 0;

            id = virDomainGetID(dp->res);

            VERTERR(id < 0);

            term = enif_make_uint(env, id);
            }
            break;

        case VERT_ATTR_INFO: {
            virDomainInfo info = {0};
            ERL_NIF_TERM buf = {0};

            struct domain_info {
                unsigned char state;
                unsigned long maxMem;
                unsigned long memory;
                unsigned short nrVirtCpu;
                unsigned long long cpuTime;
            } __attribute__((__packed__));
            struct domain_info dip = {0};
            
            VERTERR(virDomainGetInfo(dp->res, &info) < 0);

            dip.state = info.state;
            dip.maxMem = info.maxMem;
            dip.memory = info.memory;
            dip.nrVirtCpu = info.nrVirtCpu;
            dip.cpuTime = info.cpuTime;

            buf = bincopy(env, &dip, sizeof(struct domain_info));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

#ifdef HAVE_VIRDOMAINGETJOBINFO
        case VERT_ATTR_JOBINFO: {
            virDomainJobInfo info = {0};
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetJobInfo(dp->res, &info) < 0);
            buf = bincopy(env, &info, sizeof(virDomainInfo));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;
#endif

        case VERT_ATTR_MAXMEMORY: {
            unsigned long mem = 0;

            mem = virDomainGetMaxMemory(dp->res);    // XXX can also be NULL for domain0
            VERTERR(mem == 0);

            term = enif_make_ulong(env, mem);
            }
            break;

        case VERT_ATTR_MAXVCPUS: {
            int max = -1;

            max = virDomainGetMaxVcpus(dp->res);

            VERTERR(max < 0);

            term = enif_make_int(env, max);
            }
            break;

#ifdef HAVE_VIRDOMAINGETMEMORYPARAMETERS
        case VERT_ATTR_MEMORYPARAMETERS: {
            int n = 0;
            ErlNifBinary buf = {0};

            VERTERR( (virDomainGetMemoryParameters(dp->res, NULL, &n, 0) < 0) || n == 0);
            
            if (!enif_alloc_binary(sizeof(virMemoryParameter)*n, &buf))
                return atom_enomem;

            VERTERR(virDomainGetMemoryParameters(dp->res, buf.data, &n, 0) < 0);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple4(env,
                    enif_make_atom(env, "parameter"),
                    erl_make_binary(env, &buf),
                    erl_make_int(env, n)
                    ));
            }
            break;
#endif

        case VERT_ATTR_NAME: {
            const char *name = NULL;

            name = virDomainGetName(dp->res);

            VERTERR(dp->res == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            }
            break;

        case VERT_ATTR_OSTYPE: {
            char *name = NULL;  /* should be freed */

            name = virDomainGetOSType(dp->res);

            VERTERR(name == NULL);

            term = enif_make_string(env, name, ERL_NIF_LATIN1);
            free(name);
            }
            break;
            
        case VERT_ATTR_SCHEDULERPARAMETERS: {
            virSchedParameter params;
            int n = 0;
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetSchedulerParameters(dp->res, &params, &n) < 0);

            buf = bincopy(env, &params, sizeof(virSchedParameter));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_SCHEDULERTYPE: {
            char *name = NULL;
            int n = 0;
            ERL_NIF_TERM buf = {0};

            name = virDomainGetSchedulerType(dp->res, &n);

            VERTERR(name == NULL);
            buf = bincopy(env, name, strlen(name)+1);
            NOMEM(buf);

            term = enif_make_tuple2(env,
                atom_ok,
                enif_make_tuple3(env,
                    enif_make_atom(env, "parameter"),
                    buf,
                    enif_make_int(env, n)
                    ));

            free(name);
            }
            break;

        case VERT_ATTR_SECURITYLABEL: {
            virSecurityLabel label;
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetSecurityLabel(dp->res, &label) < 0);
            buf = bincopy(env, &label, sizeof(virSecurityLabel));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_RAWUUID: {
            unsigned char uuid[VIR_UUID_BUFLEN];
            ERL_NIF_TERM buf = {0};

            VERTERR(virDomainGetUUID(dp->res, uuid) < 0);
            buf = bincopy(env, &uuid, sizeof(VIR_UUID_BUFLEN));
            NOMEM(buf);

            term = enif_make_tuple2(env, atom_ok, buf);
            }
            break;

        case VERT_ATTR_UUID: {
            char uuid[VIR_UUID_STRING_BUFLEN];

            VERTERR(virDomainGetUUIDString(dp->res, uuid) < 0);

            term = enif_make_tuple2(env, atom_ok,
                enif_make_string(env, uuid, ERL_NIF_LATIN1));
            }
            break;

/*
        case VERT_ATTR_VCPUS: {
            virVcpuInfo info = {0};
            int max = 0;

            VERTERR(virDomainGetVcpus(*dp, &info, maxinfo, cpumaps, int maplen) < 0);

            }
            break;

        case VERT_ATTR_VCPUSFLAGS: {
            }
            break;
*/

        case VERT_ATTR_DESC: {
            char *desc = NULL;
            int flags = 0;

            if (argc != 3 || !enif_get_int(env, argv[2], &flags))
                return enif_make_badarg(env);

            desc = virDomainGetXMLDesc(dp->res, flags);

            VERTERR(desc == NULL);

            term = enif_make_tuple2(env, atom_ok,
                enif_make_string(env, desc, ERL_NIF_LATIN1));

            free(desc);
            }
            break;

        default:
            return error_tuple(env, atom_unsupported);
    }

    return term;
}


/* 0: VERT_RESOURCE, 1: char* */
    ERL_NIF_TERM
vert_domain_save(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;
    char file[MAXPATHLEN];

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], file, sizeof(file), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    CHECK_RESOURCE_TYPE(dp, VERT_RES_DOMAIN);

    res = virDomainSave(dp->res, file);

    VERTERR(res != 0);

    return atom_ok;
}


/* 0: VERT_RESOURCE, 1: char* */
    ERL_NIF_TERM
vert_domain_restore(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *vp = NULL;
    char file[MAXPATHLEN];

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&vp))
        return enif_make_badarg(env);

    if (enif_get_string(env, argv[1], file, sizeof(file), ERL_NIF_LATIN1) < 0)
        return enif_make_badarg(env);

    CHECK_RESOURCE_TYPE(vp, VERT_RES_CONNECT);

    res = virDomainRestore(vp->res, file);

    VERTERR(res != 0);

    return atom_ok;
}


/* 0: VERT_RESOURCE, 1: int flag */
    ERL_NIF_TERM
vert_domain_autostart(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;
    int flags = 0;

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &flags))
        return enif_make_badarg(env);

    CHECK_RESOURCE_TYPE(dp, VERT_RES_DOMAIN);

    res = virDomainSetAutostart(dp->res, flags);

    VERTERR(res != 0);

    return atom_ok;
}


/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_domain_shutdown(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    res = virDomainShutdown(dp->res);

    VERTERR(res != 0);

    return atom_ok;
}


/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_domain_suspend(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    res = virDomainSuspend(dp->res);
    VERTERR(res != 0);

    return atom_ok;
}

/* 0: VERT_RESOURCE */
    ERL_NIF_TERM
vert_domain_resume(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    VERT_RESOURCE *dp = NULL;

    int res = -1;

    if (!enif_get_resource(env, argv[0], NIF_VERT_RESOURCE, (void **)&dp))
        return enif_make_badarg(env);

    res = virDomainResume(dp->res);
    VERTERR(res != 0);

    return atom_ok;
}

