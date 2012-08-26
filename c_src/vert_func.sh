#!/bin/sh

awk -F\" '{
    if ($2 ~ "vir") {
        print "ERL_NIF_TERM vert_" $2 "(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);"
    }
}' $1
