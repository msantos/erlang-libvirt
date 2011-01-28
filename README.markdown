
Erlang bindings to the libvirt virtualization API.

## WARNING

    The libvirt API is not safe.

    Aside from being needlessly huge and error prone, the API is
    inconsistent: some functions require memory to be freed for one
    type of resource but not for other resource types. Some functions
    are blocking and will block the Erlang scheduler. Inconsistencies
    between the same functions for different resources and the sheer
    size of the API mean that there will be mistakes.

    There isn't really a reason to call the libvirt API directly since
    libvirt provides a command line shell (virsh) that can be run as a
    port. The usual reasons given for using an NIF (speed, libraries)
    in this case don't really apply since virsh looks to be complete and
    using it as a port is much safer. Alternatively, it may be possible
    to talk to libvirtd directly from Erlang.

## HOW TO BUILD IT

    sudo apt-get install libvirt-dev libvirt-bin
    make


## HOW TO USE IT

## EXAMPLES

    start(Path) ->
        {ok, Connect} = vert:open({connect, ""}),
        {ok, XML} = file:read_file(Path),
        {ok, Domain} = vert:define(Connect, {domain, XML}),
        ok = vert:create(Domain),

        Active = vert:resource(Connect, {domain, active}),
        io:format("Active Domains: ~p~n", [Active]),

        {ok, Connect, Domain}.

    halt(Connect, Domain) ->
        ok = vert:destroy(Domain),
        ok = vert:close(Connect).

## TODO

