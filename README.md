
Erlang bindings to the libvirt virtualization API.


## ALTERNATIVES

An (almost) pure Erlang libvirt binding is available here:

<https://github.com/msantos/erlang-libvirt-remote>

This version uses the libvirtd remote procotol over a Unix socket.


## WARNING

    The libvirt API is not safe.

    Aside from being needlessly huge and error prone, the API is
    inconsistent: some functions require memory to be freed for one
    type of resource but not for other resource types. Some functions
    are blocking and will block the Erlang scheduler. Inconsistencies
    between the same functions for different resources and the sheer
    size of the API mean that there will be mistakes.


## HOW TO BUILD IT

    sudo apt-get install libvirt-dev libvirt-bin
    make


## HOW TO USE IT

## EXAMPLES

### CREATING A DOMAIN

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


### SUSPENDING AND RESUMING A DOMAIN

This example is the Erlang equivalent of a Python script to manipulate a
running domain. The example is taken from:

<http://www.ibm.com/developerworks/linux/library/l-libvirt/>


    -module(ex6).

    %% Listing 6. Sample Python script for domain control (libvtest.py)
    %%
    %% import libvirt
    %%
    %% conn = libvirt.open('qemu:///system')
    %%
    %% for id in conn.listDomainsID():
    %%
    %%         dom = conn.lookupByID(id)
    %%
    %%         print "Dom %s  State %s" % ( dom.name(), dom.info()[0] )
    %%
    %%             dom.suspend()
    %%         print "Dom %s  State %s (after suspend)" % ( dom.name(), dom.info()[0] )
    %%
    %%             dom.resume()
    %%         print "Dom %s  State %s (after resume)" % ( dom.name(), dom.info()[0] )
    %%
    %%             dom.destroy()
    %%
    -export([start/0]).

    start() ->
        {ok, Connect} = vert:open(""),
        {ok, DomainIDs} = vert:resource(Connect, {domain, active}),

        [ states(Connect, DomainID) || DomainID <- DomainIDs ],

        ok.

    states(Connect, DomainID) ->
        {ok, Domain} = vert:resource(Connect, {domain, {id, DomainID}}),
        io:format("running: ~p~n", [info(Domain)]),

        ok = vert:suspend(Domain),
        io:format("suspend: ~p~n", [info(Domain)]),

        ok = vert:resume(Domain),
        io:format("resumed: ~p~n", [info(Domain)]),

        ok = vert:destroy(Domain).

    info(Domain) ->
        Name = vert:get(Domain, name),
        Info = vert:get(Domain, info),

        [{name, Name}, {info, Info}].


## TODO

* tests: use the libvirt test driver and make a set of tests

* review the Erlang interface
    * find examples of code in other languages using libvirt and discover
      how awkward the Erlang interface is by porting them
    * then of course document the interface


