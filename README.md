
Erlang bindings to the libvirt virtualization API.


## ALTERNATIVES

An (almost) pure Erlang libvirt binding is available here:

<https://github.com/msantos/erlang-libvirt-remote>

libvirt-remote uses the (unsupported) libvirtd remote procotol over a
Unix socket.


## WARNING

    The libvirt API is massive, inconsistent and error prone. So consider
    this language binding to be incomplete, buggy and erratic.

    The current implementation calls all libvirt functions in a thread so
    that the Erlang VM will not block. If libvirt blocks, the caller will
    receive an error immediately ({error, eagain}). This means only one
    call from a single Erlang VM into libvirt can be running at any time.

    These bindings have not been heavily tested, are still under
    development and will undergo many changes.


## HOW TO BUILD IT

    sudo apt-get install libvirt-dev libvirt-bin
    make


## HOW TO USE IT

The Erlang libvirt API follows the libvirt C API. For example, if the
C API has:

    virConnectOpen(char *name)
    virConnectGetLibVersion(virConnectPtr(conn), unsigned long *version)

To call the same functions in Erlang:

    {ok, Connect} = vert:virConnectOpen("qemu:///system"),
    {ok,{0,7,5}} = vert:virConnectGetLibVersion(Connect).



## EXAMPLES

### CREATING A DOMAIN

    start(Path) ->
        {ok, Connect} = vert:virConnectOpen("qemu:///system"),
        {ok, XML} = file:read_file(Path),
        {ok, Domain} = vert:virDomainDefineXML(Connect, XML),
        ok = vert:virDomainCreate(Domain, 0),

        Active = vert:virConnectNumOfDomains(Connect),
        io:format("Active Domains: ~p~n", [Active]),

        {ok, Connect, Domain}.

    halt(Connect, Domain) ->
        ok = vert:virDomainDestroy(Domain),
        ok = vert:virConnectClose(Connect).


### SUSPENDING AND RESUMING A DOMAIN

This example is the Erlang equivalent of a Python script to manipulate
running domains. The example is taken from:

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
        {ok, Connect} = vert:virConnectOpen("qemu:///system"),
        {ok, DomainIDs} = vert:virConnectListDomains(Connect),

        [ states(Connect, DomainID) || DomainID <- DomainIDs ],

        ok.

    states(Connect, DomainID) ->
        {ok, Domain} = vert:virDomainLookupByID(Connect, DomainID),
        io:format("running: ~p~n", [info(Domain)]),

        ok = vert:virDomainSuspend(Domain),
        io:format("suspend: ~p~n", [info(Domain)]),

        ok = vert:virDomainResume(Domain),
        io:format("resumed: ~p~n", [info(Domain)]),

        ok = vert:virDomainDestroy(Domain).

    info(Domain) ->
        {ok, Name} = vert:virDomainGetName(Domain),
        {ok, Info} = vert:virDomainGetInfo(Domain),

        [{name, Name}, {info, Info}].


## TODO

* tests: use the libvirt test driver and make a set of tests

* review the Erlang interface
    * find examples of code in other languages using libvirt and discover
      how awkward the Erlang interface is by porting them
    * then of course document the interface

* Carefully check the code. There are many copy/paste defects.

* Allow the caller to kill the background thread if it is blocked

* Figure out how to deal with backwards compatibility (functions
  unsupported in older versions of libvirt)

* Generate as much of the code as possible automatically from the libvirt
  XML documentation file (another rewrite!)

* Reduce macro usage, many of them obscure the logic (returning from
  functions, setting variables, ...). Rename macros to show intent, e.g.,
  VERTERR() should be something like RETURN\_ERR\_IF\_FALSE(), ISNULL()
  should be RETURN\_ERR\_IF\_NULL().

* Modify the resource structure to hold function pointers based on
  resource type, e.g., freeing the resource, etc.
