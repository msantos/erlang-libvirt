
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

## CREATING A TEST VM

    If you don't have a VM ready to test, you can download a test image
    by running:

        bin/get_image.escript

    The script will download an OpenWRT image and set up the configuration
    in priv/example.xml. By default, it will set up the VM to run under
    KVM using user mode networking.

    You can manually modify the configuration afterwards or set these
    environment variables before running the script:

        VERT_QEMU_BIN : path to the qemu binary (default: /usr/bin/kvm)
        VERT_BRIDGE_INTERFACE : bridge interface (default: user networking)

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
        ok = vert:virDomainCreate(Domain),

        Active = vert:virConnectNumOfDomains(Connect),
        io:format("Active Domains: ~p~n", [Active]),

        {ok, Domain}.

    halt(Domain) ->
        ok = vert:virDomainDestroy(Domain).

### DUMPING XML CONFIGURATION

This example dumps the XML of a defined (not running) domain.

    1> {ok, Connect}  = vert:virConnectOpen("qemu:///system"),
    {ok,{resource,connect,#Ref<0.0.0.30>,<<>>}}

    2> {ok, [Host|_]} = vert:virConnectListDefinedDomains(Connect).
    {ok, ["vm1"]}

    3> {ok, Domain} = vert:virDomainLookupByName(Connect, Host).
    {ok,{resource,domain,#Ref<0.0.0.56>,<<>>}}

    4> {ok, XML} = vert:virDomainGetXMLDesc(Domain, 0).

    5> {Doc, _} = xmerl_scan:string(XML).

    6> rr(xmerl).

    7> [Memory|_] = xmerl_xpath:string("/domain/memory/text()", Doc).

    8> Memory#xmlText.value.
    "1048576"

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
