
Erlang bindings to the libvirt virtualization API.


## ALTERNATIVES

An (almost) pure Erlang libvirt binding is available here:

<https://github.com/msantos/erlang-libvirt-remote>

libvirt-remote uses the libvirtd remote procotol over a socket (by
default, a Unix socket).


## WARNING

    The current implementation prevents the Erlang VM from being blocked
    by calling all libvirt functions in a thread.  If libvirt blocks,
    the caller will receive an error immediately ({error, eagain}).
    Only one call from Erlang into libvirt can be running at a time.

    These bindings have only been lightly tested. It's still possible
    that some of the functions may segfault.


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

    1> {ok, Connect}  = vert:virConnectOpen("qemu:///system").
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

* allow caller to optionally run a libvirt operations in a new thread

* add remaining libvirt API functions
