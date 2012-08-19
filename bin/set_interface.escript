#!/usr/bin/env escript
%%! -pa ebin

%%%
%%% Bring up a single interface for the OpenWRT VM
%%%

main([]) ->
    main([["testvm"]]);
main(Hosts) ->
    Commands = [
        "delete network.lan",
        "set network.wan=interface",
        "set network.wan.ifname=eth0",
        "set network.wan.proto=dhcp"
    ],
    [ batch(Host, Commands) || Host <- Hosts ].

batch(Host, Commands) ->
    Cmd = "uci batch <<-EOF\r\n" ++
            string:join(Commands, "\r\n") ++
            "\r\ncommit network\r\nEOF\r\n",

    error_logger:info_report([{cmd, Host, Cmd}]),

    send(Host, Cmd).

send(Host, Cmd) ->
    {ok, Ref} = vert_console:open(Host),
    ok = vert_console:send(Ref, Cmd),
    vert_console:close(Ref).
