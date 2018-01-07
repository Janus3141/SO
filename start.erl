
-module(start).
-compile(export_all).


server_start(Port, Node) ->
    case Node of
        false -> ok;
        _ -> case net_kernel:connect(Node) of
                true ->
                    ok;
                false ->
                    io:format("Error connecting to ~p~n",[Node]),
                    exit(error);
                ignored ->
                    io:format("Local node not alive")
             end
    end,
    Players = spawn(services, players_names, [[]]),
    register(players, Players),
    Games = spawn(services, games_names, [[]]),
    register(games, Games),
    Gmsgs = spawn(services, games_msgs, []),
    register(gmsg, Gmsgs),
    Pbalance = spawn(services, balance, [{node(), 0},[{node(), 0}]]),
    register(pbalance, Pbalance),
    spawn(services, pstat, []),
    interface:start_listen(Port).


