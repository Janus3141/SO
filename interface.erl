
-module(interface).
-compile(export_all).



start_server(Port, Node) ->
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
    start_listen(Port).



start_listen(Port) ->
    %% Creacion de un socket para escuchar
    case gen_tcp:listen(Port, [list,inet,{active,false},{nodelay,true}]) of
        {ok,Socket} ->
            dispatcher(Socket);
        {error, Reason} ->
            io:format("tcp listen error: ~p~n",[Reason])
    end.



dispatcher(ListenSocket) ->
    %% Creacion del socket de comunicacion, el cual le sera
    %% cedido al nuevo psocket.
    case gen_tcp:accept(ListenSocket) of
        {ok,Sock} ->
            Updts = spawn(?MODULE,updts_sender,[Sock,1]),
            Control = spawn(commands,player_control,[[],[],Updts,undefined]),
            gen_tcp:controlling_process(Sock, spawn(?MODULE,psocket,[Sock,Updts,Control]));
        {error, Reason} ->
            io:format("tcp accept error: ~p~n",[Reason])
    end,
    dispatcher(ListenSocket).



psocket(Socket, Updts, Control) ->
    %% Primero se configura "active once", de lo contrario psocket
    %% podria inundarse de paquetes del cliente y reducir
    %% la eficiencia de receive
    inet:setopts(Socket, [{active, once}]),
    receive
        %% Mensajes del cliente. Se procesan en el nodo indicado por pbalance
        {tcp,Socket,Cmd} ->
            pbalance ! {qry, self()},
            receive
                {ok,Node} -> spawn(Node, commands, pcommand, [Cmd,self(),Updts,Control])
            end;
        %% Contestaciones de pcommand
        {pcmd, Msg} ->
            gen_tcp:send(Socket, Msg ++ ";");
        %% Se cerro la conexion o el cliente se va
        {tcp_closed,Socket} ->
            spawn(commands, pcommand, ["BYE",self(),Updts,Control]);
        stop ->
            exit(normal)
    end,
    psocket(Socket, Updts, Control).



updts_sender(Socket,ID) ->
    IDS = integer_to_list(ID),
    %% Mandar updt entrante
    receive
        {updt,Msg} ->
            gen_tcp:send(Socket,"UPD " ++ IDS ++ " " ++ Msg ++ ";");
        stop ->
            exit(normal)
    end,
    %% Esperar respuesta
    receive
        {ok,IDS} ->
            ok;
        stop ->
            exit(normal)
    end,
    %% Loop
    updts_sender(Socket,ID+1).





