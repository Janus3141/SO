
-module(interface).



dispatcher(Port) ->
    %% Creacion de un socket para escuchar. Si hay algun error,
    %% se imprime y se continua con la ejecucion
    case gen_tcp:listen(Port, [list,inet,{active,false}]) of
        {ok,Socket} ->
            ok;
        {error, Reason} ->
            io:format("tcp listen error: ~p~n",[Reason]),
            dispatcher(Port)
    end,
    %% Creacion del socket de comunicacion, el cual le sera
    %% cedido al nuevo psocket.
    case gen_tcp:accept(Socket) of
        {ok,Sock} ->
            controlling_process(Sock, spawn(?MODULE,psocket,[Sock]);
        {error, Reason} ->
            io:format("tcp accept error: ~p~n",[Reason])
    end,
    dispatcher(Port).


psocket(Socket) ->
    %% Primero se configura "active once", de lo contrario psocket
    %% podria inundarse de paquetes del cliente y reducir
    %% la eficiencia de receive
    inet:setopts(Socket, [{active, once}])
    receive
        %% Contestaciones de pcommand
        {pcmd, Msg} -> gen_tcp:send(Socket,Msg);
        %% Mensajes del cliente. Se procesan en el nodo indicado por pbalance
        {tcp,Socket,Cmd} ->
            pbalance ! {get_node, self()},
            receive
                {pbalance,Node} -> spawn(Node, ?MODULE, pcommand, [Cmd,self(),Socket])
            end;
        %% Si se recibio un mensaje no deseado
        Err -> io:format("Unexpected '~p' in psocket~n",[Err])
    end,
    psocket(Socket).


pcommand(Cmd,PSocket,Sock) ->
    PSocket ! case string:tokens(Cmd) of
        ["CON",Name] -> commands:start_conn(Name,Sock);
        ["LSG",ID] -> commands:list_games(ID);
        ["NEW",ID] -> commands:new_game(ID,Sock);
        ["ACC",ID,GID] -> commands:enter_game(ID,GID);
        ["PLA",ID,GID|Play] -> commands:play(ID,GID,Play);
        ["OBS",ID,GID] -> commands:watch_game(ID,GID);
        ["LEA",ID,GID] -> commands:unwatch_game(ID,GID);
        ["OK",ID] -> games ! {ok,ID};
        ["BYE"] ->




