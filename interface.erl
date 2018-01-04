
-module(interface).
-compile(export_all).


start_listen(Port) ->
    %% Creacion de un socket para escuchar
    case gen_tcp:listen(Port, [list,inet,{active,false}]) of
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
            controlling_process(Sock, spawn(?MODULE,psocket,[Sock,Updts]);
        {error, Reason} ->
            io:format("tcp accept error: ~p~n",[Reason])
    end,
    dispatcher(ListenPort).


psocket(Socket, Updts) ->
    %% Primero se configura "active once", de lo contrario psocket
    %% podria inundarse de paquetes del cliente y reducir
    %% la eficiencia de receive
    inet:setopts(Socket, [{active, once}])
    receive
        %% Mensajes del cliente. Se procesan en el nodo indicado por pbalance
        {tcp,Socket,Cmd} ->
            pbalance ! {get_node, self()},
            receive
                {pbalance,Node} -> spawn(Node, ?MODULE, pcommand, [Cmd,self(),Updts])
            end;
        %% Contestaciones de pcommand
        {pcmd, Msg} -> gen_tcp:send(Socket,Msg);
        %% Si se recibio un mensaje no deseado
        Err -> io:format("Unexpected '~p' in psocket~n",[Err])
    end,
    psocket(Socket,Updts).


updts_sender(Socket,ID) ->
    %% Mandar updt entrante
    receive
        {updt,Msg} -> gen_tcp:send(Socket,"UPD " ++ ID ++ " " ++ Msg)
    end,
    %% Esperar respuesta
    receive
        {ok,ID} -> ok
    end,
    %% Loop
    updts_sender(Socket,ID+1).


pcommand(Cmd,PSocket,Updts) ->
    PSocket ! case string:tokens(Cmd, " ") of
        ["CON",Name] -> commands:start_conn(Name,Updts);
        ["LSG",ID] -> commands:list_games(ID);
        ["NEW",ID] -> commands:new_game(ID,Updts);
        ["ACC",ID,GID] -> commands:access_game(ID,GID,Updts);
        ["PLA",ID,GID|Play] -> commands:play(ID,GID,Play,Updts);
        ["OBS",ID,GID] -> commands:watch_game(ID,GID,Updts);
        ["LEA",ID,GID] -> commands:unwatch_game(ID,GID,Updts);
        ["OK",ID] -> Updts ! {ok,ID};
        ["BYE"] ->




