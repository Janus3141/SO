
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
            Control = spawn(commands,player_control,[[],[],Updts]);
            controlling_process(Sock, spawn(?MODULE,psocket,[Sock,Updts,Control]);
        {error, Reason} ->
            io:format("tcp accept error: ~p~n",[Reason])
    end,
    dispatcher(ListenPort).


psocket(Socket, Updts, Control) ->
    %% Primero se configura "active once", de lo contrario psocket
    %% podria inundarse de paquetes del cliente y reducir
    %% la eficiencia de receive
    inet:setopts(Socket, [{active, once}])
    receive
        %% Mensajes del cliente. Se procesan en el nodo indicado por pbalance
        {tcp,Socket,Cmd} ->
            pbalance ! {qry, self()},
            receive
                {ok,Node} -> spawn(Node, commands, pcommand, [Cmd,self(),Updts,Control])
            end;
        %% Contestaciones de pcommand
        {pcmd, Msg} ->
            gen_tcp:send(Socket,Msg);
        stop ->
            exit(normal);
        %% Si se recibio un mensaje no deseado
        Err ->
            io:format("Unexpected '~p' in psocket~n",[Err])
    end,
    psocket(Socket, Updts, Control).


updts_sender(Socket,ID) ->
    %% Mandar updt entrante
    receive
        {updt,Msg} ->
            gen_tcp:send(Socket,"UPD " ++ ID ++ " " ++ Msg);
        stop ->
            exit(normal)
    end,
    %% Esperar respuesta
    receive
        {ok,ID} ->
            ok;
        stop ->
            exit(normal)
    end,
    %% Loop
    updts_sender(Socket,ID+1).





