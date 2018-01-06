
-module(services).
-compile(export_all).



%%%
%%%
%%% Funciones que actuaran como procesos globales al nodo
%%%
%%%




%%% NOMBRE REGISTRADO: pbalance
%% Junto con pstat, balancea la carga entre los nodos
%% El actual nodo con carga minima lo localiza mediante Min

balance({MinNode,MinLoad}, Nodes) ->
    receive
        {updt, Node, Load} ->
            %% Primero, si el nodo no estaba en la lista anteriormente
            %% se empieza a monitorear y luego se agrega a la lista
            case lists:keymember(Node,1,Nodes) of
                false -> monitor_node(Node,true);
                true -> ok
            end
            %% Actualizar la informacion de carga de Node y
            %% seleccionar el nodo con carga minima
            if
                Load < MinLoad ->
                    balance({Node,Load},lists:keystore(Node,1,Nodes,{Node,Load}));
                true ->
                    balance({MinNode,MinLoad},lists:keystore(Node,1,Nodes,{Node,Load}))
            end;
        {qry, Ret} ->
            %% Devuelve el nodo con menor carga
            Ret ! {ok,MinNode};
        {nodedown, Node} ->
            %% Si un nodo se cae, se quita de la lista, y se actualiza el minimo
            NewList = lists:keydelete(Node,1,Nodes),
            if
                Node =:= MinNode ->
                    [NewMin|Tail] = lists:keysort(2,NewList),
                    balance(NewMin,NewList);
                true ->
                    balance({MinNode,MinLoad},NewList)
            end
    end,
    balance({MinNode,MinLoad}, Nodes).




%% No tiene nombre registrado
%% Envia informacion de carga a los demas nodos

pstat() ->
    %% Calcular carga
    {_,Load} = erlang:statistics(reductions),
    %% Enviar updates a todos los nodos
    AllNodes = [node() | nodes()],
    [{pbalance, Node} ! {updt, node(), Load} || Node <- AllNodes],
    %% Esperar y seguir
    timer:wait(1000),
    pstat().




%%% NOMBRE REGISTRADO: players
%% Servicio que mantiene los nombres de los jugadores conectados al nodo
%% Para hacerlo se utiliza el argumento List, que es de la forma
%% [{IdProc,Name}]

players_names(List) ->
    receive
        %% Agregar la tupla {IdProc,Name} a la lista de jugadores
        {add,IdProc,Name,Ret} ->
            %% Asegurarse que el nombre no esta registrado en este nodo
            case lists:keyfind(Name,2,List) of
                {_,Name} ->
                    Ret ! {error,"NAME TAKEN"},
                    players_names(List);
                false -> ok
            end,
            %% Asegurarse que el nombre no este registrado en otro nodo
            %% Primero se pregunta a todos los nodos
            Nodes = nodes(),
            [{players,Node} ! {qry,Name,self()} || Node <- Nodes],
            %% Despues se reciben las contestaciones
            case names_query(Nodes) of
                ok ->
                    Ret ! ok,
                    players_names([{IdProc,Name}|List]);
                taken ->
                    Ret ! {error,"NAME TAKEN"};
                %% Hay un error si un nodo esta conectado pero no
                %% contesta, en ese caso hay un deadlock, se vuelve
                %% a intentar luego de un backoff
                error ->
                    timer:sleep(rand:uniform(1000)),
                    self() ! {add,IdProc,Name,Ret}
            end;
        %% Pedido de otro nombre para verificar que Name no esta en List
        {qry,Name,Ret} ->
            case lists:keyfind(Name,2,List) of
                {_,Name} -> Ret ! taken;
                false -> Ret ! {ok,node()}
            end;
        %% Borrado de Name de List
        {del,Name} ->
            players_names(lists:keydelete(Name,2,List));
        %% Al recibir get_name simplemente se devuelve, si esta, el
        %% nombre asociado con el socket argumento
        {get_name,IdProc,Ret} ->
            case lists:keyfind(IdProc,1,List) of
                {IdProc,Name} ->
                    Ret ! {ok,Name,IdProc};
                false ->
                    Ret ! {error,node()}
            end
    end,
    players_names(List).




%%% NOMBRE REGISTRADO: games
%% Conserva todos los pids de los juegos del sistema, junto con
%% el estado en el que estan (lleno o no).
%% Puede devolver la lista de pids, agregar o quitar un juego
%% y cambiar el estado de un juego 

games_names(List) ->
    receive
        %% Devuelve la lista del juegos en el nodo
        {get_games,Ret} ->
            Ret ! [GID ++ "," ++ atom_to_list(St) || {GID,St} <- List];
        %% Agrega un nuevo juego a la lista
        {new,Ret,Game} ->
            case game_to_id(Game) of
                {ok, GID} ->
                    Game ! {get_dets,GID},
                    receive
                        {ok,GameInfo} ->
                            Ret ! {ok, GID++" "++GameInfo},
                            games_names([{GID,notfull}|List]);
                        {error,Info} ->
                            Ret ! {error,Info}
                    end;
                {error,Info} ->
                    Game ! error,
                    Ret ! {error,Info}
            end;
        %% Cambia estado St
        {cstate,GID,St} ->
            NewList = lists:keyreplace(GID,1,List,{GID,St}),
            games_names(NewList);
        %% Elimina el juego de la lista
        {del,GID} ->
            games_names(lists:keydelete(GID,1,List));
        %% Redirecciona mensaje al juego para que el cliente acceda
        %% a jugar, si la partida no esta llena
        {gcom,access,Ret,PidS,IdProc} ->
            try
                Pid = list_to_pid(PidS),
                case lists:keyfind(game_to_id(Pid),1,List) of
                    {_,notfull} ->
                        Pid ! {access,Ret,IdProc};
                    {_,full} ->
                        Ret ! {error,"FULL"};
                    false ->
                        Ret ! {error,"INVALID GID"}
                end
            catch
                error:_ -> Ret ! {error, "INVALID GID"}
            end
    end,
    games_manager(List).




%%% NOMBRE REGISTRADO: gmsg
%% Redireccion de mensajes. Los comandos ACC,PLA,OBS y LEA seran
%% redirigidos al juego al que hacen referencia. Solo ACC debe pasar,
%% antes, por el servicio games para asegurarse que el juego no este lleno

games_msgs() ->
    receive
        %% gcom: Comunicacion interna entre games de todos los nodos
        %% Redireccion de mensaje a juegos en el nodo
        {gcom,play,Ret,PidS,Play,IdProc} ->
            try
                Pid = list_to_pid(PidS),
                Pid ! {play,Ret,Play,IdProc}
            catch
                error:_ -> Ret ! {error, "INVALID GID"}
            end;
        {gcom,Other,Ret,PidS,IdProc} ->
            try
                Pid = list_to_pid(PidS),
                Pid ! {Other,Ret,IdProc}
            catch
                error:_ -> Ret ! {error, "INVALID GID"}
            end;

        %% Redireccion de mensajes a otros nodos
        {play,Ret,GID,Play,IdProc} ->
            case id_to_game(GID) of
                {ok,PidS,Node} -> 
                    {games_msgs,Node} ! {gcom,play,Ret,PidS,Play,IdProc};
                error ->
                    Ret ! {error, "INVALID GID"}
            end;
        {access,Ret,GID,IdProc} ->
            case id_to_game(GID) of
                {ok,PidS,Node} ->
                    {games,Node} ! {gcom,access,Ret,PidS,Play,IdProc};
                error ->
                    Ret ! {error,"INVALID GID"}
            end;
        {Other,Ret,GID,IdProc} ->
            case id_to_game(GID) of
                {ok,PidS,Node} -> 
                    {games_msgs,Node} ! {gcom,Other,Ret,PidS,IdProc};
                error ->
                    Ret ! {error, "INVALID GID"}
            end
    end,
    games_msgs().






%%%
%%%
%%% Funciones auxiliares de los servicios
%%%
%%%


%% Recibe respuestas de los servicios de
%% nombres de jugadores de otros nodos
name_query([]) -> ok;
name_query(Nodes) ->
    receive
        taken -> taken;
        %% Si el nombre no esta tomado en Node se sigue esperando
        %% respuestas de los otros nodos
        {ok,Node} -> name_query(lists:delete(Node,Nodes))
    after 1000 ->
        %% Si en 1 segundo no se reciben mas respuestas se controla
        %% el estado de algun nodo en la lista Nodes
        [Node|Nodes2] = Nodes,
        case lists:member(Node,nodes()) of
            %% Si el nodo esta activo, debe haber un deadlock
            true -> error;
            %% Si el nodo no esta activo, se lo descarta y se
            %% siguen esperando respuestas de los nodos en Nodes2
            false -> name_query(Nodes2)
        end
    end.



%% Pregunta a todos los servicios players cual es el nombre del
%% jugador identificado por el proceso IdProc
name_by_psock(IdProc) ->
    Nodes = [node()|nodes()],
    [{players,Node} ! {get_name,IdProc,self()} || Node <- Nodes],
    name_by_psock2(Nodes).

name_by_psock2([]) -> {error,"NOT FOUND"};
name_by_psock2(Nodes) ->
    receive
        %% Se encuentra el nombre y se devuelve
        {ok,Name,_} -> {ok,Name};
        %% El nombre no estaba registrado en Node, se continua
        %% con los otros nodos
        {error,Node} -> name_by_psock2(lists:delete(Node,Nodes))
    after 1000 ->
        %% Uno o varios nodos no contestan, se verifica conectividad
        [Node|Nodes2] -> Nodes,
        case lists:member(Node, [node() | nodes()]) of
            %% Node aun esta conectado, se vuelve a probar
            true -> name_by_psock2(Nodes);
            %% Node no esta conectado, se sigue probando con
            %% los nodos que quedan en la lista
            false -> name_by_psock2(Nodes2)
        end
    end.



%% Permite verificar que el nodo que hostea la partida
%% identificada por GID aun esta conectado, devolviendo
%% true o false segun corresponda. Si el GID es incorrecto
%% se devuelve error
conn_control(GID) ->
    case id_to_game(GID) of
        {ok,_,Node} ->
            lists:member(Node, [node() | nodes()]);
        error ->
            error
    end.


% Conversion de Pid local a un ID de juego
game_to_id(Pid) ->
    try
        PidS = pid_to_list(Pid),
        NodeS = atom_to_list(node()),
        {ok, PidS ++ "+" ++ NodeS}
    catch
        error:R -> {error,R}
    end.



%% Devuelve nodo (atomo) y pid (string) de un juego
id_to_game(GID) ->
    case string:tokens(GID,"+") of
        [Pid,Node] ->
            try
                {ok,Pid,list_to_atom(Node)}
            catch
                error:_ -> error
            end;
        _ -> error
    end.



