
-module(services).
-compile(export_all).



%%%
%%%
%%% Funciones que actuaran como procesos globales al nodo
%%%
%%%


%% Servicio que mantiene los nombres de los jugadores conectados al nodo
%% Para hacerlo se utiliza el argumento List, que es de la forma
%% [{Updts,Name}]

players_names(List) ->
    Nodes = [node()|nodes()],
    receive
        %% Si se recibe reserve, primero se chequea que el nombre no
        %% se encuentre en la lista (si lo esta se avisa al proceso)
        %% Y luego se espera un unblock del proceso que se comunico
        %% O un take que agrega el nombre y socket a la lista
        {reserve,Name,Ret} ->
            case lists:keyfind(Name,2,List) of
                {_,Name} -> Ret ! {taken,Name},
                false -> ok
            end,
            receive
                {unblock,Name} ->
                    players_names(List);
                {take,Updts,Name} ->
                    [{Node,players} ! {unblock,Name} || Node <- Nodes],
                    players_names([{Updts,Name}|List])
            end;
        %% Al recibir get_name simplemente se devuelve, si esta, el
        %% nombre asociado con el socket argumento
        {get_name,Updts,Ret} ->
            case lists:keyfind(Updts,1,List) of
                {Updts,Name} ->
                    Ret ! {players,Name,Updts};
                false ->
                    ok
            end
    end,
    players_names(List).



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
        {gcom,access,Ret,PidS,Updts} ->
            try
                Pid = list_to_pid(PidS),
                case lists:keyfind(game_to_id(Pid),1,List) of
                    {_,notfull} ->
                        Pid ! {access,Ret,Updts};
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



%% Redireccion de mensajes. Los comandos ACC,PLA,OBS y LEA seran
%% redirigidos al juego al que hacen referencia. Solo ACC debe pasar,
%% antes, por el servicio games para asegurarse que el juego no este lleno

games_msgs() ->
    receive
        %% gcom: Comunicacion interna entre games de todos los nodos
        %% Redireccion de mensaje a juegos en el nodo
        {gcom,play,Ret,PidS,Play,Updts} ->
            try
                Pid = list_to_pid(PidS),
                Pid ! {play,Ret,Play,Updts}
            catch
                error:_ -> Ret ! {error, "INVALID GID"}
            end;
        {gcom,Other,Ret,PidS,Updts} ->
            try
                Pid = list_to_pid(PidS),
                Pid ! {Other,Ret,Updts}
            catch
                error:_ -> Ret ! {error, "INVALID GID"}
            end;

        %% Redireccion de mensajes a otros nodos
        {play,Ret,GID,Play,Updts} ->
            case id_to_game(GID) of
                {ok,PidS,Node} -> 
                    {games_msgs,Node} ! {gcom,play,Ret,PidS,Play,Updts};
                error ->
                    Ret ! {error, "INVALID GID"}
            end;
        {access,Ret,GID,Updts} ->
            case id_to_game(GID) of
                {ok,PidS,Node} ->
                    {games,Node} ! {gcom,access,Ret,PidS,Play,Updts};
                error ->
                    Ret ! {error,"INVALID GID"}
            end;
        {Other,Ret,GID,Updts} ->
            case id_to_game(GID) of
                {ok,PidS,Node} -> 
                    {games_msgs,Node} ! {gcom,Other,Ret,PidS,Updts};
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



