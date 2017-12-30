
-module(services).
-compile(export_all).



%% Servicio que mantiene los nombres de los jugadores conectados al nodo
%% Para hacerlo se utiliza el argumento List, que es de la forma
%% [{Socket,Name}]

players_names(List) ->
    receive
        %% Si se recibe reserve, primero se chequea que el nombre no
        %% se encuentre en la lista (si lo esta se avisa al proceso)
        %% Y luego se espera un unblock del proceso que se comunico
        %% O un take que agrega el nombre y socket a la lista
        {reserve,Name,Ret} ->
            case lists:member({Sock,Name},List) of
                true -> Ret ! {taken,Name},
                false -> ok
            end,
            receive
                {unblock,Name} ->
                    players_names(List);
                {take,Sock,Name} ->
                    [{Node,players} ! {unblock,Name} || Node <- Nodes],
                    players_names([{Socket,Name}|List])
            end;
        %% Al recibir get_name simplemente se devuelve, si esta, el
        %% nombre asociado con el socket argumento
        {get_name,Socket,Ret} ->
            case lists:member({Socket,Name},List) of
                true ->
                    Ret ! {players,Name,Socket};
                false ->
                    ok
            end
    end,
    players_names(List).




games_names(List) ->
    receive
        {get_games,Ret} ->
            %% Devuelve la lista del juegos en el nodo
            Ret ! {games,List};
        {new,Ret,Game} ->
            Game ! {games,get_dets},
            receive
                {ok,GameInfo} ->
                    Ret ! {games,ok,GameInfo},
                    games_names(game_to_id(Game,"nf")|List);
                {error,Info} ->
                    Ret ! {games,error,Info};
        {cstate,Pid} ->
            %% Cambia estado de "not full" a "full"
            NewList = lists:delete(game_to_id(PID,"nf"),List),
            games_names([game_to_id(PID,"f")|NewList])
        {del,Pid} ->
            %% Elimina el juego de la lista, lleno o no
            NewList = lists:delete(game_to_id(PID,"nf"),List),
            games_names(lists:delete(game_to_id(PID,"f"),NewList))
    end,
    games_manager(List).





game_to_id(PID,State) ->
    PIDS = pid_to_list(PID),
    NodeS = atom_to_list(node()),
    string:join([PIDS,NodeS,State],"+").


id_to_game(GID) ->
    case string:tokens(GID,"+") of
        [Node,Pid,State] -> {ok,{Node,Pid},State};
        _ -> error
    end.




