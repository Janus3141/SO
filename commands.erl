
-module(commands).
-compile(export_all).


pcommand(Cmd, PSocket, Updts, Control) ->
    {Res,Msg} = case string:tokens(Cmd, " ") of
                    ["CON",ID,Name] ->
                        gpp ! {register,Control,self()},
                        receive
                            {error,Msg} -> {error,Msg};
                            ok ->
                                players ! {add,Control,Name,self()},
                                receive
                                    ok -> {ok,""};
                                    {error,Msg} -> {error,Msg}
                                end
                        end;
                    ["LSG",ID] ->
                        %% Se pregunta por los juegos a todos los nodos
                        [{games,Node} ! {get_games,self()} || Node <- Nodes],
                        %% Obtenemos una lista de listas de juegos
                        Games = [receive GameList -> GameList after 1000 end || _Node <- Nodes],
                        %% Conversion a string de los GIDs obtenidos de todos los nodos
                        %% En la string final cada juego se separa por '-', y su GID se separa
                        %% de su estado con ','
                        To_send = string:join(Games,"-"),
                        {ok,To_send};
                    ["NEW",ID] ->
                        Game = spawn(tateti,ttt_phase1,[Control]),
                        games ! {new,self(),Game},
                        receive
                            {Answer,Info} -> {Answer,Info}
                        end;
                    ["ACC",ID,GID] ->
                        gmsg ! {access,self(),GameID,Control},
                        answer(GameID);
                    ["PLA",ID,GID|Play] ->
                        gmsg ! {play,self(),GID,Play,Control},
                        answer(GID);
                    ["OBS",ID,GID] -> 
                        gmsg ! {watch,self(),GameID,Control},
                        answer(GameID);
                    ["LEA",ID,GID] -> 
                        gmsg ! {unwatch,self(),GameID,Control},
                        answer(GameID);
                    ["OK",ID] ->
                        Updts ! {ok,ID};
                    ["BYE"] ->
                        Playing = Control ! {qry,play,self()},
                        Watching = Control ! {qry,watch,self()},
                        [gmsg ! {play,self(),GID,"LEAVE",Control} || GID <- Playing],
                        [gmsg ! {unwatch,self(),GameID,Control} || GID <- Watching],
                        [Service ! stop || Service <- [PSocket,Updts,Control]],
                        exit(normal)
                end,
    case Res of
        ok -> PSocket ! {pcmd, "OK " ++ ID ++ " " ++ Msg};
        error -> PSocket ! {pcmd, "ERROR " ++ ID ++ " " ++ Msg}
    end.




%% Espera una respuesta a un comando para un juego ya creado
%% Si la respuesta no llega en 1 segundo, se verifica que el
%% juego aun este disponible
answer(GID) ->
    receive
        {Answer,Info} -> {Answer,Info}
    after 1000 ->
        case services:conn_control(GID) of
            %% El juego esta disponible, se sigue esperando
            true -> answer(GID);
            %% El juego no esta disponible, se devuelve error
            false -> {error,"CONNECTION ERROR"};
            %% Mensaje de error de conn_control
            %% Probablemente GID sea incorrecto
            {error,Msg} -> {error,Msg}
        end
    end.




%% Lleva las listas de los juegos jugados y observados por el
%% cliente, redirige los mensajes de update al proceso que
%% se encarga de mandarlos, y sirve como identificador para
%% el cliente (se lo asocia con el nombre elegido)

player_control(Playing,Watching,Updts) ->
    receive
        {updt,Msg} ->
            Updts ! {updt,Msg};
        {add,play,GID} ->
            player_control([GID|Playing], Watching, Updts);
        {add,watch,GID} ->
            player_control(Playing, [GID|Watching], Updts);
        {del,play,GID} ->
            player_control(lists:delete(GID,Playing), Watching, Updts);
        {del,watch,GID} ->
            player_control(Playing, lists:delete(GID,Watching), Updts);
        {qry,play,Ret} ->
            Ret ! Playing;
        {qry,watch,Ret} ->
            Ret ! Watching;
        stop ->
            exit(normal)
    end,
    player_control(Playing, Watching, Updts).

