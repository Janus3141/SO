
-module(commands).
-compile(export_all).



pcommand(Cmd, PSocket, Updts, Control) ->
    {Res,Msg,CID} = case string:tokens(Cmd, " ") of
                    ["CON",ID,Name] ->
                        players ! {add,Control,Name,self()},
                        receive
                            ok -> {ok,"",ID};
                            {error,Reason} -> {error,Reason,ID}
                        end;
                    ["LSG",ID] ->
                        Nodes = [node() | nodes()],
                        %% Se pregunta por los juegos a todos los nodos
                        [{games,Node} ! {get_games,self()} || Node <- Nodes],
                        %% Obtenemos una lista de listas de juegos
                        Games = receive_games(Nodes,[]),
                        %% Conversion a string de los GIDs obtenidos de todos los nodos
                        %% En la string final cada juego se separa por '-',
                        %% y su GID se separa de su estado con ','
                        To_send = string:join(Games,"-"),
                        {ok,To_send,ID};
                    ["NEW",ID] ->
                        Game = spawn(tateti,ttt_phase1,[Control]),
                        games ! {new,self(),Game},
                        receive
                            {Answer,Info} -> {Answer,Info,ID}
                        end;
                    ["ACC",ID,GID] ->
                        gmsg ! {access,self(),GID,Control},
                        {Resp,Msge} = answer(GID),
                        {Resp,Msge,ID};
                    ["PLA",ID,GID|Play] ->
                        gmsg ! {play,self(),GID,Play,Control},
                        {Resp,Msge} = answer(GID),
                        {Resp,Msge,ID};
                    ["OBS",ID,GID] -> 
                        gmsg ! {watch,self(),GID,Control},
                        {Resp,Msge} = answer(GID),
                        {Resp,Msge,ID};
                    ["LEA",ID,GID] -> 
                        gmsg ! {unwatch,self(),GID,Control},
                        {Resp,Msge} = answer(GID),
                        {Resp,Msge,ID};
                    ["OK",ID] ->
                        Updts ! {ok,ID},
                        exit(normal);
                    ["BYE"] ->
                        Control ! {qry,play,self()},
                        receive
                            Playing -> Playing
                        end,
                        Control ! {qry,watch,self()},
                        receive
                            Watching -> Watching
                        end,
                        [gmsg ! {play,self(),GID,["LEAVE"],Control} || GID <- Playing],
                        [gmsg ! {unwatch,self(),GID,Control} || GID <- Watching],
                        [Service ! stop || Service <- [PSocket,Updts,Control]],
                        exit(normal);
                    _ -> {error,"INVALID COMMAND","-1"}
                end,
    case Res of
        ok -> PSocket ! {pcmd, "OK " ++ CID ++ " " ++ Msg};
        error -> PSocket ! {pcmd, "ERROR " ++ CID ++ " " ++ Msg}
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



receive_games([], Games) -> Games;
receive_games(Nodes, Games) ->
    receive
        {Node, GList} ->
            receive_games(lists:delete(Node,Nodes),GList++Games)
    after 1000 ->
        %% Si no se reciben todas las respuestas, se controla
        %% que los nodos sigan conectados
        [Node | Nodes2] = Nodes,
        case lists:member(Node, [node() | nodes()]) of
            %% El nodo no esta conectado. Lo ignoramos
            %% y seguimos con los que quedan
            false ->
                receive_games(Nodes2,Games);
            %% El nodo esta conectado, seguimos tratando
            true ->
                receive_games(Nodes,Games)
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

