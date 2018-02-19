
-module(commands).
-compile(export_all).



pcommand(Cmd, PSocket, Updts, Control) ->
    {Res,Msg,CID} = case string:tokens(Cmd, " ") of
                    ["CON",ID,Name] ->
                        Control ! {add,Name,self()},
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
                        %% Contestaciones de envios UPD,
                        %% son redirigidos a updts_sender
                        Updts ! {ok,ID},
                        exit(normal);
                    ["BYE"] ->
                        %% Primero se obtienen las partidas
                        %% que se estan jugando y viendo
                        Control ! {qry,play,self()},
                        receive
                            Playing -> Playing
                        end,
                        Control ! {qry,watch,self()},
                        receive
                            Watching -> Watching
                        end,
                        %% A cada partida se envia el msj de abandono correspondiente
                        [gmsg ! {play,self(),GID,["LEAVE"],Control} || GID <- Playing],
                        [gmsg ! {unwatch,self(),GID,Control} || GID <- Watching],
                        %% Si el cliente registro un nombre, debe borrarse
                        %% del servicio de nombres (players)
                        Control ! {qry,name,self()},
                        receive
                            {ok,Name} ->
                                players ! {del,Name};
                            {error,_} ->
                                ok
                        end,
                        %% Se interrumpen los servicios que fueron creados
                        %% para este cliente especificamente
                        [Service ! stop || Service <- [PSocket,Updts,Control]],
                        exit(normal);
                    _ -> {error,"INVALID COMMAND","-1"}
                end,
    case Res of
        %% Ultimo formateo de respuestas, listas para enviar
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


%% Recibe la lista de juegos actuales de cada nodo en 'Nodes'
%% El protocolo es: Si algun nodo no esta respondiendo
%% (1s de espera), se comprueba que aun este conectado, de lo
%% contrario se continua con otros nodos que esten faltando
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
%% cliente, conoce su nombre registrado, redirige los mensajes
%% de update al proceso que se encarga de mandarlos, y sirve
%% como identificador para el cliente (se lo asocia con el
%% nombre elegido)
player_control(Playing, Watching, Updts, Name) ->
    receive
        {updt,Msg} ->
            Updts ! {updt,Msg};
        {add,play,GID} ->
            player_control([GID|Playing], Watching, Updts, Name);
        {add,watch,GID} ->
            player_control(Playing, [GID|Watching], Updts, Name);
        {del,play,GID} ->
            player_control(lists:delete(GID,Playing), Watching, Updts, Name);
        {del,watch,GID} ->
            player_control(Playing, lists:delete(GID,Watching), Updts, Name);
        {qry,play,Ret} ->
            Ret ! Playing;
        {qry,watch,Ret} ->
            Ret ! Watching;
        {add,NewName,Ret} ->
            case Name of
                undefined ->
                    players ! {add,NewName,self()},
                    receive
                        ok ->
                            Ret ! ok,
                            player_control(Playing, Watching, Updts, NewName);
                        {error,Msg} ->
                            Ret ! {error,Msg}
                    end;
                _ ->
                    Ret ! {error, "KNOWN NAME " ++ Name}
            end;
        {qry,name,Ret} ->
            case Name of
                undefined ->
                    Ret ! {error, "NO NAME"};
                _ -> 
                    Ret ! {ok,Name}
            end;
        stop ->
            exit(normal)
    end,
    player_control(Playing, Watching, Updts, Name).

