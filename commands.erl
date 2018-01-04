
-module(commands).
-compile(export_all).



start_conn(Name,Updts) ->
    %% Pedir a los procesos de nombres que reserven el nombre
    %% Asi una nueva coneccion no podra tratar de reservar el
    %% mismo nombre
    [{players,Node} ! {reserve,Name,self()} || Node <- Nodes],
    receive
        {taken,Name} ->
            %% Si el nombre ya estaba tomado, se retira la reserva
            %% Y se devuelve ERROR
            [{players,Node} ! {unblock,Name} || Node <- Nodes],
            {pcmd,"ERROR"}
    after 1000 ->
        %% Si ningun proceso reclama el nombre como tomado en
        %% 1 segundo, se hace efectiva la toma
        players ! {take,Updts,Name},
        {pcmd,"OK"}
    end.


list_games(ID) ->
    %% Se pregunta por los juegos a todos los nodos
    [{games,Node} ! {get_games,self()} || Node <- Nodes],
    %% Obtenemos una lista de listas de juegos
    Games = [receive GameList -> GameList after 1000 end || _Node <- Nodes],
    %% Conversion a string de los GIDs obtenidos de todos los nodos
    %% En la string final cada juego se separa por '-', y su GID se separa
    %% de su estado con ','
    To_send = string:join(Games,"-"),
    {pcmd,"OK " ++ ID ++ " " ++ To_send}.


new_game(ID,Updts) ->
    Game = spawn(tateti,ttt_phase1,[Updts]),
    games ! {new,self(),Game},
    receive
        {ok,GameInfo} -> 
            {pcmd, "OK " ++ ID ++ " " ++ GameInfo};
        {error,Info} ->
            {pcmd, "ERROR " ++ ID ++ " " ++ Info}
    end.


access_game(ID,GameID,Updts) ->
    games ! {access,self(),GameID,Updts},
    receive
        {ok,GameInfo} ->
            {pcmd, "OK " ++ ID ++ " " ++ GameInfo};
        {error,R} ->
            {pcmd, "ERROR " ++ ID ++ " " ++ R}
    end.


play(ID,GameID,Play,Updts) ->
    games ! {play,self(),GameID,Play,Updts},
    receive
        ok ->
            {pcmd, "OK " ++ ID};
        {error,Detail} ->
            {pcmd, "ERROR " ++ ID ++ " " ++ Detail}
    end.


watch(ID,GameID,Updts) ->
    games ! {watch,self(),GameID,Updts},
    receive
        {ok,GameInfo} ->
            {pcmd, "OK " ++ ID ++ " " ++ GameInfo};
        {games,error,Detail} ->
            {pcmd, "ERROR " ++ ID ++ " " ++ Detail}
    end.
    

unwatch(ID,GameID,Uptds) ->
    games ! {unwatch,self(),GameID,Updts},
    receive
        ok ->
            {pcmd, "OK " ++ ID};
        {error,Detail} ->
            {pcmd, "ERROR " ++ ID ++ " " ++ Detail}
    end.



