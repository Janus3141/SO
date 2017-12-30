
-module(commands).
-compile(export_all).



start_conn(Name,Sock) ->
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
        players ! {take,Sock,Name},
        {pcmd,"OK"}
    end.


list_games(ID) ->
    %% Se pregunta por los juegos a todos los nodos
    [{games,Node} ! {get_games,self()} || Node <- Nodes],
    %% Obtenemos una lista de listas de juegos
    [receive {games,List} -> List after 1000 end || _Node <- Nodes],
    %% Se aplana la lista y se lleva a una sola string donde los GID
    %% estan separados por '-'
    To_send = string:join([(lists:append(List))], "-"),
    {pcmd,lists:append(["OK ",ID," ",To_send])}.


new_game(ID,Sock) ->
    Game = spawn(?MODULE,ttt_start,[Sock]),
    games ! {new,self(),Game},
    receive
        {games,ok,GameInfo} -> 
            {pcmd,lists:append("OK ",ID," ",GameInfo)};
        {games,error,Info} ->
            {pcmd,lists:append("ERROR ",ID," ",Info)}
    end.


access_game(ID,GameID,Sock) ->
    case id_to_game(GameID) of
        {ok,Pid,"nf"} ->
            Pid ! {access,self(),Sock},
            receive
                {ok,GameInfo} ->
                    {pcmd,lists:append("OK ",ID," ",GameInfo)};
                {error,full} ->
                    {pcmd,lists:append("ERROR ",ID," fullgame")}
            end;
        {ok,Pid,_} ->
            {pcmd,lists:append("ERROR ",ID," invalid")};
        error ->
            {pcmd,lists:append("ERROR ",ID," invalid")}
    end.


play(ID,GameID,Play) ->
    case id_to_game(GameID) of
        {ok,Pid,_} ->
            Pid ! {play,self(),Play},
            receive
                {ok,GameInfo} ->
                    {pcmd,lists:append("OK ",ID," ",GameInfo)};
                {error,Detail} ->
                    {pcmd,lists:append("ERROR",ID," ",Detail)}
            end;
        error ->
            {pcmd,lists:append("ERROR ",ID," invalid")}
    end.


watch(ID,GameID,Socket) ->
    case id_to_game(GameID) of
        {ok,Pid,_} ->
            Pid ! {watch,self(),Socket};
            receive
                {games,ok} ->
                    {pcmd,lists:append("OK ",ID)};
                {games,error,Detail} ->
                    {pcmd,lists:append("ERROR ",ID," ",Detail)}
            end;
        error ->
            {pcmd,lists:append("ERROR ",ID," invalid")}
    end.
    

unwatch(ID,GameID,Socket) ->
    case id_to_game(GameID) of
        {ok,Pid,_} ->
            Pid ! {unwatch,self(),Socket};
            receive
                ok ->
                    {pcmd,lists:append("OK ",ID)};
        error ->
            {pcmd,lists:append("ERROR ",ID," invalid")}
    end.



