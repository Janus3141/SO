

name_by_sock(Sock) ->
    [{players,Node} ! {get_name,Sock,self()} || Node <- [node()|nodes()]],
    receive
        {players,Name,Sock} -> {ok,Sock,Name}
    after 1000 ->
        receive
            {games,get_dets} -> {error,"notfound"}
        end
    end.


ttt_start(Sock) ->
    case name_by_sock(Sock) of
        {ok,Sock,Name} -> ok
        {error,R} -> games ! {error,R}
    end,
    





%% Para representar un tablero de ta-te-ti se puede usar una tupla de
%% nueve numeros para representar cada una de las casillas
ttt_play(Table,Play,T) ->
    try
        %% Asegurarse que la jugada sea un numero
        N = list_to_integer(Play),
        %% Verificar que los numeros son correctos, y que el lugar no
        %% fue jugado antes
        0 = element(N,Table),
        %% Se devuelve el tablero con el N-esimo numero T
        setelement(N,Table,T)
    catch
        E:R -> {error,badargs}
    end.
