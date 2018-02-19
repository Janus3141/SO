
-module(tateti).
-compile(export_all).


%% Creacion de la partida. Responde a games con el nombre del
%% jugador que la creo y se sigue con phase 2
ttt_phase1(IdProcP1) ->
    IdProcP1 ! {qry,name,self()},
    receive
        {ok, NameP1} ->
            receive
                {get_dets, GID} ->
                    IdProcP1 ! {add, play, GID},
                    games ! {ok, NameP1},
                    ttt_phase2(IdProcP1,NameP1,GID);
                error ->
                    error
            end;
        {error, Msg} ->
            games ! {error, Msg}
    end.


%% Espera a que un jugador acceda a la partida creada
%% Si todo va bien se entra en el juego (phase 3)
%% Si el jugador que responde no tiene nombre,
%% se lo rechaza y se vuelve a esperar a otro jugador
ttt_phase2(IdProcP1, NameP1, GID) ->
    receive
       {access, Ret, IdProcP2} ->
            IdProcP2 ! {qry, name, self()},
            receive
                {ok, NameP2} ->
                    IdProcP2 ! {add, play, GID},
                    Turn = random:uniform(2), % Primer turno
                    TurnS = integer_to_list(Turn),
                    Ret ! {ok, NameP1 ++ " " ++ NameP2 ++ " " ++ TurnS}, % Contestacion al jugador que entra
                    IdProcP1 ! {updt, GID ++ " ACCEPTED " ++ NameP2 ++ " " ++ TurnS}, % Aviso de comienzo al jugador que crea
                    games ! {cstate, GID, full}, % Cambia el estado del juego a lleno
                    Tab = [0,0,0,0,0,0,0,0,0], % Tablero de juego vacio
                    ttt_phase3([IdProcP1, IdProcP2], [NameP1, NameP2],
                               Tab, Turn, [], GID, 1);
                {error, Msg} ->
                    Ret ! {error, Msg},
                    ttt_phase2(IdProcP1, NameP1, GID)
            end
    end.


%% Ayuda para recibir solo mensajes que sean watch y unwatch
%% Se retorna la lista de los nuevos observadores y la de 
%% los que dejan de observar
watch_list(Wtcs, Out, Info, GID) ->
    receive
        {watch, Ret, IdProc} ->
            Ret ! {ok, Info},
            IdProc ! {add, watch, GID},
            watch_list([IdProc|Wtcs], Out, Info, GID);
        {unwatch, Ret, IdProc} ->
            Ret ! {ok,""},
            IdProc ! {del,watch,GID},
            watch_list(Wtcs, [IdProc|Out], Info, GID)
    after 0 ->
        {Wtcs, Out}
    end.


%% Broadcast de Msg para todo elemento en Receivers
updt_bcast(Msg, Receivers) ->
    [Client ! {updt,Msg} || Client <- Receivers].


%% Realiza la jugada argumento en Table y devuelve el nuevo estado
%% del tablero, si la jugada no es "LEAVE", y si es valida
ttt_play(Table, [Play], T) ->
    try
        %% Asegurarse que la jugada sea un numero
        N = list_to_integer(Play),
        %% Verificar que los numeros son correctos, y que el lugar no
        %% fue jugado antes
        0 = lists:nth(N, Table),
        %% Se devuelve el tablero con el N-esimo numero T
        lists:sublist(Table, N-1) ++ [T] ++ lists:nthtail(N, Table)
    catch
        %% Error de list_to_integer
        error:badarg -> if
                         Play =:= "LEAVE" -> leave;
                         true -> badargs
                        end;
        %% Errores de lists:nth
        error:function_clause -> badargs;
        error:{badmatch,_} -> badargs
    end.


%% Chequea si la ultima jugada formo una linea de 3
%% Si lo hizo devuelve win y la linea ganadora, si no ok
ttt_test_table([A1,A2,A3,B1,B2,B3,C1,C2,C3], T) ->
    if
        (A1 =:= T) and (A2 =:= T) and (A3 =:= T) -> {win,[1,2,3]};
        (B1 =:= T) and (B2 =:= T) and (B3 =:= T) -> {win,[4,5,6]};
        (C1 =:= T) and (C2 =:= T) and (C3 =:= T) -> {win,[7,8,9]};
        (A1 =:= T) and (B1 =:= T) and (C1 =:= T) -> {win,[1,4,7]};
        (A2 =:= T) and (B2 =:= T) and (C2 =:= T) -> {win,[2,5,8]};
        (A3 =:= T) and (B3 =:= T) and (C3 =:= T) -> {win,[3,6,9]};
        (A1 =:= T) and (B2 =:= T) and (C3 =:= T) -> {win,[1,5,9]};
        (A3 =:= T) and (B2 =:= T) and (C1 =:= T) -> {win,[3,5,7]};
        true -> ok
    end.


%% El juego en si
ttt_phase3(IdProcs,Names,Table,Turn,Wtcs,GID,Count) ->
    OtherPlayer = (Turn rem 2) + 1,
    %% Recibir pedidos de (des)observacion
    Info = string:join(Names, " "),
    {InWtcs,OutWtcs} = watch_list([],[],Info,GID),
    NewWtcs = InWtcs ++ (Wtcs -- OutWtcs),
    %% Mandar updt a todos menos el jugador que hizo la ultima jugada
    TableStr = lists:append([integer_to_list(X) || X <- Table]),
    Info1 = GID ++ " " ++ TableStr ++ " " ++ integer_to_list(Turn),
    updt_bcast(Info1, [lists:nth(Turn,IdProcs) | NewWtcs]),
    %% Recibir y realizar jugada
    PlayerID = lists:nth(Turn,IdProcs),
    receive
        {play,Ret,Play,PlayerID} ->
            case ttt_play(Table,Play,Turn) of
                leave ->
                    %% El jugador se retira, se termina el juego
                    Ret ! {ok, GID ++ " LEFT"},
                    ttt_end(OtherPlayer, [lists:nth(OtherPlayer,IdProcs)], NewWtcs, GID, leave);
                badargs ->
                    %% Jugada invalida, se pierde el turno
                    Ret ! {error, GID ++ " INVALID"},
                    ttt_phase3(IdProcs,Names,Table,OtherPlayer,NewWtcs,GID,Count);
                NewTable ->
                    Ret ! {ok, GID},
                    %% Comprobar tabla
                    case ttt_test_table(NewTable, Turn) of
                        ok ->
                            %% Si no hay mas casillas para rellenar, hay empate
                            case Count of
                                9 -> ttt_end(false,IdProcs,NewWtcs,GID,draw);
                                _ -> ttt_phase3(IdProcs,Names,NewTable,OtherPlayer,NewWtcs,GID,Count+1)
                            end;
                        {win,Hit} -> ttt_end(Turn,IdProcs,NewWtcs,GID,Hit)
                    end
            end
    end.


%% Terminacion del juego
%% Se manda un ultimo update a los jugadores y observadores informando
%% quien gano y por que
ttt_end(WinnerN, Players, Wtcs, GID, Reason) ->
    %% Avisa a games que borre el juego de la lista del servidor
    games ! {del,GID},
    %% Avisa a los procesos control de los jugadores
    %% que borren la partida de sus listas
    [Player ! {del,play,GID} || Player <- Players],
    [Watcher ! {del,watch,GID} || Watcher <- Wtcs],
    %% Lista de clientes a los que se envia ultimo mensaje de la partida
    InfRecs = Players ++ Wtcs,
    case WinnerN of
        %% Si hubo un empate, WinnerN es false
        false -> WinnerS = "";
        %% Sino es el numero de jugador
        _ -> WinnerS = integer_to_list(WinnerN)
    end,
    case Reason of
        draw ->
            updt_bcast(GID ++ " END DRAW", InfRecs);
        leave ->
            updt_bcast(GID ++ " END " ++  WinnerS ++ " GAMELEFT", InfRecs);
        Hit ->
            HitS = lists:append([integer_to_list(X) || X <- Hit]),
            updt_bcast(GID ++ " END " ++  WinnerS ++ " " ++ HitS, InfRecs)
    end.



