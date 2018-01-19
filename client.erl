
-module(client).
-compile(export_all).


start(IP,Port) ->
    try
        case gen_tcp:connect(IP, Port, [list,inet,{active,false},{nodelay,true}]) of
            {ok, Sock} -> 
                io:format("Conexion establecida~n"),
                Conn = spawn(?MODULE, connection_manager, [Sock]),
                register(connection, Conn),
                gen_tcp:controlling_process(Sock,Conn),
                register(cmds_send, spawn(?MODULE,cmds_sender,[1,Sock])),
                register(updt_recv, spawn(?MODULE,updates_recv,[])),
                register(games, spawn(?MODULE, games_ids, [[]])),
                register(reader, self()),
                read_command();
            {error, Reason} ->
                io:format("Error: " ++ atom_to_list(Reason) ++ "~n"),
                exit(normal)
        end
    catch
        error:R -> io:format("Error in starting function:~p~n",[R])
    end.


connection_manager(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Msg} ->
            %% Puede darse el caso de recibir mas de un mensaje al
            %% mismo tiempo, por lo cual primero se los separa por
            %% su ; final, y luego se los procesa en orden
            MsgList = string:tokens(Msg, ";"),
            lists:map(fun(M) -> process_msg(Socket,M) end, MsgList);
        {tcp_closed, Socket} ->
            io:format("Conexion cerrada~n"),
            exit(normal)
    end,
    connection_manager(Socket). 


process_msg(Sock, Msg) ->
    io:format(Msg++"~n"),
    case string:tokens(Msg, " ") of
        ["UPD",ID|Tail] ->
            gen_tcp:send(Sock, "OK "++ID),
            updt_recv ! {upd,Tail};
        ["OK",ID|Tail] ->
            cmds_send ! {ok,ID,Tail};
        ["ERROR",ID|Tail] ->
            cmds_send ! {error,ID,Tail}
    end.


read_command() ->
    Cmd = io:get_line(">>> "),
    cmds_send ! {cmd, string:strip(Cmd,right,$\n)},
    receive
        ok -> read_command();
        stop -> exit(normal)
    end.


cmds_sender(IntID, Sock) ->
    receive
        {cmd, Cmd} ->
            StrID = integer_to_list(IntID),
            case string:tokens(Cmd, " ") of
                ["nombre",Name] ->
                    gen_tcp:send(Sock,"CON "++StrID++" "++Name),
                    receive
                        {ok,StrID,_} ->
                            io:format(Name ++ " ha sido registrado correctamente~n");
                        {error,StrID,["NAME","TAKEN"]} ->
                            io:format("Error: nombre no disponible~n");
                        {error,StrID,["KNOWN","NAME",Known]} ->
                            io:format("Su nombre es " ++ Known ++ "~n");
                        {error,StrID,Unex} ->
                            Reformat = string:join(Unex," "),
                            io:format("Error inesperado: " ++ Reformat ++ "~n")
                    end;
                ["crear"] ->
                    gen_tcp:send(Sock,"NEW " ++ StrID),
                    receive
                        {ok,StrID,[GID,Name]} ->
                            io:format("Juego creado con ID "++GID),
                            updt_recv ! {new,GID,Name},
                            reader ! ok,
                            ingame_cmds(IntID+1, Sock, GID, false);
                        {error,StrID,"NO NAME"} ->
                            io:format("No puede jugar sin elegir un nombre (use 'ayuda')~n");
                        {error,StrID,Unex} ->
                            Reformat = string:join(Unex," "),
                            io:format("Mensaje inesperado: "++Reformat++"~n")
                    end;
                ["juegos"] ->
                    gen_tcp:send(Sock,"LSG " ++ StrID),
                    receive
                        {ok,StrID,[Games]} ->
                            games ! {upd, Games};
                        {ok,StrID,[]} ->
                            io:format("No hay juegos~n");
                        {error,StrID,Unex} ->
                            Reformat = string:join(Unex," "),
                            io:format("Mensaje inesperado: " ++ Reformat ++ "~n")
                    end;
                ["entrar",ID] ->
                    games ! {get, ID, self()},
                    receive
                        {ok, GID, St} ->
                            case St of
                                "full" -> 
                                    io:format("Partida llena~n"),
                                    cmds_sender(IntID, Sock);
                                _ -> ok
                            end,
                            gen_tcp:send(Sock,"ACC "++StrID++" "++GID),
                            receive
                                {ok,StrID,[NameP1,NameP2,Turn]} ->
                                    updt_recv ! {acc,GID,NameP1,NameP2,Turn},
                                    reader ! ok,
                                    ingame_cmds(IntID+1, Sock, GID, false);
                                {error,StrID,["FULL"]} ->
                                    io:format("Partida llena~n");
                                {error,StrID,["INVALID","GID"]} ->
                                    io:format("Partida terminada~n");
                                {error,StrID,["CONNECTION","ERROR"]} ->
                                    io:format("La partida no esta disponible~n");
                                {error,StrID,Unex} ->
                                    Reformat = string:join(Unex, " "),
                                    io:format("Mensaje inesperado: "++Reformat++"~n")
                            end;
                        error ->
                            io:format("ID de partida incorrecto~n")
                    end;
                ["observar",ID] ->
                    games ! {get, ID, self()},
                    receive
                        {ok, GID, _} ->
                            gen_tcp:send(Sock,"OBS "++StrID++" "++GID),
                            receive
                                {ok,StrID,[NameP1,NameP2]} ->
                                    updt_recv ! {watch,GID,NameP1,NameP2},
                                    reader ! ok,
                                    ingame_cmds(IntID+1, Sock, GID, true);
                                {error,StrID,["INVALID","GID"]} ->
                                    io:format("Partida terminada~n");
                                {error,StrID,["CONNECTION","ERROR"]} ->
                                    io:format("Partida no disponible~n");
                                {error,StrID,Unex} ->
                                    Reformat = string:join(Unex, " "),
                                    io:format("Mensaje inesperado: "++Reformat++"~n")
                            end;
                        error ->
                            io:format("ID de partida incorrecto~n")
                    end;
                ["salir"] ->
                    gen_tcp:send(Sock,"BYE"),
                    reader ! stop,
                    io:format("Juego terminado~n"),
                    exit(normal);
                ["ayuda"] ->
                    print_help();
                _ ->
                    io:format("Comando invalido~n")
            end
    end,
    io:format("~n"),
    reader ! ok,
    cmds_sender(IntID + 1, Sock).


ingame_cmds(IntID, Sock, GID, Just_watch) ->
    receive
        {cmd, Cmd} ->
            StrID = integer_to_list(IntID),
            case string:tokens(Cmd, " ") of
                ["jugar",Play] ->
                    case Just_watch of
                        true ->
                            io:format("Comando invalido~n");
                        false ->
                            gen_tcp:send(Sock,"PLA "++StrID++" "++GID++" "++Play),
                            receive
                                {ok,StrID,[GID]} ->
                                    updt_recv ! {play,ok,Play};
                                {error,StrID,["CONNECTION","ERROR"]} ->
                                    io:format("Error en la conexion~n"),
                                    leave_ingame(IntID+1, Sock);
                                {error,StrID,Info} ->
                                    updt_recv ! {play,error,Info}
                            end
                    end;
                ["abandonar"] ->
                    case Just_watch of
                        true ->
                            gen_tcp:send(Sock,"LEA "++StrID++" "++GID),
                            receive
                                {ok,StrID,_} ->
                                    leave_ingame(IntID+1, Sock);
                                {error,StrID,["CONNECTION","ERROR"]} ->
                                    leave_ingame(IntID+1, Sock);
                                {error,StrID,Unex} ->
                                    Reformat = string:join(Unex," "),
                                    io:format("Error inesperado: "++Reformat++"~n"),
                                    exit(normal)
                            end;
                        false ->
                            gen_tcp:send(Sock,"PLA "++StrID++" "++GID++" LEAVE"),
                            receive
                                {ok,StrID,[GID,"LEFT"]} ->
                                    leave_ingame(IntID+1, Sock);
                                {error,StrID,["CONNECTION","ERROR"]} ->
                                    leave_ingame(IntID+1, Sock);
                                {error,StrID,Unex} ->
                                    Reformat = string:join(Unex," "),
                                    io:format("Error inesperado: "++Reformat++"~n"),
                                    exit(normal)
                            end
                    end;
                ["ayuda"] ->
                    print_help();
                ["salir"] ->
                    gen_tcp:send(Sock,"BYE"),
                    reader ! stop,
                    io:format("Juego terminado"),
                    exit(normal);
                _ ->
                    io:format("Comando invalido~n")
            end;
        finish ->
            cmds_sender(IntID, Sock)
    end,
    reader ! ok,
    ingame_cmds(IntID+1, Sock, GID, Just_watch).


leave_ingame(IntID, Socket) ->
    updt_recv ! leave,
    reader ! ok,
    cmds_sender(IntID, Socket).


print_help() ->
    io:format("Comandos que puede utilizar:~n"),
    io:format("- nombre <Nombre>: Toma <Nombre> como alias en el servidor. Sin tomar un nombre, puede observar pero no jugar.~n"),
    io:format("- crear: Crea un nuevo juego.~n"),
    io:format("- juegos: Devuelve la lista de juegos disponibles, junto con un ID cada uno que se debera usar para acceder a uno.~n"),
    io:format("- entrar <ID>: Para entrar a la partida identificada por <ID>.~n"),
    io:format("- observar <ID>: Para observar la partida identificada por <ID>.~n"),
    io:format("- jugar <Nro>: Juega la casilla <Nro>. Las casillas se numeran del 1 al 9, de izq. a dcha., de arriba a abajo.~n"),
    io:format("- abandonar: Abandona la partida que se esta jugando/observando.~n"),
    io:format("- salir: Abandonar el juego.~n").


games_ids(List) ->
    receive
        {upd, Games} ->
            Games2 = [string:tokens(Game,",") || Game <- string:tokens(Games,"-")],
            Lamb = fun(ID,[GID,St]) -> {ID,GID,St} end,
            Games3 = lists:zipwith(Lamb, lists:seq(1,length(Games2)), Games2),
            print_games(Games3),
            games_ids(Games3);
        {get, ID, Ret} ->
            case lists:keyfind(list_to_integer(ID),1,List) of
                {_, GID, St} -> Ret ! {ok, GID, St};
                false -> Ret ! error
            end
    end,
    games_ids(List).


print_games([]) ->
    io:format("~n");
print_games([{ID,GID,St}|Tl]) ->
    case St of
        "full" ->
            io:format(integer_to_list(ID) ++ " - " ++ GID ++ " - lleno~n");
        "notfull" ->
            io:format(integer_to_list(ID) ++ " - " ++ GID ++ "~n")
    end,
    print_games(Tl).


updates_recv() ->
    receive
        {new,GID,NP1} ->
            io:format("Esperando a otro jugador~n"),
            receive
                {upd,[GID,"ACCEPTED",NP2,Turn]} -> 
                    io:format("Partida aceptada, comienza el juego~n"),
                    case Turn of
                        "1" -> recv_play(GID,NP1,NP2,false,false);
                        "2" -> recv_play(GID,NP1,NP2,false,Turn)
                    end
            end;
        {acc,GID,NP1,NP2,Turn} ->
            case Turn of
                "1" -> recv_play(GID,NP1,NP2,false,Turn);
                "2" -> recv_play(GID,NP1,NP2,false,false)
            end;
        {watch,GID,NP1,NP2} ->
            recv_play(GID,NP1,NP2,true,false);
        _ ->
            io:format("Comando invalido~n"),
            updates_recv()
    end.


recv_play(GID, NameP1, NameP2, Just_watch, Turn) ->
    case Turn of
        "1" -> io:format("Turno de "++NameP1++"~n");
        "2" -> io:format("Turno de "++NameP2++"~n");
        false -> ok
    end,
    receive
        leave ->
            io:format("Juego abandonado~n"),
            updates_recv();
        {upd,[GID,"END","DRAW"]} ->
            io:format("Empate!~n"),
            cmds_send ! finish,
            updates_recv();
        {upd,[GID,Table,NextTurn]} ->
            print_table(Table),
            case Just_watch of
                true -> recv_play(GID,NameP1,NameP2,true,NextTurn);
                false -> self_play(GID,NameP1,NameP2,Table,NextTurn)
            end;

        {upd,[GID,"END",Winner,"GAMELEFT"]} ->
            case Winner =:= NameP1 of
                true -> io:format(Winner++" ha ganado! "++NameP2++" abandono el juego.~n");
                false -> io:format(Winner++" ha ganado! "++NameP1++" abandono el juego.~n")
            end,
            cmds_send ! finish,
            updates_recv();
        {upd,[GID,"END",Winner,Hit]} ->
            case Winner of
                "1" -> 
                    io:format(NameP1 ++ " ha ganado!~n"),
                    print_hit(Hit,1);
                "2" -> 
                    io:format(NameP2 ++ " ha ganado!~n"),
                    print_hit(Hit,2)
            end,
            cmds_send ! finish,
            updates_recv()
    end.


self_play(GID, NameP1, NameP2, Last_table, Turn) ->
    case Turn of
        "1" ->
            io:format("Turno de "++NameP1++"~n"),
            NextTurn = "2";
        "2" ->
            io:format("Turno de "++NameP2++"~n"),
            NextTurn = "1"
    end,
    receive
        {play,ok,Play} ->
            {Init,[_|Tail]} = lists:split(list_to_integer(Play)-1,Last_table),
            New_table = Init ++ Turn ++ Tail,
            print_table(New_table);
        {play,error,[GID,"INVALID","GID"]} ->
            io:format("Error: partida inexistente~n"),
            cmds_send ! finish,
            updates_recv();
        {play,error,[GID,"INVALID"]} ->
            io:format("Jugada invalida, turno perdido~n");
        {play,error,Unex} ->
            Reformat = string:join(Unex, " "),
            io:format("Mensaje inesperado: "++Reformat++"~n");
        leave ->
            io:format("Juego abandonado~n"),
            updates_recv()
    end,
    recv_play(GID,NameP1,NameP2,false,NextTurn).


get_symbol(N) ->
    case N of
        $0 -> $-;
        $1 -> $X;
        $2 -> $O
    end.


print_table(Table) ->
    Row1 = lists:sublist(Table,3),
    Row2 = lists:sublist(Table,4,3),
    Row3 = lists:sublist(Table,7,3),
    io:format((lists:map(fun get_symbol/1,Row1))++"~n"),
    io:format((lists:map(fun get_symbol/1,Row2))++"~n"),
    io:format((lists:map(fun get_symbol/1,Row3))++"~n"),
    io:nl().


print_hit(Hit,Who) ->
    Table = [case lists:member(X+48,Hit) of true -> Who+48; false -> $0 end || X <- lists:seq(1,9)],
    print_table(Table).




