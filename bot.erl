

-module(bot).
-compile(export_all).


bot_play(Sock,GID,Place) ->
    PlaceS = integer_to_list(Place),
    receive
        {tcp,Sock,Upd} -> case string:tokens(Upd," ") of
                            ["UPD",ID,GID,Table,Turn] ->
                                gen_tcp:send(Sock,"OK "++ID),
                                io:format(Table++" "++Turn++"~n");
                            ["UPD",_,GID|Rest] ->
                                io:format(string:join(Rest," ")++"~n"),
                                exit(normal)
                          end
    end,
    gen_tcp:send(Sock,"PLA "++ PlaceS ++ " " ++ GID ++ " " ++ PlaceS),
    receive
        {tcp,Sock,Conf} -> io:format(Conf++"~n")
    end,
    bot_play(Sock,GID,Place+1).


bot(Name) ->
    {ok,Sock} = gen_tcp:connect("127.0.0.1",8000,[]),
    gen_tcp:send(Sock,"CON 1 " ++ Name),
    receive
        {tcp,Sock,Msg1} -> io:format(Msg1 ++ "~n")
    end,
    gen_tcp:send(Sock,"NEW 2"),
    receive
        {tcp,Sock,Msg2} -> io:format(Msg2 ++ "~n")
    end,
    case string:tokens(Msg2," ") of
        ["OK","2",GID,_] ->
            gen_tcp:send(Sock,"ACC 3 " ++ GID),
            WillReceive = "OK 3 " ++ Name ++ " " ++ Name,
            receive
                {tcp,Sock,WillReceive} -> io:format(WillReceive ++ "~n")
            end,
            AlsoReceive = "UPD 1 " ++ GID ++ " ACCEPTED " ++ Name,
            receive
                {tcp,Sock,AlsoReceive} -> io:format(AlsoReceive ++ "~n")
            end,
            gen_tcp:send(Sock,"OK 1"),
            bot_play(Sock,GID,1);
        ["ERROR","2"|_] -> exit(normal)
    end.
    

