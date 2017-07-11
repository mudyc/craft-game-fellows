% I hate all database issues but I just need to work with these

-module(db).
-compile(export_all).

init_database() ->
   sqlite3:open(users),
   sqlite3:create_table(users, users, [{id, integer, [primary_key]},
                                              {username, text},
                                              {hash, text},
                                              {email, text},
                                              {age, integer},
                                              {created, integer}
                                              ]),
   sqlite3:create_table(users, sessions, [{id, text, [primary_key]},
                                              {user_id, integer},
                                              {lastlogin, integer}
                                              ]),
   sqlite3:close(users).
   
has_session(SessionId) ->
    sqlite3:open(users),
    [{columns, Columns}, {rows, Rows}] = sqlite3:read(users, sessions, {id, SessionId}),
    sqlite3:close(users),
    case length(Rows) of
      1 -> true;
      _ -> false
    end.