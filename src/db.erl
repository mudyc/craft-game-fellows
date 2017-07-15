% I hate all database issues but I just need to work with these

-module(db).
-compile(export_all).

init_database() ->
   sqlite3:open(users),
   sqlite3:create_table(users, user, [{id, integer, [primary_key]},
                                          {username, text, [unique]},
                                          {hash, text},
                                          {email, text},
                                          {age, text},
                                          {sex, text},
                                          {created, integer}
                                          ]),
   sqlite3:create_table(users, session, [{id, integer, [primary_key]},
                                          {session_id, text},
                                          {user_id, integer},
                                          {created, integer}
                                          ]),
   sqlite3:close(users).
   
has_session(SessionId) ->
    sqlite3:open(users),
    [{columns, Cols}, {rows, Rows}] = sqlite3:read(users, session, {session_id, SessionId}),
    sqlite3:close(users),
    case Rows of
      [] -> {false, -1};
       _ -> 
            Combined = lists:zip(Cols, tuple_to_list(lists:nth(1,Rows))),
            {true, proplists:get_value("user_id", Combined)}
    end.

get_username(UserId) ->
    sqlite3:open(users),
    [{columns, Cols}, {rows, Rows}] = sqlite3:read(users, user, {id, UserId}),
    sqlite3:close(users),
    Combined = lists:zip(Cols, tuple_to_list(lists:nth(1,Rows))),
    proplists:get_value("username", Combined).
    

is_username_reserved(Username) ->
    sqlite3:open(users),
    [{columns, Columns}, {rows, Rows}] = sqlite3:read(users, user, {username, Username}),
    sqlite3:close(users),
    case Rows of
      [] -> false;
       _ -> true
    end.

register_fellow(Username, Password, Age, Sex, Email) ->
    sqlite3:open(users),
    TimeStamp = os:system_time(seconds),
    Hash = hash(Password, TimeStamp),
    {rowid, Id1} = sqlite3:write(users, user, [
                                              {username, Username},
                                              {hash, Hash},
                                              {email, Email},
                                              {age, Age},
                                              {sex, Sex},
                                              {created, TimeStamp}
                                              ]),
    sqlite3:close(users).

hash(Password, TimeStamp) ->
    % Adding the timestamp to hash function might be bad idea since attacker could
    % use that knowledge since people join at certain time range.
    % Better might be to give random bits to every user but this service is not bank.
    % If attacker get database access we are screwed in any case.
    
    BinTimeStamp = list_to_binary(integer_to_list(TimeStamp)),
    api_auth:to_hex(crypto:hash_final(crypto:hash_update(crypto:hash_update(
               crypto:hash_init(sha224), Password), BinTimeStamp))).


% Creates new session id if login succesful. Usually login not needed since 
% session exists and can exists on different machines.
login(Username, Password, FuncSessionId) ->
    sqlite3:open(users),
    [{columns, Cols}, {rows, Rows}] = sqlite3:read(users, user, {username, Username}),
    case Rows of
      [] -> none;
       _ ->
        Combined = lists:zip(Cols, tuple_to_list(lists:nth(1,Rows))),
        TimeStamp = proplists:get_value("created", Combined),
        B = proplists:get_value("hash", Combined),
        A = list_to_binary(hash(Password, TimeStamp)),
        if
          A =/= B -> none;
          A =:= B ->
              SessionId = FuncSessionId(),
              UserId = proplists:get_value("id", Combined),
              {rowid, Id1} = sqlite3:write(users, session, [
                                                      {session_id, SessionId},
                                                      {user_id, UserId},
                                                      {created, os:system_time(seconds)}
                                                      ]),
              sqlite3:close(users),
              SessionId
        end
    end.

logout(SessionId) ->
    sqlite3:open(users),
    sqlite3:delete(users, session, {session_id, SessionId}),
    sqlite3:close(users).
  