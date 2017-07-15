-module(api_auth).

-export([init/2, session_id/0]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([handle_post/2]).
-compile(export_all).

init(Req, Opts) ->	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

is_authorized(Req, State) ->
  case State of
    {no_auth} -> { true, Req, State};
    _ -> 
          #{session_id := SessionId }
              = cowboy_req:match_cookies([{session_id, [], <<"missing">>}], Req),
          case db:has_session(SessionId) of
            {true, X} -> {true, Req, X};
            {false, _} -> { { false , <<"Login to continue">>}, Req, State}
          end
  end.

content_types_accepted(Req, State) ->
  Handler = [
          {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_post}],
  {Handler, Req, State}.

handle_post(Req, State) ->
  case cowboy_req:path(Req) of
    <<"/api/register">> -> do_registration(Req, State);
    <<"/api/login">>    -> do_login(Req, State);
                      _ -> false
  end.
  
do_registration(Req, State) ->  
  {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
  % If keyfind fails to find, this returns false and will crash.
  {_, Username} = lists:keyfind(<<"username">>, 1, Body),
  {_, Password} = lists:keyfind(<<"password">>, 1, Body),
  {_, Email} = lists:keyfind(<<"parent_email">>, 1, Body),
  {_, Age} = lists:keyfind(<<"age">>, 1, Body),
  {_, Sex} = case lists:keyfind(<<"sex">>, 1, Body) of false -> {1, ""}; X -> X end,

  Req2 = case validate_registration(Username, Password, Email) of
    [] -> % no errors, continue login
      db:register_fellow(Username, Password, Age, Sex, Email),
      SessionId = db:login(Username, Password, fun session_id/0),
      
      cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req1, #{path => <<"/">>});
    Err -> % validation errors
      cowboy_req:reply(200, #{
      	<<"content-type">> => <<"application/json">>
      }, jsone:encode(Err), Req1)
  end,
  {true, Req2, State}.

validate_registration(Username, Password, Email) ->
  LenUser = string:length(Username),
  LenPass = string:length(Password),
  Err1 = if LenUser < 3 -> [ username_too_short ]; true -> [] end,
  Err2 = if LenPass < 5 -> [ password_too_short ]; true -> [] end,
  Err3 = case db:is_username_reserved(Username) of
                   true -> [ username_is_reserved ]; _ -> [] end,
  Err4 = case string:lexemes(Email, "@") of
                  [_,_] -> [];
                      _ -> [ email_is_not_valid ] end,
  lists:append([Err1, Err2, Err3, Err4]).

do_login(Req, State) ->
  {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
  % If keyfind fails to find, this returns false and will crash.
  {_, Username} = lists:keyfind(<<"username">>, 1, Body),
  {_, Password} = lists:keyfind(<<"password">>, 1, Body),
  Req2 = case db:login(Username, Password, fun session_id/0) of
      none -> cowboy_req:reply(200, #{
      	         <<"content-type">> => <<"application/json">>
              }, jsone:encode([password_is_invalid]), Req1);
      SessionId ->
               cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req1, #{path => <<"/">>})
  end,
  {true, Req2, State}.


% GET methods


content_types_provided(Req, State) ->
	{[ 
     { {<<"*">>, <<"*">>, '*'}, accept_content}
	], Req, State}.

accept_content(Req, State) ->
  case cowboy_req:path(Req) of
    <<"/api/is_authenticated">> -> 
                                   UserId = State,
                                   { db:get_username(UserId), Req, State };
    <<"/api/logout">>           -> 
                                   #{session_id := SessionId }
                                       = cowboy_req:match_cookies([{session_id, [], <<"missing">>}], Req),
                                   db:logout(SessionId),
                                   { <<>>, Req, State };
                              _ -> {<<>>, Req, State }
  end.





session_id() ->
  N = 16,
  Id = try crypto:strong_rand_bytes(N) of
          X -> X
        catch
          error:low_entropy -> list_to_binary(lists:map(fun(X) -> rand:uniform(256) end, lists:seq(1,N)))
        end,
  to_hex(Id).

to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) ->
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].
 
to_digit(N) when N < 10 -> $0 + N;
to_digit(N) -> $a + N-10.