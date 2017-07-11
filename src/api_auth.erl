-module(api_auth).

-export([init/2, session_id/0]).
-export([is_authorized/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
  #{session_id := SessionId }
      = cowboy_req:match_cookies([{session_id, [], <<"missing">>}], Req),
  case db:has_session(SessionId) of
    true -> {true, Req, State};
    false -> { { false , <<"Login to continue">>}, Req, State}
  end.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, to_json}
	], Req, State}.

to_json(Req, State) ->
  % db:
	Body = <<"{\"user\": \"ohto\"}">>,
	{Body, Req, State}.



%init(Req0, Opts) ->
%	Req = cowboy_req:reply(200, #{
%		<<"content-type">> => <<"text/html">>
%	}, list_to_binary(element(2, index_view:render(#{}))), Req0),
%	{ok, Req, Opts}.
  
  
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