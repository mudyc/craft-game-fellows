-module(index_lang_redirect).

-export([init/2]).

% Redirect based on accept language or "en" by default.
init(Req0, Opts) ->
  Langs = cowboy_req:parse_header(<<"accept-language">>, Req0),
  SortedLangs = lists:sort(fun(A,B) -> element(2,A) > element(2,B) end, Langs),
  MappedLangs = lists:map(fun(X) -> 
                           Lang = element(1, X),
                           hd(string:split(Lang, "-"))
                         end,
                         SortedLangs),
  PrefLang = case MappedLangs of
               [] -> "en";
                _ -> hd(MappedLangs)
             end,
  ValidLang = case lists:member(PrefLang, [<<"en">>, <<"es">>, <<"fi">>]) of
                true -> PrefLang;
                   _ -> <<"en">>
              end,
  
	Req = cowboy_req:reply(302, #{
		<<"Location">> => <<"/",ValidLang/binary,"/">>
	}, Req0),
	{ok, Req, Opts}.