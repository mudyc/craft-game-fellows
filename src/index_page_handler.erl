-module(index_page_handler).

-export([init/2]).

init(Req0, Opts) ->
  Html = list_to_binary(element(2, index_view:render(#{}))),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, Html, Req0),
	{ok, Req, Opts}.