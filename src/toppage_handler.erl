-module(toppage_handler).

-export([init/2]).

init(Req0, Opts) ->
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, list_to_binary(element(2, index_view:render(#{}))), Req0),
	{ok, Req, Opts}.