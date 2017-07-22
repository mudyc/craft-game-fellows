-module(index_page_handler).

-export([init/2]).

init(Req0, Opts) ->
  Path = string:lexemes(binary_to_list(cowboy_req:path(Req0)), "/"),
  
  Render = case Path of
    [_]                 -> index_view:render([{navbar, navbar()}]);
    [_, "tutorials"]    -> tutorials_view:render([{navbar, navbar()}]);
    [_, "tutorials", X] -> 
                           Module = list_to_atom(lists:append(["tut_", X, "_view"])),
                           erlang:apply(Module, render, [[{navbar, navbar()}]]);
    [_, User, Proj]     -> project_view:render([{navbar, navbar()}, {project, User ++ "/" ++ Proj }]);
                      _ -> index_view:render([{navbar, navbar()}])
  end, 
  Html = list_to_binary(element(2, Render)),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, Html, Req0),
	{ok, Req, Opts}.
  
navbar() -> element(2, a__navbar_view:render(#{})).
  