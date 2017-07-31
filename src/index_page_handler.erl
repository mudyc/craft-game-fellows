-module(index_page_handler).

-export([init/2]).

init(Req0, Opts) ->
  Path = string:lexemes(binary_to_list(cowboy_req:path(Req0)), "/"),
  
  Render = case Path of
    [_]                 -> index_view:render([{navbar, navbar()}]);
    [_, "tutorials"]    -> tutorials_view:render([{navbar, navbar("..")}]);
    [_, "tutorials", X] -> 
                           Module = list_to_atom(lists:append(["tut_", X, "_view"])),
                           erlang:apply(Module, render, [[{navbar, navbar("..")}, {templates, templates()}]]);
    [_, User, Proj]     -> project_view:render([
                              {navbar, navbar("..")},
                              {project, User ++ "/" ++ Proj },
                              {asset_options, asset_options()},
                              {templates, templates()}]);
                      _ -> index_view:render([{navbar, navbar()}])
  end, 
  Html = list_to_binary(element(2, Render)),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, Html, Req0),
	{ok, Req, Opts}.
  
navbar() -> element(2, a__navbar_view:render([{root,"."}])).
navbar(Root) -> element(2, a__navbar_view:render([{ root,Root}])).

templates() -> element(2, a__templates_view:render([])).

asset_options() ->
  AssetData = element(2, application:get_env(craft, assets)),
  AssetCategories = proplists:get_keys(AssetData),
  lists:map(fun(X) ->
    io:fwrite("~p ~n",[X]),
              Category = atom_to_list(element(1, X)),
              Git = element(2, X),
              Path = element(3, X),
              "<option data-git=\"" ++ Git ++ "\" data-path=\"" ++ Path ++ "\" value=\"" ++ Category ++ "\">" ++ Category ++ "</option>"
            end, AssetData).