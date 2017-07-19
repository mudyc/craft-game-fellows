-module(project_handler).

-export([init/2]).

init(Req, Opts) ->
    %io:fwrite("Re ~p~n", [Req]),
    User = cowboy_req:binding(user, Req),
    Project = cowboy_req:binding(project, Req),
    A = git:assets_list(User, Project),
    S = git:sources_list(User, Project),
    io:fwrite("~p ~p~n", [A,S]),
    Html = list_to_binary(element(2, canvas_view:render([]))),
  	Req1 = cowboy_req:reply(200, #{
  		<<"content-type">> => <<"text/html">>
  	}, Html, Req),
    {ok, Req1, Opts}.
