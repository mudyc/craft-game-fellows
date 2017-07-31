-module(project_handler).

-export([init/2]).

init(Req, Opts) ->
    %io:fwrite("Re ~p~n", [Req]),
    User = cowboy_req:binding(user, Req),
    Project = cowboy_req:binding(project, Req),
    Source = cowboy_req:binding(source, Req),

    case Source of
      undefined -> canvas_html(User, Project, Req, Opts);
              _ -> source_file(User, Project, Source, Req, Opts)
    end.
    
canvas_html(User, Project, Req, Opts) ->    
    A = git:assets_list(User, Project),
    %io:fwrite("assets ~p ~n", [A]),
    Assets = lists:map(fun(X) -> 
               {ok, Val} = maps:find(<<"name">>, X),
               {ok, Val2} = maps:find(<<"url">>, X),
               <<"\"", Val/binary, "\",\"", Val2/binary, "\"">> 
             end, A),
    S = git:sources_list(User, Project),
    Sources = lists:map(fun(X) -> 
               <<"/project/", User/binary, "/", Project/binary, "/", X/binary>> 
             end, S),
    Html = list_to_binary(element(2, canvas_view:render([{assets, Assets}, {sources, Sources}]))),
  	Req1 = cowboy_req:reply(200, #{
  		<<"content-type">> => <<"text/html">>
  	}, Html, Req),
    {ok, Req1, Opts}.

source_file(User, Project, Source, Req, Opts) ->
    File = git:read_source(User, Project, Source),
  	Req1 = cowboy_req:reply(200, #{
  		<<"content-type">> => <<"application/javascript">>
  	}, File, Req),
    {ok, Req1, Opts}.
  