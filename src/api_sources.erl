-module(api_sources).

-compile(export_all).


init(Req, Opts) ->	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
    case State of
      {edit} ->
                User = cowboy_req:binding(user, Req),
                #{session_id := SessionId }
                    = cowboy_req:match_cookies([{session_id, [], <<"missing">>}], Req),
                case db:has_access(SessionId, User) of
                  true -> {true, Req, State};
                  false -> { { false , <<"Login to continue">>}, Req, State}
                end;
      {fork} ->
                #{session_id := SessionId }
                    = cowboy_req:match_cookies([{session_id, [], <<"missing">>}], Req),
                case db:has_session(SessionId) of
                  {true, UserId} -> {true, Req, {fork, UserId}};
                  {false, _}     -> { { false , <<"Login to continue">>}, Req, State}
                end
    end.
content_types_accepted(Req, State) ->
  Handler = [
          {{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, handle_post}],
  {Handler, Req, State}.

handle_post(Req, State) ->
    case State of
      {edit} ->
                        User = cowboy_req:binding(user, Req),
                        Project = cowboy_req:binding(project, Req),
                        {ok, Body, Req1} = cowboy_req:read_urlencoded_body(Req),
                        % If keyfind fails to find, this returns false and will crash.
                        {_, File} = lists:keyfind(<<"file">>, 1, Body),
                        {_, Data} = lists:keyfind(<<"data">>, 1, Body),

                        git:edit(User, Project, File, Data),
                        {true, Req1, State};
      {fork, UserId} ->
                        From = cowboy_req:binding(user, Req),
                        Project = cowboy_req:binding(project, Req),
                        User = db:get_username(UserId),
                        git:fork(From, Project, User),
                        io:fwrite("~p ~p",[User, Project]),
                        Req1 = cowboy_req:reply(200, #{
                        	       <<"content-type">> => <<"text/plain">>
                               }, "/" ++ binary_to_list(User) ++ "/" ++ binary_to_list(Project), Req),
                        {true, Req1, State};
                  _ ->  {false, Req, State}
    end.
    