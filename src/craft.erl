-module(craft).
-behavior(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    error_logger:logfile({open, <<"logger.log">>}),
    error_logger:tty(true),
  	Dispatch = cowboy_router:compile([
  		{'_', [
        
        % Authentication service
  			{"/api/is_authenticated", api_auth, []},
  			{"/api/register",         api_auth, {no_auth}},
  			{"/api/login",            api_auth, {no_auth}},
  			{"/api/logout",           api_auth, []},

        % Project creation service
        % {"/api/create_proj",    api_proj, []},
        % {"/api/fork_proj",      api_proj, []},
        % {"/api/delete_proj",    api_proj, []},

        % Resource service
        % {"/api/:project/img/list",     api_resource, []},
        % {"/api/:project/img/bind",     api_resource, []},
        % {"/api/:project/img/unbind",   api_resource, []},
        % {"/api/:project/audio/list",   api_resource, []},
        % {"/api/:project/audio/bind",   api_resource, []},
        % {"/api/:project/audio/unbind", api_resource, []},

        % Web content delivery (html body, tutorial conten)
  			{"/",                 index_lang_redirect, []},
        {"/js/[...]",         cowboy_static, {priv_dir, craft, "js/", [{mimetypes, cow_mimetypes, web}] }},
        {"/css/[...]",        cowboy_static, {priv_dir, craft, "css/", [{mimetypes, cow_mimetypes, web}] }},
  			{"/:lang/",           index_page_handler, []}
  			%{"/:lang/tutorials", index_page_handler, []}
  			%{"/:lang/tutorial/:area", index_page_handler, []}
  			%{"/:lang/images", index_page_handler, []}
  			%{"/:lang/audios", index_page_handler, []}
        
        % The game bits (pixi, project files etc.)
  			%{"/game/lib", index_page_handler, []}
  			%{"/game/:user/:project/js/:file", index_page_handler, []}
  			%{"/game/:user/:project/img/:file", index_page_handler, []}
  			%{"/game/:user/:project/audio/:file", index_page_handler, []}
        
  		]}
  	]),
  	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
  		env => #{dispatch => Dispatch}}
  	),
    craft_sup:start_link().


stop(_State) -> ok.