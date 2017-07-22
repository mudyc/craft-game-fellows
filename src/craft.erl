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

        % Assets service
        % {"/api/:user/:project/img/list",     api_assets, {no_auth}},
        % {"/api/:project/img/bind/:img", api_assets, []},
        % {"/api/:project/img/unbind",   api_assets, []},
        % {"/api/:project/audio/list",   api_assets, {no_auth}},
        % {"/api/:project/audio/bind",   api_assets, []},
        % {"/api/:project/audio/unbind", api_assets, []},
        {"/assets/img/[...]", api_assets, {no_auth}},
        
        % Projects
        {"/project/:user/:project",         project_handler, []},
        {"/project/:user/:project/:source", project_handler, []},

        % Project creation service
        % {"/api/create_proj",    api_proj, []},
        % {"/api/fork_proj",      api_proj, []},
        % {"/api/delete_proj",    api_proj, []},

        

        % Web content delivery (html body, tutorial conten)
  			{"/",                 index_lang_redirect, []},
        {"/js/[...]",         cowboy_static, {priv_dir, craft, "js/", [{mimetypes, cow_mimetypes, web}] }},
        {"/img/[...]",        cowboy_static, {priv_dir, craft, "img/", [{mimetypes, cow_mimetypes, web}] }},
        {"/css/[...]",        cowboy_static, {priv_dir, craft, "css/", [{mimetypes, cow_mimetypes, web}] }},
  			{"/:lang/",           index_page_handler, []},
  			{"/:lang/tutorials/", index_page_handler, []},
  			{"/:lang/tutorials/:area", index_page_handler, []},
        {"/:lang/:user/:project", index_page_handler, []}
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