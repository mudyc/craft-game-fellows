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
        {"/api/asset/add/:user/:project",      api_assets, #{ method => asset_add }},
        {"/api/asset/del/:user/:project",      api_assets, #{ method => asset_del }},
        {"/api/assets-list/:category",         api_assets, #{ method => assets_list, auth => not_needed }},
        {"/api/asset/:category/:hash",         api_assets, #{ method => asset_data, auth => not_needed }},
        
        % Git service
        {"/api/edit/:user/:project",           api_sources, {edit}},
        {"/api/fork/:user/:project",           api_sources, {fork}},
        
        % Project content
        {"/project/:user/:project",            project_handler, []},
        {"/project/:user/:project/:source",    project_handler, []},
        

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
  	{ok, _} = cowboy:start_clear(http, [{port, 8009}], #{
  		env => #{dispatch => Dispatch}}
  	),
    craft_sup:start_link().


stop(_State) -> ok.
