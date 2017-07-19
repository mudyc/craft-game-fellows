% Git module has many faces
% - provide assets from assets repos
% - provide source files from project repos
% - edit files in project repos
% - create bare repos for projects
% - delete projects

-module(git).

-compile(export_all).

assets_list(User, Project) ->
  [{craftman, "/assets/img/craftman.png" }].

sources_list(User, Project) ->
  U = binary_to_list(User),
  P = binary_to_list(Project),
  lists:map(fun(X) -> string:join(["",U, P, X], "/") end, [ "example.js" ]).