% Git module has many faces
% - provide assets from assets repos
% - provide source files from project repos
% - edit files in project repos
% - create bare repos for projects
% - delete projects

-module(git).

-compile(export_all).

assets_list(User, Project) ->
  U = binary_to_list(User),
  P = binary_to_list(Project),
  Assets = os:cmd("git -C repos/" ++ U ++ "/" ++ P ++ " show master:assets.json"),
  jsone:decode(unicode:characters_to_binary(Assets)).

sources_list(User, Project) ->
  U = binary_to_list(User),
  P = binary_to_list(Project),
  Assets = os:cmd("git -C repos/" ++ U ++ "/" ++ P ++ " show master:sources.json"),
  jsone:decode(unicode:characters_to_binary(Assets)).

read_source(User, Project, Source) ->
  U = binary_to_list(User),
  P = binary_to_list(Project),
  S = binary_to_list(Source),
  Assets = os:cmd("git -C repos/" ++ U ++ "/" ++ P ++ " show master:" ++ S).

