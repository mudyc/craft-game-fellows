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
  Assets = os:cmd("git -C repos/" ++ U ++ "/" ++ P ++ " show master:" ++ S),
  io:fwrite("read: ~p",[Assets]),
  Assets.

fork(FromUser, Project, ToUser) ->
  F = binary_to_list(FromUser),
  P = binary_to_list(Project),
  T = binary_to_list(ToUser),
  os:cmd("git clone --bare repos/" ++ F ++ "/" ++ P ++ " repos/" ++ T ++ "/" ++ P).

edit(User, Project, File, Data_) ->
  U = binary_to_list(User),
  P = binary_to_list(Project),
  Repo = "repos/" ++ U ++ "/" ++ P,
  F = binary_to_list(File),
  TMP = lib:nonl(os:cmd("mktemp")),
  Data = unicode:characters_to_binary(http_uri:decode(binary_to_list(Data_))),
  %io:fwrite("~p", [Data]),
  file:write_file(TMP, Data), %io_lib:fwrite("~p", [Data])),
  %io:fwrite("file: ~p~n",[TMP]),

  ParentCommit = lib:nonl(os:cmd("git -C " ++ Repo ++ " show-ref -s master")),
  
  % Empty the index, not sure if this step is necessary
  os:cmd("git -C " ++ Repo ++ " read-tree --empty"),

  % Load the current tree. A commit ref is fine, it'll figure it out.
  os:cmd("git -C " ++ Repo ++ " read-tree " ++ ParentCommit),

  % Create a blob object. Some systems have "shasum" instead of "sha1sum"
  % Might want to check if it already exists. Left as an excercise. :)
  %BLOB_ID=$(printf "blob %d\0%s" $(echo -n "$MY_FILE_CONTENTS" | wc -c) "$MY_FILE_CONTENTS" | sha1sum | cut -d ' ' -f 1)
  %mkdir -p "objects/${BLOB_ID:0:2}"
  %printf "blob %d\0%s" $(echo -n "$MY_FILE_CONTENTS" | wc -c) "$MY_FILE_CONTENTS" | perl -MCompress::Zlib -e 'undef $/; print compress(<>)' > "objects/${BLOB_ID:0:2}/${BLOB_ID:2}"
  BlobId = lib:nonl(os:cmd("git -C " ++ Repo ++ " hash-object -w --path=" ++ TMP ++ " -- " ++ TMP)),
  file:delete(TMP),

  % Now add it to the index.
  os:cmd("git -C " ++ Repo ++ " update-index --add --cacheinfo 100644 " ++ BlobId ++ " " ++ F),

  % Create a tree from your new index
  TreeId = lib:nonl(os:cmd("git -C " ++ Repo ++ " write-tree")), 

  % Commit it.
  NewCommit = lib:nonl(os:cmd("git -C " ++ Repo ++ " commit-tree " ++ TreeId ++ " -p " ++ ParentCommit ++ " -m \"Test\"")),

%io:fwrite("tmp ~p, blob ~p, tree ~p, new commit ~p, parent ~p~n",[TMP, BlobId, TreeId, NewCommit, ParentCommit]),

  % Update the branch
  os:cmd("git -C " ++ Repo ++ " update-ref refs/heads/master " ++ NewCommit ++ " " ++ ParentCommit).
  
  % Done
  %rm -f lock
  


asset_ls_tree(Repo, Path) ->
  os:cmd("git -C " ++ Repo ++ " ls-tree --name-only master:"++Path).

asset_ls_tree_all(Repo, Path) ->
  os:cmd("git -C " ++ Repo ++ " ls-tree master:"++Path).
  
show(Repo, Hash) ->
  {ExitCode, DataList} = my_exec("git -C " ++ Repo ++ " show " ++ Hash),
  list_to_binary(DataList).
  
my_exec(Command) ->
    Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
    get_data(Port, []).

get_data(Port, Sofar) ->
    receive
    {Port, {data, Bytes}} ->
        get_data(Port, [Sofar|Bytes]);
    {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        {ExitCode, lists:flatten(Sofar)}
    end.