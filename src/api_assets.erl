-module(api_assets).

-compile(export_all).


init(Req, Opts) ->	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

is_authorized(Req, #{ auth := not_needed } = State) -> {true, Req, State}.

content_types_provided(Req, State) ->
	{[ 
     { {<<"*">>, <<"*">>, '*'}, accept_content}
	], Req, State}.

accept_content(Req, #{method := assets_list } = State) ->
  Category = binary_to_existing_atom(cowboy_req:binding(category, Req), utf8),
  AssetData = element(2, application:get_env(craft, assets)),
  Asset = lists:keyfind(Category, 1, AssetData),
  
  LsAll = git:asset_ls_tree_all(element(2, Asset), element(3, Asset)),
  NamesAll = string:lexemes(LsAll, "\n"),
  
  OkEnd = [".txt", ".png", ".jpg"],
  DataMap = lists:foldl(fun(Line, Map)->
                % 100644 blob 12ce80304715586910f65dd4ec3bbee4ce1e149e	plants.txt
                [Rights, Rest1] = string:split(Line, " "),
                [Type,   Rest2] = string:split(Rest1, " "),
                [Hash1,   Name] = string:split(Rest2, "\t"),
                Hash = unicode:characters_to_binary(Hash1),
                End = string:find(Name, ".", trailing),
                case lists:member(End, OkEnd) of
                  false -> Map;
                  true  ->
                            [Key1, _] = string:split(Name, End, trailing),
                            Key = unicode:characters_to_binary(Key1),
                            Data = maps:get(Key, Map, #{}),
                            NewData = case End of
                              ".txt" -> maps:put(<<"note">>, <<"xyz">>, Data);
                              ".png" -> maps:put(<<"url">>, Hash, maps:put(<<"type">>, <<"img">>, Data));
                              ".jpg" -> maps:put(<<"url">>, Hash, maps:put(<<"type">>, <<"img">>, Data))
                            end,
                            maps:put(Key, NewData, Map)
                end
  end, #{}, NamesAll),
  Req1 = cowboy_req:reply(200, #{
        	         <<"content-type">> => <<"application/json">>
                }, jsone:encode(#{ length => maps:size(DataMap), data => DataMap}), Req),
  {Req1, State};

accept_content(Req, #{method := asset_data } = State) ->
  Category = binary_to_existing_atom(cowboy_req:binding(category, Req), utf8),
  AssetData = element(2, application:get_env(craft, assets)),
  {_, GitRepo, Path} = lists:keyfind(Category, 1, AssetData),
  Hash = binary_to_list(cowboy_req:binding(hash, Req)),
  Blob = git:show(GitRepo, Hash),

  Req1 = cowboy_req:reply(200, #{
        	         <<"content-type">> => data2mime(Blob)
                }, Blob, Req),
  {Req1, State}.
  

data2mime(<<16#89,16#50,16#4E,16#47,16#0D,16#0A,16#1A,16#0A, Rest/binary>>) -> <<"image/png">>;
data2mime(<<16#FF,16#D8, Rest/binary>>) -> <<"image/jpeg">>;
data2mime(Data) ->  "application".