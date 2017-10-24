%%%-------------------------------------------------------------------
%%% @author Hamdullah Shah
%%% ©copyright (C) 2017-2018, Affinitas.de
%%% @doc
%%%
%%%
%%% @end
%%% Create: 2 October 2017
%%%-------------------------------------------------------------------

-module(mod_inbox).
-author("Hamdullah Shah").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("mod_mam.hrl").

-export([start/2,
	 stop/1,
	 mod_opt_type/1,
   get_inbox/2,
	 depends/2]).


start(Host, Opts) ->
  ejabberd_commands:register_commands(get_commands_spec()).

stop(Host) ->
    ejabberd_commands:unregister_commands(get_commands_spec()).

get_commands_spec() ->
  [#ejabberd_commands{name = get_inbox, tags = [purge],
      desc = "Latest message in each unique conversation",
      longdesc = "It return the latest messages with each user",
      module = ?MODULE, function = get_inbox,
      args = [{host, binary},{user, binary}],
      args_example = ["localhost","user1"],
      result = {messages,string},
      result_desc = "Array of JSON: Latest message from each conversation"}].


get_inbox(Host,User) ->
  DB_Type = gen_mod:db_type(Host, mod_mam),
  fetch_inbox(Host,User,DB_Type).

mod_opt_type(_) ->
	[].

depends(_,_) ->
	[].



%%%===================================================================
%%% Internal functions
%%%===================================================================

fetch_inbox(Host, User,  sql) ->
  Query = makeQuery(User),
  Messges = case ejabberd_sql:sql_query(Host,Query) of
        {selected,Columns,Rest} ->
          % {[[binary_to_list(X) || X <- Row] || Row <- Rest]};
          RowDic=[{lists:zip(Columns,Row)} || Row <- Rest],
          jiffy:encode(RowDic);
        _ ->
          {[]}
      end,
  Messges;

fetch_inbox(Host, User, DB_Type) ->
  throw({error, iolist_to_binary(io_lib:format("Unsupported backend: ~p",
             [DB_Type]))}).


makeQuery(User) ->
  [<<"select bare_peer, txt, id, timestamp from (
select bare_peer, txt, timestamp, id, rank() over (partition by bare_peer order by timestamp desc)
FROM archive WHERE username='">> , User , <<"' ) yo
WHERE rank=1 order by created_at desc">>].





