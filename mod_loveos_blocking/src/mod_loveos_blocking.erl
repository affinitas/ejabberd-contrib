%%%-------------------------------------------------------------------
%%% @author Igor Dultsev
%%% ©copyright (C) 2017-2018, Affinitas.de
%%% @doc
%%%
%%%
%%% @end
%%% Create: 28th of February 2018
%%%-------------------------------------------------------------------

-module(mod_loveos_blocking).
-author("Igor Dultsev").

-behavior(gen_mod).
-include("ejabberd.hrl").
-include("xmpp.hrl").
-include("logger.hrl").
-include("ejabberd_commands.hrl").
-include("ejabberd_sql_pt.hrl").
-include("mod_mam.hrl").

-define(EXCLUDED_TABLE, <<"loveos_excluded">>).
-define(PROFILES_TABLE, <<"loveos_profiles">>).

-export([
  start/2,
  stop/1,
  reload/3,
	mod_opt_type/1,
  filter_packet/1,
  depends/2
]).

start(Host, _Opts) ->
  ensure_sql(Host),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 120).

stop(_Host) -> 
  ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 120).

reload(Host, NewOpts, OldOpts) ->
    NewMod = gen_mod:db_mod(Host, NewOpts, ?MODULE),
    OldMod = gen_mod:db_mod(Host, OldOpts, ?MODULE),
    if NewMod /= OldMod ->
      NewMod:init(Host, NewOpts);
        true ->
      ok
    end.

% This module can only be run on SQL-type DBs

ensure_sql(Host) -> ensure_sql(Host, gen_mod:db_type(Host, mod_mam)).
ensure_sql(_Host, sql) -> ok;
ensure_sql(_Host, DbType) -> erlang:error(iolist_to_binary(io_lib:format("Unsupported backend: ~p", [DbType]))).

mod_opt_type(access) ->
    fun (A) when is_atom(A) -> A end;

mod_opt_type(_) ->
    [access].

depends(_,_) ->
	[].

%%%===================================================================
%%% Internal functions
%%%===================================================================

query_block(User, Excluded) ->
  [<< "select (excluded_users::jsonb ? '" >>, Excluded, << "') from " >>, ?EXCLUDED_TABLE, << " where user_id='" >>, User, << "';" >>].

is_user_blocked(Host, User, Excluded) ->
  Query = query_block(User, Excluded),
  QueryResult = ejabberd_sql:sql_query(Host, Query),
  case QueryResult of
    {selected, _Columns, [[<<"t">>]]} -> true;
    {selected, _Columns, _Result} -> false;
    Err ->
      ?ERROR_MSG("Error getting excluded for ~p: ~p", [User, Err]),
      Err
  end.

query_profiles(User) ->
  [<<"select is_fraud FROM ">>, ?PROFILES_TABLE ,<<" WHERE user_id='">> , User , <<"'">>].

is_user_fraud(Host, User) ->
  Query = query_profiles(User),
  QueryResult = ejabberd_sql:sql_query(Host, Query),
  case QueryResult of
    {selected, _Columns, [[ <<"t">> ]]} -> true;
    {selected, _Columns, [[ <<"f">> ]]} -> false;
    Err ->
      ?ERROR_MSG("Error getting is_fraud for ~p: ~p", [User, Err]),
      Err
  end.

filter_packet(drop) -> drop;
filter_packet(#message{ from = #jid{ lserver = Host, luser = User }, to = #jid{ luser = Excluded}} = Input) ->
  UserFraud = is_user_fraud(Host, User),
  if 
    UserFraud -> drop;
    true -> 
      UserBlocked = is_user_blocked(Host, User, Excluded),
      if 
        UserBlocked -> drop;
        true -> Input
      end
  end;
filter_packet(Input) -> Input.

