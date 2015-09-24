-module(aws_client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1,
                end_per_testcase/2]).
-export([test_insert_static_creds/1,
         test_dedup_acces_key/1]).

all() -> [test_insert_static_creds,
          test_dedup_acces_key].

init_per_suite(Config) ->
  application:load(aws),
  {ok, Apps} = application:ensure_all_started(aws),
  [{apps, Apps}|Config].

end_per_suite(Config) ->
  Apps = ?config(apps, Config),
  lists:foreach(fun(App) -> ok = application:stop(App) end,
                lists:reverse(Apps)),
  Config.

end_per_testcase(_, Config) ->
  ets:delete_all_objects(creds),
  ets:delete_all_objects(creds_dedup),
  Config.

test_insert_static_creds(_Config) ->
  AccessKey = <<"SomeAccessKey">>,
  SecretKey = <<"SomeSecretKey">>,
  Region = <<"eu-west-1">>,
  {ok, Ref} = aws_client:make_client(AccessKey, SecretKey, [{region, Region}]),

  ?assertMatch(#{region := Region,
                 access_key := AccessKey,
                 secret_key := SecretKey},
               aws_client:get_creds(Ref)).

test_dedup_acces_key(_Config) ->
  AccessKey = <<"SomeOtherAccessKey">>,
  SecretKey = <<"SomeOtherSecretKey">>,
  {ok, Ref} = aws_client:make_client(AccessKey, SecretKey, []),
  ?assertMatch({ok, Ref},
               aws_client:make_client(AccessKey, SecretKey, [])).
