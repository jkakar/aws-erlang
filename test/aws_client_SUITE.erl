-module(aws_client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
-export([test_insert_static_creds/1,
         test_dedup_acces_key/1,
         fetch_metadata_creds/1,
         metadata_dedup/1]).

all() -> [test_insert_static_creds,
          test_dedup_acces_key,
          {group, mecked_metadata}].

groups() -> [{mecked_metadata, [],
              [fetch_metadata_creds,
               metadata_dedup]}].

init_per_suite(Config) ->
  application:load(aws),
  {ok, Apps} = application:ensure_all_started(aws),
  [{apps, Apps}|Config].

end_per_suite(Config) ->
  Apps = ?config(apps, Config),
  lists:foreach(fun(App) -> ok = application:stop(App) end,
                lists:reverse(Apps)),
  Config.

init_per_group(mecked_metadata, Config) ->
  RoleList = make_ref(),
  Creds = make_ref(),
  Role = <<"cedar-wallet-user">>,
  AccessKey = <<"SomeAccessKey">>,
  SecretKey = <<"SecretAccessKey">>,
  Expiry = <<"2019-09-25T23:43:56Z">>,
  Body = <<"{
      \"Code\" : \"Success\",
      \"LastUpdated\" : \"2015-09-25T17:19:52Z\",
      \"Type\" : \"AWS-HMAC\",
      \"AccessKeyId\" : \"SomeAccessKey\",
      \"SecretAccessKey\" : \"SecretAccessKey\",
      \"Token\" : \"token\",
      \"Expiration\" : \"2019-09-25T23:43:56Z\"
   }">>,
  meck:expect(hackney, get, fun(URL) when is_binary(URL) ->
                                {ok, 200, {}, RoleList};
                                ([URL, Rolef]) when is_binary(URL)
                                                    andalso Role == Rolef ->
                                {ok, 200, {}, Creds}
                            end),

  meck:expect(hackney, body, fun(Ref) when Ref == RoleList ->
                                 {ok, Role};
                                (Ref) when Ref == Creds ->
                                 {ok, Body}
                             end),
  [{access_key, AccessKey},
   {secret_key, SecretKey},
   {expiry, Expiry}|Config].

end_per_group(mecked_metadata, Config) ->
  meck:unload(hackney),
  Config.

init_per_testcase(_, Config) -> Config.
end_per_testcase(_, Config) ->
  ets:delete_all_objects(creds),
  ets:delete_all_objects(creds_dedup),
  Config.

test_insert_static_creds(_Config) ->
  AccessKey = <<"SomeAccessKey">>,
  SecretKey = <<"SomeSecretKey">>,
  Region = <<"eu-west-1">>,
  {ok, Ref} = aws_client:make_client(static, {AccessKey, SecretKey},
                                     [{region, Region}]),

  ?assertMatch(#{region := Region,
                 access_key := AccessKey,
                 secret_key := SecretKey},
               aws_client:get_creds(Ref)).

test_dedup_acces_key(_Config) ->
  AccessKey = <<"SomeOtherAccessKey">>,
  SecretKey = <<"SomeOtherSecretKey">>,
  Region = <<"us-west-1">>,
  {ok, Ref} = aws_client:make_client(AccessKey, SecretKey, Region),
  ?assertMatch({ok, Ref},
               aws_client:make_client(static, {AccessKey, SecretKey},
                                      [{region, Region}])).

fetch_metadata_creds(Config) ->
  AccessKey = ?config(access_key, Config),
  SecretKey = ?config(secret_key, Config),
  Region = <<"eu-west-1">>,
  {ok, Ref} = aws_client:make_client(metadata, [{region, Region}]),
  ?assertMatch(#{region := Region,
                 access_key := AccessKey,
                 secret_key := SecretKey},
               aws_client:get_creds(Ref)).

metadata_dedup(_Config) ->
  {ok, Ref} = aws_client:make_client(),
  ?assertMatch({ok, Ref},
               aws_client:make_client(metadata, [])).
