-module(aws_client).
-behaviour(gen_server).

-define(CREDENTIAL_URL, <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>).
%% as per http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html#instance-metadata-security-credentials
%% We make new credentials available at least five minutes prior to the expiration of the old credentials.
-define(ALERT_BEFORE_EXPIRY, 4 * 60).

% behaviour funs
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-export([start_link/0, stop/0,
         make_client/0, make_client/2, make_client/3,
         get_creds/1, delete_client/1]).

-record(state, {
          metadata_ref = undefined :: reference() | undefined
         }).
%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

make_client() ->
    make_client(metadata, []).
make_client(metadata, Opts) ->
    gen_server:call(?MODULE, {add_creds, metadata, Opts}).
% Backwards compatibility
make_client(AccessKey, Secret, Region) when is_binary(Region) ->
    make_client(static, {AccessKey, Secret}, [{region, Region}]);
make_client(static, {AccessKey, Secret}, Opts) when is_list(Opts) ->
    gen_server:call(?MODULE, {add_creds, {static, AccessKey, Secret}, Opts}).

delete_client(Ref) ->
    %% synchronous to prevent race conditions
    gen_server:call(?MODULE, {delete, Ref}).
get_creds(Ref) ->
    case ets:lookup(creds, Ref) of
        [{_Ref, _Type, Creds}] -> Creds;
        [] -> undefined
    end.

%%====================================================================
%% Behaviour
%%====================================================================
init(_Args) ->
    ets:new(creds, [set, named_table]),
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.


%% Only allow one metadata reference to exist
handle_call({add_creds, metadata, Opts}, _From,
            State=#state{metadata_ref=undefined}) ->
    {ok, Role} = get_role(),
    {ok, AccessKey, SecretKey, Expiry} = get_metadata_creds(Role),
    {Region, Endpoint} = parse_opts(Opts),
    Ref = make_ref(),
    true = ets:insert(creds, {Ref, metadata, #{access_key => AccessKey,
                                               secret_key => SecretKey,
                                               region     => Region,
                                               endpoint   => Endpoint}}),
    setup_update_callback(Expiry, Ref, Role),
    {reply, {ok, Ref}, State#state{metadata_ref=Ref}};
%% Only one metadata reference
handle_call({add_creds, metadata, _}, _From,
            State=#state{metadata_ref=Ref}) when is_reference(Ref)->
    {reply, {ok, Ref}, State};
handle_call({add_creds, {static, AccessKey, SecretKey}, Opts}, _From, State) ->
    {Region, Endpoint} = parse_opts(Opts),
    Ref = make_ref(),
    true = ets:insert(creds, {Ref, static, #{access_key => AccessKey,
                                             secret_key => SecretKey,
                                             region     => Region,
                                             endpoint   => Endpoint}}),
    {reply, {ok, Ref}, State};
%% Never delete the only metadata reference
handle_call({delete, Ref}, _From,
            State=#state{metadata_ref=MetaRef}) when Ref =:= MetaRef ->
    error_logger:warning_msg("skipping delete of metadata reference"),
    {reply, ok, State};
handle_call({delete, Ref}, _From, State) ->
    ets:delete(creds, Ref),
    {reply, ok, State};
handle_call(Args, _From, State) ->
    error_logger:warning_msg("Unknown call ~p~n", [Args]),
    {noreply, State}.

handle_cast(Message, State) ->
    error_logger:warning_msg("Unknown Cast ~p~n", [Message]),
    {noreply, State}.

handle_info({refresh_metadata, Ref, Role}, State) ->
    {ok, AccessKey, SecretKey, Expiry} = get_metadata_creds(Role),
    Creds = get_creds(Ref),
    true = ets:insert(creds, {Ref, Creds#{access_key => AccessKey,
                                          secret_key => SecretKey}}),
    setup_update_callback(Expiry, Ref, Role),
    {noreply, State};

handle_info(Message, State) ->
    error_logger:warning_msg("Unknown message ~p~n", [Message]),
    {noreply, State}.

code_change(_Prev, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Helper funcs
%%====================================================================
parse_opts(Opts) ->
    Region = proplists:get_value(region, Opts, <<"us-east-1">>),
    Endpoint = proplists:get_value(endpoint, Opts, <<"amazonaws.com">>),
    {Region, Endpoint}.


get_role() ->
    {ok, 200, _, ClientRef} = hackney:get(?CREDENTIAL_URL),
    hackney:body(ClientRef).


get_metadata_creds(Role) ->
    {ok, 200, _, ClientRef} = hackney:get([?CREDENTIAL_URL, Role]),
    {ok, Body} = hackney:body(ClientRef),
    Map = jsx:decode(Body, [return_maps]),
    {ok, maps:get(<<"AccessKeyId">>, Map),
     maps:get(<<"SecretAccessKey">>, Map),
     maps:get(<<"Expiration">>, Map)}.

setup_update_callback(Timestamp, Ref, Role) ->
    AlertAt = seconds_until_timestamp(Timestamp) - ?ALERT_BEFORE_EXPIRY,
    erlang:send_after(AlertAt, ?MODULE, {refresh_metadata, Ref, Role}).

seconds_until_timestamp(Timestamp) ->
    calendar:datetime_to_gregorian_seconds(iso8601:parse(Timestamp))
    - (erlang:system_time(seconds)
       + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).
