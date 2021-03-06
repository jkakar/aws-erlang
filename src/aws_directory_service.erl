%% WARNING: DO NOT EDIT, AUTO-GENERATED CODE!
%% See https://github.com/jkakar/aws-codegen for more details.

%% @doc <fullname>AWS Directory Service</fullname>
%%
%% This is the <i>AWS Directory Service API Reference</i>. This guide
%% provides detailed information about AWS Directory Service operations, data
%% types, parameters, and errors.
-module(aws_directory_service).

-export([add_tags_to_resource/2,
         add_tags_to_resource/3,
         connect_directory/2,
         connect_directory/3,
         create_alias/2,
         create_alias/3,
         create_computer/2,
         create_computer/3,
         create_conditional_forwarder/2,
         create_conditional_forwarder/3,
         create_directory/2,
         create_directory/3,
         create_microsoft_a_d/2,
         create_microsoft_a_d/3,
         create_snapshot/2,
         create_snapshot/3,
         create_trust/2,
         create_trust/3,
         delete_conditional_forwarder/2,
         delete_conditional_forwarder/3,
         delete_directory/2,
         delete_directory/3,
         delete_snapshot/2,
         delete_snapshot/3,
         delete_trust/2,
         delete_trust/3,
         deregister_event_topic/2,
         deregister_event_topic/3,
         describe_conditional_forwarders/2,
         describe_conditional_forwarders/3,
         describe_directories/2,
         describe_directories/3,
         describe_event_topics/2,
         describe_event_topics/3,
         describe_snapshots/2,
         describe_snapshots/3,
         describe_trusts/2,
         describe_trusts/3,
         disable_radius/2,
         disable_radius/3,
         disable_sso/2,
         disable_sso/3,
         enable_radius/2,
         enable_radius/3,
         enable_sso/2,
         enable_sso/3,
         get_directory_limits/2,
         get_directory_limits/3,
         get_snapshot_limits/2,
         get_snapshot_limits/3,
         list_tags_for_resource/2,
         list_tags_for_resource/3,
         register_event_topic/2,
         register_event_topic/3,
         remove_tags_from_resource/2,
         remove_tags_from_resource/3,
         restore_from_snapshot/2,
         restore_from_snapshot/3,
         update_conditional_forwarder/2,
         update_conditional_forwarder/3,
         update_radius/2,
         update_radius/3,
         verify_trust/2,
         verify_trust/3]).

-include_lib("hackney/include/hackney_lib.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Adds or overwrites one or more tags for the specified Amazon
%% Directory Services directory. Each directory can have a maximum of 10
%% tags. Each tag consists of a key and optional value. Tag keys must be
%% unique per resource.
add_tags_to_resource(Client, Input)
  when is_map(Client), is_map(Input) ->
    add_tags_to_resource(Client, Input, []).
add_tags_to_resource(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"AddTagsToResource">>, Input, Options).

%% @doc Creates an AD Connector to connect to an on-premises directory.
connect_directory(Client, Input)
  when is_map(Client), is_map(Input) ->
    connect_directory(Client, Input, []).
connect_directory(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"ConnectDirectory">>, Input, Options).

%% @doc Creates an alias for a directory and assigns the alias to the
%% directory. The alias is used to construct the access URL for the
%% directory, such as <code>http://&lt;alias&gt;.awsapps.com</code>.
%%
%% <important> After an alias has been created, it cannot be deleted or
%% reused, so this operation should only be used when absolutely necessary.
%%
%% </important>
create_alias(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_alias(Client, Input, []).
create_alias(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateAlias">>, Input, Options).

%% @doc Creates a computer account in the specified directory, and joins the
%% computer to the directory.
create_computer(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_computer(Client, Input, []).
create_computer(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateComputer">>, Input, Options).

%% @doc Creates a conditional forwarder associated with your AWS directory.
%% Conditional forwarders are required in order to set up a trust
%% relationship with another domain. The conditional forwarder points to the
%% trusted domain.
create_conditional_forwarder(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_conditional_forwarder(Client, Input, []).
create_conditional_forwarder(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateConditionalForwarder">>, Input, Options).

%% @doc Creates a Simple AD directory.
create_directory(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_directory(Client, Input, []).
create_directory(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateDirectory">>, Input, Options).

%% @doc Creates a Microsoft AD in the AWS cloud.
create_microsoft_a_d(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_microsoft_a_d(Client, Input, []).
create_microsoft_a_d(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateMicrosoftAD">>, Input, Options).

%% @doc Creates a snapshot of a Simple AD or Microsoft AD directory in the
%% AWS cloud.
%%
%% <note> You cannot take snapshots of AD Connector directories.
%%
%% </note>
create_snapshot(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_snapshot(Client, Input, []).
create_snapshot(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateSnapshot">>, Input, Options).

%% @doc AWS Directory Service for Microsoft Active Directory allows you to
%% configure trust relationships. For example, you can establish a trust
%% between your Microsoft AD in the AWS cloud, and your existing on-premises
%% Microsoft Active Directory. This would allow you to provide users and
%% groups access to resources in either domain, with a single set of
%% credentials.
%%
%% This action initiates the creation of the AWS side of a trust relationship
%% between a Microsoft AD in the AWS cloud and an external domain.
create_trust(Client, Input)
  when is_map(Client), is_map(Input) ->
    create_trust(Client, Input, []).
create_trust(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateTrust">>, Input, Options).

%% @doc Deletes a conditional forwarder that has been set up for your AWS
%% directory.
delete_conditional_forwarder(Client, Input)
  when is_map(Client), is_map(Input) ->
    delete_conditional_forwarder(Client, Input, []).
delete_conditional_forwarder(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteConditionalForwarder">>, Input, Options).

%% @doc Deletes an AWS Directory Service directory.
delete_directory(Client, Input)
  when is_map(Client), is_map(Input) ->
    delete_directory(Client, Input, []).
delete_directory(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteDirectory">>, Input, Options).

%% @doc Deletes a directory snapshot.
delete_snapshot(Client, Input)
  when is_map(Client), is_map(Input) ->
    delete_snapshot(Client, Input, []).
delete_snapshot(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteSnapshot">>, Input, Options).

%% @doc Deletes an existing trust relationship between your Microsoft AD in
%% the AWS cloud and an external domain.
delete_trust(Client, Input)
  when is_map(Client), is_map(Input) ->
    delete_trust(Client, Input, []).
delete_trust(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteTrust">>, Input, Options).

%% @doc Removes the specified directory as a publisher to the specified SNS
%% topic.
deregister_event_topic(Client, Input)
  when is_map(Client), is_map(Input) ->
    deregister_event_topic(Client, Input, []).
deregister_event_topic(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeregisterEventTopic">>, Input, Options).

%% @doc Obtains information about the conditional forwarders for this
%% account.
%%
%% If no input parameters are provided for RemoteDomainNames, this request
%% describes all conditional forwarders for the specified directory ID.
describe_conditional_forwarders(Client, Input)
  when is_map(Client), is_map(Input) ->
    describe_conditional_forwarders(Client, Input, []).
describe_conditional_forwarders(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeConditionalForwarders">>, Input, Options).

%% @doc Obtains information about the directories that belong to this
%% account.
%%
%% You can retrieve information about specific directories by passing the
%% directory identifiers in the <i>DirectoryIds</i> parameter. Otherwise, all
%% directories that belong to the current account are returned.
%%
%% This operation supports pagination with the use of the <i>NextToken</i>
%% request and response parameters. If more results are available, the
%% <i>DescribeDirectoriesResult.NextToken</i> member contains a token that
%% you pass in the next call to <a>DescribeDirectories</a> to retrieve the
%% next set of items.
%%
%% You can also specify a maximum number of return results with the
%% <i>Limit</i> parameter.
describe_directories(Client, Input)
  when is_map(Client), is_map(Input) ->
    describe_directories(Client, Input, []).
describe_directories(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeDirectories">>, Input, Options).

%% @doc Obtains information about which SNS topics receive status messages
%% from the specified directory.
%%
%% If no input parameters are provided, such as DirectoryId or TopicName,
%% this request describes all of the associations in the account.
describe_event_topics(Client, Input)
  when is_map(Client), is_map(Input) ->
    describe_event_topics(Client, Input, []).
describe_event_topics(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeEventTopics">>, Input, Options).

%% @doc Obtains information about the directory snapshots that belong to this
%% account.
%%
%% This operation supports pagination with the use of the <i>NextToken</i>
%% request and response parameters. If more results are available, the
%% <i>DescribeSnapshots.NextToken</i> member contains a token that you pass
%% in the next call to <a>DescribeSnapshots</a> to retrieve the next set of
%% items.
%%
%% You can also specify a maximum number of return results with the
%% <i>Limit</i> parameter.
describe_snapshots(Client, Input)
  when is_map(Client), is_map(Input) ->
    describe_snapshots(Client, Input, []).
describe_snapshots(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeSnapshots">>, Input, Options).

%% @doc Obtains information about the trust relationships for this account.
%%
%% If no input parameters are provided, such as DirectoryId or TrustIds, this
%% request describes all the trust relationships belonging to the account.
describe_trusts(Client, Input)
  when is_map(Client), is_map(Input) ->
    describe_trusts(Client, Input, []).
describe_trusts(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeTrusts">>, Input, Options).

%% @doc Disables multi-factor authentication (MFA) with the Remote
%% Authentication Dial In User Service (RADIUS) server for an AD Connector
%% directory.
disable_radius(Client, Input)
  when is_map(Client), is_map(Input) ->
    disable_radius(Client, Input, []).
disable_radius(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DisableRadius">>, Input, Options).

%% @doc Disables single-sign on for a directory.
disable_sso(Client, Input)
  when is_map(Client), is_map(Input) ->
    disable_sso(Client, Input, []).
disable_sso(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DisableSso">>, Input, Options).

%% @doc Enables multi-factor authentication (MFA) with the Remote
%% Authentication Dial In User Service (RADIUS) server for an AD Connector
%% directory.
enable_radius(Client, Input)
  when is_map(Client), is_map(Input) ->
    enable_radius(Client, Input, []).
enable_radius(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"EnableRadius">>, Input, Options).

%% @doc Enables single-sign on for a directory.
enable_sso(Client, Input)
  when is_map(Client), is_map(Input) ->
    enable_sso(Client, Input, []).
enable_sso(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"EnableSso">>, Input, Options).

%% @doc Obtains directory limit information for the current region.
get_directory_limits(Client, Input)
  when is_map(Client), is_map(Input) ->
    get_directory_limits(Client, Input, []).
get_directory_limits(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"GetDirectoryLimits">>, Input, Options).

%% @doc Obtains the manual snapshot limits for a directory.
get_snapshot_limits(Client, Input)
  when is_map(Client), is_map(Input) ->
    get_snapshot_limits(Client, Input, []).
get_snapshot_limits(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"GetSnapshotLimits">>, Input, Options).

%% @doc Lists all tags on an Amazon Directory Services directory.
list_tags_for_resource(Client, Input)
  when is_map(Client), is_map(Input) ->
    list_tags_for_resource(Client, Input, []).
list_tags_for_resource(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"ListTagsForResource">>, Input, Options).

%% @doc Associates a directory with an SNS topic. This establishes the
%% directory as a publisher to the specified SNS topic. You can then receive
%% email or text (SMS) messages when the status of your directory changes.
%% You get notified if your directory goes from an Active status to an
%% Impaired or Inoperable status. You also receive a notification when the
%% directory returns to an Active status.
register_event_topic(Client, Input)
  when is_map(Client), is_map(Input) ->
    register_event_topic(Client, Input, []).
register_event_topic(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"RegisterEventTopic">>, Input, Options).

%% @doc Removes tags from an Amazon Directory Services directory.
remove_tags_from_resource(Client, Input)
  when is_map(Client), is_map(Input) ->
    remove_tags_from_resource(Client, Input, []).
remove_tags_from_resource(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"RemoveTagsFromResource">>, Input, Options).

%% @doc Restores a directory using an existing directory snapshot.
%%
%% When you restore a directory from a snapshot, any changes made to the
%% directory after the snapshot date are overwritten.
%%
%% This action returns as soon as the restore operation is initiated. You can
%% monitor the progress of the restore operation by calling the
%% <a>DescribeDirectories</a> operation with the directory identifier. When
%% the <b>DirectoryDescription.Stage</b> value changes to
%% <code>Active</code>, the restore operation is complete.
restore_from_snapshot(Client, Input)
  when is_map(Client), is_map(Input) ->
    restore_from_snapshot(Client, Input, []).
restore_from_snapshot(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"RestoreFromSnapshot">>, Input, Options).

%% @doc Updates a conditional forwarder that has been set up for your AWS
%% directory.
update_conditional_forwarder(Client, Input)
  when is_map(Client), is_map(Input) ->
    update_conditional_forwarder(Client, Input, []).
update_conditional_forwarder(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"UpdateConditionalForwarder">>, Input, Options).

%% @doc Updates the Remote Authentication Dial In User Service (RADIUS)
%% server information for an AD Connector directory.
update_radius(Client, Input)
  when is_map(Client), is_map(Input) ->
    update_radius(Client, Input, []).
update_radius(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"UpdateRadius">>, Input, Options).

%% @doc AWS Directory Service for Microsoft Active Directory allows you to
%% configure and verify trust relationships.
%%
%% This action verifies a trust relationship between your Microsoft AD in the
%% AWS cloud and an external domain.
verify_trust(Client, Input)
  when is_map(Client), is_map(Input) ->
    verify_trust(Client, Input, []).
verify_trust(Client, Input, Options)
  when is_map(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"VerifyTrust">>, Input, Options).

%%====================================================================
%% Internal functions
%%====================================================================

-spec request(aws_client:aws_client(), binary(), map(), list()) ->
    {ok, Result, {integer(), list(), hackney:client()}} |
    {error, Error, {integer(), list(), hackney:client()}} |
    {error, term()} when
    Result :: map() | undefined,
    Error :: {binary(), binary()}.
request(Client, Action, Input, Options) ->
    Client1 = Client#{service => <<"ds">>},
    Host = get_host(<<"ds">>, Client1),
    URL = get_url(Host, Client1),
    Headers = [{<<"Host">>, Host},
               {<<"Content-Type">>, <<"application/x-amz-json-1.1">>},
               {<<"X-Amz-Target">>, << <<"DirectoryService_20150416.">>/binary, Action/binary>>}],
    Payload = jsx:encode(Input),
    Headers1 = aws_request:sign_request(Client1, <<"POST">>, URL, Headers, Payload),
    Response = hackney:request(post, URL, Headers1, Payload, Options),
    handle_response(Response).

handle_response({ok, 200, ResponseHeaders, Client}) ->
    case hackney:body(Client) of
        {ok, <<>>} ->
            {ok, undefined, {200, ResponseHeaders, Client}};
        {ok, Body} ->
            Result = jsx:decode(Body, [return_maps]),
            {ok, Result, {200, ResponseHeaders, Client}}
    end;
handle_response({ok, StatusCode, ResponseHeaders, Client}) ->
    {ok, Body} = hackney:body(Client),
    Error = jsx:decode(Body, [return_maps]),
    Exception = maps:get(<<"__type">>, Error, undefined),
    Reason = maps:get(<<"message">>, Error, undefined),
    {error, {Exception, Reason}, {StatusCode, ResponseHeaders, Client}};
handle_response({error, Reason}) ->
    {error, Reason}.

get_host(_EndpointPrefix, #{region := <<"local">>}) ->
    <<"localhost">>;
get_host(EndpointPrefix, #{region := Region, endpoint := Endpoint}) ->
    aws_util:binary_join([EndpointPrefix,
			  <<".">>,
			  Region,
			  <<".">>,
			  Endpoint],
			 <<"">>).

get_url(Host, Client) ->
    Proto = maps:get(proto, Client),
    Port = maps:get(port, Client),
    aws_util:binary_join([Proto, <<"://">>, Host, <<":">>, Port, <<"/">>],
			 <<"">>).
