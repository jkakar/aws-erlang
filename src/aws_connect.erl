%% WARNING: DO NOT EDIT, AUTO-GENERATED CODE!
%% See https://github.com/aws-beam/aws-codegen for more details.

%% @doc Amazon Connect is a cloud-based contact center solution that makes it
%% easy to set up and manage a customer contact center and provide reliable
%% customer engagement at any scale.
%%
%% Amazon Connect provides rich metrics and real-time reporting that allow
%% you to optimize contact routing. You can also resolve customer issues more
%% efficiently by putting customers in touch with the right agents.
%%
%% There are limits to the number of Amazon Connect resources that you can
%% create and limits to the number of requests that you can make per second.
%% For more information, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html">Amazon
%% Connect Service Quotas</a> in the <i>Amazon Connect Administrator
%% Guide</i>.
%%
%% To connect programmatically to an AWS service, you use an endpoint. For a
%% list of Amazon Connect endpoints, see <a
%% href="https://docs.aws.amazon.com/general/latest/gr/connect_region.html">Amazon
%% Connect Endpoints</a>.
-module(aws_connect).

-export([create_user/3,
         create_user/4,
         delete_user/4,
         delete_user/5,
         describe_user/3,
         describe_user/4,
         describe_user_hierarchy_group/3,
         describe_user_hierarchy_group/4,
         describe_user_hierarchy_structure/2,
         describe_user_hierarchy_structure/3,
         get_contact_attributes/3,
         get_contact_attributes/4,
         get_current_metric_data/3,
         get_current_metric_data/4,
         get_federation_token/2,
         get_federation_token/3,
         get_metric_data/3,
         get_metric_data/4,
         list_contact_flows/2,
         list_contact_flows/3,
         list_hours_of_operations/2,
         list_hours_of_operations/3,
         list_phone_numbers/2,
         list_phone_numbers/3,
         list_queues/2,
         list_queues/3,
         list_routing_profiles/2,
         list_routing_profiles/3,
         list_security_profiles/2,
         list_security_profiles/3,
         list_tags_for_resource/2,
         list_tags_for_resource/3,
         list_user_hierarchy_groups/2,
         list_user_hierarchy_groups/3,
         list_users/2,
         list_users/3,
         start_chat_contact/2,
         start_chat_contact/3,
         start_outbound_voice_contact/2,
         start_outbound_voice_contact/3,
         stop_contact/2,
         stop_contact/3,
         tag_resource/3,
         tag_resource/4,
         untag_resource/3,
         untag_resource/4,
         update_contact_attributes/2,
         update_contact_attributes/3,
         update_user_hierarchy/4,
         update_user_hierarchy/5,
         update_user_identity_info/4,
         update_user_identity_info/5,
         update_user_phone_config/4,
         update_user_phone_config/5,
         update_user_routing_profile/4,
         update_user_routing_profile/5,
         update_user_security_profiles/4,
         update_user_security_profiles/5]).

-include_lib("hackney/include/hackney_lib.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Creates a user account for the specified Amazon Connect instance.
%%
%% For information about how to create user accounts using the Amazon Connect
%% console, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/user-management.html">Add
%% Users</a> in the <i>Amazon Connect Administrator Guide</i>.
create_user(Client, InstanceId, Input) ->
    create_user(Client, InstanceId, Input, []).
create_user(Client, InstanceId, Input0, Options) ->
    Method = put,
    Path = ["/users/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Deletes a user account from the specified Amazon Connect instance.
%%
%% For information about what happens to a user's data when their account is
%% deleted, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/delete-users.html">Delete
%% Users from Your Amazon Connect Instance</a> in the <i>Amazon Connect
%% Administrator Guide</i>.
delete_user(Client, InstanceId, UserId, Input) ->
    delete_user(Client, InstanceId, UserId, Input, []).
delete_user(Client, InstanceId, UserId, Input0, Options) ->
    Method = delete,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Describes the specified user account. You can find the instance ID in
%% the console (it’s the final part of the ARN). The console does not display
%% the user IDs. Instead, list the users and note the IDs provided in the
%% output.
describe_user(Client, InstanceId, UserId)
  when is_map(Client) ->
    describe_user(Client, InstanceId, UserId, []).
describe_user(Client, InstanceId, UserId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Describes the specified hierarchy group.
describe_user_hierarchy_group(Client, HierarchyGroupId, InstanceId)
  when is_map(Client) ->
    describe_user_hierarchy_group(Client, HierarchyGroupId, InstanceId, []).
describe_user_hierarchy_group(Client, HierarchyGroupId, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/user-hierarchy-groups/", http_uri:encode(InstanceId), "/", http_uri:encode(HierarchyGroupId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Describes the hierarchy structure of the specified Amazon Connect
%% instance.
describe_user_hierarchy_structure(Client, InstanceId)
  when is_map(Client) ->
    describe_user_hierarchy_structure(Client, InstanceId, []).
describe_user_hierarchy_structure(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/user-hierarchy-structure/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Retrieves the contact attributes for the specified contact.
get_contact_attributes(Client, InitialContactId, InstanceId)
  when is_map(Client) ->
    get_contact_attributes(Client, InitialContactId, InstanceId, []).
get_contact_attributes(Client, InitialContactId, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/contact/attributes/", http_uri:encode(InstanceId), "/", http_uri:encode(InitialContactId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Gets the real-time metric data from the specified Amazon Connect
%% instance.
%%
%% For a description of each metric, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/real-time-metrics-definitions.html">Real-time
%% Metrics Definitions</a> in the <i>Amazon Connect Administrator Guide</i>.
get_current_metric_data(Client, InstanceId, Input) ->
    get_current_metric_data(Client, InstanceId, Input, []).
get_current_metric_data(Client, InstanceId, Input0, Options) ->
    Method = post,
    Path = ["/metrics/current/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Retrieves a token for federation.
get_federation_token(Client, InstanceId)
  when is_map(Client) ->
    get_federation_token(Client, InstanceId, []).
get_federation_token(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/user/federate/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Gets historical metric data from the specified Amazon Connect
%% instance.
%%
%% For a description of each historical metric, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/historical-metrics-definitions.html">Historical
%% Metrics Definitions</a> in the <i>Amazon Connect Administrator Guide</i>.
get_metric_data(Client, InstanceId, Input) ->
    get_metric_data(Client, InstanceId, Input, []).
get_metric_data(Client, InstanceId, Input0, Options) ->
    Method = post,
    Path = ["/metrics/historical/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Provides information about the contact flows for the specified Amazon
%% Connect instance.
%%
%% For more information about contact flows, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html">Contact
%% Flows</a> in the <i>Amazon Connect Administrator Guide</i>.
list_contact_flows(Client, InstanceId)
  when is_map(Client) ->
    list_contact_flows(Client, InstanceId, []).
list_contact_flows(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/contact-flows-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides information about the hours of operation for the specified
%% Amazon Connect instance.
%%
%% For more information about hours of operation, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/set-hours-operation.html">Set
%% the Hours of Operation for a Queue</a> in the <i>Amazon Connect
%% Administrator Guide</i>.
list_hours_of_operations(Client, InstanceId)
  when is_map(Client) ->
    list_hours_of_operations(Client, InstanceId, []).
list_hours_of_operations(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/hours-of-operations-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides information about the phone numbers for the specified Amazon
%% Connect instance.
%%
%% For more information about phone numbers, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/contact-center-phone-number.html">Set
%% Up Phone Numbers for Your Contact Center</a> in the <i>Amazon Connect
%% Administrator Guide</i>.
list_phone_numbers(Client, InstanceId)
  when is_map(Client) ->
    list_phone_numbers(Client, InstanceId, []).
list_phone_numbers(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/phone-numbers-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides information about the queues for the specified Amazon
%% Connect instance.
%%
%% For more information about queues, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/concepts-queues-standard-and-agent.html">Queues:
%% Standard and Agent</a> in the <i>Amazon Connect Administrator Guide</i>.
list_queues(Client, InstanceId)
  when is_map(Client) ->
    list_queues(Client, InstanceId, []).
list_queues(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/queues-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides summary information about the routing profiles for the
%% specified Amazon Connect instance.
%%
%% For more information about routing profiles, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/concepts-routing.html">Routing
%% Profiles</a> and <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/routing-profiles.html">Create
%% a Routing Profile</a> in the <i>Amazon Connect Administrator Guide</i>.
list_routing_profiles(Client, InstanceId)
  when is_map(Client) ->
    list_routing_profiles(Client, InstanceId, []).
list_routing_profiles(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/routing-profiles-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides summary information about the security profiles for the
%% specified Amazon Connect instance.
%%
%% For more information about security profiles, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/connect-security-profiles.html">Security
%% Profiles</a> in the <i>Amazon Connect Administrator Guide</i>.
list_security_profiles(Client, InstanceId)
  when is_map(Client) ->
    list_security_profiles(Client, InstanceId, []).
list_security_profiles(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/security-profiles-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Lists the tags for the specified resource.
%%
%% For sample policies that use tags, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/security_iam_id-based-policy-examples.html">Amazon
%% Connect Identity-Based Policy Examples</a> in the <i>Amazon Connect
%% Administrator Guide</i>.
list_tags_for_resource(Client, ResourceArn)
  when is_map(Client) ->
    list_tags_for_resource(Client, ResourceArn, []).
list_tags_for_resource(Client, ResourceArn, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/tags/", http_uri:encode(ResourceArn), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides summary information about the hierarchy groups for the
%% specified Amazon Connect instance.
%%
%% For more information about agent hierarchies, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/agent-hierarchy.html">Set
%% Up Agent Hierarchies</a> in the <i>Amazon Connect Administrator Guide</i>.
list_user_hierarchy_groups(Client, InstanceId)
  when is_map(Client) ->
    list_user_hierarchy_groups(Client, InstanceId, []).
list_user_hierarchy_groups(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/user-hierarchy-groups-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Provides summary information about the users for the specified Amazon
%% Connect instance.
list_users(Client, InstanceId)
  when is_map(Client) ->
    list_users(Client, InstanceId, []).
list_users(Client, InstanceId, Options)
  when is_map(Client), is_list(Options) ->
    Path = ["/users-summary/", http_uri:encode(InstanceId), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    request(Client, get, Path, Headers, undefined, Options, SuccessStatusCode).

%% @doc Initiates a contact flow to start a new chat for the customer.
%% Response of this API provides a token required to obtain credentials from
%% the <a
%% href="https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html">CreateParticipantConnection</a>
%% API in the Amazon Connect Participant Service.
%%
%% When a new chat contact is successfully created, clients need to subscribe
%% to the participant’s connection for the created chat within 5 minutes.
%% This is achieved by invoking <a
%% href="https://docs.aws.amazon.com/connect-participant/latest/APIReference/API_CreateParticipantConnection.html">CreateParticipantConnection</a>
%% with WEBSOCKET and CONNECTION_CREDENTIALS.
%%
%% A 429 error occurs in two situations:
%%
%% <ul> <li> API rate limit is exceeded. API TPS throttling returns a
%% <code>TooManyRequests</code> exception from the API Gateway.
%%
%% </li> <li> The <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html">quota
%% for concurrent active chats</a> is exceeded. Active chat throttling
%% returns a <code>LimitExceededException</code>.
%%
%% </li> </ul> For more information about how chat works, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/chat.html">Chat</a>
%% in the <i>Amazon Connect Administrator Guide</i>.
start_chat_contact(Client, Input) ->
    start_chat_contact(Client, Input, []).
start_chat_contact(Client, Input0, Options) ->
    Method = put,
    Path = ["/contact/chat"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc This API places an outbound call to a contact, and then initiates the
%% contact flow. It performs the actions in the contact flow that's specified
%% (in <code>ContactFlowId</code>).
%%
%% Agents are not involved in initiating the outbound API (that is, dialing
%% the contact). If the contact flow places an outbound call to a contact,
%% and then puts the contact in queue, that's when the call is routed to the
%% agent, like any other inbound case.
%%
%% There is a 60 second dialing timeout for this operation. If the call is
%% not connected after 60 seconds, it fails.
%%
%% <note> UK numbers with a 447 prefix are not allowed by default. Before you
%% can dial these UK mobile numbers, you must submit a service quota increase
%% request. For more information, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/amazon-connect-service-limits.html">Amazon
%% Connect Service Quotas</a> in the <i>Amazon Connect Administrator
%% Guide</i>.
%%
%% </note>
start_outbound_voice_contact(Client, Input) ->
    start_outbound_voice_contact(Client, Input, []).
start_outbound_voice_contact(Client, Input0, Options) ->
    Method = put,
    Path = ["/contact/outbound-voice"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Ends the specified contact.
stop_contact(Client, Input) ->
    stop_contact(Client, Input, []).
stop_contact(Client, Input0, Options) ->
    Method = post,
    Path = ["/contact/stop"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Adds the specified tags to the specified resource.
%%
%% The supported resource type is users.
%%
%% For sample policies that use tags, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/security_iam_id-based-policy-examples.html">Amazon
%% Connect Identity-Based Policy Examples</a> in the <i>Amazon Connect
%% Administrator Guide</i>.
tag_resource(Client, ResourceArn, Input) ->
    tag_resource(Client, ResourceArn, Input, []).
tag_resource(Client, ResourceArn, Input0, Options) ->
    Method = post,
    Path = ["/tags/", http_uri:encode(ResourceArn), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Removes the specified tags from the specified resource.
untag_resource(Client, ResourceArn, Input) ->
    untag_resource(Client, ResourceArn, Input, []).
untag_resource(Client, ResourceArn, Input0, Options) ->
    Method = delete,
    Path = ["/tags/", http_uri:encode(ResourceArn), ""],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Creates or updates the contact attributes associated with the
%% specified contact.
%%
%% You can add or update attributes for both ongoing and completed contacts.
%% For example, you can update the customer's name or the reason the customer
%% called while the call is active, or add notes about steps that the agent
%% took during the call that are displayed to the next agent that takes the
%% call. You can also update attributes for a contact using data from your
%% CRM application and save the data with the contact in Amazon Connect. You
%% could also flag calls for additional analysis, such as legal review or
%% identifying abusive callers.
%%
%% Contact attributes are available in Amazon Connect for 24 months, and are
%% then deleted.
%%
%% <b>Important:</b> You cannot use the operation to update attributes for
%% contacts that occurred prior to the release of the API, September 12,
%% 2018. You can update attributes only for contacts that started after the
%% release of the API. If you attempt to update attributes for a contact that
%% occurred prior to the release of the API, a 400 error is returned. This
%% applies also to queued callbacks that were initiated prior to the release
%% of the API but are still active in your instance.
update_contact_attributes(Client, Input) ->
    update_contact_attributes(Client, Input, []).
update_contact_attributes(Client, Input0, Options) ->
    Method = post,
    Path = ["/contact/attributes"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Assigns the specified hierarchy group to the specified user.
update_user_hierarchy(Client, InstanceId, UserId, Input) ->
    update_user_hierarchy(Client, InstanceId, UserId, Input, []).
update_user_hierarchy(Client, InstanceId, UserId, Input0, Options) ->
    Method = post,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), "/hierarchy"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Updates the identity information for the specified user.
%%
%% <important> Someone with the ability to invoke
%% <code>UpdateUserIndentityInfo</code> can change the login credentials of
%% other users by changing their email address. This poses a security risk to
%% your organization. They can change the email address of a user to the
%% attacker's email address, and then reset the password through email. We
%% strongly recommend limiting who has the ability to invoke
%% <code>UpdateUserIndentityInfo</code>. For more information, see <a
%% href="https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-best-practices.html">Best
%% Practices for Security Profiles</a> in the <i>Amazon Connect Administrator
%% Guide</i>.
%%
%% </important>
update_user_identity_info(Client, InstanceId, UserId, Input) ->
    update_user_identity_info(Client, InstanceId, UserId, Input, []).
update_user_identity_info(Client, InstanceId, UserId, Input0, Options) ->
    Method = post,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), "/identity-info"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Updates the phone configuration settings for the specified user.
update_user_phone_config(Client, InstanceId, UserId, Input) ->
    update_user_phone_config(Client, InstanceId, UserId, Input, []).
update_user_phone_config(Client, InstanceId, UserId, Input0, Options) ->
    Method = post,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), "/phone-config"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Assigns the specified routing profile to the specified user.
update_user_routing_profile(Client, InstanceId, UserId, Input) ->
    update_user_routing_profile(Client, InstanceId, UserId, Input, []).
update_user_routing_profile(Client, InstanceId, UserId, Input0, Options) ->
    Method = post,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), "/routing-profile"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%% @doc Assigns the specified security profiles to the specified user.
update_user_security_profiles(Client, InstanceId, UserId, Input) ->
    update_user_security_profiles(Client, InstanceId, UserId, Input, []).
update_user_security_profiles(Client, InstanceId, UserId, Input0, Options) ->
    Method = post,
    Path = ["/users/", http_uri:encode(InstanceId), "/", http_uri:encode(UserId), "/security-profiles"],
    SuccessStatusCode = undefined,
    Headers = [],
    Input = Input0,
    request(Client, Method, Path, Headers, Input, Options, SuccessStatusCode).

%%====================================================================
%% Internal functions
%%====================================================================

-spec request(aws_client:aws_client(), atom(), iolist(),
              list(), map() | undefined, list(), pos_integer() | undefined) ->
    {ok, Result, {integer(), list(), hackney:client()}} |
    {error, Error, {integer(), list(), hackney:client()}} |
    {error, term()} when
    Result :: map() | undefined,
    Error :: {binary(), binary()}.
request(Client, Method, Path, Headers0, Input, Options, SuccessStatusCode) ->
    Client1 = Client#{service => <<"connect">>},
    Host = get_host(<<"connect">>, Client1),
    URL = get_url(Host, Path, Client1),
    Headers1 = [
        {<<"Host">>, Host},
        {<<"Content-Type">>, <<"application/x-amz-json-1.1">>}
        | Headers0
    ],
    Payload = encode_payload(Input),
    MethodBin = aws_request:method_to_binary(Method),
    SignedHeaders = aws_request:sign_request(Client1, MethodBin, URL, Headers1, Payload),
    Response = hackney:request(Method, URL, SignedHeaders, Payload, Options),
    handle_response(Response, SuccessStatusCode).

handle_response({ok, StatusCode, ResponseHeaders, Client}, SuccessStatusCode)
  when StatusCode =:= 200;
       StatusCode =:= 202;
       StatusCode =:= 204;
       StatusCode =:= SuccessStatusCode ->
    case hackney:body(Client) of
        {ok, <<>>} when StatusCode =:= 200;
                        StatusCode =:= SuccessStatusCode ->
            {ok, undefined, {StatusCode, ResponseHeaders, Client}};
        {ok, Body} ->
            Result = jsx:decode(Body, [return_maps]),
            {ok, Result, {StatusCode, ResponseHeaders, Client}}
    end;
handle_response({ok, StatusCode, ResponseHeaders, Client}, _) ->
    {ok, Body} = hackney:body(Client),
    Error = jsx:decode(Body, [return_maps]),
    Reason1 = maps:get(<<"message">>, Error, undefined),
    Reason2 = maps:get(<<"Message">>, Error, Reason1),
    {error, Reason2, {StatusCode, ResponseHeaders, Client}};
handle_response({error, Reason}, _) ->
  {error, Reason}.

get_host(_EndpointPrefix, #{region := <<"local">>}) ->
    <<"localhost">>;
get_host(EndpointPrefix, #{region := Region, endpoint := Endpoint}) ->
    aws_util:binary_join([EndpointPrefix, <<".">>, Region, <<".">>, Endpoint], <<"">>).

get_url(Host, Path0, Client) ->
    Proto = maps:get(proto, Client),
    Path = erlang:iolist_to_binary(Path0),
    Port = maps:get(port, Client),
    aws_util:binary_join([Proto, <<"://">>, Host, <<":">>, Port, Path], <<"">>).

-spec encode_payload(undefined | map()) -> binary().
encode_payload(undefined) ->
  <<>>;
encode_payload(Input) ->
  jsx:encode(Input).
