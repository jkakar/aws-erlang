%% WARNING: DO NOT EDIT, AUTO-GENERATED CODE!
%% See https://github.com/jkakar/aws-codegen for more details.

%% @doc Amazon EC2 Simple Systems Manager (SSM) enables you to configure and
%% manage your EC2 instances. You can create a configuration document and
%% then associate it with one or more running instances.
%%
%% You can use a configuration document to automate the following tasks for
%% your Windows instances:
%%
%% <ul> <li>Join an AWS Directory
%%
%% </li> <li>Install, repair, or uninstall software using an MSI package
%%
%% </li> <li>Run PowerShell scripts
%%
%% </li> <li>Configure CloudWatch Logs to monitor applications and systems
%%
%% </li> </ul> Note that configuration documents are not supported on Linux
%% instances.
-module(aws_ssm).

-export([create_association/2,
         create_association/3,
         create_association_batch/2,
         create_association_batch/3,
         create_document/2,
         create_document/3,
         delete_association/2,
         delete_association/3,
         delete_document/2,
         delete_document/3,
         describe_association/2,
         describe_association/3,
         describe_document/2,
         describe_document/3,
         get_document/2,
         get_document/3,
         list_associations/2,
         list_associations/3,
         list_documents/2,
         list_documents/3,
         update_association_status/2,
         update_association_status/3]).

-include_lib("hackney/include/hackney_lib.hrl").

%%====================================================================
%% API
%%====================================================================

%% @doc Associates the specified configuration document with the specified
%% instance.
%%
%% When you associate a configuration document with an instance, the
%% configuration agent on the instance processes the configuration document
%% and configures the instance as specified.
%%
%% If you associate a configuration document with an instance that already
%% has an associated configuration document, we replace the current
%% configuration document with the new configuration document.
create_association(Client, Input)
  when is_reference(Client), is_map(Input) ->
    create_association(Client, Input, []).
create_association(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateAssociation">>, Input, Options).

%% @doc Associates the specified configuration documents with the specified
%% instances.
%%
%% When you associate a configuration document with an instance, the
%% configuration agent on the instance processes the configuration document
%% and configures the instance as specified.
%%
%% If you associate a configuration document with an instance that already
%% has an associated configuration document, we replace the current
%% configuration document with the new configuration document.
create_association_batch(Client, Input)
  when is_reference(Client), is_map(Input) ->
    create_association_batch(Client, Input, []).
create_association_batch(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateAssociationBatch">>, Input, Options).

%% @doc Creates a configuration document.
%%
%% After you create a configuration document, you can use
%% <a>CreateAssociation</a> to associate it with one or more running
%% instances.
create_document(Client, Input)
  when is_reference(Client), is_map(Input) ->
    create_document(Client, Input, []).
create_document(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"CreateDocument">>, Input, Options).

%% @doc Disassociates the specified configuration document from the specified
%% instance.
%%
%% When you disassociate a configuration document from an instance, it does
%% not change the configuration of the instance. To change the configuration
%% state of an instance after you disassociate a configuration document, you
%% must create a new configuration document with the desired configuration
%% and associate it with the instance.
delete_association(Client, Input)
  when is_reference(Client), is_map(Input) ->
    delete_association(Client, Input, []).
delete_association(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteAssociation">>, Input, Options).

%% @doc Deletes the specified configuration document.
%%
%% You must use <a>DeleteAssociation</a> to disassociate all instances that
%% are associated with the configuration document before you can delete it.
delete_document(Client, Input)
  when is_reference(Client), is_map(Input) ->
    delete_document(Client, Input, []).
delete_document(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DeleteDocument">>, Input, Options).

%% @doc Describes the associations for the specified configuration document
%% or instance.
describe_association(Client, Input)
  when is_reference(Client), is_map(Input) ->
    describe_association(Client, Input, []).
describe_association(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeAssociation">>, Input, Options).

%% @doc Describes the specified configuration document.
describe_document(Client, Input)
  when is_reference(Client), is_map(Input) ->
    describe_document(Client, Input, []).
describe_document(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"DescribeDocument">>, Input, Options).

%% @doc Gets the contents of the specified configuration document.
get_document(Client, Input)
  when is_reference(Client), is_map(Input) ->
    get_document(Client, Input, []).
get_document(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"GetDocument">>, Input, Options).

%% @doc Lists the associations for the specified configuration document or
%% instance.
list_associations(Client, Input)
  when is_reference(Client), is_map(Input) ->
    list_associations(Client, Input, []).
list_associations(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"ListAssociations">>, Input, Options).

%% @doc Describes one or more of your configuration documents.
list_documents(Client, Input)
  when is_reference(Client), is_map(Input) ->
    list_documents(Client, Input, []).
list_documents(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"ListDocuments">>, Input, Options).

%% @doc Updates the status of the configuration document associated with the
%% specified instance.
update_association_status(Client, Input)
  when is_reference(Client), is_map(Input) ->
    update_association_status(Client, Input, []).
update_association_status(Client, Input, Options)
  when is_reference(Client), is_map(Input), is_list(Options) ->
    request(Client, <<"UpdateAssociationStatus">>, Input, Options).

%%====================================================================
%% Internal functions
%%====================================================================

request(CredRef, Action, Input, Options) ->
    Client = aws_client:get_creds(CredRef),
    Client1 = Client#{service => <<"ssm">>},
    Host = aws_util:binary_join([<<"ssm.">>,
                                 maps:get(region, Client1),
                                 <<".">>,
                                 maps:get(endpoint, Client1)],
                                <<"">>),
    URL = aws_util:binary_join([<<"https://">>, Host, <<"/">>], <<"">>),
    Headers = [{<<"Host">>, Host},
               {<<"Content-Type">>, <<"application/x-amz-json-1.1">>},
               {<<"X-Amz-Target">>, << <<"AmazonSSM.">>/binary, Action/binary>>}],
    Payload = jsx:encode(Input),
    Headers1 = aws_request:sign_request(Client1, <<"POST">>, URL, Headers, Payload),
    Response = hackney:request(post, URL, Headers1, Payload, Options),
    handle_response(Response).

handle_response({ok, 200, ResponseHeaders, Client}) ->
    {ok, Body} = hackney:body(Client),
    Result = jsx:decode(Body, [return_maps]),
    {ok, Result, {200, ResponseHeaders, Client}};
handle_response({ok, StatusCode, ResponseHeaders, Client}) ->
    {ok, Body} = hackney:body(Client),
    Reason = maps:get(<<"__type">>, jsx:decode(Body, [return_maps])),
    {error, Reason, {StatusCode, ResponseHeaders, Client}};
handle_response({error, Reason}) ->
    {error, Reason}.
