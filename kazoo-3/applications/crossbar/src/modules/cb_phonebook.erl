%%%-------------------------------------------------------------------
%%% @copyright (C) 2016 - 2035 Open Phone Net AG
%%% @doc
%%% Upload a phonebook, query phonebook for a given DID
%%% @end
%%% @contributors
%%%   Urs Rueedi
%%%-------------------------------------------------------------------
-module(cb_phonebook).

-export([init/0
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1 ,allowed_methods/2
         ,resource_exists/0, resource_exists/1 ,resource_exists/2
         ,content_types_accepted/1
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/1, post/2
         ,patch/2
         ,delete/2
        ]).

-include("../crossbar.hrl").

-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(PVT_TYPE, <<"phonebook">>).
-define(NUMBER, <<"number">>).
-define(CB_LIST, <<"phonebook/crossbar_listing">>).

-define(UPLOAD_MIME_TYPES, [{<<"text">>, <<"csv">>}
                            ,{<<"text">>, <<"comma-separated-values">>}
                           ]).

-define(NUMBER_RESP_FIELDS, [<<"Name">>
                             ,<<"Company">>, <<"Address">>, <<"ZIP">>
                             ,<<"City">>, <<"Country">>
                             ,<<"Email">>, <<"Local">>
                            ]).

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = init_db(),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.phonebook">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.phonebook">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.phonebook">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.phonebook">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.execute.put.phonebook">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.phonebook">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.patch.phonebook">>, ?MODULE, 'patch'),
    crossbar_bindings:bind(<<"*.execute.delete.phonebook">>, ?MODULE, 'delete').

init_db() ->
    _ = couch_mgr:db_create(?WH_PHONEBOOK_DB),
    couch_mgr:revise_doc_from_file(?WH_PHONEBOOK_DB, 'crossbar', "views/phonebook.json").

-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    AccountId = cb_context:account_id(Context),
    AuthId = cb_context:auth_account_id(Context),
    authorize(cb_context:req_verb(Context), AccountId, AuthId).

-spec authorize(http_method(), ne_binary(), ne_binary()) -> boolean().
authorize(?HTTP_GET, AccountId, AccountId) ->
    'true';
authorize(_, AccountId, AuthId) ->
    (
     wh_util:is_in_account_hierarchy(AuthId, AccountId, 'true')
     andalso wh_services:is_reseller(AuthId)
    )
    orelse cb_modules_util:is_superduper_admin(AuthId).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE].

-spec allowed_methods(path_token(),path_token()) -> http_methods().
allowed_methods(?NUMBER,_) ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(),path_token()) -> 'true'.
resource_exists(?NUMBER,_) -> 'true'.

-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    lager:debug("content_type ~p",[Context]),
    content_types_accepted_by_verb(Context, cb_context:req_verb(Context)).

-spec content_types_accepted_by_verb(cb_context:context(), http_method()) -> cb_context:context().
content_types_accepted_by_verb(Context, ?HTTP_POST) ->
    cb_context:set_content_types_accepted(Context, [{'from_binary', ?UPLOAD_MIME_TYPES}]);
content_types_accepted_by_verb(Context, _) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_phonebooks(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_phonebook(Context, Id, cb_context:req_verb(Context)).
validate(Context, ?NUMBER, Phonenumber) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'phonebook'),
    validate_number(Phonenumber, Context2).

-spec validate_phonebooks(cb_context:context(), http_method()) -> cb_context:context().
validate_phonebooks(Context, ?HTTP_GET) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'get'),
    AccountId = wh_util:format_account_id(cb_context:account_id(Context2), 'raw'),
    AuthId = wh_util:format_account_id(cb_context:auth_account_id(Context2), 'raw'),
    Parents = account_parents(Context2),
    summary(Context2, AccountId, AuthId, Parents);
validate_phonebooks(Context, ?HTTP_PUT) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'put'),
    create(set_account_db(Context2));
validate_phonebooks(Context, ?HTTP_POST) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'post'),
    check_uploaded_file(set_account_db(Context2)).

-spec validate_phonebook(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_phonebook(Context, Id, ?HTTP_GET) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'get'),
    read(Id, set_account_db(Context2));
validate_phonebook(Context, Id, ?HTTP_POST) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'post'),
    update(Id, set_account_db(Context2));
validate_phonebook(Context, Id, ?HTTP_PATCH) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'patch'),
    validate_patch(Id, set_account_db(Context2));
validate_phonebook(Context, Id, ?HTTP_DELETE) ->
    Context2 = maybe_validate_for_multiphonebook(Context, 'delete'),
    read(Id, set_account_db(Context2)).

-spec post(cb_context:context()) -> cb_context:context().
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context) ->
    _ = init_db(),
    _ = wh_util:spawn(fun() -> upload_csv(Context) end),
    crossbar_util:response_202(<<"attempting to insert phonebook from the uploaded document">>, Context).
post(Context, _RateId) ->
    Context2 = maybe_check_for_multiphonebook(Context, 'post'),
    crossbar_doc:save(Context2).

patch(Context, _RateId) ->
    Context2 = maybe_check_for_multiphonebook(Context, 'patch'),
    crossbar_doc:save(Context2).

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    Context2 = maybe_check_for_multiphonebook(Context, 'put'),
    crossbar_doc:save(Context2).

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _RateId) ->
    Context2 = maybe_check_for_multiphonebook(Context, 'delete'),
    crossbar_doc:delete(Context2).

-spec validate_number(ne_binary(), cb_context:context()) -> cb_context:context().
validate_number(Phonenumber, Context) ->
    case wnm_util:is_reconcilable(Phonenumber) of
        'true' ->
            phonebook_for_number(wnm_util:to_e164(Phonenumber), Context);
        'false' ->
            cb_context:add_validation_error(
              <<"number format">>
              ,<<"error">>
              ,wh_json:from_list([{<<"message">>, <<"Number is un-phonebookable">>}])
              ,Context
             )
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"phonebook">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"phonebook">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update-merge an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec validate_patch(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch(Id, Context) ->
    crossbar_doc:patch_and_validate(Id, Context, fun update/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_doc:set_type(cb_context:doc(Context), <<"phonebook">>));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context(), ne_binary(), ne_binary(), ne_binaries()) -> cb_context:context().
summary(Context, _AccountId, _AuthId, []) ->
    lager:debug("loading global phonebook with Acc:~s, AuthId:~s",[_AccountId, _AuthId]),
    summary(Context, [?WH_PHONEBOOK_DB]);
summary(Context, AccountId, AccountId, Parents) ->
    lager:debug("loading self phonebook with Acc:~s Parents:~p",[AccountId, Parents]),
    ResellerId = wh_services:find_reseller_id(AccountId),
    AuthId = cb_context:auth_account_id(Context),
    case wh_services:is_reseller(AuthId) or cb_modules_util:is_superduper_admin(AuthId) of
        'true' -> Dbs = find_phonebook_db(AccountId);
        'false' -> Dbs = find_phonebook_db(ResellerId)
    end,
    lager:info("loading client phonebook in Dbs:~p",[Dbs]),
    summary(Context, Dbs);
summary(Context, AccountId, _AuthId, _Parents) ->
    Db = find_phonebook_db(AccountId),
    summary(Context, Db).

-spec summary(cb_context:context(), ne_binaries()) -> cb_context:context().
summary(Context, DB) ->
    lager:info("loading phonebook from ~p",[DB]),
    crossbar_doc:load_view(?CB_LIST, [{'databases', DB}, 'include_docs'], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file(cb_context:context()) -> cb_context:context().
check_uploaded_file(Context) ->
    check_uploaded_file(Context, cb_context:req_files(Context)).

check_uploaded_file(Context, [{_Name, File}|_]) ->
    lager:debug("checking file ~s", [_Name]),
    case wh_json:get_value(<<"contents">>, File) of
        'undefined' ->
            cb_context:add_validation_error(
                <<"file">>
                ,<<"required">>
                ,wh_json:from_list([
                    {<<"message">>, <<"file contents not found">>}
                 ])
                ,Context
            );
        Bin when is_binary(Bin) ->
            lager:debug("file: ~s", [Bin]),
            cb_context:set_resp_status(Context, 'success')
    end;
check_uploaded_file(Context, _ReqFiles) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"no file to process">>}
         ])
        ,Context
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to phonebook documents
%% @end
%%--------------------------------------------------------------------
-spec upload_csv(cb_context:context()) -> 'ok'.
upload_csv(Context) ->
    _ = cb_context:put_reqid(Context),
    Now = wh_util:now(),
    {'ok', {Count, Rates}} = process_upload_file(Context),
    lists:foreach(fun(Rate) -> crossbar_doc:ensure_saved(cb_context:set_doc(Context, Rate), [{'publish_doc', 'false'}]) end, Rates),
    _  = crossbar_doc:save(cb_context:set_doc(Context, Rates), [{'publish_doc', 'false'}]),
    lager:debug("it took ~b milli to process and save ~b phonebook", [wh_util:elapsed_ms(Now), Count]).

-spec process_upload_file(cb_context:context()) ->
                                 {'ok', {non_neg_integer(), wh_json:objects()}}.
process_upload_file(Context) ->
    process_upload_file(Context, cb_context:req_files(Context)).
process_upload_file(Context, [{_Name, File}|_]) ->
    lager:debug("converting file ~s", [_Name]),
    convert_file(wh_json:get_binary_value([<<"headers">>, <<"content_type">>], File)
                 ,wh_json:get_value(<<"contents">>, File)
                 ,Context
                );
process_upload_file(Context, _ReqFiles) ->
    cb_context:add_validation_error(
        <<"file">>
        ,<<"required">>
        ,wh_json:from_list([
            {<<"message">>, <<"no file to process">>}
         ])
        ,Context
    ).

-spec convert_file(ne_binary(), ne_binary(), cb_context:context()) ->
                          {'ok', {non_neg_integer(), wh_json:objects()}}.
convert_file(<<"text/csv">>, FileContents, Context) ->
    csv_to_phonebooks(FileContents, Context);
convert_file(<<"text/comma-sepaphonebookd-values">>, FileContents, Context) ->
    csv_to_phonebooks(FileContents, Context);
convert_file(ContentType, _, _) ->
    lager:debug("unknown content type: ~s", [ContentType]),
    throw({'unknown_content_type', ContentType}).

-spec csv_to_phonebooks(ne_binary(), cb_context:context()) ->
                          {'ok', {integer(), wh_json:objects()}}.
csv_to_phonebooks(CSV, Context) ->
    BulkInsert = couch_util:max_bulk_insert(),
    ecsv:process_csv_binary_with(CSV
                                 ,fun(Row, {Count, JObjs}) ->
                                          process_row(Context, Row, Count, JObjs, BulkInsert)
                                  end
                                 ,{0, []}
                                ).

%% NOTE: Support row formats-
%%    [Prefix, ISO, Desc, Rate]
%%    [Prefix, ISO, Desc, InternalRate, Rate]
%%    [Prefix, ISO, Desc, Surcharge, InternalRate, Rate]
%%    [Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate]

-type phonebook_row() :: [string(),...] | string().
-type phonebook_row_acc() :: {integer(), wh_json:objects()}.

-spec process_row(cb_context:context(), phonebook_row(), integer(), wh_json:objects(), integer()) ->
                         phonebook_row_acc().
process_row(Context, Row, Count, JObjs, BulkInsert) ->
    J = case Count > 1 andalso (Count rem BulkInsert) =:= 0 of
            'false' -> JObjs;
            'true' ->
                _Pid = save_processed_phonebooks(cb_context:set_doc(Context, JObjs), Count),
                []
        end,
    process_row(Row, {Count, J}).

-spec process_row(phonebook_row(), phonebook_row_acc()) -> phonebook_row_acc().
process_row(Row, {Count, JObjs}=Acc) ->
    case get_row_prefix(Row) of
        'undefined' -> Acc;
        Prefix ->
            ISO = get_row_iso(Row),
            Description = get_row_description(Row),
            InternalRate = get_row_internal_phonebook(Row),
            %% The idea here is the more expensive phonebook will have a higher CostF
            %% and decrement it from the weight so it has a lower weight #
            %% meaning it should be more likely used
            Weight = constrain_weight(byte_size(wh_util:to_binary(Prefix)) * 10
                                      - trunc(InternalRate * 100)),
            Id = <<ISO/binary, "-", (wh_util:to_binary(Prefix))/binary>>,
            Props = props:filter_undefined(
                      [{<<"_id">>, Id}
                       ,{<<"prefix">>, wh_util:to_binary(Prefix)}
                       ,{<<"weight">>, Weight}
                       ,{<<"description">>, Description}
                       ,{<<"phonebook_name">>, Id}
                       ,{<<"iso_country_code">>, ISO}
                       ,{<<"pvt_phonebook_cost">>, InternalRate}
                       ,{<<"pvt_carrier">>, <<"default">>}
                       ,{<<"pvt_type">>, <<"phonebook">>}
                       ,{<<"phonebook_increment">>, 60}
                       ,{<<"phonebook_minimum">>, 60}
                       ,{<<"phonebook_surcharge">>, get_row_surcharge(Row)}
                       ,{<<"phonebook_cost">>, get_row_phonebook(Row)}
                       ,{<<"pvt_internal_phonebook_cost">>, get_row_phonebook(Row)}
                       ,{<<"pvt_phonebook_surcharge">>, get_row_internal_surcharge(Row)}
                       ,{<<"routes">>, [<<"^\\+", (wh_util:to_binary(Prefix))/binary, "(\\d*)$">>]}
                       ,{<<"options">>, []}
                      ]),

            {Count + 1, [wh_json:from_list(Props) | JObjs]}
    end.

-spec get_row_prefix(phonebook_row()) -> api_binary().
get_row_prefix([Prefix | _]=_R) ->
    try wh_util:to_integer(Prefix) of
        P -> P
    catch
        _:_ ->
            lager:info("non-integer prefix on row: ~p", [_R]),
            'undefined'
    end;
get_row_prefix(_R) ->
    lager:info("prefix not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_iso(phonebook_row()) -> ne_binary().
get_row_iso([_, ISO | _]) -> strip_quotes(wh_util:to_binary(ISO));
get_row_iso(_R) ->
    lager:info("iso not found on row: ~p", [_R]),
    <<"XX">>.

-spec get_row_description(phonebook_row()) -> api_binary().
get_row_description([_, _, Description | _]) ->
    strip_quotes(wh_util:to_binary(Description));
get_row_description(_R) ->
    lager:info("description not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_internal_surcharge(phonebook_row()) -> api_float().
get_row_internal_surcharge([_, _, _, InternalSurcharge, _, _ | _]) ->
    wh_util:to_float(InternalSurcharge);
get_row_internal_surcharge(_R) ->
    lager:info("internal surcharge not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_surcharge(phonebook_row()) -> api_float().
get_row_surcharge([_, _, _, Surcharge, _, _]) ->
    wh_util:to_float(Surcharge);
get_row_surcharge([_, _, _, _, Surcharge, _ | _]) ->
    wh_util:to_float(Surcharge);
get_row_surcharge([_|_]=_R) ->
    lager:info("surcharge not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_internal_phonebook(phonebook_row()) -> api_float().
get_row_internal_phonebook([_, _, _, Rate]) ->
    wh_util:to_float(Rate);
get_row_internal_phonebook([_, _, _, InternalRate, _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_phonebook([_, _, _, _, InternalRate, _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_phonebook([_, _, _, _, _, InternalRate | _]) ->
    wh_util:to_float(InternalRate);
get_row_internal_phonebook([_|_]=_R) ->
    lager:info("internal phonebook not found on row: ~p", [_R]),
    'undefined'.

-spec get_row_phonebook(phonebook_row()) -> api_float().
get_row_phonebook([_, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_phonebook([_, _, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_phonebook([_, _, _, _, _, Rate]) -> wh_util:to_float(Rate);
get_row_phonebook([_, _, _, _, _, _, Rate | _]) -> wh_util:to_float(Rate);
get_row_phonebook([_|_]=_R) ->
    lager:info("phonebook not found on row: ~p", [_R]),
    'undefined'.

-spec strip_quotes(ne_binary()) -> ne_binary().
strip_quotes(Bin) ->
    binary:replace(Bin, [<<"\"">>, <<"\'">>], <<>>, ['global']).

-spec constrain_weight(integer()) -> 1..100.
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.

-spec save_processed_phonebooks(cb_context:context(), integer()) -> pid().
save_processed_phonebooks(Context, Count) ->
    wh_util:spawn(
      fun() ->
              Now = wh_util:now(),
              _ = cb_context:put_reqid(Context),
              _ = crossbar_doc:save(Context, [{'publish_doc', 'false'}]),
              lager:debug("saved up to ~b docs (took ~b ms)", [Count, wh_util:elapsed_ms(Now)])
      end).

-spec phonebook_for_number(ne_binary(), cb_context:context()) -> cb_context:context().
phonebook_for_number(Phonenumber, Context) ->
    case wh_amqp_worker:call([{<<"To-DID">>, Phonenumber}
                              ,{<<"Send-Empty">>, 'true'}
                              ,{<<"Account-ID">>, cb_context:account_id(Context)}
                              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                             ]
                             ,fun wapi_phonebook:publish_req/1
                             ,fun wapi_phonebook:resp_v/1
                             ,10 * ?MILLISECONDS_IN_SECOND
                            )
    of
        {'ok', Rate} ->
            lager:debug("found phonebook for ~s", [Phonenumber]),
            maybe_handle_phonebook(Phonenumber, Context, Rate);
        _E ->
            lager:debug("failed to query for number ~s: ~p", [Phonenumber, _E]),
            cb_context:add_system_error('No phonebook found for this number', Context)
    end.

-spec maybe_handle_phonebook(ne_binary(), cb_context:context(), wh_json:object()) ->
                               cb_context:context().
maybe_handle_phonebook(Phonenumber, Context, Rate) ->
    case wh_json:get_value(<<"Base-Cost">>, Rate) of
        'undefined' ->
            lager:debug("empty phonebook response for ~s", [Phonenumber]),
            cb_context:add_system_error('No phonebook found for this number', Context);
        _BaseCost ->
            normalize_view(wh_json:set_value(<<"E164-Number">>, Phonenumber, Rate), Context)
    end.

-spec normalize_view(wh_json:object(),cb_context:context()) -> cb_context:context().
normalize_view(Rate, Context) ->
    crossbar_util:response(filter_view(Rate), Context).

-spec filter_view(wh_json:object()) -> wh_json:object().
filter_view(Rate) ->
    normalize_fields(
      wh_json:filter(fun filter_fields/1, wh_api:remove_defaults(Rate))
     ).

-spec filter_fields(tuple()) -> boolean().
filter_fields({K,_}) ->
    lists:member(K, ?NUMBER_RESP_FIELDS).

-spec normalize_fields(wh_json:object()) -> wh_json:object().
normalize_fields(Rate) ->
    wh_json:map(fun normalize_field/2, Rate).

-spec normalize_field(wh_json:key(), wh_json:json_term()) ->
                             {wh_json:key(), wh_json:json_term()}.
normalize_field(<<"Base-Cost">> = K, BaseCost) ->
    {K, wht_util:units_to_dollars(BaseCost)};
normalize_field(<<"pvt_phonebook_cost">> = K, InternalRate) ->
    {K, wht_util:units_to_dollars(InternalRate)};
normalize_field(<<"Rate">> = K, Rate) ->
    {K, wht_util:units_to_dollars(Rate)};
normalize_field(<<"Surcharge">> = K, Surcharge) ->
    {K, wht_util:units_to_dollars(Surcharge)};
normalize_field(K, V) ->
    {K, V}.

-spec find_phonebook_db(ne_binary()) -> ne_binary().
find_phonebook_db([]) ->
    [?WH_PHONEBOOK_DB];
find_phonebook_db(AccountId) ->
    ResellerId = wh_services:find_reseller_id(AccountId),
    AccDb = <<?WH_PHONEBOOK_DB/binary, "-", AccountId/binary>>,
    ResDb = <<?WH_PHONEBOOK_DB/binary, "-", ResellerId/binary>>,
    case couch_mgr:db_exists(AccDb) of
        'true' ->
                [AccDb];
        'false' ->
            case couch_mgr:db_exists(ResDb) of
                'true' ->
                    [ResDb];
                'false' ->
                    [?WH_PHONEBOOK_DB]
            end
    end.

-spec account_parents(cb_context:context()) -> ne_binaries().
account_parents(Context) ->
    AccountId = cb_context:account_id(Context)
    ,kz_account:tree(cb_context:doc(crossbar_doc:load(AccountId, Context))).

-spec init_db(ne_binary()) -> 'ok'.
init_db(DbName) ->
    case couch_mgr:db_exists(DbName) of
        'true' -> 'ok';
        'false' ->
            _ = couch_mgr:db_create(DbName),
            couch_mgr:revise_doc_from_file(DbName, 'crossbar', "views/phonebook.json"),
            'ok'
    end.

-spec set_account_db(cb_context:context()) -> cb_context:context().
set_account_db(Context) ->
    AccountId = cb_context:account_id(Context),
    Db = case account_parents(Context) of
             [] -> ?WH_PHONEBOOK_DB;
             _Parents -> find_phonebook_db(AccountId)
         end,
    init_db(Db),
    cb_context:set_account_db(Context, Db).

%% ---------------------------------------------------------
%%
%% additional for mutliple phonebookdecks 
%% check and validation
%%
%% ---------------------------------------------------------

-spec maybe_validate_for_multiphonebook(cb_context:context(), text()) -> cb_context:context().
maybe_validate_for_multiphonebook(Context, Route) ->
    case Route of
        'get' -> Context;
        _ ->
            OriginalAuth = cb_context:auth_doc(Context),
            OriginalAccountId = wh_json:get_value(<<"account_id">>, OriginalAuth, <<>>),
            AuthId = cb_context:auth_account_id(Context),
            AccountId = cb_context:account_id(Context),
            OriginalAccDb = <<?WH_PHONEBOOK_DB/binary, "-", OriginalAccountId/binary>>,
            AccDb = <<?WH_PHONEBOOK_DB/binary, "-", AccountId/binary>>,
            DeckDb = <<?WH_PHONEBOOK_DB/binary>>,
            TestDb = find_phonebook_db(AccountId),
            case cb_modules_util:is_superduper_admin(AuthId) of
                'true' -> Context;
                _ -> lager:debug("not superduper Db:~s Route:~s Context:~p",[AccDb, Route, Context])
            end,
            case wh_services:is_reseller(AuthId) and not cb_modules_util:is_superduper_admin(AuthId) of
                'true' ->
                    case TestDb of
                        [OriginalAccDb] -> lager:debug("*-*allow but not pvt_internal change Reseller OriDb:~s / OriAcc:~s",[OriginalAccDb, OriginalAccountId])
                                    ,maybe_output_for_multiphonebook(Context, Route);
                        [DeckDb] -> lager:debug("*-*allow not any Reseller on DeckDb:~s / OriAcc:~s",[DeckDb, OriginalAccountId])
                                    ,maybe_output_for_multiphonebook(Context, Route);
                        [AccDb] -> lager:debug("*-*allowed change all of Reseller AccDb:~s / OriAcc:~s",[AccDb, OriginalAccountId])
                                    ,Context
                    end;
                _ -> lager:debug("not allowed any change AccDb:~s / OriAcc:~s",[AccDb, OriginalAccountId])
                        ,maybe_output_for_multiphonebook(Context, Route)
            end
    end.

-spec maybe_output_for_multiphonebook(cb_context:context(), text()) -> cb_context:context().
maybe_output_for_multiphonebook(Context, Route) ->
    case Route of
        'delete' ->
            Context2 = cb_context:add_validation_error(
              <<"phonebook">>
              ,<<"required">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"Delete for reseller phonebook isn't support!">>}]
                )
              ,Context
             ),Context2;
        'put' ->
            Context2 = cb_context:add_validation_error(
              <<"phonebook">>
              ,<<"required">>
              ,wh_json:from_list(
                 [{<<"message">>, <<"Only subaccount of reseller are supported to add phonebook!">>}]
                )
              ,Context
             ),Context2;
        _ -> Context
    end.

-spec maybe_check_for_multiphonebook(cb_context:context(), text()) -> cb_context:context().
maybe_check_for_multiphonebook(Context, Route) ->
    case Route of
        'post' -> maybe_update_pvt(Context);
        'put' -> maybe_update_pvt(Context);
        'get' -> maybe_update_pvt(Context);
        _ -> Context
    end.

%% --------------------------------------------------------------------
%% check put and push if is reseller and not on his phonebookdeck-AccoutId
%% --------------------------------------------------------------------

-spec maybe_update_pvt(cb_context:context()) -> cb_context:context().
maybe_update_pvt(Context) ->
    JObj = cb_context:doc(Context),
    OriginalAuth = cb_context:auth_doc(Context),
    Internal = wh_json:get_value(<<"internal_phonebook_cost">>, JObj, <<"1.00">>),
    OriginalAccountId = wh_json:get_value(<<"account_id">>, OriginalAuth, <<>>),
    AuthId = cb_context:auth_account_id(Context),
    AccountId = cb_context:account_id(Context),
    OriginalAccDb = <<?WH_PHONEBOOK_DB/binary, "-", OriginalAccountId/binary>>,
    AccDb = <<?WH_PHONEBOOK_DB/binary, "-", AccountId/binary>>,
    DeckDb = <<?WH_PHONEBOOK_DB/binary>>,
    TestDb = cb_context:account_db(Context),
    case cb_modules_util:is_superduper_admin(AuthId) of
        'true' -> cb_context:set_doc(Context, wh_json:set_value(<<"pvt_internal_phonebook_cost">>, Internal, JObj));
        'false' -> lager:debug("*-*not superduper Db:~s",[AccDb])
    end,
    case wh_services:is_reseller(AuthId) and not cb_modules_util:is_superduper_admin(AuthId) of
        'true' ->
            case TestDb of
                [OriginalAccDb] -> lager:debug("*-*allow but not pvt_internal change Reseller OriDb:~s / OriAcc:~s",[OriginalAccDb, OriginalAccountId])
                                    ,cb_context:set_doc(Context, wh_json:delete_key(<<"internal_phonebook_cost">>, JObj));
                [DeckDb] -> lager:debug("*-*allow not any Reseller DeckDb:~s / OriAcc:~s",[DeckDb, OriginalAccountId]);
                [AccDb] -> lager:debug("*-*allowed change all of Reseller AccDb:~s / OriAcc:~s",[AccDb, OriginalAccountId])
                            ,cb_context:set_doc(Context, wh_json:set_value(<<"pvt_internal_phonebook_cost">>, Internal, JObj))
            end;
        'false' ->
            case cb_modules_util:is_superduper_admin(AuthId) of
                'true' -> lager:debug("*-*allow all change pvt_internal_phonebook_cost is superduper AccDb:~s",[AccDb])
                            ,cb_context:set_doc(Context, wh_json:set_value(<<"pvt_internal_phonebook_cost">>, Internal, JObj));
                'false' -> lager:debug("not allow to change any Acc:~s / OriAcc:~s",[AccountId, OriginalAccountId])
            end
    end.
