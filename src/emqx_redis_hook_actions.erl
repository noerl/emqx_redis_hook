-module(emqx_redis_hook_actions).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-define(RESOURCE_TYPE_REDISHOOK, 'redis_hook').
-define(RESOURCE_CONFIG_SPEC, #{
            host => #{type => string,
                     required => true,
                     order => 1,
                     default => <<"127.0.0.1">>,
                     title => #{en => <<"HOST">>,
                                zh => <<"HOST"/utf8>>},
                     description => #{en => <<"HOST">>,
                                      zh => <<"HOST"/utf8>>}},
            port => #{type => number,
                     default => 6379,
                     order => 2,
                     title => #{en => <<"PORT">>,
                                zh => <<"端口"/utf8>>},
                     description => #{en => <<"PORT">>,
                                      zh => <<"端口"/utf8>>}},
            db => #{type => number,
                     default => 0,
                     order => 4,
                     title => #{en => <<"Database">>,
                                zh => <<"数据库"/utf8>>},
                     description => #{en => <<"Database">>,
                                      zh => <<"数据库"/utf8>>}},
            pwd => #{type => string,
                     default => <<"">>,
                     order => 3,
                     title => #{en => <<"Password">>,
                                zh => <<"密码"/utf8>>},
                     description => #{en => <<"Password">>,
                                      zh => <<"密码"/utf8>>}}
        }).

-define(ACTION_PARAM_RESOURCE, #{
            type => string,
            required => true,
            title => #{en => <<"Resource ID">>,
                       zh => <<"资源 ID"/utf8>>},
            description => #{en => <<"Bind a resource to this action">>,
                             zh => <<"给动作绑定一个资源"/utf8>>}
        }).

-define(ACTION_DATA_SPEC, #{
            '$resource' => ?ACTION_PARAM_RESOURCE
        }).

-define(JSON_REQ(URL, HEADERS, BODY), {(URL), (HEADERS), "application/json", (BODY)}).

-resource_type(#{name => ?RESOURCE_TYPE_REDISHOOK,
                 create => on_resource_create,
                 status => on_get_resource_status,
                 destroy => on_resource_destroy,
                 params => ?RESOURCE_CONFIG_SPEC,
                 title => #{en => <<"RedisHook">>,
                            zh => <<"RedisHook"/utf8>>},
                 description => #{en => <<"RedisHook">>,
                                  zh => <<"RedisHook"/utf8>>}
                }).

-rule_action(#{name => data_to_redis,
               for => '$any',
               create => on_action_create_data_to_redis,
               params => ?ACTION_DATA_SPEC,
               types => [?RESOURCE_TYPE_REDISHOOK],
               title => #{en => <<"Data to Redis">>,
                          zh => <<"写数据到 Redis 服务"/utf8>>},
               description => #{en => <<"Write Messages to Redis">>,
                                zh => <<"写数据到 Redis 服务"/utf8>>}
              }).

-type(action_fun() :: fun((Data :: map(), Envs :: map()) -> Result :: any())).


-export_type([action_fun/0]).

-export([ on_resource_create/2
        , on_get_resource_status/2
        , on_resource_destroy/2
        ]).

-export([ on_action_create_data_to_redis/2
        ]).

%%------------------------------------------------------------------------------
%% Actions for redis hook
%%------------------------------------------------------------------------------

-spec(on_resource_create(binary(), map()) -> map()).
on_resource_create(ResId, Conf = #{<<"host">> := Host,<<"port">> := Port, <<"db">> := DB, <<"pwd">> := Pwd}) ->
    io:format("Conf:~p~n", [Conf]),
    case eredis:start_link(binary_to_list(Host), Port, DB, binary_to_list(Pwd)) of
        {ok, Pid} ->
            erlang:register(redis_client, Pid),
            Conf;
        {error, Reason} ->
            ?LOG(error, "Initiate Resource ~p failed, ResId: ~p, ~0p",
                [?RESOURCE_TYPE_REDISHOOK, ResId, Reason]),
            error({connect_failure, Reason})
    end.
    

-spec(on_get_resource_status(binary(), map()) -> map()).
on_get_resource_status(_ResId, _Params) ->
    #{is_alive => erlang:is_process_alive(whereis(redis_client))}.

-spec(on_resource_destroy(binary(), map()) -> ok | {error, Reason::term()}).
on_resource_destroy(_ResId, _Params) ->
    ok.

%% An action that forwards publish messages to redis.
-spec(on_action_create_data_to_redis(Id::binary(), #{}) -> action_fun()).
on_action_create_data_to_redis(_Id, _Params}) ->
    fun(Selected, _Envs) ->
        #{id := Id, payload := Payload} = Selected,
        io:format("Id:~p~n", [Id]),
        eredis:q(redis_client, ["SET", Id, Payload])
    end.
    
    

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

