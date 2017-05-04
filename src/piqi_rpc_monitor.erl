%% Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% @doc Piqi-RPC monitor (controls the state of Piqi-RPC services)
%%
-module(piqi_rpc_monitor).

-behaviour(gen_server).


-export([start_link/0, start_link/1, start/0, start/1, stop/0]).
% API
-export([add_service/1, remove_service/1]).
-export([pause_service/1, resume_service/1, get_service_status/1,
         get_status/0]).
%-compile(export_all).
% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-include("piqi_rpc.hrl").
%-define(DEBUG, 1).
-include("debug.hrl").


% TODO: coordinate code upgrades of RPC-service implemenatation and Rpc modules
% and other Erlang modules generated by "piqic erlang"


% gen_server name
-define(SERVER, ?MODULE).


% time to wait before the next attempt to start Piqi tools server
-define(RESTART_RETRY_TIMEOUT, 10 * 1000). % 10 seconds


-type service_status() :: 'active' | 'paused'.


% Piqi-RPC service
-record(service, {
    rpc_mod :: atom(),
    impl_mod :: atom(),
    status :: service_status(),
    rpc_service :: piqi_rpc_service()
}).


% gen_server state
-record(state, {
    services = [] :: [ #service{} ]
}).


%
% starting gen_server manually
%

start_link() ->
    RpcServices = piqi_rpc:get_services(),
    start_link(RpcServices).


start_link(RpcServices) ->
    start_common(start_link, RpcServices).


% manual start (not as a part of supervision tree)
start() ->
    RpcServices = piqi_rpc:get_services(),
    start(RpcServices).


start(RpcServices) ->
    start_common(start, RpcServices).


start_common(StartFun, RpcServices) ->
    gen_server:StartFun({local, ?SERVER}, ?MODULE, RpcServices, []).


% manual stop
stop() ->
    gen_server:cast(?SERVER, stop).


%
% gen_server callbacks
%


%% @private
init(RpcServices) ->
    Services = [ make_service(X) || X <- RpcServices ],
    State = #state{ services = Services },
    {ok, State}.


make_service(RpcService = {ImplMod, RpcMod, _UrlPath, _Options}) ->
    % loading the implementation module, otherwise
    % erlang:function_exported() called from Piqi-RPC runtime would
    % return false
    % NOTE: this will work correctly for both embedded and interactive
    % Erlang VM modes; also, we don't care about errors
    case code:is_loaded(ImplMod) of
        false -> code:load_file(ImplMod);
        _ -> ok
    end,

    #service{
        impl_mod = ImplMod,
        rpc_mod = RpcMod,
        status = 'active',
        rpc_service = RpcService
    }.


%% @private
handle_call({add_service, RpcService}, _From, State) ->
    Services = State#state.services,
    case find_rpc_service(RpcService, Services) of
        'undefined' ->
            % add a new Piqi-RPC service to the list of known services
            NewService = make_service(RpcService),
            NewState = State#state{ 
                services = add_serv(NewService, Services)
            },
            {reply, ok, NewState};
        _ ->
            % the service has been added already
            {reply, ok, State}
    end;


handle_call({remove_service, RpcService}, _From, State) ->
    % remove a Piqi-RPC service from the list of known services
    Services = State#state.services,
    case find_rpc_service(RpcService, Services) of
        'undefined' ->
            % NOTE: just returning ok, even if there's no such service -- this
            % is the expected behavior of remove_service/1
            {reply, ok, State};
        Service ->
            NewState = State#state{ services = Services -- [Service] },
            {reply, ok, NewState}
    end;

handle_call({update_status, ImplMod, NewStatus}, _From, State) ->
    case take_serv(ImplMod, State#state.services) of
        {Service, Services} ->
            NewService = Service#service{status = NewStatus},
            NewState = State#state{ 
                services = add_serv(NewService, Services)
            },
            {reply, ok, NewState};
        'undefined' ->
            {reply, {error, "no such service"}, State}
    end;


handle_call(get_status, _From, State) ->
    {reply, 'active', State};


handle_call({get_status, ImplMod}, _From, State) ->
    Response =
        case find_serv(ImplMod, State#state.services) of
            'undefined' -> 'undefined'; % no such service
            #service{ status = Status } -> Status
        end,
    {reply, Response, State};


handle_call(_Message, _From, State) ->
    % XXX:
    {noreply, State}.


%% @private
handle_cast(stop, State) ->
    {stop, normal, State}; 

handle_cast(_Msg, State) ->
    % XXX
    {noreply, State}.


%% @private
handle_info(_Info, State) ->
    % XXX
    {noreply, State}.


%% @private
terminate(_Reason, _State) -> ok.


%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%
% Utility functions
%

% interface for accessing the list of services by the name of implementation
% module

add_serv(Service, Services) ->
    [ Service | Services ].


find_serv(ImplMod, Services) ->
    case lists:keyfind(ImplMod, #service.impl_mod, Services) of
        false -> 'undefined'; % no such service
        X -> X
    end.


take_serv(ImplMod, Services) ->
    case lists:keytake(ImplMod, #service.impl_mod, Services) of
        {value, Service, OtherServices} ->
            {Service, OtherServices};
        false ->
            'undefined'
    end.

find_rpc_service(RpcService, Services) ->
    case lists:keyfind(RpcService, #service.rpc_service, Services) of
        false -> 'undefined'; % no such service
        X -> X
    end.


%
% API implementation
%

-spec add_service( RpcService :: piqi_rpc_service() ) -> ok.


add_service(RpcService) ->
    gen_server:call(?SERVER, {add_service, RpcService}).


-spec remove_service( RpcService :: piqi_rpc_service() ) -> ok.

remove_service(RpcService) ->
    gen_server:call(?SERVER, {remove_service, RpcService}).


-spec pause_service( ImplMod :: atom() ) -> ok | {error, any()}.

pause_service(ImplMod) ->
    gen_server:call(?SERVER, {update_status, ImplMod, 'paused'}).


-spec resume_service( ImplMod :: atom() ) -> ok | {error, any()}.

resume_service(ImplMod) ->
    gen_server:call(?SERVER, {update_status, ImplMod, 'active'}).


-spec get_service_status( ImplMod :: atom() ) -> service_status() | 'undefined'.

get_service_status(ImplMod) ->
    gen_server:call(?SERVER, {get_status, ImplMod}).


-spec get_status() -> 'active'.

get_status() ->
    gen_server:call(?SERVER, get_status).

