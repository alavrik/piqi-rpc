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

%% Piqi compiler for Piqi-RPC/Erlang: top-level module and Escript entry point
%% (see main/1)

-module(piqic_erlang_rpc).
-compile(export_all).


-include_lib("piqi/src/piqic.hrl").


main(Args) ->
    % call piqic_erlang with command-line arguments and ?MODULE as a callback
    % module
    case piqic_erlang:piqic_erlang(?MODULE, Args) of
        ok ->
            ok;
        {error, ErrorStr} ->
            piqic_erlang:print_error(ErrorStr),
            erlang:halt(1)
    end.

%
% Callback functions
%

generate(Context) ->
    % call the main "piqic-erlang" compiler to generate *_piqi.{erl,hrl}
    piqic_erlang:generate(Context),

    gen_rpc_erl(Context),
    gen_impl_hrl(Context),
    gen_default_impl_erl(Context),
    ok.


%
% Generating Piqi-RPC server stubs: <ErlMod>_rpc.erl
%

gen_rpc_erl(Context) ->
    Piqi = Context#context.piqi,
    Mod = Piqi#piqi.module,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    FuncList = Piqi#piqi.func,

    Filename = ErlMod ++ "_rpc.erl",

    Code = iod("\n\n", [
        [
            "-module(", ErlMod, "_rpc).\n",
            "-compile(export_all).\n\n",
            "-include(\"", ErlMod, ".hrl\")."
        ],
        maybe_gen_rpc_callback_specs(Context, FuncList),
        gen_embedded_piqi(ErlMod),
        gen_get_piqi(ErlMod),
        gen_server_stubs(Context, FuncList, Mod, ErlMod)
    ]),
    ok = piqic:write_file(Filename, Code).


gen_embedded_piqi(ErlMod) ->
    [
        "piqi() ->\n",
        "    ", ErlMod, ":piqi().\n"
    ].


gen_get_piqi(_ErlMod) ->
    [
        "get_piqi(OutputFormat, Options) ->\n",
        "    piqi_rpc_runtime:get_piqi(piqi(), OutputFormat, Options).\n"
    ].


gen_server_stubs(Context, FuncList, Mod, ErlMod) ->
    FuncClauses = [ gen_func_clause(Context, X, Mod, ErlMod) || X <- FuncList ],
    [
        "rpc(Mod, Name, InputData, _InputFormat, _OutputFormat, Options) ->\n",
        "    try\n",
        "    case Name of\n",
            iod("\n", FuncClauses), "\n",
            gen_default_clause(),
        "    end\n",
        "    catch\n",
        "        Class:Reason -> piqi_rpc_runtime:handle_runtime_exception(Class, Reason, Options)\n",
        "    end.\n"
    ].


gen_default_clause() ->
    [
"        _ ->\n",
"            piqi_rpc_runtime:handle_unknown_function()\n"
    ].


gen_func_clause(Context, F, Mod, ErlMod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    ScopedName = [ Mod, "/", Name ],
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
                [
"            piqi_rpc_runtime:check_empty_input(InputData),\n",
"            case piqi_rpc_runtime:call(Mod, ",  ErlName, ", 'undefined') of\n"
                ];
            _ ->
                InputType = gen_param_typename(Context, F, "input"),
                [
"            Input = piqi_rpc_runtime:decode_input(?MODULE, fun ", ErlMod, ":parse_", InputType, "/1, <<\"", ScopedName, "-input\">>, _InputFormat, InputData, Options),\n"
"            case piqi_rpc_runtime:call(Mod, ", ErlName, ", Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                OutputType = gen_param_typename(Context, F, "output"),
                [
"                {ok, Output} ->\n"
"                    piqi_rpc_runtime:encode_output(?MODULE, fun ", ErlMod, ":gen_", OutputType, "/1, <<\"", ScopedName, "-output\">>, _OutputFormat, Output, Options)"
                ]
        end,

    ErrorCode =
        case F#func.error of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                ErrorType = gen_param_typename(Context, F, "error"),
                [[
"                {error, Error} ->\n"
"                    piqi_rpc_runtime:encode_error(?MODULE, fun ", ErlMod, ":gen_", ErrorType, "/1, <<\"", ScopedName, "-error\">>, _OutputFormat, Error, Options)"
                ]]
        end,

    DefaultCaseCode = [
"                X -> piqi_rpc_runtime:handle_invalid_result(Name, X)"
    ],

    Code = [
"        <<\"", Name, "\">> ->\n",
                InputCode,
                iod(";\n", [OutputCode] ++ ErrorCode ++ [DefaultCaseCode]),
                "\n",
"            end;\n"
    ],
    Code.

% Generating Piqi-RPC callback specs for: <ErlMod>_rpc.erl
%
% Since callbacks are only supported since R15B, will only generate if Erlang
% version is R15B+
maybe_gen_rpc_callback_specs(Context, FuncList) ->
    case can_use_callbacks() of
        true ->
            gen_rpc_callback_specs(Context, FuncList);
        _ ->
            []
    end.

%% Check if Erlang version is R15+, to determine whether callbacks can be used.
can_use_callbacks() ->
    case erlang:system_info(otp_release) of
        %% Erlang uses lexicographic ordering, so this works
        [$R, A, B, $B | _] ->
            ([A, B] >= "15");
        _ ->
            false
    end.

gen_rpc_callback_specs(Context, FuncList) ->
    [gen_callback_spec(Context, F) || F <- FuncList].

gen_callback_spec(Context, F) ->
    gen_spec("-callback", Context, F).

%
% Generating Piqi-RCP function specs: <ErlMod>_impl.hrl
%

gen_impl_hrl(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    FuncList = Piqi#piqi.func,

    Filename = ErlMod ++ "_impl.hrl",

    HeaderMacro = ["__", string:to_upper(ErlMod), "_IMPL_HRL__" ],
    Code =
        [
            "-ifndef(", HeaderMacro, ").\n"
            "-define(", HeaderMacro, ", 1).\n\n"
            "-include(\"", ErlMod, ".hrl\").\n\n",

            gen_function_specs(Context, FuncList),

            "-endif.\n"
        ],
    ok = piqic:write_file(Filename, iolist_to_binary(Code)).


gen_function_specs(Context, FuncList) ->
    [ gen_function_spec(Context, X) || X <- FuncList ].

gen_function_spec(Context, F) ->
    gen_spec("-spec", Context, F).

gen_spec(SpecAttribute, Context, F) ->
    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ ->
                InputType = gen_scoped_param_typename(Context, F, "input"),
                [ InputType, "()" ]
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ ->
                OutputType = gen_scoped_param_typename(Context, F, "output"),
                [ "{ok, ", OutputType, "()}" ]
        end,

    Error =
        case F#func.error of
            'undefined' -> "";
            _ ->
                ErrorType = gen_scoped_param_typename(Context, F, "error"),
                [ " |\n    {error, ", ErrorType, "()}" ]
        end,

    [
        SpecAttribute, " ", F#func.erlang_name, "(", Input, ") ->\n",
        "    ", Output,
        Error, ".\n\n"
    ].


gen_scoped_param_typename(Context, F, Param) ->
    ErlTypeName = gen_param_typename(Context, F, Param),
    piqic:scoped_name(Context, ErlTypeName).


gen_param_typename(Context, F, Param) ->
    TypeName = to_string(F#func.name) ++ "-" ++ Param,
    {_Parent, Typedef} = piqic:resolve_type_name(Context, TypeName),
    piqic:typedef_erlname(Typedef).


%
% Generating Piqi-RPC default implementation: <ErlMod>_default_impl.erl
%

gen_default_impl_erl(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    FuncList = Piqi#piqi.func,

    Filename = ErlMod ++ "_default_impl.erl",

    Code =
        [
            "-module(", ErlMod, "_default_impl).\n"
            "-compile(export_all).\n\n",
            impl_include_or_behaviour_export(Piqi),
            "\n",
            gen_default_impls(Context, FuncList)
        ],
    ok = piqic:write_file(Filename, iolist_to_binary(Code)).

impl_include_or_behaviour_export(Piqi) ->
    ErlMod = to_string(Piqi#piqi.erlang_module),
    FuncList = Piqi#piqi.func,
    case can_use_callbacks() of
        true ->
            [
                "-behaviour(", ErlMod, "_rpc).\n\n",
                [ gen_export(F) || F <- FuncList ]
            ];
        false ->
            ["-include(\"", ErlMod, "_impl.hrl\").\n"]
    end.

gen_export(F) ->
    ["-export([", F#func.erlang_name, "/1]).\n"].

gen_default_impls(Context, FuncList) ->
    [ gen_default_impl(Context, X) || X <- FuncList ].

gen_default_impl(Context, F) ->
    Piqi = Context#context.piqi,
    ErlMod = Piqi#piqi.erlang_module,

    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ -> "_Input"
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ ->
                ErlTypeName = gen_param_typename(Context, F, "output"),
                [ "{ok, ", ErlMod, ":default_", ErlTypeName, "()}" ]
        end,

    [ F#func.erlang_name, "(", Input, ") -> ", Output, ".\n\n" ].

