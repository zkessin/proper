%%% Copyright 2010 Manolis Papadakis (manopapad@gmail.com)
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @author Manolis Papadakis <manopapad@gmail.com>
%%% @copyright 2010 Manolis Papadakis
%%% @version {@version}
%%% @doc This module contains functions used when symbolically generating
%%%	 datatypes.

-module(proper_symb).
-export([eval/1, eval/2, defined/1, well_defined/1, pretty_print/1,
	 pretty_print/2]).

-export_type([]).

-include("proper_internal.hrl").


%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-type function_name() :: atom().
%% -type symb_call()  :: {'call', module(), function_name(), [symb_term()]}.
-type var_id() :: atom().
%% -type symb_var() :: {'var', var_id()}.
-type var_values_list() :: [{var_id(),term()}].
-type symb_term() :: term().
-type handled_term() :: term().
-type call_handler() :: fun((module(), function_name(), [handled_term()]) ->
				handled_term()).
-type term_handler() :: fun((term()) -> handled_term()).


%%------------------------------------------------------------------------------
%% Symbolic generation functions
%%------------------------------------------------------------------------------

-spec eval(symb_term()) -> term().
eval(SymbTerm) ->
    eval([], SymbTerm).

-spec eval(var_values_list(), symb_term()) -> term().
eval(VarValues, SymbTerm) ->
    Identity = fun(X) -> X end,
    symb_walk(VarValues, SymbTerm, fun erlang:apply/3, Identity).

-spec defined(symb_term()) -> boolean().
defined(SymbTerm) ->
    try eval(SymbTerm) of
	_Term -> true
    catch
	_Exception:_Reason -> false
    end.

-spec well_defined(proper_types:type()) -> proper_types:type().
well_defined(SymbType) ->
    ?SUCHTHAT(X, SymbType, defined(X)).

-spec pretty_print(symb_term()) -> string().
pretty_print(SymbTerm) ->
    pretty_print([], SymbTerm).

-spec pretty_print(var_values_list(), symb_term()) -> string().
pretty_print(VarValues, SymbTerm) ->
    ParseTree =
	symb_walk(VarValues, SymbTerm, fun parse_fun/3, fun parse_term/1),
    erl_prettypr:format(ParseTree).

-spec parse_fun(module(), function_name(), [erl_syntax:syntaxTree()]) ->
	  erl_syntax:syntaxTree().
parse_fun(Module, Function, ArgTreeList) ->
    ModuleTree = erl_syntax:atom(Module),
    FunctionTree = erl_syntax:atom(Function),
    erl_syntax:application(ModuleTree, FunctionTree, ArgTreeList).

-spec parse_term(term()) -> erl_syntax:syntaxTree().
parse_term(TreeList) when is_list(TreeList) ->
    case proper_arith:cut_improper_tail(TreeList) of
	{ProperHead, ImproperTail} ->
	    erl_syntax:list(ProperHead, ImproperTail);
	ProperList ->
	    erl_syntax:list(ProperList, none)
    end;
parse_term(TreeTuple) when is_tuple(TreeTuple) ->
    erl_syntax:tuple(tuple_to_list(TreeTuple));
parse_term(Term) ->
    %% TODO: pid, port, reference, function value?
    erl_syntax:abstract(Term).

-spec symb_walk(var_values_list(), symb_term(), call_handler(),
		term_handler()) -> handled_term().
%% TODO: should this handle improper lists?
%% TODO: only atoms are allowed as variable identifiers?
symb_walk(VarValues, {call,Module,Function,Args}, HandleCall, HandleTerm) ->
    SymbWalk = fun(X) -> symb_walk(VarValues, X, HandleCall, HandleTerm) end,
    HandledArgs = proper_arith:safemap(SymbWalk, Args),
    HandleCall(Module, Function, HandledArgs);
symb_walk(VarValues, SymbTerm = {var,VarId}, _HandleCall, HandleTerm) ->
    Term =
	case lists:keyfind(VarId, 1, VarValues) of
	    {VarId,VarValue} -> VarValue;
	    false            -> SymbTerm
	end,
    HandleTerm(Term);
symb_walk(VarValues, SymbTerm, HandleCall, HandleTerm) ->
    SymbWalk = fun(X) -> symb_walk(VarValues, X, HandleCall, HandleTerm) end,
    Term =
	if
	    is_list(SymbTerm)  -> proper_arith:safemap(SymbWalk, SymbTerm);
	    is_tuple(SymbTerm) -> proper_arith:tuplemap(SymbWalk, SymbTerm);
	    true               -> SymbTerm
	end,
    HandleTerm(Term).