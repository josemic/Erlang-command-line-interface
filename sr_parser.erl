%%% Author: M. Josenhans
%%%---------------------------------------------------------------------
%%% Based on idea from Jeffrey A. Meunier
%%%---------------------------------------------------------------------
%%% This module is based on the Haskell Parsec parser library written
%%% by Erik Meijer.


-module(sr_parser).
-export([match_string/3, pNumber/0, pString/1, pStr/0, pAnd/1, 
         example1/0, example2/0, example3/0, example4/0, pMaybe/1, pOr/1, 
	 pWhiteSpace/0, pNumberWhiteSpace/0]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").


example1()->
    A = pNumber(),
    {ok, B} = A("1234dfg5"),
    B#state.number_list.

example2() ->
    A= pWhiteSpace(),
    B= pMaybe(A),
    C= pString("michael"),
    D= pMaybe(C),
    E= pString("andreas"),
    % F= pMaybe(E),
    G= pNumber(),
    H= pMaybe(G),
    I= pStr(),
    U= pAnd([D,A,E, B, I, B, I, B, G, B, H]),
    V= pAnd([U]),
    {Status, X} = V(#state{input="michael andreas BlaBla 12 34 54"}),
    io:format("Status: ~p~n", [Status]),
    io:format("Input: ~p~n", [X#state.input]),
    io:format("Parsed: ~p~n", [X#state.parsed]),  
    io:format("Parsed_list: ~p~n", [X#state.parsed_list]),
    io:format("Completion: ~p~n", [X#state.completion]),
    io:format("Number list: ~w~n",[X#state.number_list]),
    io:format("Str list: ~p~n",[X#state.str_list]),
    X.

example3() ->
    A= pString("michael"),
    B= pString("andreas"),
    C= pString("Bernd"),
    W= pWhiteSpace(),
    D= pOr([A,B,C]),
    E= pAnd([A,W,D,W,C]),
    F= pAnd([E]),
    {Status, X} = F(#state{input="michael Andreas Beq"}),
    io:format("Status: ~p~n", [Status]),
    io:format("Input: ~p~n", [X#state.input]),
    io:format("Parsed: ~p~n", [X#state.parsed]),  
    io:format("Parsed_list: ~p~n", [X#state.parsed_list]),    
    io:format("Selection_list: ~p~n", [X#state.selection_list]),
    io:format("Completion: ~p~n", [X#state.completion]),
    io:format("Number list: ~w~n",[X#state.number_list]),
    io:format("Str list: ~p~n",[X#state.str_list]),
    X.


example4() ->
    A= pString("michael"),
    B= pString("andreas"),
    C= pString("bernd"),
    W= pWhiteSpace(),
    D= pAnd([B]),
    E= pAnd([A,W,C,W,D]),
    {Status, X} = E(#state{input="michael bernd Andreas"}),
    io:format("Status: ~p~n", [Status]),
    io:format("Input: ~p~n", [X#state.input]),
    io:format("Parsed: ~p~n", [X#state.parsed]),  
    io:format("Parsed_list: ~p~n", [X#state.parsed_list]),    
    io:format("Selection_list: ~p~n", [X#state.selection_list]),
    io:format("Completion: ~p~n", [X#state.completion]),
    io:format("Number list: ~w~n",[X#state.number_list]),
    io:format("Str list: ~p~n",[X#state.str_list]),
    X.
%%%---------------------------------------------------------------------
%%% Match a specific string.
%%%---------------------------------------------------------------------
pString( MatchString )-> 
    fun( State )-> 
	    %% State = #state{input=Input},
	    match_string( MatchString, State)
    end.

match_string( MatchString, State ) -> 
    match_string( MatchString, State, []).

%%% parsing is completed and input is empty
match_string( [], #state{input=[]}=State, Acc) ->
    NewState = State#state{parsed = lists:reverse(Acc),completion = []},
    {ok, NewState};

%%% match matchstring to input characterwise
match_string( [M | Ms], #state{input=[I|Is]}=State, Acc) when M == I -> % M: MatchString, I: Input
    NewState = State#state{input= Is},
    match_string( Ms, NewState, [M |Acc]);

%%% ignore case, thus match uppercase char against lowercase char
match_string( [M | Ms], #state{input=[I|Is]}=State, Acc) when I>=65, I=<90, M == I+32 -> % M: MatchString, I: Input 
    NewState = State#state{input= Is},
    match_string( Ms, NewState, [M |Acc]);

%%% parsing is completed, however further input data exist
match_string( [], State, Acc) -> 
    NewState = State#state {parsed = lists:reverse(Acc),completion = []},
    {incomplete, NewState};

%%% matchstring is neigther empty nor completed, however input is empty
match_string( MatchString, #state{input=[]}=State, Acc) -> 
    NewState = State#state {parsed = lists:reverse(Acc), completion = MatchString},
    {partially, NewState};

%%% matchstring does not match and input is not empty
match_string( MatchString,  State, Acc) -> 
    NewState = State#state {parsed = lists:reverse(Acc),completion = MatchString},
    {fail, NewState}.


pNumber()-> 
    fun( State ) -> 
	    match_number(State)
    end.

match_number(State)->
    match_number(0, State, []).

match_number(Number, #state{input=[I|Is]}=State, Acc) when (I >= $0), (I =< $9) ->
    NewNumber = Number * 10 +  (I - $0),
    %%io:format("Number: ~p~n",[NewNumber]),
    NewState = State#state{input= Is},
    %%io:format("State: ~w ~n", [NewState#state.input]), 
    match_number(NewNumber, NewState, [I |Acc]);

match_number(_Number,  #state{input=[]}=State, []) ->
    NewState = State#state { parsed = []},
    {fail_number, NewState};

match_number(_Number, State, []) ->
    NewState = State#state { parsed = []},
    {fail, NewState};

match_number(Number,  #state{input=[]}=State, Acc) ->   
    NumberList = State#state.number_list,
    NewNumberList = NumberList ++ [Number],
    %%io:format("NewNumberList: ~p~n",[NewNumberList]),
    NewState = State#state {number_list = NewNumberList, parsed = lists:reverse(Acc)},
    {ok, NewState};

match_number(Number,  State, Acc) ->   
    NumberList = State#state.number_list,
    NewNumberList = NumberList ++ [Number],
    %%io:format("NewNumberList: ~p~n",[NewNumberList]),
    NewState = State#state {number_list = NewNumberList, parsed = lists:reverse(Acc)},
    {incomplete, NewState}.


pStr()-> 
    fun( State ) -> 
	    match_str(State)
    end.

match_str(State)->
    match_str("", State, []).

match_str(String, #state{input=[I|Is]}=State, Acc) when (I >= 33), (I =< 126) ->
    NewString = String ++ [I|""],
    %%io:format("String: ~p~n",[NewString]),
    NewState = State#state{input= Is},
    %%io:format("State: ~w ~n", [NewState#state.input]), 
    match_str(NewString, NewState, [I |Acc]);

match_str(_String,  #state{input=[]}=State, []) ->
    NewState = State#state { parsed = []},
    {fail_str, NewState};

match_str(_String, State, []) ->
    NewState = State#state { parsed = []},
    {fail, NewState};

match_str(String,  #state{input=[]}=State, Acc) ->   
    StrList = State#state.str_list,
    NewStrList =  StrList++[String],
    %%io:format("NewStrList: ~p~n",[NewStrList]),
    NewState = State#state {str_list = NewStrList, parsed = lists:reverse(Acc)},
    {ok, NewState};

match_str(String,  State, Acc) ->   
    StrList = State#state.str_list,
    NewStrList = StrList++[String],
    %%io:format("NewStrList: ~p~n",[NewStrList]),
    NewState = State#state {str_list = NewStrList, parsed = lists:reverse(Acc)},
    {incomplete, NewState}.




%%%---------------------------------------------------------------------
%%% Succeed if all parsers succeed.  This can be used as a
%%% sequencing parser.
%%%---------------------------------------------------------------------
pAnd(Parsers) -> 
    fun(State)->
	    all( Parsers, State, [])
    end.

all([], #state{input=[], parsed_list = Parsed_List}=State, Acc) ->
    NewState = State#state{parsed_list =  lists:reverse( Acc ) ++Parsed_List},
    %% io:format("lists:reverse( Acc ) ~p~n",[lists:reverse( Acc )]),
    {ok, NewState};

all([], #state{parsed_list = Parsed_List}=State, Acc) ->
    NewState = State#state{parsed_list = lists:reverse( Acc ) ++ Parsed_List},
    {incomplete, NewState};


all([P | Parsers], #state{parsed_list = Parsed_List}=State, Acc) ->
    case P(State) of
	{fail, #state{parsed=[]}=NewState} -> 
	    Parsed_List1 = NewState#state.parsed_list,
	    NewState1 = NewState#state{parsed_list = Parsed_List1 ++ lists:reverse(Acc), parsed=""},
	    {fail, NewState1};

	{fail, #state{parsed=Parsed}=NewState} -> 
	    Parsed_List1 = NewState#state.parsed_list,
	    NewState1 = NewState#state{parsed_list = Parsed_List1 ++ lists:reverse(Acc) ++ [Parsed], parsed=""},
	    {fail, NewState1};

	{fail_number, NewState} -> 
	    NewState1 = NewState#state{parsed_list = Parsed_List ++ lists:reverse(Acc)},
	    {fail_number, NewState1};

	{fail_str, NewState} -> 
	    NewState1 = NewState#state{parsed_list = Parsed_List ++ lists:reverse(Acc)},
	    {fail_str, NewState1};

	{partially, NewState} ->
	    NewState1 = NewState#state{parsed_list = Parsed_List ++ lists:reverse(Acc)},
	    {partially, NewState1};

	{incomplete, #state{parsed=[]}=NewState} ->
	    all(Parsers, NewState#state{parsed=""}, Acc);

	{incomplete, NewState} ->
	    all(Parsers, NewState#state{parsed=""}, [NewState#state.parsed | Acc]);

	{ok,  #state{parsed=[]}=NewState} ->
	    all(Parsers, NewState#state{parsed=""}, Acc);

	{ok, NewState} ->
	    all(Parsers, NewState#state{parsed=""}, [NewState#state.parsed | Acc])
    end.

%%%---------------------------------------------------------------------
%%% Succeed when one of a list of parsers succeeds.
%%%---------------------------------------------------------------------
pOr( Parsers )
-> fun( State )
      -> pTry( Parsers, State )
   end.

pTry( [], State )-> 
    {fail, State};

pTry( [P | Parsers], State )-> 
    case P( State ) of
	{fail, _NewState} -> 
	    pTry( Parsers, State#state{parsed=""} );

	{partially, _NewState} ->         
	    pTry( Parsers, State#state{parsed=""} );

	{incomplete, NewState} ->
	    {incomplete, NewState#state{selection_list =   [NewState#state.parsed | State#state.selection_list]}};

	{ok, NewState}->
	    {ok, NewState#state{selection_list =   [NewState#state.parsed | State#state.selection_list]}}

    end.

%%%---------------------------------------------------------------------
%%% Parse 0 or 1 element.
%%%---------------------------------------------------------------------
pMaybe( P ) -> 
    fun(State)->
	    case P(State) of
		{fail, #state{input=[]}=_NewState} -> % when receiving fail, however input is empty -> its ok
		    {ok, State};

		{fail, _NewState} ->
		    {incomplete, State};

		{fail_number,  #state{input=[]}=_NewState} -> % when receiving fail, however input is empty -> its ok
		    {ok, State};

		{fail_number, _NewState} ->
		    {incomplete, State};

		{partially, #state{input=[]}=_NewState} -> % when receiving partially, however input is empty -> its ok
		    {ok, State};

		{partially, _NewState} -> 
		    {incomplete, State};

		{incomplete, NewState} ->
		    {incomplete, NewState};

		{ok, NewState} ->
		    {ok, NewState} 
	    end
    end.

pWhiteSpace() ->
    fun(State) ->
	    match_whitespace(State)
    end.

match_whitespace(State ) -> 
    match_whitespace(State, []).

match_whitespace( #state{input=[]}=State, []) ->
    NewState = State#state{parsed = [], completion=" "},
    {partially, NewState};

match_whitespace( #state{input=[]}=State, Acc) ->
    NewState = State#state{parsed = lists:reverse(Acc)},
    {ok, NewState};

match_whitespace(#state{input=[I|Is]}=State, Acc) when I== $\s -> % Test (I: Input) against Space " "
    NewState = State#state{input= Is},
    match_whitespace(NewState, [$\s |Acc]);

match_whitespace(#state{input=Input}=State, Acc) when size(Acc) > 0, length(Input) == 0 -> 
    NewState = State#state {parsed = lists:reverse(Acc)},
    {ok, NewState};

match_whitespace(State, Acc) when length(Acc) > 0 -> 
    NewState = State#state {parsed = lists:reverse(Acc)},
    {incomplete, NewState};

match_whitespace(State, []) -> 
    NewState = State#state {parsed = []},
    {fail, NewState}.

pNumberWhiteSpace() ->
    fun(State) ->
	    match_number_whitespace(State)
    end.

match_number_whitespace(State ) -> 
    match_number_whitespace(State, []).

match_number_whitespace( #state{input=[]}=State, []) ->
    NewState = State#state{parsed = []},
    {fail_number, NewState};

match_number_whitespace( #state{input=[]}=State, Acc) ->
    NewState = State#state{parsed = lists:reverse(Acc)},
    {ok, NewState};

match_number_whitespace(#state{input=[I|Is]}=State, Acc) when I== $\s -> % Test (I: Input) against Space " "
    NewState = State#state{input= Is},
    match_number_whitespace(NewState, [$\s |Acc]);

match_number_whitespace(#state{input=Input}=State, Acc) when size(Acc) > 0, length(Input) == 0 -> 
    NewState = State#state {parsed = lists:reverse(Acc)},
    {ok, NewState};

match_number_whitespace(State, Acc) when length(Acc) > 0 -> 
    NewState = State#state {parsed = lists:reverse(Acc)},
    {incomplete, NewState};

match_number_whitespace(State, []) -> 
    NewState = State#state {parsed = []},
    {fail, NewState}.


