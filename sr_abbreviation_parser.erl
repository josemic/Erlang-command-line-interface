-module(sr_abbreviation_parser).
-export([parse/2, create_parser_list/1, test_commandstring_abbreviated/2]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").


test_commandstring_abbreviated(NodeID, CommandlineString)->
    case ets:lookup(commandTable, NodeID) of
	[#node{nodeID = NodeID, commandListTableID = NodeTableID}] ->
	    CommandList = sr_telnet_registration:create_command_list(ets:tab2list(NodeTableID)),
	    io:format("Table for node: ~p:~n~p~n", [NodeID, CommandList]),
	    MatchListCommandAbbreviated = match_list(CommandlineString, CommandList),
	    io:format("MatchListCommandAbbreviated: ~p~n", [MatchListCommandAbbreviated]),
	    MatchListCommandAbbreviated;

	[] -> % should not occur
	    throw({error, {node_unknown, NodeID}})
    end. 

match_list(MatchString, CommandList) ->
    match_list(MatchString, CommandList, []).

match_list(_MatchString, [], Acc) ->
    lists:reverse(Acc);

match_list(MatchString, [Head|Tail], Acc) ->
    match_list(MatchString, Tail, [match(MatchString, Head)|Acc]).

match(MatchString, Command) ->
    MatchStringStripped = string:strip(MatchString),
    ParseResult = sr_parser2:parse(MatchStringStripped ,Command#command.cmdstr),	
    Ok_or_partially_fun = fun(X)->case X of {ok, _State} -> true;{partially, _State} -> true;_-> false end end,
    Ok_or_partially_status = lists:all(Ok_or_partially_fun, ParseResult),
    Extract_fun = fun(X,Accu)->{_, State} =X, AccuNew = #state{number_list = [State#state.number_list|Accu#state.number_list], selection_list= [State#state.selection_list|Accu#state.selection_list], str_list = [State#state.str_list|Accu#state.str_list]},AccuNew end,
    Combined_state = lists:foldr(Extract_fun, #state{}, ParseResult),
    Not_empty_fun = fun(X)->case X of [] -> false;_-> true end end,
    Fun_flatten = fun([X])  -> X end,
    Number_list = lists:map(Fun_flatten, lists:filter(Not_empty_fun, Combined_state#state.number_list)),
    Selection_list = lists:map(Fun_flatten, lists:filter(Not_empty_fun, Combined_state#state.selection_list)),
    Str_list = lists:map(Fun_flatten, lists:filter(Not_empty_fun, Combined_state#state.str_list)),
    {Ok_or_partially_status, Number_list, Selection_list, Str_list, Command}.
    
parse(MatchString, ParseList) ->
    ParserList = create_parser_list(ParseList),
    %% io:format("ParserList: ~p~n",[ParserList]),
    Matchlist = string:tokens(string:strip(MatchString), " "),
    %% io:format("Matchlist: ~p~n",[Matchlist]),
    zipwithspecial(fun(X, Y) -> X(#state{input=Y}) end, ParserList, Matchlist).

zipwithspecial(F, [], [_Y | Ys]) -> [{incomplete,#state{input=""}} | zipwithspecial(F, [], Ys)];
zipwithspecial(F, [X | Xs], []) -> [F(X, "") | zipwithspecial(F, Xs, [])];
zipwithspecial(F, [X | Xs], [Y | Ys]) -> [F(X, Y) | zipwithspecial(F, Xs, Ys)];
zipwithspecial(F, [], []) when is_function(F, 2) -> [].

create_parser_list(ParseList) ->  
    create_parser_list(ParseList, []).

create_parser_list([], Acc) ->  
    lists:reverse(Acc);

create_parser_list([Head|Tail], Acc) ->
    Number = sr_parser:pNumber(), 
    MaybeNumber = sr_parser:pMaybe(Number),
    Str = sr_parser:pStr(),       % e.g. password in "set PASSWORD"
    OptionalNumberGuardOpening = string:chr(Head, $[ ), 
    OptionalNumberGuardClosing = string:chr(Head, $] ), 
    MandatoryNumberGuardOpening = string:chr(Head, $< ), 
    MandatoryNumberGuardClosing = string:chr(Head, $> ),    
    SelectionListGuardOpening = string:chr(Head, ${ ), 
    SelectionListGuardClosing = string:chr(Head, $} ),
    StrGuard = (string:to_upper(Head) == Head),
    if 
	((OptionalNumberGuardOpening == 1) and (OptionalNumberGuardClosing == length(Head))) -> % [XXX]: Optional Number 
	    NewAcc = MaybeNumber; % optional match for empty or number
	(( MandatoryNumberGuardOpening == 1) and (MandatoryNumberGuardClosing == length(Head))) -> % <XXX>: Mandatory Number 	    
	    NewAcc = Number; % match for number
	(( SelectionListGuardOpening == 1) and (SelectionListGuardClosing == length(Head))) -> % {XXX|YYY|ZZZ}: Selection list 
	    SelectionList = string:tokens(string:substr(Head, 2,length(Head)-2), "|"),
	    Fun = fun(X) -> sr_parser:pString(X) end,
	    NewAcc = sr_parser:pOr(lists:map(Fun, SelectionList)); % match for selection list
	StrGuard->
	    NewAcc = Str;
	true -> 
	    NewAcc =sr_parser:pString(Head)
    end,
    create_parser_list(Tail, [NewAcc|Acc]). 
