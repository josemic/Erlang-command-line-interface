-module(sr_telnet_registration).
-export([ start/0, telnet_process/0, create_parser_list/1, test_commandstring/3, 
	  get_command_list/1, get_command_execution_list/1, get_completion_list/1, 
	  get_option_list/1, install_node/2, install_element/2]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").



%%% To be called by srv telnet
start()->
    register(srv_telnet, spawn_link(?MODULE,telnet_process,[])),
    sr_command:install_all(),
    timer:sleep(100).

install_node(NodeID, NodePropperties)->
    srv_telnet ! {install_node, NodeID, NodePropperties}.

install_element(NodeIDList, CommandRecord)->
    srv_telnet ! {install_element, NodeIDList, CommandRecord}.




telnet_process()->
    ets:new(commandTable,[ordered_set, named_table, {keypos, #node.nodeID}]),
    telnet_process_loop().

telnet_process_loop()->
    receive
	{install_node, NodeID, NodePropperties} ->
	    %%io:format("Received install_node: NodeID: ~p, NodeCommand: ~p~n", [NodeID, NodeCommand]),
	    register_node(NodeID, NodePropperties);
	{install_element, NodeIDList, CommandRecord} ->
	    %%io:format("Received install_element: NodeIDList: ~p, CommandRecord: ~p~n", [NodeIDList, CommandRecord]),
	    register_element(NodeIDList, CommandRecord)
    end, 
    telnet_process_loop().




test_commandstring(NodeID, CommandlineString, Match_fun)->
    case ets:lookup(commandTable, NodeID) of
	[#node{nodeID = NodeID, commandListTableID = NodeTableID}] -> 
	    io:format("Table for node: ~p:~n~p~n", [NodeID, ets:tab2list(NodeTableID)]),
	    Extract_Cmdstr_fun = fun(CommandRecord) ->
					 {NodeID, #command{cmdstr = Cmdstr, helpstr = _Helpstr, funcname = _Funcname}=Command}=CommandRecord,
					 Fun = create_parser_list(Cmdstr),
					 Fun(#state{input=CommandlineString, command=Command}) % store command record for later processing.
				 end,
	    %% Examples for match funs
	    %% Ok_fun = fun(X)->case X of {ok, _State} -> true;_-> false end end,
	    %% Fail_number_fun = fun(X)->case X of {fail_number, _State} -> true;_-> false end end,
	    %% Partially_fun = fun(X)->case X of {partially, _State} -> true;_-> false end end,
	    %% Fail_fun = fun(X)->case X of {fail, _State} -> true;_-> false end end,
	    %% Incomplete_fun = fun(X)->case X of {incomplete, _State} -> true;_-> false end end,
	    A = lists:map(Extract_Cmdstr_fun, ets:tab2list(NodeTableID)),
	    lists:filter(Match_fun, A);

	[] -> % should not occur
	    throw({error, {node_unknown, NodeID}})
    end.  

get_command_list(CommandList) -> 
    get_command_list(CommandList, []).

get_command_list([], Acc) -> 
    ordsets:from_list(lists:reverse(Acc));

get_command_list([Head|Tail], Acc) ->
    %% io:format("Head: ~p~n", [Head]),
    {_, State} = Head,
    io:format("State: ~p~n", [State#state.command]),
    get_command_list(Tail, [State#state.command|Acc]).

get_command_execution_list(CommandList) -> 
    get_command_execution_list(CommandList, []).

get_command_execution_list([], Acc) -> 
    ordsets:from_list(lists:reverse(Acc));

get_command_execution_list([Head|Tail], Acc) ->
    %% io:format("Head: ~p~n", [Head]),
    {_, State} = Head,
    io:format("State: ~p~n", [State#state.command]),
    get_command_execution_list(Tail, [{State#state.number_list ,State#state.selection_list, State#state.str_list, State#state.command}|Acc]).

get_completion_list(CommandList) -> 
    get_completion_list(CommandList, []).

get_completion_list([], Acc) -> 
    ordsets:from_list(lists:reverse(Acc));

get_completion_list([Head|Tail], Acc) ->
    %% io:format("Head: ~p~n", [Head]),
    {_, State} = Head,
    get_completion_list(Tail, [State#state.completion|Acc]).

get_option_list(CommandList) -> 
    get_option_list(CommandList, []).

get_option_list([], Acc) -> 
    ordsets:from_list(lists:reverse(Acc));

get_option_list([Head|Tail], Acc) ->
    %% io:format("Head: ~p~n", [Head]),
    {_, State} = Head,
    get_option_list(Tail, [(State#state.parsed ++ State#state.completion)|Acc]).




create_parser_list(String) ->
    create_parser_list(String, true, []).

create_parser_list([], _Start, Acc) ->  
    sr_parser:pAnd(lists:reverse(Acc));

create_parser_list([Head|Tail], Start, Acc) -> % Start flags the first item to ensure that no whitespace is requested before the first item of the list 
    Whitespace = sr_parser:pWhiteSpace(),
    Number = sr_parser:pNumber(), 
    WhitespaceAndNumber = sr_parser:pAnd([Whitespace, Number]),    
    MaybeWhitespaceAndNumber = sr_parser:pMaybe(WhitespaceAndNumber),
    Str = sr_parser:pStr(),       % e.g. password in "set PASSWORD"
    WhitespaceAndStr = sr_parser:pAnd([Whitespace, Str]),
    io:format("Head: ~p~n", [Head]),
    OptionalNumberGuardOpening = string:chr(Head, $[ ), 
    OptionalNumberGuardClosing = string:chr(Head, $] ), 
    MandatoryNumberGuardOpening = string:chr(Head, $< ), 
    MandatoryNumberGuardClosing = string:chr(Head, $> ),    
    SelectionListGuardOpening = string:chr(Head, ${ ), 
    SelectionListGuardClosing = string:chr(Head, $} ),
    StrGuard = (string:to_upper(Head) == Head),
    if 
	((OptionalNumberGuardOpening == 1) and (OptionalNumberGuardClosing == length(Head))) -> % [XXX]: Optional Number 
	    NewAcc = MaybeWhitespaceAndNumber; % optional match for whitespace followed by number
	(( MandatoryNumberGuardOpening == 1) and (MandatoryNumberGuardClosing == length(Head))) -> % <XXX>: Mandatory Number 	    
	    NewAcc = WhitespaceAndNumber; % match for whitespace followed by number
	(( SelectionListGuardOpening == 1) and (SelectionListGuardClosing == length(Head))) -> % {XXX|YYY|ZZZ}: Selection list 
	    SelectionList = string:tokens(string:substr(Head, 2,length(Head)-2), "|"),
	    Fun = fun(X) -> sr_parser:pString(X) end,
	    NewAcc = case Start  of
			 true -> 
			     sr_parser:pOr(lists:map(Fun, SelectionList)); % match for selection list
			 _ ->
			     sr_parser:pAnd([Whitespace, sr_parser:pOr(lists:map(Fun, SelectionList))]) % match for selection list and whitespace
		     end;
	StrGuard->
	    NewAcc = WhitespaceAndStr;

	true -> % XXXX: String
	    NewAcc = case Start  of
			 true -> 
			     sr_parser:pString(Head);
			 _ ->
			     sr_parser:pAnd([Whitespace, sr_parser:pString(Head)])
		     end
    end,
    create_parser_list(Tail, false, [NewAcc|Acc]). 

register_node(NodeID, NodePropperties)-> 
    case ets:lookup(commandTable, NodeID) of
	[#node{nodeID = NodeID}] -> 
	    io:format("Warning!! ~ninstall_node(~w, ...). ~w has already been registered. ~nCommand will be ignored. ~n", [NodeID, NodePropperties]);
	[] ->
	    NodeTableID = ets:new(NodeID,[bag]),
	    Node = #node{prompt=[],
			 write_command =NodePropperties#node_propperties.write_fun,
			 exec_mode =NodePropperties#node_propperties.exec_mode,
			 configuration_level =NodePropperties#node_propperties.configuration_level,
			 commandListTableID = NodeTableID, 
			 nodeID = NodeID},
	    ets:insert(commandTable, Node)
    end.

register_element([], _NodeCommand) -> 
    true;

register_element([NodeIDHead|NodeIDTail],NodeCommand) ->
    %%io:format("Node: ~w~n", [ets:lookup(commandTable, NodeIDHead)]),
    case ets:lookup(commandTable, NodeIDHead) of 
	[ #node{commandListTableID =CommandTableID}] ->
	    ets:insert(CommandTableID, {NodeIDHead, NodeCommand});
	[] ->
	    io:format("Error!! ~ninstall_element([~p], ...). ~w has not been registered. ~nCommand will be ignored. ~n", [NodeIDHead, NodeIDHead])
    end,
    register_element(NodeIDTail, NodeCommand).










