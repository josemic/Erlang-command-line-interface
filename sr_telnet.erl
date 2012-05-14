-module(sr_telnet).
-export([server/1, server/2, s/0, wait_connect/2, get_request/2, print_process/1]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").

%%% Records


						% state of the prompt:
-record(status, {position = 0            ::integer(),   % curser position, 
		 configuration_path     ,% queue:new() set during initialization
		 node = view_node        ::atom(),
		 index= []               ::[[integer()]],
		 port = undefined        ::port(),
		 history_buffer          ,% queue:new() set during initialization
		 history_index = 0       ::integer(),
		 history_depth = 100     ::integer(),
		 interface = undefined   ::string(),
		 insertion_mode = insert ::insert|delete,
		 window_height   = 24    ::integer,  % Default value, in case no Window size negotiation occurs
		 window_width    = 80    ::integer,   % Default value, in case no Window size negotiation occurs 
		 print_process           :: pid()     % set during initialization
		}).

s()->
    server(1025).

%% Workaround for starting from makefile.
server([PortAtom]) when is_atom(PortAtom)->
    server(list_to_integer(atom_to_list(PortAtom)));

server([PortAtom, FilenameAtom]) when is_atom(PortAtom), is_atom(FilenameAtom)->
    server(list_to_integer(atom_to_list(PortAtom)), atom_to_list(FilenameAtom));

%% Start of program
server(PortNr)  when is_integer(PortNr) ->
    server(PortNr, "").

server(PortNr, Filename) ->
    io:format("Listening on Port: ~p for telnet console~n~n",[PortNr]),
    %% initialize server dara
    ets:new(server_data_table,[ordered_set, named_table, public]),
    ets:insert(server_data_table, {hostname, "ErlangCLI"}),
    %% register nodes
    sr_telnet_registration:start(),
    %% load configuration from file
    if
	Filename /= "" ->
	    io:format("~nReading configuration file:\"~s\"~n",[Filename]),
	    case sr_read_file:execute_file_commands(vty_direct, Filename) of
		ok -> 
		    io:format("Reading configuration successfull!!~n~n"),
		    {ok, ListenSocket} = gen_tcp:listen(PortNr, [binary, {active, false}, {reuseaddr,true}]),
		    wait_connect(ListenSocket,0);
		{error, Error} ->
		    io:format("Failed with error: ~s~n~n",[Error])
	    end;
	true ->
	    {ok, ListenSocket} = gen_tcp:listen(PortNr, [binary, {active, false}, {reuseaddr,true}]),
	    wait_connect(ListenSocket,0)
    end.


wait_connect(ListenSocket, Count) when is_port(ListenSocket),is_integer(Count)->
    io:format("Wait connect: ~p~n",[Count]),
    {ok,Socket} = gen_tcp:accept(ListenSocket),
    PID = spawn_link(?MODULE, get_request, [Socket,Count]),  
    gen_tcp:controlling_process(Socket, PID),
    wait_connect(ListenSocket, Count).
%%spawn_link(?MODULE, wait_connect, [ListenSocket,Count+1]),  
%%get_request(Socket, Count).

%% try get_request(Socket, Count) of
%% 	Val ->
%% 	    {normal, Val}
%% catch
%% 	error:Error ->
%% 	    gen_tcp:close(Socket),
%% 	    io:format("Backtrace ~p~n", [erlang:get_stacktrace()]),
%% 	    {error,Error}
%% end.



print_process(Socket)->
    receive	
	{output, List} when is_list(List) ->
	    gen_tcp:send(Socket, List),
	    print_process(Socket);
	{'EXIT',_PID, normal} ->
	    ok;
	Other ->
	    io:format("Received message, this should not occur!!! ~p", [Other]),
	    print_process(Socket)
    end.

get_request(Socket, Count)  when is_port(Socket),is_integer(Count) ->
    PrintProcessPID = spawn_link(?MODULE, print_process, [Socket]), 
    %% set default values for window size
    %% set curser position in prompt
    Status = #status{configuration_path = queue:new(),
		     history_buffer = queue_new(),
		     print_process = PrintProcessPID}, 
    gen_tcp:send(Socket, <<?IAC, ?WILL, ?ECHO>>),
    gen_tcp:send(Socket, <<?IAC, ?WILL, ?SUPPRESS_GO_AHEAD>>),
    gen_tcp:send(Socket, <<?IAC, ?DONT, ?LINE_MODE>>),
    gen_tcp:send(Socket, <<?IAC, ?DO,   ?WINDOW_SIZE>>), % Do window size negotiation
    gen_tcp:send(Socket,<<?CR, ?LF>>),
    gen_tcp:send(Socket, 
		 <<"Welcome to Erlang commandline interface.", ?LF,?CR,
		   "Copyright(C) 2012 Michael Josenhans", ?LF,?CR,
		   "License AGPL v3+: GNU AGPL version 3 or later <http://www.gnu.org/licenses/agpl-3.0.html>", ?LF, ?CR,
		   ?LF,?CR,
		   "This is free software: you are free to change and redistribute it.", ?LF,?CR,
		   "There is NO WARRANTY, to the extent permitted by law.", ?LF,?CR>>),
    gen_tcp:send(Socket,get_prompt(Status)),
    do_recv(Socket, Status, <<>>).
%%gen_tcp:close(Socket).

do_recv(Socket, Status, CommandBufferBin) when is_port(Socket), is_record(Status,status), is_binary(CommandBufferBin) ->
    inet:setopts(Socket, [{active,once}, {keepalive,true}]), % active,once -> use flow control
    receive 
	{tcp, Socket, BytesBin} ->
	    io:format("Received: ~p~n", [BytesBin]),
	    {BytesFilteredBin, FilteredStatus} = filter_negotiation_messages(BytesBin, <<>>, Status),
	    io:format("Filtered: ~p~n",[BytesFilteredBin]),
	    %% handle navigation key like backspace, ...

	    case navigate_in_buffer(BytesFilteredBin, CommandBufferBin, FilteredStatus, Socket) of
		exit -> 
		    true;
		{NewCommandBufferBin, NewStatus} ->
		    io:format("Position: ~p, Buffer: ~p~n",[NewStatus#status.position, NewCommandBufferBin]),
		    do_recv(Socket, NewStatus, NewCommandBufferBin)
	    end;
	{tcp_closed, Socket} -> 
	    gen_tcp:close(Socket),
	    {ok, CommandBufferBin};
	{tcp_error, Socket, Reason} ->
	    io:format("Socket error ~p: ~p~n", [Socket, Reason]),
	    gen_tcp:close(Socket),
	    {error, {telnet, Reason}};
	{'EXIT',_PID, normal} ->
	    do_recv(Socket, Status, CommandBufferBin);
	Other ->
	    io:format("Received message, this should not occur!!! ~p", [Other]),
	    do_recv(Socket, Status, CommandBufferBin)
    end.

navigate_in_buffer(<<>>, Acc, Status, Socket) when is_binary(Acc), is_record(Status, status), is_port(Socket)->
    {Acc, Status};

navigate_in_buffer(BytesBin, Acc, Status, Socket) when is_binary(BytesBin), is_binary(Acc), is_record(Status, status),is_port(Socket)->
    case BytesBin of 
	%% Backspace-key
	<<127, Remain/binary>> when Status#status.position > 0->
	    gen_tcp:send(Socket, <<?BS>>),
	    SecondPartLen = byte_size(Acc)- Status#status.position,
	    SecondPart = binary:part(Acc, Status#status.position, SecondPartLen),
	    io:format("~p, ~p, ~p ~n",[SecondPart, Status#status.position,SecondPartLen]),
	    gen_tcp:send(Socket,<<SecondPart/binary," ">>),
	    gen_tcp:send(Socket,n_times((SecondPartLen+1), <<?BS>>)),
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position-1))/binary, (binary:part(Acc, Status#status.position, byte_size(Acc)- Status#status.position))/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status#status{position = Status#status.position-1}, Socket);
	<<127, Remain/binary>> when Status#status.position == 0->
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);
	%% DEL-key
	<<"\e[3~", Remain/binary>>  when  byte_size(Acc) > Status#status.position->
	    SecondPartLen = byte_size(Acc)- (Status#status.position+1),
	    SecondPart = binary:part(Acc, Status#status.position+1, SecondPartLen),
	    io:format("~p, ~p, ~p ~n",[SecondPart, Status#status.position+1, SecondPartLen]),
	    gen_tcp:send(Socket,<<SecondPart/binary," ">>),
	    gen_tcp:send(Socket,n_times((SecondPartLen+1), <<?BS>>)),
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position))/binary, (binary:part(Acc, Status#status.position+1, byte_size(Acc)- (Status#status.position+1)))/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status, Socket);
	%% DEL-key
	<<"\e[3~", Remain/binary>>  when  byte_size(Acc) == Status#status.position->
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);
	%% left-key
	<<"\e[D", Remain/binary>>  when Status#status.position > 0->
	    gen_tcp:send(Socket,<<"\e[D">>),
	    %% delete all whitespaces right to the curser position
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position-1))/binary, 
		       (binary:list_to_bin(string:strip(binary:bin_to_list(binary:part(Acc, Status#status.position-1, byte_size(Acc)- (Status#status.position-1))),right)))/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status#status{position = Status#status.position-1}, Socket);
	%% left-key
	<<"\e[D", Remain/binary>>  when Status#status.position == 0->
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);
	%% right-key
	<<"\e[C", Remain/binary>> when  byte_size(Acc) > Status#status.position->
	    gen_tcp:send(Socket,<<"\e[C">>),
	    navigate_in_buffer(Remain, Acc, Status#status{position = Status#status.position+1}, Socket);
	%% right-key
	<<"\e[C", Remain/binary>> when  byte_size(Acc) == Status#status.position->
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);
	%% Pos1-Key
	<<"\eOH", Remain/binary>>->
	    gen_tcp:send(Socket,n_times((Status#status.position), <<?BS>>)),
	    navigate_in_buffer(Remain, Acc, Status#status{position=0}, Socket);
	%% End-key
	<<"\eOF", Remain/binary>> ->
	    SecondPartLen = byte_size(Acc)- Status#status.position,
	    SecondPart = binary:part(Acc, Status#status.position, SecondPartLen),
	    io:format("~p, ~p, ~p ~n",[SecondPart, Status#status.position,SecondPartLen]),
 	    gen_tcp:send(Socket,<<SecondPart/binary>>),
	    navigate_in_buffer(Remain, Acc, Status#status{position=byte_size(Acc)}, Socket);

	%% Tab-key: Command completion, when position is last character of line
	<<"\t", Remain/binary>> when byte_size(Acc) == Status#status.position ->
	    io:format("Commandline buffer: '~p'~n",[Acc]),
	    Partially_fun = fun(X)->case X of {partially, _State} -> true;_-> false end end,
	    Matchlist = sr_telnet_registration:test_commandstring(Status#status.node, binary:bin_to_list(Acc), Partially_fun, provide_hidden),
	    io:format("Matchlist : ~p~n", [Matchlist]),
	    Set_of_Completions = sr_telnet_registration:get_completion_list(Matchlist),
	    io:format("Set_of_Completions : ~p~n", [Set_of_Completions]),
	    case length(Set_of_Completions) of
		%% no completions
		0 -> gen_tcp:send(Socket,<<?BEL>>), 
		     navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc)}, Socket);
		%% exactly one completion
		1 -> [TabCompletion] = ordsets:to_list(Set_of_Completions),
		     TabCompletionBin = binary:list_to_bin(TabCompletion),
		     io:format("Tab-Completion ~p~n",[TabCompletionBin]),
		     TabCompletionBinLen = byte_size(TabCompletionBin),
		     io:format("~p, ~p, ~p ~n",[TabCompletionBin, Status#status.position, TabCompletionBinLen]),
		     gen_tcp:send(Socket,<<TabCompletionBin/binary>>),
		     NewAcc = <<Acc/binary, TabCompletionBin/binary>>,
		     navigate_in_buffer(Remain, NewAcc, Status#status{position = byte_size(NewAcc)}, Socket);
		%% multiple completions.
		_ -> Set_of_Options = sr_telnet_registration:get_option_list(Matchlist),
		     io:format("Options: ~p~n",[Set_of_Options]),
		     gen_tcp:send(Socket,<<?CR>>),
		     gen_tcp:send(Socket,<<?LF>>), 
		     print_options(ordsets:to_list(Set_of_Options), Status#status.window_width, Socket),
		     gen_tcp:send(Socket,<<?CR>>),
		     gen_tcp:send(Socket,<<?LF>>),
		     gen_tcp:send(Socket,get_prompt(Status)),
						%SecondPartLen = byte_size(Acc)- Status#status.position,
		     gen_tcp:send(Socket,<<Acc/binary>>),
						%gen_tcp:send(Socket,n_times((SecondPartLen), <<?BS>>)),
		     navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc)}, Socket)
	    end;


	%% Tab-key: Ignore tab, when position is not last character of line
	<<"\t", Remain/binary>> ->
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);

	%% Return-key: Execute the command
	<<?CR, Remain/binary>> ->
	    io:format("Commandline buffer: '~p'~n",[Acc]),
	    Ok_fun = fun(X)->case X of {ok, _State} -> true;_-> false end end,
	    Matchlist = sr_telnet_registration:test_commandstring(Status#status.node, string:strip(binary:bin_to_list(Acc), right), Ok_fun, provide_hidden),
	    io:format("Matchlist : ~p~n", [Matchlist]),
	    Set_of_Matching_Commands = sr_telnet_registration:get_command_execution_list(Matchlist),
	    io:format("Set_of_Matching_Commands : ~p~n", [Set_of_Matching_Commands]),
	    case length(Set_of_Matching_Commands) of
		%% no matching commands, thus look at the failed commands and determine the longest failed command to put the indicator '^'
		0 ->  Partially_fun = fun(X)->case X of {partially, _State} -> true;_-> false end end,
		      PartiallyMatchlist = sr_telnet_registration:test_commandstring(Status#status.node, binary:bin_to_list(Acc), Partially_fun,provide_hidden),
		      io:format("PartiallyMatchlist : ~p~n", [PartiallyMatchlist]),
		      case length(PartiallyMatchlist) of
			  0 -> Fail_fun = fun(X)->case X of {fail, _State} -> true;_-> false end end,
			       FailMatchlist = sr_telnet_registration:test_commandstring(Status#status.node, binary:bin_to_list(Acc), Fail_fun, provide_hidden),
			       io:format("FailMatchlist : ~p~n", [FailMatchlist]),
			       FailMatchlist_mapped = lists:map(fun(H)-> {fail,#state{parsed_list=A}}=H,io:format("A: ~p~n", [A]), A end, FailMatchlist), 
			       io:format("FailMatchlist mappped: ~p~n", [FailMatchlist_mapped]),
			       ErrorPosition=lists:max(lists:map(fun(H)-> string:len(string:join (H, "")) end, FailMatchlist_mapped)), 
			       gen_tcp:send(Socket,<<?BEL>>), 
			       gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>),
		 	       gen_tcp:send(Socket,get_prompt(Status)),
			       gen_tcp:send(Socket,n_times((ErrorPosition), <<" ">>)), 
			       gen_tcp:send(Socket,<<"^">>),
			       gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>),
			       gen_tcp:send(Socket,<<"% Invald input detected at '^' marker">>),
			       gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>),
			       gen_tcp:send(Socket,get_prompt(Status)),
			       gen_tcp:send(Socket,<<Acc/binary>>), 
			       navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc), 
									     history_index = 0,
									     history_buffer = queue_element(binary:list_to_bin(string:strip(binary:bin_to_list(Acc))), Status#status.history_buffer, Status#status.history_depth)}, Socket);

			  _ -> gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>),
			       gen_tcp:send(Socket,<<"% Incomplete command, complete with:">>),
			       Set_of_Options = sr_telnet_registration:get_option_list(PartiallyMatchlist),
			       io:format("Options: ~p~n",[Set_of_Options]),
			       gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>), 
			       print_options(ordsets:to_list(Set_of_Options), Status#status.window_width, Socket),
			       gen_tcp:send(Socket,<<?CR>>),
			       gen_tcp:send(Socket,<<?LF>>),
			       gen_tcp:send(Socket,get_prompt(Status)),
			       gen_tcp:send(Socket,<<Acc/binary>>), 
			       navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc), 
									     history_index = 0, 
									     history_buffer = queue_element(binary:list_to_bin(string:strip(binary:bin_to_list(Acc))), Status#status.history_buffer, Status#status.history_depth) }, Socket)
		      end;
		%% exactly one matching command, thus execute the command with the parameters
		1 -> [{NumberList, SelectionList, StrList, Command}] = ordsets:to_list(Set_of_Matching_Commands),
		     gen_tcp:send(Socket,<<?CR>>),
		     gen_tcp:send(Socket,<<?LF>>),
		     io:format("Command: ~p SelectionList: ~p NumberList: ~p StrList: ~p~n",[Command, SelectionList, NumberList, StrList]),
		     Command_fun = Command#command.funcname,
		     %% Execute the fun 
		     Result = case Command_fun({vty, Status#status.print_process}, #command_param{selection_list = SelectionList, number_list = NumberList, str_list = StrList, index_list = Status#status.index}) of
				  cmd_warning ->
				      gen_tcp:send(Socket,<<?BEL>>),
				      NewStatus1 = Status,
				      cmd_warning;
				  cmd_enable ->
				      NewStatus1 = Status#status{node = enable_node},
				      cmd_success;	 
				  cmd_disable ->
				      NewStatus1 = Status#status{node = view_node},
				      cmd_success;
				  cmd_end ->
				      NewStatus1 = Status#status{
						     configuration_path = queue:new(),
						     node = enable_node, 
						     index = []},
				      cmd_success;
				  cmd_exit ->
				      case queue:is_empty(Status#status.configuration_path) of
					  true ->
					      NewStatus1 = Status,
					      cmd_exit; % Exit the program
					  false ->
					      %% get the last node from the queue
					      {{value,{Node, Index}}, ConfigurationPath} = queue:out_r(Status#status.configuration_path),
					      NewStatus1 = Status#status{
							     configuration_path = ConfigurationPath,
							     node = Node,
							     index = Index},
					      io:format("Index: ~p~n",[Index]),
					      cmd_success
				      end;
				  {cmd_enter_node, NewNode} ->
				      %% put current node in the configuration path queue
				      NewStatus1 = Status#status{
						     configuration_path = queue:in({Status#status.node, Status#status.index}, Status#status.configuration_path),
						     node = NewNode,
						     index = Status#status.index},
				      io:format("Index: ~p~n",[Status#status.index]),
				      cmd_success;
				  {cmd_enter_node, NewNode, NewIndex} ->
				      %% put current node in the configuration path queue
				      NewStatus1 = Status#status{
						     configuration_path = queue:in({Status#status.node, Status#status.index}, Status#status.configuration_path),
						     node = NewNode,
						     index = [NewIndex|Status#status.index]},
				      io:format("Index: ~p~n",[NewStatus1#status.index]),
				      cmd_success;
				  cmd_list ->
				      %% list all commands on the screen
				      CommandList = get_non_hidden_command_list(Status#status.node),
				      Fun = fun(A,B) -> A < B end,
				      SortedCommandList = lists:sort(Fun, CommandList),
				      print_list_to_telnet_console(Socket, SortedCommandList),
						%print_command_list(Socket, Status#status.node),
				      NewStatus1 = Status,
				      cmd_success;
				  cmd_history ->
				      %% display commandline history on the screen
				      CommandHistoryList = generate_commandline_history_list(Status#status.history_buffer),
				      print_list_to_telnet_console(Socket, CommandHistoryList),
						%print_commandline_history(Socket, Status#status.history_buffer),
				      NewStatus1 = Status,
				      cmd_success;
				  cmd_success ->
				      NewStatus1 = Status,
				      cmd_success;
				  _ ->
				      NewStatus1 = Status,
				      cmd_unknown

			      end,
		     %% make sure the output from the fun comes before the prompt below.
		     timer:sleep(10), 
		     case Result of
			 cmd_exit ->
			     exit; 
			 cmd_success ->
			     gen_tcp:send(Socket,<<?CR>>),
			     gen_tcp:send(Socket,<<?LF>>),
			     NewAcc = <<"">>,
			     %% put entered command into histroy buffer
			     NewStatus =NewStatus1#status{
					  history_buffer = queue_element(binary:list_to_bin(string:strip(binary:bin_to_list(Acc))), NewStatus1#status.history_buffer, NewStatus1#status.history_depth), 
					  history_index = 0,
					  position = 0},	
			     gen_tcp:send(Socket,get_prompt(NewStatus)),
			     gen_tcp:send(Socket,<<NewAcc/binary>>), 
			     navigate_in_buffer(Remain, NewAcc, NewStatus, Socket);

			 cmd_warning ->
			     gen_tcp:send(Socket,<<?CR>>),
			     gen_tcp:send(Socket,<<?LF>>),
			     NewAcc = <<"">>,
			     %% put entered command into histroy buffer
			     NewStatus =NewStatus1#status{
					  history_buffer = queue_element(binary:list_to_bin(string:strip(binary:bin_to_list(Acc))), NewStatus1#status.history_buffer, NewStatus1#status.history_depth), 
					  history_index = 0,
					  position = 0},
			     gen_tcp:send(Socket,<<"Warning error!!!", ?CR, ?LF>>),
			     gen_tcp:send(Socket,<<"Be arware of what you are doing !!!", ?BEL, ?CR, ?LF>>),	
			     gen_tcp:send(Socket,get_prompt(NewStatus)),
			     gen_tcp:send(Socket,<<NewAcc/binary>>), 
			     navigate_in_buffer(Remain, NewAcc, NewStatus, Socket);
			 _ ->
			     gen_tcp:send(Socket,<<?CR>>),
			     gen_tcp:send(Socket,<<?LF>>),
			     NewAcc = <<"">>,
			     %% put entered command into histroy buffer
			     NewStatus =NewStatus1#status{
					  history_buffer = queue_element(binary:list_to_bin(string:strip(binary:bin_to_list(Acc))), NewStatus1#status.history_buffer, NewStatus1#status.history_depth), 
					  history_index = 0,
					  position = 0},
			     gen_tcp:send(Socket,<<"Implementation error!!!", ?CR, ?LF>>),
			     gen_tcp:send(Socket,<<"Invald command exit command. This should not occurr !!!", ?BEL, ?CR, ?LF>>),	
			     gen_tcp:send(Socket,get_prompt(NewStatus)),
			     gen_tcp:send(Socket,<<NewAcc/binary>>), 
			     navigate_in_buffer(Remain, NewAcc, NewStatus, Socket)
		     end;
		%% multiple matching commands, this is an error to register more than one command matching at the same time. 
		%% Thus list the errorous commands.
		_ -> io:format("Ambigous commands: ~p~n",[Acc]),
		     gen_tcp:send(Socket,<<?CR>>),
		     gen_tcp:send(Socket,<<?LF>>),
		     gen_tcp:send(Socket,<<"% Ambigous commands registered, this should not occur!!">>),
		     gen_tcp:send(Socket,<<?CR>>),
		     gen_tcp:send(Socket,<<?LF>>),
		     gen_tcp:send(Socket,get_prompt(Status)),
		     SecondPartLen = byte_size(Acc)- Status#status.position,
		     gen_tcp:send(Socket,<<Acc/binary>>),
		     gen_tcp:send(Socket,n_times((SecondPartLen), <<?BS>>)),
		     navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc), history_index = 0}, Socket)
	    end;

	%% Insert-key 
	<<"\e[2~", Remain/binary>> when Status#status.insertion_mode == insert ->
	    navigate_in_buffer(Remain, Acc, Status#status{insertion_mode = delete}, Socket);

	%% Insert-key
	<<"\e[2~", Remain/binary>> when Status#status.insertion_mode == delete ->
	    navigate_in_buffer(Remain, Acc, Status#status{insertion_mode = insert}, Socket);

	%% Page-Up-key ignore
	<<"\e[5~", Remain/binary>> ->
	    navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc)}, Socket);

	%% Page-Down-key ignore
	<<"\e[6~", Remain/binary>> ->
	    navigate_in_buffer(Remain, Acc, Status#status{position = byte_size(Acc)}, Socket);

	%% Up-key ignore
	<<"\e[A", Remain/binary>> ->
	    gen_tcp:send(Socket,n_times(Status#status.position, <<?BS>>)),
	    gen_tcp:send(Socket,n_times(byte_size(Acc), <<" ">>)),
	    gen_tcp:send(Socket,n_times(byte_size(Acc), <<?BS>>)),
	    case get_nth_element(Status#status.history_index,Status#status.history_buffer ) of 
		empty ->
		    NewAcc = <<"">>;
		{value, NewAcc} ->
		    ok
	    end,
	    gen_tcp:send(Socket,<<NewAcc/binary>>),
	    NewStatus = Status#status{history_index = min(Status#status.history_index+1, queue_len(Status#status.history_buffer))},
	    navigate_in_buffer(Remain, NewAcc, NewStatus#status{position = byte_size(NewAcc)}, Socket);

	%% Down-key ignore
	<<"\e[B", Remain/binary>> ->
	    gen_tcp:send(Socket,n_times(Status#status.position, <<?BS>>)),
	    gen_tcp:send(Socket,n_times(byte_size(Acc), <<" ">>)),
	    gen_tcp:send(Socket,n_times(byte_size(Acc), <<?BS>>)),
	    case get_nth_element(Status#status.history_index,Status#status.history_buffer ) of 
		empty ->
		    NewAcc = <<"">>;
		{value, NewAcc} ->
		    ok
	    end,
	    gen_tcp:send(Socket,<<NewAcc/binary>>),
	    NewStatus = Status#status{history_index = max(Status#status.history_index-1, 0)},
	    navigate_in_buffer(Remain, NewAcc, NewStatus#status{position = byte_size(NewAcc)}, Socket);

	%% ?-key
	<<"?", Remain/binary>> ->
	    %%io:format("SpaceBefore~n"),
	    Ok_or_partially_fun = fun(X)->case X of {ok, _State} -> true;{partially, _State} -> true;_-> false end end,
	    Matchlist = sr_telnet_registration:test_commandstring(Status#status.node, string:strip(binary:bin_to_list(Acc)), Ok_or_partially_fun, filter_hidden),
	    io:format("Matchlist : ~p~n", [Matchlist]),
	    Set_of_Commands = sr_telnet_registration:get_command_list(Matchlist),
	    io:format("Set_of_Commands : ~p~n", [Set_of_Commands]),
	    MatchedElements = length(string:tokens(string:strip(binary:bin_to_list(Acc)), " ")),
	    gen_tcp:send(Socket,<<?CR>>),
	    gen_tcp:send(Socket,<<?LF>>), 
	    %% check for space before curser position
	    ElementOffset = case (Status#status.position > 0) of % and (binary_part(Acc, (Status#status.position-1),1) == <<" ">>) of
				true  -> 
				    case (binary_part(Acc, (Status#status.position-1),1) ==  <<" ">>) of
					true -> 1;
					false -> 0
				    end;
				false -> 0
			    end,
	    Command_help_List = generate_command_help_list(Set_of_Commands, max(MatchedElements+ElementOffset,1)),
	    io:format("Command_help_List: ~p~n",[Command_help_List]),
	    print_command_help_list(Command_help_List, Socket),
	    gen_tcp:send(Socket,<<?CR>>),
	    gen_tcp:send(Socket,<<?LF>>),
	    gen_tcp:send(Socket,get_prompt(Status)),
	    SecondPartLen = byte_size(Acc)- Status#status.position,
	    gen_tcp:send(Socket,<<Acc/binary>>),
	    gen_tcp:send(Socket,n_times((SecondPartLen), <<?BS>>)),
	    navigate_in_buffer(Remain, Acc, Status , Socket);

	%% Ignore characters with Bit 7 set or smaller than 32 
	%% (hope thats the right approach to filter-out multi-byte characters)  

	<<26, Remain/binary>> -> % control-z is same as 'exit' command.
	    case queue:is_empty(Status#status.configuration_path) of
		true ->
		    exit; % Exit the program
		false ->
		    %% get the last node from the queue
		    {{value,Node}, ConfigurationPath} = queue:out_r(Status#status.configuration_path),
		    NewStatus = Status#status{
				  configuration_path = ConfigurationPath,
				  node = Node, 
				  history_index = 0,
				  position = 0},
		    receive_output(Socket), % receive characters for 100 ms
		    gen_tcp:send(Socket,<<?CR>>),
		    gen_tcp:send(Socket,<<?LF>>),
		    NewAcc = <<"">>,	
		    gen_tcp:send(Socket,get_prompt(NewStatus)),
		    gen_tcp:send(Socket,<<NewAcc/binary>>), 
		    navigate_in_buffer(Remain, NewAcc, NewStatus, Socket)
	    end;

	<<0, Remain/binary>> -> % Ignore ASCII character 0x00, no bell
	    navigate_in_buffer(Remain, Acc, Status, Socket);

	<<NewChar, Remain/binary>> when  NewChar < 32;  NewChar >= 128 ->
	    io:format("ErrorusNewChar:~p~n ",[NewChar]),
	    gen_tcp:send(Socket,<<?BEL>>),
	    navigate_in_buffer(Remain, Acc, Status, Socket);

	<<NewChar, Remain/binary>> when  byte_size(Acc) == Status#status.position->
	    gen_tcp:send(Socket,<<NewChar>>),
	    SecondPart = <<"">>,
	    io:format("Second part: ~p~n",[SecondPart]),
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position))/binary, NewChar, SecondPart/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status#status{position= Status#status.position+1}, Socket);

        %% overwrite mode
	<<NewChar, Remain/binary>> when  byte_size(Acc) > Status#status.position, Status#status.insertion_mode == delete ->
	    gen_tcp:send(Socket,<<NewChar>>),
	    SecondPart = binary:part(Acc, Status#status.position+1, byte_size(Acc)- (Status#status.position+1)),
	    io:format("Second part: ~p~n",[SecondPart]),
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position))/binary, NewChar, SecondPart/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status#status{position=Status#status.position+1}, Socket);

        %% insert mode
	<<NewChar, Remain/binary>> when  byte_size(Acc) > Status#status.position, Status#status.insertion_mode == insert->
	    gen_tcp:send(Socket,<<NewChar>>),
	    SecondPartLen = byte_size(Acc)- (Status#status.position),
	    SecondPart = binary:part(Acc, Status#status.position, SecondPartLen),
	    io:format("~p, ~p, ~p ~n",[SecondPart, Status#status.position+1, SecondPartLen]),
	    io:format("Second part: ~p~n",[SecondPart]),
	    gen_tcp:send(Socket,<<SecondPart/binary," ">>),
	    gen_tcp:send(Socket,n_times((SecondPartLen+1), <<?BS>>)),
	    NewAcc = <<(binary:part(Acc, 0, Status#status.position))/binary, NewChar, SecondPart/binary>>,
	    navigate_in_buffer(Remain, NewAcc, Status#status{position=Status#status.position+1}, Socket)
    end.



filter_negotiation_messages(<<>>, Acc, Status) when is_binary(Acc)->
    {Acc, Status};
filter_negotiation_messages(BytesBin, Acc, Status) when is_binary(BytesBin),is_binary(Acc)->
    case BytesBin of
	<<?IAC:8, ?SB:8, ?WINDOW_SIZE:8, WIDTH:16, ?IAC:8, HEIGHT:16, ?IAC:8, ?IAC:8, ?SE:8, Remain/binary>>  
	  when HEIGHT==255, WIDTH ==255-> 
	    %% for now hight or width <> 255 is allowed
	    io:format("Negotiate About Window Size: width ~p, height ~p ~n",[WIDTH,HEIGHT]),		    
	    filter_negotiation_messages(Remain, Acc, Status#status{window_width=WIDTH, window_height=HEIGHT});

	<<?IAC:8, ?SB:8, ?WINDOW_SIZE:8, WIDTH:16, HEIGHT:16, ?IAC:8, ?IAC:8, ?SE:8, Remain/binary>> when HEIGHT==255-> 
	    %% for hight = 255 
	    io:format("Negotiate About Window Size: width ~p, height ~p ~n",[WIDTH,HEIGHT]),		    
	    filter_negotiation_messages(Remain, Acc, Status#status{window_width=WIDTH, window_height=HEIGHT});

	<<?IAC:8, ?SB:8, ?WINDOW_SIZE:8, WIDTH:16, ?IAC:8, HEIGHT:16, ?IAC:8, ?SE:8, Remain/binary>> when WIDTH ==255-> 
	    %% for width = 255
	    io:format("Negotiate About Window Size: width ~p, height ~p ~n",[WIDTH,HEIGHT]),		    
	    filter_negotiation_messages(Remain, Acc, Status#status{window_width=WIDTH, window_height=HEIGHT});

	<<?IAC:8, ?SB:8, ?WINDOW_SIZE:8, WIDTH:16, HEIGHT:16, ?IAC:8, ?SE:8, Remain/binary>> -> 
	    %% for now hight or width <> 255 is allowed
	    io:format("Negotiate About Window Size: width ~p, height ~p ~n",[WIDTH,HEIGHT]),		    
	    filter_negotiation_messages(Remain, Acc, Status#status{window_width=WIDTH, window_height=HEIGHT});

	<<?IAC:8, ?DONT:8, Third:8, Remain/binary>> ->
	    io:format("DONT : ~p~n",[option(Third)]),
	    filter_negotiation_messages(Remain, Acc, Status);

	<<?IAC:8, ?DO:8, Third:8, Remain/binary>> ->
	    io:format("DO : ~p~n",[option(Third)]),
	    filter_negotiation_messages(Remain, Acc, Status);

	<<?IAC:8, ?WONT:8, Third:8, Remain/binary>> ->
	    io:format("WONT : ~p~n",[option(Third)]),
	    filter_negotiation_messages(Remain, Acc, Status);

	<<?IAC:8, ?WILL:8, Third:8, Remain/binary>> ->
	    io:format("WILL : ~p~n",[option(Third)]),		    
	    filter_negotiation_messages(Remain, Acc, Status);

	<<NewChar:8, Remain/binary>> ->
	    filter_negotiation_messages(Remain, <<Acc/binary,NewChar>>, Status)
    end.

option(?TRANSMIT_BINARY) -> 'TRANSMIT-BINARY';
option(?ECHO) -> 'ECHO';
option(?SUPPRESS_GO_AHEAD) -> 'SUPPRESS-GO-AHEAD';
option(N) when is_integer(N) -> N.

n_times(N, Value)  when N >= 0, is_binary(Value) ->
    n_times(N, Value, <<>>).

n_times(0, Value, Acc) when is_binary(Value), is_binary(Acc)->
    Acc;

n_times(N, Value, Acc) when N >= 0,is_binary(Value), is_binary(Acc)->
    n_times(N-1, Value, <<Acc/binary,Value/binary>>).

get_prompt(Status)->
    get_name_prompt()++
	get_configuration_level_prompt(Status)++
	get_exec_mode_prompt(Status).

get_name_prompt()->
    ets:lookup_element(server_data_table,hostname, 2).

get_configuration_level_prompt(Status)->
    case ets:lookup(commandTable, Status#status.node) of
	[Node] -> 
	    case Node#node.configuration_level of
		undefined ->
		    "";
		Config_Level -> 
		    "("++Config_Level++")"
	    end
    end.

get_exec_mode_prompt(Status)->
    case ets:lookup(commandTable, Status#status.node) of
	[Node] -> 
	    case Node#node.exec_mode of
		user ->
		    "> ";
		privileged -> 
		    "# "
	    end
    end.


print_options(Option_List, WindowWidth, Socket) -> 
    print_options(Option_List, [], 0, WindowWidth, Socket).

print_options([], _Acc, _Pos, _WindowWidth, _Socket) -> 
    true;

print_options([Head|Tail], Acc, Pos, WindowWidth, Socket) ->
    Offset = 20,
    if 
	(Pos + Offset) > WindowWidth -> 
	    NewPos = Offset,
	    gen_tcp:send(Socket,<<?CR>>),
	    gen_tcp:send(Socket,<<?LF>>),
	    gen_tcp:send(Socket,string:left(Head, max(Offset,length(Head)+1)));
	true ->
	    NewPos = Pos + Offset,
	    gen_tcp:send(Socket,string:left(Head, max(Offset,length(Head)+1)))    
    end,
    print_options(Tail, Acc, NewPos, WindowWidth, Socket).

generate_command_help_list(Command_List, MatchedElements) -> 
    generate_command_help_list(Command_List, [], MatchedElements).

generate_command_help_list([], Acc, _MatchedElements) ->  
    %% remove duplicates 
    ordsets:to_list(ordsets:from_list(Acc));

generate_command_help_list([Head|Tail], Acc,  MatchedElements) ->
    %% match the nth command-string with the nth help-sting%
    %% add info for pressing-carrige return
    generate_command_help_list(Tail, [{lists:nth(MatchedElements, Head#command.cmdstr ++ ["<cr>"]), lists:nth(MatchedElements, Head#command.helpstr ++ [""])}|Acc], MatchedElements).

print_command_help_list([], _Socket) ->
    true;

print_command_help_list([Head|Tail], Socket) ->
    {CmdElementStr,HelpElementStr} = Head,  
    gen_tcp:send(Socket, CmdElementStr),
    gen_tcp:send(Socket," "),
    gen_tcp:send(Socket,n_times(max((20-length(CmdElementStr)),0), <<" ">>)),
    gen_tcp:send(Socket, HelpElementStr),  
    gen_tcp:send(Socket,<<?CR>>),
    gen_tcp:send(Socket,<<?LF>>),	 
    print_command_help_list(Tail, Socket).

receive_output(Socket) ->
    receive
	{output, List} when is_list(List) ->
	    gen_tcp:send(Socket, List),
	    receive_output(Socket);
	Other ->
	    io:format("Received message, this should not occur!! ~p", [Other])
    after
	100 -> 
	    ok
    end.


%%% command history queue

queue_new() ->
    queue:new().

queue_len(Queue) ->
    queue:len(Queue).

queue_element(Element, Queue, MaxLength) ->
    QueueLength = queue:len(Queue), 
    if  
	QueueLength < MaxLength -> 
	    QueueNew = queue:in(Element,Queue);
	true -> 
	    QueueNew1 = queue:in(Element,Queue),
	    {_QueueDump, QueueNew} = queue:split(1, QueueNew1),
	    io:format("QueueNew1: ~p, QueueDump: ~p ~n", [QueueNew1, _QueueDump])
    end,
    QueueNew.

get_nth_element(N, Queue) ->
    io:format("Get_nth_element, N:~p Queue: ~p~n",[N,Queue]),
    {FirstPart,_SecondPart} = queue:split(queue:len(Queue)-N,Queue),
    io:format("First Part: ~p~n", [FirstPart]),
    case queue:out_r(FirstPart) of
	{{value, Nth_element}, _Queue} -> 
	    io:format("value:~p ~n",[Nth_element]),
	    {value, Nth_element};
	{empty, _Queue} ->
	    io:format("empty:~n",[]),
	    empty
    end.


get_non_hidden_command_list(NodeID)->
    %% lookup ets table identifier of given node in commandTable
    [#node{nodeID = NodeID, commandListTableID = NodeTableID}] = ets:lookup(commandTable, NodeID),
    %% convert table of current node to list
    CommandList = sr_telnet_registration:create_command_list(ets:tab2list(NodeTableID)),
    NonHiddenCommandList = lists:filter(fun(X) -> X#command.hidden /= yes end, CommandList),
    lists:map(fun(X) -> string:join(X#command.cmdstr, " ") end, NonHiddenCommandList).

generate_commandline_history_list(Queue) ->
    Commandline_history_list = queue:to_list(Queue),
    io:format("Commandline Queue as list: ~p~n",[Commandline_history_list]),
    Commandline_history_list.


print_list_to_telnet_console(_Socket, [])->
    true;

print_list_to_telnet_console(Socket, [Head|Tail]) ->
    gen_tcp:send(Socket, <<"  ">>), 
    gen_tcp:send(Socket, Head), 
    gen_tcp:send(Socket, <<?CR,?LF >>),
    print_list_to_telnet_console(Socket,Tail).

