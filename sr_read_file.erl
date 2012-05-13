-module(sr_read_file).
-export([execute_file_commands/1]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").
-record(exe_status, {configuration_path     ,% queue:new() set during initialization
 		     node = config_node        ::atom(),
		     line_number =0            ::integer()
		    }).

execute_file_commands(File) ->
    case file:open(File, [read]) of
	{ok, IoDevice} ->
	    Result = execute_file_command(IoDevice),
	    file:close(IoDevice),
	    Result;

	Error ->
	    Error
    end.

execute_file_command(IoDevice) ->
    Exe_status = #exe_status{configuration_path = queue:new()},
    execute_file_command(IoDevice,  Exe_status).

execute_file_command(IoDevice, Exe_status) ->
   NewExe_status = Exe_status#exe_status{line_number = Exe_status#exe_status.line_number +1},
    case file:read_line(IoDevice) of
	{ok,CommandWithCRLF} ->
	    Command = string:strip(stripCRLF(CommandWithCRLF)),
	    case string:left(Command,1) of
		"!" ->  % comment line, ignore
		    execute_file_command(IoDevice, NewExe_status);
		" " -> % empty line, ignore
		    execute_file_command(IoDevice, NewExe_status);
		_Other -> % command
		    Result = evaluate_command(Command, NewExe_status),
		    case Result of
			{ok,NewResultExe_status} ->
			    execute_file_command(IoDevice, NewResultExe_status);
			{error,Error}  ->
			    {error,Error}
		    end
	    end;
	{error,Error} ->
	    {error,Error};
	eof ->
	    ok
    end.

stripCRLF(String)->
    stripCRLF(String, []).

stripCRLF([Head|Tail], Acc) when Head == ?LF->
    stripCRLF(Tail, Acc);

stripCRLF([Head|Tail], Acc) when Head == ?CR->
    stripCRLF(Tail, Acc);

stripCRLF([], Acc)->
    lists:reverse(Acc);

stripCRLF([Head|Tail], Acc) ->
    stripCRLF(Tail, [Head|Acc]).

evaluate_command(CommandStr, Exe_status) ->
    %% io:format("Execute: ~s~n",[CommandStr]),
    Ok_fun = fun(X)->case X of {ok, _State} -> true;_-> false end end,
    Matchlist = sr_telnet_registration:test_commandstring(Exe_status#exe_status.node, CommandStr, Ok_fun, provide_hidden),
    %% io:format("Matchlist : ~p~n", [Matchlist]),
    Set_of_Matching_Commands = sr_telnet_registration:get_command_execution_list(Matchlist),
    %% io:format("Set_of_Matching_Commands : ~p~n", [Set_of_Matching_Commands]),
    case length(Set_of_Matching_Commands) of
	%% no matching commands, thus look at the failed commands and determine the longest failed command to put the indicator '^'
	0 ->      
	    case queue:is_empty(Exe_status#exe_status.configuration_path) of
		true ->
		    {error, "'Unknown command': \"" ++ CommandStr ++"\"" ++ "at line:" ++ integer_to_list(Exe_status#exe_status.line_number)}; % Exit the program
		false ->
		    %% get the last node from the queue
		    {{value,Node}, ConfigurationPath} = queue:out_r(Exe_status#exe_status.configuration_path),
		    NewExe_status = Exe_status#exe_status{
				      configuration_path = ConfigurationPath,
				      node = Node},
		    evaluate_command(CommandStr, NewExe_status)
	    end;
	%% exactly one matching command, thus execute the command with the parameters
	1 ->     MatchList = ordsets:to_list(Set_of_Matching_Commands), 
		 %% io:format("MatchList:~p~n",[MatchList]),
		 [{NumberList, SelectionList, StrList, Command}] = MatchList,
		 %% io:format("Command: ~p SelectionList: ~p NumberList: ~p StrList: ~p~n",[Command, SelectionList, NumberList, StrList]),
		 Command_fun = Command#command.funcname,
		 %% Execute the fun 
		 case Command_fun({vty, self()}, SelectionList, NumberList, StrList) of
		     cmd_warning ->
			 NewExe_Status = Exe_status,
			 io:format("Warning occured by command ~s !!!!~n: ",[Command]),
			 {ok,NewExe_Status};
		     {cmd_enter_node, NewNode} ->
			 %% put current node in the configuration path queue
			 NewExe_status = Exe_status#exe_status{
					   configuration_path = queue:in(Exe_status#exe_status.node, Exe_status#exe_status.configuration_path),
					   node = NewNode},
			 {ok,NewExe_status};
		     cmd_success ->
			 NewExe_status = Exe_status,
			 {ok,NewExe_status};
		     Other -> 
			 {error, "Command \"" ++ CommandStr ++ "\" exited at line " ++ integer_to_list(Exe_status#exe_status.line_number) ++ " with: " ++ atom_to_list(Other)}

		 end;	
	%% multiple matching commands, this is an error to register more than one command matching at the same time. 
	%% Thus list the errorous commands.
	_ -> 
	    io:format("Ambigous commands: ~p~n",[Set_of_Matching_Commands]),
	    {error, ambigous_command}

    end.



