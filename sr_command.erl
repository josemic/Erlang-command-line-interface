-module(sr_command).
-export([install_all/0, install_default/1, vty_out/2, vty_out/3]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").


install_default(NodeID) ->
    %% Help display function for all node.
    Config_help_fun =  fun (VTY, _Command_param) ->
			       vty_out(VTY,
				       "This VTY provides advanced help features.  When you need help,~n"
				       "anytime at the command line please press '?'.~n"
				       "~n"
				       "If nothing matches, the help list will be empty and you must backup~n"
				       "until entering a '?' shows the available options.~n"
				       "Two styles of help are provided:~n"
				       "1. Full help is available when you are ready to enter a~n"
				       "command argument (e.g. 'show ?') and describes each possible~n"
				       "argument.~n"
				       "2. Partial help is provided when an abbreviated argument is entered~n"
				       "and you want to know what arguments match the input~n"
				       "(e.g. 'show me?'.)~n~n",[]),
			       cmd_success
		       end,

    Config_help_cmd= #command{funcname= Config_help_fun,
			      cmdstr  = ["help"],
			      helpstr = ["Description of the interactive help system"]},

    sr_telnet_registration:install_element([NodeID], Config_help_cmd),

    %% List all functions of this node
    Config_list_fun =  fun (_VTY, _Command_param) ->
			       %% Let main program list the commands of this node,
			       cmd_list
		       end,

    Config_list_cmd= #command{funcname= Config_list_fun,
			      cmdstr  = ["list"],
			      helpstr = ["Print command list"]},

    sr_telnet_registration:install_element([NodeID], Config_list_cmd),

    %% Display commandline history
    Show_history_fun =  fun (_VTY, _Command_param) ->
				%% Let main program display commandline history,
				cmd_history
			end,

    Show_history_cmd= #command{funcname= Show_history_fun,
			       cmdstr  = ["show", "history"],
			       helpstr = [ ?SHOW_STR, 
					   "Display the session command history"]},

    sr_telnet_registration:install_element([NodeID], Show_history_cmd), 

    Write_file_fun =  fun (VTY, Command_param) ->
			      [Filename] = Command_param#command_param.str_list,
			      Result = file:open(Filename, write),
			      case Result of 
				  {ok, IoDevice} ->
				      vty_out(VTY, "%% Writing nodes to file: ~s ~n", [Filename]),
				      write_nodes({file, IoDevice}),
				      file:close(IoDevice),
				      cmd_success;
				  {error, Reason} ->
				      vty_out(VTY, "~nFailed writing to file \"~s\" !!!!! ~n", [Filename]),
				      vty_out(VTY, "Error reason: ~s ~n", [Reason]),
				      cmd_warning 
			      end
		      end,

    Write_file_cmd = 
	#command{funcname = Write_file_fun,
		 cmdstr   = ["write", "file", "FILENAME"],
		 helpstr  = ["Write running configuration to memory, network, or terminal",
			     "Write to configuration file"]},

    sr_telnet_registration:install_element([NodeID], Write_file_cmd),

    Read_file_fun =  fun (VTY, Command_param) ->
			     [Filename] = Command_param#command_param.str_list,
			     io:format("~nReading configuration file:\"~s\"~n",[Filename]),
			     case sr_read_file:execute_file_commands(VTY, Filename) of
				 ok -> 
				     vty_out(VTY, "Reading configuration successfull!!~n~n"),
				     cmd_success;
				 {error, Error} ->
				     vty_out(VTY, "Failed with error: ~s~n~n",[Error]),
				     cmd_warning
			     end
		     end,

    Read_file_cmd = 
	#command{funcname = Read_file_fun,
		 cmdstr   = ["read","file", "FILENAME"],
		 helpstr  = ["Read configuration.",
			     "Read configuration from file"]},

    sr_telnet_registration:install_element([NodeID], Read_file_cmd),

    Write_terminal_fun =  fun (VTY, _Command_param) ->
				  vty_out(VTY, "%% Writing nodes... ~n"),
				  write_nodes(VTY),
				  cmd_success
			  end,

    Write_terminal_cmd = 
	#command{funcname = Write_terminal_fun,
		 cmdstr   = ["write","terminal"],
		 helpstr  = ["Write running configuration to memory, network, or terminal",
			     "Write to terminal"]},

    sr_telnet_registration:install_element([NodeID], Write_terminal_cmd),


    Echo_fun =  fun (VTY, Command_param) ->
			[Str] = Command_param#command_param.str_list,
			vty_out(VTY, "%% ~p ~n",[Str]),
			cmd_success
		end,

    Echo_cmd = 
	#command{funcname = Echo_fun,
		 cmdstr   = ["echo","MESSAGE"],
		 helpstr  = ["Echo a message back to the vty",
			     "The message to echo"]},

    sr_telnet_registration:install_element([NodeID], Echo_cmd)


    %% not implemented yet in sr_command:install_default(NodeID):
    %%sr_telnet_registration:install_element([NodeID], Config_write_terminal_cmd),
    %%sr_telnet_registration:install_element([NodeID], Config_write_file_cmd),
    %%sr_telnet_registration:install_element([NodeID], Config_write_memory_cmd),
    %%sr_telnet_registration:install_element([NodeID], Config_write_cmd),
    %%sr_telnet_registration:install_element([NodeID], Show_running_config_cmd)
	.

install_all()->

    Enable_fun =  fun (_VTY, _Command_param) ->
			  cmd_enable
		  end,

    Enable_cmd = #command{funcname= Enable_fun,
			  cmdstr  = ["enable"],
			  helpstr = ["Enable command"]},

    Disable_fun =  fun (_VTY, _Command_param) ->
			   cmd_disable
		   end,

    Disable_cmd = #command{funcname= Disable_fun,
			   cmdstr  = ["disable"],
			   helpstr = ["Disable command"]},


    sr_telnet_registration:install_node(enable_node, 
					#node_propperties{node_entry_fun = undefined,
							  exec_mode = privileged,
							  configuration_level = undefined}),
    sr_telnet_registration:install_node(view_node, 
					#node_propperties{node_entry_fun = undefined,
							  exec_mode = user,
							  configuration_level= undefined}),

    sr_command:install_default(enable_node),
    sr_command:install_default(view_node),

    sr_telnet_registration:install_element([enable_node], Disable_cmd),
    sr_telnet_registration:install_element([view_node], Enable_cmd),




    %% Install the other nodes:
    sr_config:install(),
    sr_gsmnet:install(), 
    sr_demo:install(). % demo node is within gsmnet_node

vty_out(VTY, String) ->
    vty_out(VTY, String,[]).

vty_out(VTY, StringWithCR, List) ->
    %% Substitute all ?CR with ?CR ?LF for telnet
    FormattedStringWithCR = io_lib:format(StringWithCR, List),
    case VTY of
	{vty, VTY_PID} ->
	    FormattedStringWithCRLF = re:replace(FormattedStringWithCR,binary_to_list(<<?LF>>),binary_to_list(<<?CR>>) ++ binary_to_list(<<?LF>>),[global, {return, list}]), 
	    VTY_PID ! {output, FormattedStringWithCRLF};
	vty_direct ->
	    io:format(FormattedStringWithCR);
	{file, IoDevice} ->
	    file:write(IoDevice, FormattedStringWithCR)
    end.

write_nodes(VTY)->
    FirstNodeKey = ets:first(commandTable),
    write_node(VTY, FirstNodeKey).

write_node(VTY, NodeKey)->
    case NodeKey of 
	'$end_of_table' -> % last registered node already written
	    ok;
	_ ->
	    [Node] = ets:lookup(commandTable, NodeKey),
	    io:format("Node: ~w~n",[Node]),
	    vty_out(VTY,"! NodeID: ~s~n",[Node#node.nodeID]),
            Indention_level = Node#node.indention_level,
 	    case Node#node.node_entry_fun of
 		undefined -> % nothing to write for this node
 		    %% vty_out(VTY,"Not writing node: ~p~n",[Node#node.nodeID]),
 		    ok;
 		_ -> % execute the node's fun
 		    %% vty_out(VTY,"Writing node: ~p~n",[Node#node.nodeID]),
		    vty_out(VTY, "~s", [string:copies(" ", Indention_level)]),
 		    Node_entry_write_fun = Node#node.node_entry_fun,
 		    Node_entry_write_fun(VTY)
 	    end,
	    CommandListTableID = Node#node.commandListTableID,
	    CommandListTable = ets:tab2list(CommandListTableID),
	    %% io:format("CommandListTable: ~p~n",[CommandListTable]),
	    write_elements(VTY, CommandListTable, Indention_level),
	    NextNodeKey = ets:next(commandTable,NodeKey),
	    write_node(VTY, NextNodeKey)
    end.

write_elements(_VTY, [], _Indention_level)->
    ok;

write_elements(VTY, [Head|Tail], Indention_level)->
    {_NodeID, Command} = Head,
    case Command#command.basicwrite of
	undefined -> % nothing to write for this element
	    io:format("Not writing command: ~p~n",[Command#command.cmdstr]),
	    ok;
	_ -> % execute the elements's fun
	    io:format("Writing command: ~p~n",[Command#command.cmdstr]),
            vty_out(VTY, "~s", [string:copies(" ", Indention_level+1)]),
	    BasicWrite_fun = Command#command.basicwrite,
	    BasicWrite_fun(VTY)
    end,
    case Command#command.enhancedwrite of
	undefined -> % nothing to write for this element
	    io:format("Not writing command: ~p~n",[Command#command.cmdstr]),
	    ok;
	_ -> % execute the elements's fun
	    io:format("Writing command: ~p~n",[Command#command.cmdstr]),
	    vty_out(VTY, "~s", [string:copies(" ", Indention_level+1)]),
	    CmdStrStrippedList = commandStringStrip(Command#command.cmdstr),
	    vty_out(VTY, "~s ", [string:join(CmdStrStrippedList," ")]),
	    EnhancedWrite_fun = Command#command.enhancedwrite,
	    EnhancedWrite_fun(VTY)
    end,
    write_elements(VTY, Tail, Indention_level).

commandStringStrip(List)->
    commandStringStrip(List, []).

commandStringStrip([], Acc)->
    lists:reverse(Acc);

commandStringStrip([Head|Tail], Acc)->
    OptionalNumberGuardOpening = string:chr(Head, $[ ), 
    OptionalNumberGuardClosing = string:chr(Head, $] ), 
    MandatoryNumberGuardOpening = string:chr(Head, $< ), 
    MandatoryNumberGuardClosing = string:chr(Head, $> ),    
    SelectionListGuardOpening = string:chr(Head, ${ ), 
    SelectionListGuardClosing = string:chr(Head, $} ),
    StrGuard = (string:to_upper(Head) == Head),
    if 
	((OptionalNumberGuardOpening == 1) and (OptionalNumberGuardClosing == length(Head))) -> % [XXX]: Optional Number 
	    %% Suppress all Optional Numbers and following list elements
	    commandStringStrip([], Acc);
	(( MandatoryNumberGuardOpening == 1) and (MandatoryNumberGuardClosing == length(Head))) -> % <XXX>: Mandatory Number
	    %% Suppress all Mandatory Numbers and following list elements
	    commandStringStrip([], Acc); 	    
	(( SelectionListGuardOpening == 1) and (SelectionListGuardClosing == length(Head))) -> % {XXX|YYY|ZZZ}: Selection list 
  	    %% Suppress all Selection list elements and following list elements
	    commandStringStrip([], Acc); 	 
	StrGuard->
  	    %% Suppress all Capitalized elements and following list elements
	    commandStringStrip([], Acc);
	true -> % XXXX: String
	    %% Keep all other elements and following list elements
	    commandStringStrip(Tail, [Head|Acc])
    end.
