-module(sr_command).
-export([install_all/0, install_default/1, vty_out/2, vty_out/3]).
-include("sr_command.hrl").
-include("sr_telnet.hrl").


install_default(NodeID) ->
    %% Help display function for all node.
    Config_help_fun =  fun (VTY_PID, _SelectionList, _NumberList, _StrList) ->
			       vty_out(VTY_PID,
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
    Config_list_fun =  fun (_VTY_PID, _SelectionList, _NumberList, _StrList) ->
			       %% Let main program list the commands of this node,
			       cmd_list
		       end,

    Config_list_cmd= #command{funcname= Config_list_fun,
			      cmdstr  = ["list"],
			      helpstr = ["Print command list"]},

    sr_telnet_registration:install_element([NodeID], Config_list_cmd),

    %% Display commandline history
    Show_history_fun =  fun (_VTY_PID, _SelectionList, _NumberList, _StrList) ->
				%% Let main program display commandline history,
				cmd_history
			end,

    Show_history_cmd= #command{funcname= Show_history_fun,
			       cmdstr  = ["show", "history"],
			       helpstr = [ ?SHOW_STR, 
					   "Display the session command history"]},

    sr_telnet_registration:install_element([NodeID], Show_history_cmd), 

    Exit_fun =  fun (VTY_PID, _SelectionList, _NumberList, _StrList) ->
			vty_out(VTY_PID, "%% Exiting... ~n", []),
			cmd_exit
		end,

    Exit_cmd = #command{funcname= Exit_fun,
			cmdstr  = ["exit"],
			helpstr = ["Exit command"]},

    sr_telnet_registration:install_element([NodeID], Exit_cmd),


    Write_terminal_fun =  fun (VTY_PID, _SelectionList, _NumberList, _StrList) ->
				  vty_out(VTY_PID, "%% Writing nodes... ~n"),
				  write_nodes(VTY_PID),
				  cmd_success
			  end,

    Write_terminal_cmd = 
	#command{funcname = Write_terminal_fun,
		 cmdstr   = ["write","terminal"],
		 helpstr  = ["Write running configuration to memory, network, or terminal",
			     "Write to terminal"]},

    sr_telnet_registration:install_element([NodeID], Write_terminal_cmd),

    Config_end_fun =  fun (_VTY_PID, _SelectionList, _NumberList, _StrList) ->
			      cmd_end
		      end,

    Config_end_cmd = 
	#command{funcname = Config_end_fun,
		 cmdstr   = ["end"],
		 helpstr  = ["End current mode and change to enable mode."]},

    case NodeID of
	enable_node ->
	    ok; % Command not supported
	view_node ->
	    ok; % Command not supported
	_ ->
	    sr_telnet_registration:install_element([NodeID], Config_end_cmd)
    end,

    Echo_fun =  fun (VTY_PID, _SelectionList, _NumberList, StrList) ->
			[Str] = StrList,
			vty_out(VTY_PID, "%% ~p ~n",[Str]),
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
    Hostname_fun =  fun (VTY_PID, _SelectionList, _NumberList, StrList) ->
			    [Hostname] = StrList,
			    vty_out(VTY_PID, "%% Set hostname: ~p~n", [Hostname]),
			    {cmd_hostname, Hostname}
		    end,

    Hostname_cmd = 
	#command{funcname = Hostname_fun,
		 cmdstr   = ["hostname", "WORD"],
		 helpstr  = ["Set system's network name",
			     "This system's network name"]},

    Enable_fun =  fun (_VTY_PID, _SelectionList, _NumberList, _StrList) ->
			  cmd_enable
		  end,

    Enable_cmd = #command{funcname= Enable_fun,
			  cmdstr  = ["enable"],
			  helpstr = ["Enable command"]},

    Disable_fun =  fun (_VTY_PID, _SelectionList, _NumberList, _StrList) ->
			   cmd_disable
		   end,

    Disable_cmd = #command{funcname= Disable_fun,
			   cmdstr  = ["disable"],
			   helpstr = ["Disable command"]},


    sr_telnet_registration:install_node(enable_node, 
					#node_propperties{write_fun = undefined,
							  exec_mode = privileged,
							  configuration_level = undefined}),
    sr_telnet_registration:install_node(view_node, 
					#node_propperties{write_fun = undefined,
							  exec_mode = user,
							  configuration_level= undefined}),
    sr_telnet_registration:install_node(config_node, 
					#node_propperties{write_fun = undefined,
							  exec_mode = privileged,
							  configuration_level= "config"}),

    sr_command:install_default(enable_node),
    sr_command:install_default(view_node),
    sr_command:install_default(config_node),


    sr_telnet_registration:install_element([enable_node], Disable_cmd),
    sr_telnet_registration:install_element([view_node], Enable_cmd),
    sr_telnet_registration:install_element([config_node], Hostname_cmd),



    %% Install the other nodes:	
    sr_gsmnet:install(), 
    sr_demo:install(). % demo node is within gsmnet_node

vty_out(VTY_PID, String) ->
    vty_out(VTY_PID, String,[]).

vty_out(VTY_PID, StringWithCR, List) ->
    %% Substitute all ?CR with ?CR ?LF for telnet
    FormattedStringWithCR = io_lib:format(StringWithCR, List),
    FormattedStringWithCRLF = re:replace(FormattedStringWithCR,binary_to_list(<<?LF>>),binary_to_list(<<?CR>>) ++ binary_to_list(<<?LF>>),[global, {return, list}]), 
    VTY_PID ! {output, FormattedStringWithCRLF}.

%% write_nodes(VTY_PID)->
%%     FirstNodeKey = ets:first(commandTable),
%%     write_node(VTY_PID, FirstNodeKey).

%% write_node(VTY_PID, NodeKey)->
%%     case NodeKey of 
%% 	'$end_of_table' -> % last registered node already written
%% 	    ok;
%% 	_ ->
%% 	    [Node] = ets:lookup(commandTable, NodeKey),
%% 	    io:format("Node: ~p~n",[Node]),
%% 	    Write_command = Node#node.write_command,
%% 	    case Write_command of
%% 		undefined -> % nothing to write for this node
%% 		    %% vty_out(VTY_PID,"Not writing node: ~p~n",[Node#node.nodeID]),
%% 		    ok;
%% 		_ -> % execute the node's fun
%% 		    %% vty_out(VTY_PID,"Writing node: ~p~n",[Node#node.nodeID]),
%% 		    Write_fun = Node#node.write_command,
%% 		    Write_fun(VTY_PID)
%% 	    end,
%% 	    NextNodeKey = ets:next(commandTable,NodeKey),
%% 	    write_node(VTY_PID, NextNodeKey)
%%     end.

write_nodes(VTY_PID)->
    FirstNodeKey = ets:first(commandTable),
    write_node(VTY_PID, FirstNodeKey).

write_node(VTY_PID, NodeKey)->
    case NodeKey of 
	'$end_of_table' -> % last registered node already written
	    ok;
	_ ->
	    [Node] = ets:lookup(commandTable, NodeKey),
	    %% io:format("Node: ~w~n",[Node]),
	    CommandListTableID = Node#node.commandListTableID,
	    CommandListTable = ets:tab2list(CommandListTableID),
	    %% io:format("CommandListTable: ~p~n",[CommandListTable]),
	    write_elements(VTY_PID, CommandListTable),
	    NextNodeKey = ets:next(commandTable,NodeKey),
	    write_node(VTY_PID, NextNodeKey)
    end.

write_elements(_VTY_PID, [])->
    ok;

write_elements(VTY_PID, [Head|Tail])->
    {_NodeID, Command} = Head,
    case Command#command.basicwrite of
	undefined -> % nothing to write for this element
	    io:format("Not writing command: ~p~n",[Command#command.cmdstr]),
	    ok;
	_ -> % execute the elements's fun
	    io:format("Writing command: ~p~n",[Command#command.cmdstr]),
	    BasicWrite_fun = Command#command.basicwrite,
	    BasicWrite_fun(VTY_PID)
    end,
    case Command#command.enhancedwrite of
	undefined -> % nothing to write for this element
	    io:format("Not writing command: ~p~n",[Command#command.cmdstr]),
	    ok;
	_ -> % execute the elements's fun
	    io:format("Writing command: ~p~n",[Command#command.cmdstr]),
	    CmdStrStrippedList = commandStringStrip(Command#command.cmdstr),
	    vty_out(VTY_PID, "~s ", [string:join(CmdStrStrippedList," ")]),
	    EnhancedWrite_fun = Command#command.enhancedwrite,
	    EnhancedWrite_fun(VTY_PID)
    end,
    write_elements(VTY_PID, Tail).

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
