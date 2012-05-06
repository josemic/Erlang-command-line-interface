-module(sr_demo).
-export([install/0]).
-include("sr_command.hrl").

install()->
    Cfg_demo_fun = fun(_VTY, _SelectionList, _NumberList, _StrList)-> 
			   %% enter demo_node
			   {cmd_enter_node, demo_node} 
		   end,

    Cfg_demo_cmd = #command{ funcname= Cfg_demo_fun, 
			     cmdstr  = ["demo"],
			     helpstr = ["Demotration node"]},


%%% Really for test puroses only:
    Show_number_fun = fun(VTY, _SelectionList, NumberList, _StrList)-> 
			      %% Selection list is not used here.

			      io:format("NumberList: ~p~n",[NumberList]),
			      case NumberList of 
				  []-> 
				      %% use the BTS number that the user has specified 
				      sr_command:vty_out(VTY, "%% no number was given~n", []),
				      cmd_success;
				  [Nr0]  -> 
				      %% one number was given
				      sr_command:vty_out(VTY, "%% number 0: ~p was given'~n", [Nr0]),
				      cmd_success;
				  [Nr0,Nr1] ->
				      %% two numbers were given
				      sr_command:vty_out(VTY, "%% number 0: ~p and number 1: ~p were given~n", [Nr0, Nr1]),
				      cmd_success
			      end
		      end, 

    Show_a_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show","a"],
			  helpstr = [?SHOW_STR,
				     "Display number"]},

    Show_b_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show","b","[nr0]"],
			  helpstr = [?SHOW_STR,
				     "Display number",
				     "Optional Number 0"]},

    Show_c_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show", "c","[nr0]","[nr1]"],
			  helpstr = [?SHOW_STR, 
				     "Display number",
				     "Optional Number 0", 
				     "Optional Number 1"]},

    Show_d_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show","d", "<nr0>"],
			  helpstr = [?SHOW_STR,
				     "Display number",
				     "Mandatory Number 0"],
			  hidden =yes},

    Show_e_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show", "e", "<nr0>", "<nr1>"],
			  helpstr = [?SHOW_STR,
				     "Display number",
				     "Mandatory Number 0",
				     "Mandatory Number 1"]},

    Show_f_cmd = #command{funcname= Show_number_fun,
			  cmdstr  = ["show", "f", "<bts_nr0>", "[bts_nr1]"],
			  helpstr = [?SHOW_STR,
				     "Display number",
				     "Mandatory Number 0", 
				     "Optional Number 1"]},

%%% Really for demonstration puroses only:
    Show_selection_fun = fun(VTY, SelectionList, _NumberList, _StrList)-> 
				 %% Selection list is not used here.

				 io:format("SelectionList: ~p~n",[SelectionList]),
				 case SelectionList of 
				     [Selection]  -> 
					 %% one number was given
					 sr_command:vty_out(VTY, "%% Selection: ~p was given'~n", [Selection]),
					 cmd_success
				 end
			 end, 

    Show_selection_cmd = #command{funcname= Show_selection_fun,
				  cmdstr  = ["show","{0|1|2}", "selection"],
				  helpstr = [?SHOW_STR,
					     "Display number",
					     "BTS Number: 0, 1 or 2}", 
					     "selection"]},



    sr_telnet_registration:install_node(demo_node, 
					#node_propperties{write_fun = undefined,
							  exec_mode = privileged,
							  configuration_level = "demo"}),

    sr_command:install_default(demo_node),

    %% Enter demo_node
    sr_telnet_registration:install_element([gsmnet_node], Cfg_demo_cmd),
    sr_telnet_registration:install_element([demo_node], Show_a_cmd),
    sr_telnet_registration:install_element([demo_node], Show_b_cmd),
    sr_telnet_registration:install_element([demo_node], Show_c_cmd),
    sr_telnet_registration:install_element([demo_node], Show_d_cmd),
    sr_telnet_registration:install_element([demo_node], Show_e_cmd),
    sr_telnet_registration:install_element([demo_node], Show_f_cmd),   
    sr_telnet_registration:install_element([demo_node], Show_selection_cmd).

