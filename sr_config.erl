-module(sr_config).
-export([install/0]).
-include("sr_command.hrl").

install()->

    Cfg_config_fun = fun(_VTY, _Command_param)-> 
			   %% enter config_node
			   {cmd_enter_node, config_node} 
		   end,

    Cfg_config_cmd = #command{ funcname= Cfg_config_fun, 
			     cmdstr  = ["configure","terminal"],
			     helpstr = ["Configuration from vty interface", 
					"Configuration terminal"]},

    Hostname_fun =  fun (VTY, Command_param) ->
			    [Hostname] = Command_param#command_param.str_list,
			    ets:insert(server_data_table, {hostname,  Hostname}),
			    sr_command:vty_out(VTY, "%% Set hostname: ~p~n", [Hostname]),
			    cdmd_success
		    end,

    Hostname_cmd = 
	#command{funcname = Hostname_fun,
		 cmdstr   = ["hostname", "WORD"],
		 helpstr  = ["Set system's network name",
			     "This system's network name"]},


    sr_telnet_registration:install_node(config_node, 
					#node_propperties{node_entry_fun = undefined,
							  exec_mode = privileged,
							  configuration_level = "config"}),

    sr_command:install_default(config_node),
    %% Enter config_node
    sr_telnet_registration:install_element([enable_node], Cfg_config_cmd),
    sr_telnet_registration:install_element([config_node], Hostname_cmd).

