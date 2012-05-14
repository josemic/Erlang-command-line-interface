-module(sr_gsmnet).
-export([install/0]).
-include("sr_command.hrl").

initialize_gsmnet_data()->
    ets:new(gsmnet_table,[ordered_set, named_table, public]),
    %% just dummy data!!!!
    ets:insert(gsmnet_table, {country_code, "Germany"}),
    ets:insert(gsmnet_table, {network_code, "00"}),
    ets:insert(gsmnet_table, {num_bts, 8}),
    ets:insert(gsmnet_table, {name_long, "Test-mobile"}),
    ets:insert(gsmnet_table, {name_short, "Tm"}),
    ets:insert(gsmnet_table, {auth_policy, 0}),
    ets:insert(gsmnet_table, {reject_cause, "unknown"}),
    ets:insert(gsmnet_table, {a5_encryption, "A5"}),
    ets:insert(gsmnet_table, {neci, 0}),
    ets:insert(gsmnet_table, {pag_amy_tch, "00"}),
    ets:insert(gsmnet_table, {rrlp_mode, 0}),
    ets:insert(gsmnet_table, {send_mm_info, true}),
    ets:insert(gsmnet_table, {handover_active, true}),
    ets:insert(gsmnet_table, {auth_policy, 0}),
    ets:insert(gsmnet_table, {reject_cause, "unknown"}),
    ets:insert(gsmnet_table, {a5_encryption, "A5"}),
    Msc_data_table_ID = ets:new(msc_data_table, [ordered_set]),
    ets:insert(gsmnet_table,{msc_data_table, Msc_data_table_ID}),
    ets:insert(Msc_data_table_ID,{last_RF_command, "RFCommand1"}).


install()->    
    Show_net_fun = fun (VTY, _Command_param) ->
			   %% NET = gsmnet_from_vty(VTY),
			   net_dump_vty(VTY, gsmnet_table),
			   cmd_success
		   end,

    Show_net_cmd =   #command{funcname= Show_net_fun, 
			      cmdstr  = ["show","network"], 
			      helpstr = [?SHOW_STR, 
					 "Display information about a GSM NETWORK"]},


    Show_bts_fun = fun (VTY, Command_param) ->
			   %%io:format("SelectionList: ~p~n",[SelectionList]),
			   %%io:format("NumberList: ~p~n",[NumberList]),
			   NumberList = Command_param#command_param.number_list,
                           NumBTS = ets:lookup_element(gsmnet_table,num_bts, 2),
			   case NumberList of 
			       [BTS_NR] when BTS_NR > NumBTS-> 
				   %% use the BTS number that the user has specified 
				   sr_command:vty_out(VTY, "%% can't find BTS '~p'~n", [BTS_NR]),
				   cmd_warning;
			       [BTS_NR]  -> 
				   dump_bts(VTY,BTS_NR),
				   %%bts_dump_vty(VTY, gsm_bts_num(NET, BTS_NR)),
				   cmd_success;
			       [] ->
				   %% print all BTS's
				   %% dump_all(ets:lookup(NET, num_bts), gsm_bts_num(net, bts_nr)),
				   dump_all_bts(VTY),
				   cmd_success
			   end
		   end,

    Show_bts_cmd = #command{funcname= Show_bts_fun,           %% funcname
			    cmdstr  = ["show","bts","[number]"],       %% cmdstr
			    helpstr = [?SHOW_STR, 
				       "Display information about a BTS", 
				       "BTS number"]}, %% helpstr 

    Show_trx_fun = fun(_VTY, Command_param)-> 
                           NumberList = Command_param#command_param.number_list,
			   io:format("NumberList: ~w~n",[NumberList]),
			   cmd_success end, %% dummy

    Show_trx_cmd = #command{funcname= Show_trx_fun, 
			    cmdstr  = ["show","trx","[bts_nr]","[trx_nr]"],
			    helpstr = [?SHOW_STR,
				       "Display information about a TRX",
				       "BTS Number",
				       "TRX Number"]},

    Show_ts_fun = fun(_VTY, Command_param)-> 
		          NumberList = Command_param#command_param.number_list,
			  io:format("NumberList: ~w~n",[NumberList]),
			  cmd_success end, %% dummy

    Show_ts_cmd = #command{funcname= Show_ts_fun,
			   cmdstr  = ["show", "timeslot","[bts_nr]","[trx_nr]", "[ts_nr]"],
			   helpstr = [?SHOW_STR, 
				      "Display information about a TS",
				      "BTS Number",
				      "TRX Number",
				      "Timeslot Number"]},

    Show_lchan_fun = fun(_VTY, Command_param)->NumberList = 
                             Command_param#command_param.number_list,
			     io:format("NumberList: ~w~n",[NumberList]),
			     cmd_success end, %% dummy

    Show_lchan_cmd= #command{funcname= Show_lchan_fun,
			     cmdstr  = ["show","lchan","[bts_nr]","[trx_nr]","[ts_nr]","[lchan_nr]"],
			     helpstr = [?SHOW_STR,
					"Display information about a logical channel",
					"BTS Number",
					"TRX Number",
					"Timeslot Number",
					?LCHAN_NR_STR]},

    Show_lchan_summary_fun = fun(_VTY, Command_param)->
				     NumberList = Command_param#command_param.number_list,
				     io:format("NumberList: ~w~n",[NumberList]),
				     cmd_success end, %% dummy

    Show_lchan_summary_cmd= #command{funcname= Show_lchan_summary_fun,
				     cmdstr  = ["show","lchan","summary","[bts_nr]", "[trx_nr]", "[ts_nr]", "[lchan_nr]"],
				     helpstr= [?SHOW_STR,
					       "Display information about a logical channel",
					       "Short summary",
					       "BTS Number",
					       "TRX Number", 
					       "Timeslot Number",
					       ?LCHAN_NR_STR]},

    Show_paging_fun = fun(_VTY, Command_param)-> 
                              NumberList = Command_param#command_param.number_list,
			      io:format("NumberList: ~w~n",[NumberList]),
			      cmd_success end, %% dummy

    Show_paging_cmd = #command{funcname= Show_paging_fun,
			       cmdstr  = ["show","paging","[bts_nr]"],
			       helpstr = [?SHOW_STR,
					  "Display information about paging requests of a BTS",
					  "BTS Number"]},



    Cfg_net_fun = fun(_VTY, _Command_param)-> 
			  %% enter gsmnet_node
			  {cmd_enter_node, gsmnet_node} 
		  end,

    Cfg_net_cmd = #command{ funcname= Cfg_net_fun, 
			    cmdstr  = ["network"],
			    helpstr = [?NETWORK_STR]},

    Cfg_net_ncc_fun = fun(_VTY, Command_param)->
			      NumberList = Command_param#command_param.number_list,
			      io:format("NumberList: ~w~n",[NumberList]),
			      cmd_success
		      end, %% dummy

    Cfg_net_ncc_cmd = #command{ funcname=Cfg_net_ncc_fun, 
				cmdstr  = ["network", "country", "code", "<1-999>"],
				helpstr = ["Set the GSM network country code",
					   "Country commands",
					   ?CODE_CMD_STR,
					   "Network Country Code to use"]},

    Cfg_net_name_short_fun =  fun(VTY, Command_param)->
				      StrList = Command_param#command_param.str_list,
				      %%io:format("StrList ~w~n", [StrList]),
				      [NameShort] = StrList,
				      ets:insert(gsmnet_table, {name_short,  NameShort}),
				      sr_command:vty_out(VTY,  "short network name ~p ~n", [ets:lookup_element(gsmnet_table,name_short, 2)]),
				      cmd_success
			      end,
    Cfg_net_name_short_basicwrite_fun = fun(VTY) ->
						NameShort = ets:lookup_element(gsmnet_table,name_short, 2),   
						sr_command:vty_out(VTY,  "short name ~s ~n", [NameShort]) 
					end,

    Cfg_net_name_short_cmd = #command{funcname= Cfg_net_name_short_fun,
				      cmdstr  = ["short", "name", "NAME"],
				      helpstr = ["Set the short GSM network name", 
						 ?NAME_CMD_STR, 
						 ?NAME_STR],
				      basicwrite = Cfg_net_name_short_basicwrite_fun},

    Cfg_net_name_long_fun =  fun(VTY, Command_param)->
				     %%io:format("StrList ~w~n", [StrList]),
				     [NameLong] = Command_param#command_param.str_list,
				     ets:insert(gsmnet_table, {name_long,  NameLong}),
				     sr_command:vty_out(VTY,  "long network name ~p ~n", [ets:lookup_element(gsmnet_table,name_long, 2)]),
			             cmd_success
			     end,

    Cfg_net_name_long_enhancedwrite_fun = fun(VTY) ->
						  NameLong = ets:lookup_element(gsmnet_table,name_long, 2),   
						  sr_command:vty_out(VTY,  "~s ~n", [NameLong])  
					  end,

    Cfg_net_name_long_cmd = #command{funcname      = Cfg_net_name_long_fun,
				     cmdstr        = ["long", "name", "NAME"],
				     helpstr       = ["Set the long GSM network name", 
						      ?NAME_CMD_STR, 
						      ?NAME_STR],
				     enhancedwrite = Cfg_net_name_long_enhancedwrite_fun},

    Cfg_trx_arfcn_fun = fun(_VTY, _Command_param)->
				cmd_success end,  %% dummy

    Cfg_trx_arfcn_cmd = #command{funcname = Cfg_trx_arfcn_fun,
				 cmdstr   =["arfcn", "<0-1024>"],
				 helpstr  =["Set the ARFCN for this TRX"]},

    Cfg_bts_lac_fun = fun(_VTY, _SelectionList, _NumberList, _StrList)-> 
			      cmd_success end,  %% dummy

    Cfg_bts_lac_cmd = #command{funcname= Cfg_bts_lac_fun,
			       cmdstr  = ["location_area_code", "<0-65535>"],
			       helpstr = ["Set the Location Area Code (LAC) of this BTS"]},



    Gsmnet_node_entry_fun = fun(VTY) ->
					sr_command:vty_out(VTY,  "network~n")
					%% CountryCode = ets:lookup_element(gsmnet_table,country_code, 2),   
					%% sr_command:vty_out(VTY,  "network country code ~p ~n", [CountryCode]),  
					%% NameShort = ets:lookup_element(gsmnet_table,name_short, 2),
					%% sr_command:vty_out(VTY,  "short name ~p~n", [NameShort])
				end,

    sr_telnet_registration:install_node(gsmnet_node, 
					#node_propperties{node_entry_fun = Gsmnet_node_entry_fun,
							  exec_mode = privileged,
							  indention_level =2,
							  configuration_level = "net"}),

    sr_command:install_default(gsmnet_node),
    initialize_gsmnet_data(),
    %% Enter gsmnet_node
    sr_telnet_registration:install_element([config_node], Cfg_net_cmd),
    sr_telnet_registration:install_element([gsmnet_node], Cfg_net_ncc_cmd),
    sr_telnet_registration:install_element([gsmnet_node], Cfg_net_name_short_cmd), 
    sr_telnet_registration:install_element([gsmnet_node], Cfg_net_name_long_cmd),
    sr_telnet_registration:install_element([gsmnet_node], Cfg_trx_arfcn_cmd),
    sr_telnet_registration:install_element([gsmnet_node], Cfg_bts_lac_cmd),
    %% should be config node instead of gsmnet_node	
    sr_telnet_registration:install_element([enable_node, view_node], Show_net_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_bts_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_trx_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_ts_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_lchan_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_lchan_summary_cmd),
    sr_telnet_registration:install_element([enable_node, view_node], Show_paging_cmd).




dump_all_bts(VTY)->
    NumBTS = ets:lookup_element(gsmnet_table, num_bts, 2),
    sr_command:vty_out(VTY, "This is no real output!!!~n"),
    dump_all_bts(VTY,NumBTS, 0).

dump_all_bts(VTY,NumBTS, BTS_NR)->
    if 
	BTS_NR < NumBTS ->
	    dump_bts(VTY,BTS_NR),
	    dump_all_bts(VTY, NumBTS, BTS_NR+1);
	true -> ok
    end.

dump_bts(VTY,BTS_NR)->
    sr_command:vty_out(VTY, "BTS Number: ~p ", [BTS_NR]),
    sr_command:vty_out(VTY, "is active~n").

net_dump_vty(VTY,  Gsmnet_table_name) ->	
    CountryCode = ets:lookup_element(Gsmnet_table_name,country_code, 2),
    NetworkCode = ets:lookup_element(Gsmnet_table_name,network_code, 2),
    NumBTS = ets:lookup_element(Gsmnet_table_name,num_bts, 2),
    sr_command:vty_out(VTY,  "BSC is on Country Code ~p, Network Code ~p "++
			   "and has ~p BTS~n", [CountryCode, NetworkCode, NumBTS]),  
    NameLong = ets:lookup_element(Gsmnet_table_name,name_long, 2),
    sr_command:vty_out(VTY,  "  Long network name: ~p~n", [NameLong]),
    NameShort = ets:lookup_element(Gsmnet_table_name,name_short, 2),
    sr_command:vty_out(VTY,  "  Short network name: ~p~n", [NameShort]),	
    AuthPolicy =  gsm_auth_policy_name(ets:lookup_element(Gsmnet_table_name,auth_policy, 2)),
    sr_command:vty_out(VTY,  "  Authentication policy: ~p~n", [AuthPolicy]),
    RejectCause = ets:lookup_element(Gsmnet_table_name, reject_cause, 2),
    sr_command:vty_out(VTY,  "  Location updating reject cause: ~p~n", [RejectCause]),
    A5Encryption =  ets:lookup_element(Gsmnet_table_name,a5_encryption, 2),
    sr_command:vty_out(VTY,  "  Encryption: A5/~p~n", [A5Encryption]),
    Neci = ets:lookup_element(Gsmnet_table_name, neci, 2),
    sr_command:vty_out(VTY,  "  NECI (TCH/H): ~p~n", [Neci]),
    PagAnyTCH = ets:lookup_element(Gsmnet_table_name, pag_amy_tch, 2),
    sr_command:vty_out(VTY,  "  Use TCH for Paging any: ~p~n", [PagAnyTCH]),
    RrlpMode = ets:lookup_element(Gsmnet_table_name, rrlp_mode, 2),
    sr_command:vty_out(VTY,  "  RRLP Mode: ~p~n", [RrlpMode]),
    SendMMInfo = case ets:lookup_element(Gsmnet_table_name, send_mm_info, 2) of
		     true -> "On";
		     false -> "Off"
		 end,
    sr_command:vty_out(VTY,  "  MM Info: ~p~n", [SendMMInfo]),
    HandoverActive = case ets:lookup_element(Gsmnet_table_name, handover_active, 2) of
			 true -> "On";
			 false -> "Off"
		     end,
    sr_command:vty_out(VTY,  "  Handover: ~p~n", [HandoverActive]),
    ChannelLoad = network_chan_load(Gsmnet_table_name),
    sr_command:vty_out(VTY,  "  Current Channel Load: ~p~n", [ChannelLoad]),
    case ets:member(Gsmnet_table_name, msc_data) and ets:member(Gsmnet_table_name, msc_data_rf_ctrl) of
	true ->
	    Msc_data_table_ID = ets:lookup_element(Gsmnet_table_name, msc_data_table, 2),
	    Msc_data_Rf_ctrl_Last_state_command = ets:lookup_element(Msc_data_table_ID, last_RF_command, 2),
	    sr_command:vty_out(VTY,  "  Last RF Command: ~p~n", [Msc_data_Rf_ctrl_Last_state_command]); 
	false ->
	    true
    end.


gsm_auth_policy_name(A)-> % dummy
    A. 

network_chan_load(A)-> % dummy
    A. 

%% gsmnet_from_vty(VTY)->
%%     case ets:lookup(commandTable,VTY) of
%% 	[] -> 
%% 	    Vty_TableID = ets:new(vty_table,[ordered_set]),
%% 	    ets:insert(commandTable,{VTY,Vty_TableID}),
%% 	    Gsmnet_TableID = ets:new(gsmnetTable,[ordered_set]),
%% 	    ets:insert(Vty_TableID,{gsmnet,Gsmnet_TableID});
%% 	[{VTY, Vty_TableID}]-> 
%% 	    case ets:lookup(Vty_TableID,gsmnet) of
%% 		[] -> 
%% 		    Gsmnet_TableID = ets:new(gsmnetTable,[ordered_set]),
%% 		    ets:insert(Vty_TableID,{gsmnet,Gsmnet_TableID});
%% 		[{gsmnet, Gsmnet_TableID}]->
%% 		    true
%% 	    end
%%     end, 
%%     Gsmnet_TableID.

