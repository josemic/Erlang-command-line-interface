-define(NETWORK_STR,       "Configure the GSM network").
-define(CODE_CMD_STR,      "Code commands").
-define(NAME_CMD_STR,      "Name Commands").
-define(NAME_STR,          "Name to use").
-define(LCHAN_NR_STR,      "Logical Channel Number").

-define(SHOW_STR,  "Show running system information").

-record(node_propperties, 
	{write_fun           = undefined ::function(), % function or undefined
	 configuration_level = undefined ::string(),   % string or undefined
	 exec_mode           = root      ::user | root}).

-record(command, {funcname     ::function(),
		  cmdstr       ::[string()],
		  helpstr      ::[string()]}).
