-define(NETWORK_STR,       "Configure the GSM network").
-define(CODE_CMD_STR,      "Code commands").
-define(NAME_CMD_STR,      "Name Commands").
-define(NAME_STR,          "Name to use").
-define(LCHAN_NR_STR,      "Logical Channel Number").

-define(SHOW_STR,  "Show running system information").

-record(node_propperties, 
	{node_entry_fun       = undefined ::function()|undefined, % function or undefined
	 configuration_level  = undefined ::string()|undefined,   % string or undefined
	 indention_level      = 0         ::integer(),
	 exec_mode            = root      ::user | privileged}).

-record(command, {funcname           = undefined ::function()|undefined,
		  cmdstr                         ::[string()],
		  helpstr                        ::[string()],
		  basicwrite         = undefined ::function()|undefined,  % function or undefined
		  enhancedwrite      = undefined ::function()|undefined,  % function or undefined
	          alias_ref          = undefined ::atom()|undefined,
	          alias_def          = undefined ::atom()|undefined,
		  hidden             = undefined ::yes | no |undefined    % yes, no or undefined   
		 }).

-record(command_param, 
	{selection_list                  ::[string()],
	 number_list                     ::[integer()],
	 str_list                        ::[string()],
	 index_list                      ::[[integer()]]
	}).
