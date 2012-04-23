
-record(state, {input=[],     
		parsed=""               ::string(),
		parsed_list=[]          ::[string()],
		selection_list=[]       ::[string()],
		str_list=[]             ::[string()],
		completion=[]           ::string(),
		number_list =[]         ::[integer()],
		command=[]              ::#command{}    % holds the command record for later use
	       }).

-record(node,   {prompt=[],     
		 write_command           ::function(),
		 commandListTableID = [] ::[atom()],
		 nodeID=[]               ::atom(),
		 exec_mode               ::user | root,
		 configuration_level     ::string()
		}).


%%% escape character
-define(IAC,        255). %

%%% Telnet commands
-define(DONT,       254). %
-define(DO,         253). %
-define(WONT,       252). %
-define(WILL,       251). %
-define(SB,         250). % Subnegotiation
-define(GA,         249). % Go ahead
-define(EL,         248). % Erase line
-define(EC,         247). % Erase character
-define(AYT,        246). % Are you there
-define(AO,         245). % Abort output
-define(IP,         244). % Interrupt process
-define(BRK,        243). % Break
-define(DATA_MARK,  242). % Data stream
-define(NOP,        241). % No operation
-define(SE,         240). % End of subnegotiation

-define(CR,    13). % Carriage return
-define(FF,    12). % Form feed
-define(VT,    11). % Vertical tab
-define(LF,    10). % Line feed
-define(HT,     9). % Horizontal Tab
-define(BS,     8). % Backspace
-define(BEL,    7). % Bell
-define(NUL,    0). % NULL (No operation)

%%% Telnet Options
-define(ENVIRONMENT_VARIABLES, 36). % RFC 1408
-define(LINE_MODE,             34). % RFC 1184
-define(REMOTE_FLOW_CONTROL,   33). % RFC 1372
-define(TERMINAL_SPEED,        32). % RFC 1079
-define(WINDOW_SIZE,           31). % RFC 1073 Negotiate About Window Size "NAWS"
-define(TERMINAL_TYPE,         24). % RFC 1091
-define(TIMING_MARK,            6). % RFC  860
-define(STATUS,                 5). % RFC  859
-define(SUPPRESS_GO_AHEAD,      3). % RFC  858
-define(ECHO,                   1).
-define(TRANSMIT_BINARY,        0).
