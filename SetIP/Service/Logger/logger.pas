unit logger;

interface
const
  /// <summary> used constants </summary>
  /// <param name="STACK_LEVEL"> obsolete name, replaced by cSTACK_LEVEL </param>
  STACK_LEVEL = 2;
  /// <param name="cSTACK_LEVEL"> If there goes 1 into JclDebug.mID or JclDebug.uID, actual function is presented. If there goes 2, function caller is presented. This behavior might be used for performance optimization (uID or mID can be OPTIONALLY called only if debug log will be processed) </param>
  cSTACK_LEVEL = 2;
  /// <param name="MAX_LOG_LINE_LENGTH"> maximum length of one line in log file </param>
  cMAX_LOG_LINE_LENGTH = 512;
  cLOGLEVEL_LEVELS_MAX_LENGTH = 3;

  WSABASEERR             = 10000;               //< network error - base address
  WSAENOTCONN            = WSABASEERR +   57;   //< network error - Socket is not
  ERR_SEND          =  3;   // error during sending
  ERR_RECEIVE       =  4;   // error during receiving
  ERR_CONNECT       =  5;   // error during network connecting
  ERR_RECV_TIMEOUT = -1017;
  ERR_TFTP_UNDEFINED = -1047;
  ERR_READ_TFTP     = 10;   // error during reading by TFTP protocoll
  ERR_OTHER = -1020;
  ERR_TFTP_UNKNOWNCOMMAND = -1055;

type
    /// <summary> Log level type </summary>
  /// <param name="logLevel_None"> debugging disabled </param>
  /// <param name="logLevel_Critical"> critical conditions </param>
  /// <param name="logLevel_Error"> error conditions </param>
  /// <param name="logLevel_Warning"> may indicate that an error will occur if action is not taken </param>
  /// <param name="logLevel_Notice"> events that are unusual, but not error conditions </param>
  /// <param name="logLevel_Info"> normal operational messages that require no action </param>
  /// <param name="logLevel_Debug"> information useful to developer for debugging the library </param>
  /// <param name="logLevel_Verbose"> most detailed debugging </param>
  TlogLevel = ( logLevel_None
              , logLevel_Critical
              , logLevel_Error
              , logLevel_Warning
              , logLevel_Notice
              , logLevel_Info
              , logLevel_Debug
              , logLevel_Verbose
              );
  PlogLevel = ^TlogLevel;
  /// <summary> object for log feature </summary>
  TLogger = class(TObject)
  public
    /// <summary> log event to file in case no file is currently opened </summary>
    /// <param name="logStr">  </param>
    /// <param name="aLogLevel"> wanted log level (<see cref="logger|TlogLevel"/> definition for details) </param>
    procedure event(const logStr: AnsiString; const aLogLevel: TLogLevel = logLevel_Info); overload;

    /// <summary>  </summary>
    /// <param name="stackLevel">  </param>
    /// <param name="logStr">  </param>
    /// <param name="aLogLevel"> wanted log level (<see cref="logger|TlogLevel"/> definition for details) </param>
    procedure event(const stackLevel: Byte; const logStr: AnsiString; const aLogLevel: TLogLevel = logLevel_Info); overload;
    /// <summary>  </summary>
    /// <param name="stackLevel">  </param>
    /// <param name="fmtStr">  </param>
    /// <param name="Args">  </param>
    /// <param name="aLogLevel"> wanted log level (<see cref="logger|TlogLevel"/> definition for details) </param>
    procedure event(const stackLevel: Byte; const fmtStr: AnsiString; const Args: array of const; const aLogLevel: TLogLevel = logLevel_Info); overload;

    /// <summary>  </summary>
    /// <param name="unitID">  </param>
    /// <param name="methodID">  </param>
    /// <param name="msg">  </param>
    /// <param name="aLogLevel"> wanted log level (<see cref="logger|TlogLevel"/> definition for details) </param>
    procedure event(const unitID: AnsiString; const methodID: AnsiString; const msg: AnsiString; const aLogLevel: TLogLevel = logLevel_Info); overload;

    /// <summary>  </summary>
    /// <param name="unitID">  </param>
    /// <param name="methodID">  </param>
    /// <param name="msg">  </param>
    /// <param name="aLogLevel"> wanted log level (<see cref="logger|TlogLevel"/> definition for details) </param>
    procedure event(const unitID: AnsiString; const methodID: AnsiString; const fmtStr: AnsiString; const Args: array of const; const aLogLevel: TLogLevel = logLevel_Info); overload;
  end;
  var
  log: TLogger;
  uID: AnsiString='';
  mID: AnsiString='';
function getWinErrText(const errCode: Integer): AnsiString;
function getWinErrDescript(const errCode: Integer): AnsiString;

implementation
uses Windows;

Function FehlerStr(Code:Cardinal;const Args: array of const):AnsiString;
var
lpMsgBuf:PAnsichar;
Flags:Cardinal;
begin
lpMsgBuf:=nil;
if Length(Args)=0 then Flags:=FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ALLOCATE_BUFFER
else  Flags:=FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY or FORMAT_MESSAGE_ALLOCATE_BUFFER;
FormatMessageA(Flags,nil,Code,(SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL, lpMsgBuf,Length(Args),@Args);
Result:=lpMsgBuf;
LocalFree(Cardinal(lpMsgBuf));
end;

function getWinErrText(const errCode: Integer): AnsiString;
begin
  Result:=FehlerStr( errCode,[]);
end;
function getWinErrDescript(const errCode: Integer): AnsiString;
begin
  Result:=FehlerStr( errCode,[]);
end;

procedure TLogger.event(const logStr: AnsiString; const aLogLevel: TLogLevel = logLevel_Info);
begin
  if ( self = nil ) then exit;

end;


procedure TLogger.event(const stackLevel: Byte; const logStr: AnsiString; const aLogLevel: TLogLevel = logLevel_Info);
begin
  if ( self = nil ) then exit;

end;

    procedure TLogger.event(const stackLevel: Byte; const fmtStr: AnsiString; const Args: array of const; const aLogLevel: TLogLevel = logLevel_Info);
begin
  if ( self = nil ) then exit;

end;
    procedure TLogger.event(const unitID: AnsiString; const methodID: AnsiString; const msg: AnsiString; const aLogLevel: TLogLevel = logLevel_Info);
begin
  if ( self = nil ) then exit;

end;
    procedure TLogger.event(const unitID: AnsiString; const methodID: AnsiString; const fmtStr: AnsiString; const Args: array of const; const aLogLevel: TLogLevel = logLevel_Info); 
begin
  if ( self = nil ) then exit;

end;

end.
