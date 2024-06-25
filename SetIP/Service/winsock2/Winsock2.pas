unit WinSock2;
(* **************************************************************************** *)
(* ********* this Unit process Network communication and TFTP-protocol  ******* *)
(* **************************************************************************** *)

interface

uses Windows, Winsock;

{$IF CompilerVersion >= 24.0 }
{$LEGACYIFEND ON}
{$IFEND}
{$IF CompilerVersion >= 15.0 }
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$IFEND}

// Structure used in getaddrinfo() call
type

{$IFDEF NEXTGEN} // !!! HACK ADDED BY EMBT
  AnsiChar = Byte;
{$EXTERNALSYM AnsiChar}
  PAnsiChar = MarshaledAString;
{$EXTERNALSYM PAnsiChar}
{$ENDIF}

  // Must define the following types because the older versions of Delphi do not support them

{$IFNDEF HAS_ULONG_PTR}
{$EXTERNALSYM ULONG_PTR}
  ULONG_PTR = DWORD;
{$ENDIF}
{$IFNDEF HAS_DWORD_PTR}
{$EXTERNALSYM DWORD_PTR}
  DWORD_PTR = DWORD;
{$ENDIF}
{$IFNDEF HAS_PVOID}
{$EXTERNALSYM PVOID}
  PVOID = Pointer;
{$ENDIF}
  SIZE_T = Cardinal;
{$NODEFINE PAddrInfo}
  PAddrInfo = ^ADDRINFO;
{$NODEFINE PPaddrinfo}
  PPaddrinfo = ^PAddrInfo;
{$NODEFINE PPaddrinfoW}
  PPaddrinfoW = ^PAddrInfoW;
{$EXTERNALSYM ADDRINFO}

  ADDRINFO = record
    ai_flags: Integer; // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer; // PF_xxx
    ai_socktype: Integer; // SOCK_xxx
    ai_protocol: Integer; // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: SIZE_T; // Length of ai_addr
    ai_canonname: PAnsiChar; // Canonical name for nodename
    ai_addr: PSOCKADDR; // Binary address
    ai_next: PAddrInfo; // Next structure in linked list
  end;
{$NODEFINE TAddrInfo}

  TAddrInfo = ADDRINFO;
{$EXTERNALSYM LPADDRINFO}
  LPADDRINFO = PAddrInfo;

{$NODEFINE PAddrInfoW}
  PAddrInfoW = ^ADDRINFOW;
{$EXTERNALSYM ADDRINFOW}

  ADDRINFOW = record
    ai_flags: Integer; // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    ai_family: Integer; // PF_xxx
    ai_socktype: Integer; // SOCK_xxx
    ai_protocol: Integer; // 0 or IPPROTO_xxx for IPv4 and IPv6
    ai_addrlen: SIZE_T; // Length of ai_addr
    ai_canonname: PWideChar; // Canonical name for nodename
    ai_addr: PSOCKADDR; // Binary address
    ai_next: PAddrInfoW; // Next structure in linked list
  end;
{$NODEFINE TAddrInfoW}

  TAddrInfoW = ADDRINFOW;
{$EXTERNALSYM LPADDRINFOW}
  LPADDRINFOW = PAddrInfoW;

{$EXTERNALSYM WSABUF}

  WSABUF = record
    len: u_long; { the length of the buffer }
    buf: PAnsiChar; { the pointer to the buffer }
  end;
{$NODEFINE TWSABuf}

  TWSABuf = WSABUF;
{$NODEFINE PWSABuf}
  PWSABuf = ^TWSABuf;
{$EXTERNALSYM LPWSABUF}
  LPWSABUF = PWSABuf;

{$EXTERNALSYM SERVICETYPE}
  SERVICETYPE = ULONG;
{$NODEFINE TServiceType}
  TServiceType = SERVICETYPE;
{$EXTERNALSYM FLOWSPEC}

  FLOWSPEC = record
    TokenRate, // In Bytes/sec
    TokenBucketSize, // In Bytes
    PeakBandwidth, // In Bytes/sec
    Latency, // In microseconds
    DelayVariation: ULONG; // In microseconds
    SERVICETYPE: TServiceType;
    MaxSduSize, MinimumPolicedSize: ULONG; // In Bytes
  end;
{$NODEFINE TFlowSpec}

  TFlowSpec = FLOWSPEC;
{$EXTERNALSYM PFLOWSPEC}
  PFLOWSPEC = ^TFlowSpec;
{$EXTERNALSYM LPFLOWSPEC}
  LPFLOWSPEC = PFLOWSPEC;
  LPWSAOVERLAPPED = ^TOverlapped;

{$EXTERNALSYM QOS}

  QOS = record
    SendingFlowspec: TFlowSpec; { the flow spec for data sending }
    ReceivingFlowspec: TFlowSpec; { the flow spec for data receiving }
    ProviderSpecific: TWSABuf; { additional provider specific stuff }
  end;
{$NODEFINE TQualityOfService}

  TQualityOfService = QOS;
{$NODEFINE PQOS}
  PQOS = ^QOS;
{$EXTERNALSYM LPQOS}
  LPQOS = PQOS;

{$EXTERNALSYM GROUP}
  GROUP = DWORD;
{$EXTERNALSYM PGROUP}
  PGROUP = ^GROUP;

{$EXTERNALSYM LPCONDITIONPROC}
  LPCONDITIONPROC = function(lpCallerId: LPWSABUF; lpCallerData: LPWSABUF; lpSQOS, pGQOS: LPQOS; lpCalleeId, lpCalleeData: LPWSABUF; g: PGROUP;
    dwCallbackData: DWORD_PTR): Integer; stdcall;
{$EXTERNALSYM LPWSAOVERLAPPED_COMPLETION_ROUTINE}
  LPWSAOVERLAPPED_COMPLETION_ROUTINE = procedure(dwError, cbTransferred: DWORD; lpOverlapped: LPWSAOVERLAPPED; dwFlags: DWORD); stdcall;

  // WinSock 2 extension -- WSAPROTOCOL_INFO manifest constants

const
{$EXTERNALSYM MAX_PROTOCOL_CHAIN}
  MAX_PROTOCOL_CHAIN = 7;
{$EXTERNALSYM BASE_PROTOCOL}
  BASE_PROTOCOL = 1;
{$EXTERNALSYM LAYERED_PROTOCOL}
  LAYERED_PROTOCOL = 0;
{$EXTERNALSYM WSAPROTOCOL_LEN}
  WSAPROTOCOL_LEN = 255;
  // WinSock 2 extension -- manifest constants for WSASocket()
{$EXTERNALSYM WSA_FLAG_OVERLAPPED}
  WSA_FLAG_OVERLAPPED = $01;
{$EXTERNALSYM WSA_FLAG_MULTIPOINT_C_ROOT}
  WSA_FLAG_MULTIPOINT_C_ROOT = $02;
{$EXTERNALSYM WSA_FLAG_MULTIPOINT_C_LEAF}
  WSA_FLAG_MULTIPOINT_C_LEAF = $04;
{$EXTERNALSYM WSA_FLAG_MULTIPOINT_D_ROOT}
  WSA_FLAG_MULTIPOINT_D_ROOT = $08;
{$EXTERNALSYM WSA_FLAG_MULTIPOINT_D_LEAF}
  WSA_FLAG_MULTIPOINT_D_LEAF = $10;
  { WinSock 2 extension -- new error codes and type definition }
{$EXTERNALSYM WSA_IO_PENDING}
  WSA_IO_PENDING = ERROR_IO_PENDING;
{$EXTERNALSYM WSA_IO_INCOMPLETE}
  WSA_IO_INCOMPLETE = ERROR_IO_INCOMPLETE;
{$EXTERNALSYM WSA_INVALID_HANDLE}
  WSA_INVALID_HANDLE = ERROR_INVALID_HANDLE;
{$EXTERNALSYM WSA_INVALID_PARAMETER}
  WSA_INVALID_PARAMETER = ERROR_INVALID_PARAMETER;
{$EXTERNALSYM WSA_NOT_ENOUGH_MEMORY}
  WSA_NOT_ENOUGH_MEMORY = ERROR_NOT_ENOUGH_MEMORY;
{$EXTERNALSYM WSA_OPERATION_ABORTED}
  WSA_OPERATION_ABORTED = ERROR_OPERATION_ABORTED;

  // WSA_INVALID_EVENT       = WSAEVENT(nil);
{$EXTERNALSYM WSA_MAXIMUM_WAIT_EVENTS}
  WSA_MAXIMUM_WAIT_EVENTS = MAXIMUM_WAIT_OBJECTS;
{$EXTERNALSYM WSA_WAIT_FAILED}
  WSA_WAIT_FAILED = $FFFFFFFF;
{$EXTERNALSYM WSA_WAIT_EVENT_0}
  WSA_WAIT_EVENT_0 = WAIT_OBJECT_0;
{$EXTERNALSYM WSA_WAIT_IO_COMPLETION}
  WSA_WAIT_IO_COMPLETION = WAIT_IO_COMPLETION;
{$EXTERNALSYM WSA_WAIT_TIMEOUT}
  WSA_WAIT_TIMEOUT = WAIT_TIMEOUT;
{$EXTERNALSYM WSA_INFINITE}
  WSA_INFINITE = INFINITE;

const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
{$EXTERNALSYM MAX_ADAPTER_DESCRIPTION_LENGTH}
  MAX_ADAPTER_NAME_LENGTH = 256; // arb.
{$EXTERNALSYM MAX_ADAPTER_NAME_LENGTH}
  MAX_ADAPTER_ADDRESS_LENGTH = 8; // arb.
{$EXTERNALSYM MAX_ADAPTER_ADDRESS_LENGTH}
  DEFAULT_MINIMUM_ENTITIES = 32; // arb.
{$EXTERNALSYM DEFAULT_MINIMUM_ENTITIES}
  MAX_HOSTNAME_LEN = 128; // arb.
{$EXTERNALSYM MAX_HOSTNAME_LEN}
  MAX_DOMAIN_NAME_LEN = 128; // arb.
{$EXTERNALSYM MAX_DOMAIN_NAME_LEN}
  MAX_SCOPE_ID_LEN = 256; // arb.
{$EXTERNALSYM MAX_SCOPE_ID_LEN}
  ERROR_OBJECT_ALREADY_EXISTS = 5010;

type
{$EXTERNALSYM WSAPROTOCOLCHAIN}
  WSAPROTOCOLCHAIN = record
    ChainLen: Integer; // the length of the chain,
    // length = 0 means layered protocol,
    // length = 1 means base protocol,
    // length > 1 means protocol chain
    ChainEntries: Array [0 .. MAX_PROTOCOL_CHAIN - 1] of DWORD; // a list of dwCatalogEntryIds
  end;
{$NODEFINE TWSAProtocolChain}

  TWSAProtocolChain = WSAPROTOCOLCHAIN;
{$EXTERNALSYM LPWSAPROTOCOLCHAIN}
  LPWSAPROTOCOLCHAIN = ^TWSAProtocolChain;

{$EXTERNALSYM WSAPROTOCOL_INFOA}

  WSAPROTOCOL_INFOA = record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: TWSAProtocolChain;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: Array [0 .. WSAPROTOCOL_LEN + 1 - 1] of AnsiChar;
  end;
{$NODEFINE TWSAProtocol_InfoA}

  TWSAProtocol_InfoA = WSAPROTOCOL_INFOA;
{$NODEFINE PWSAProtocol_InfoA}
  PWSAProtocol_InfoA = ^WSAPROTOCOL_INFOA;
{$EXTERNALSYM LPWSAPROTOCOL_INFOA}
  LPWSAPROTOCOL_INFOA = PWSAProtocol_InfoA;

{$EXTERNALSYM WSAPROTOCOL_INFOW}

  WSAPROTOCOL_INFOW = record
    dwServiceFlags1: DWORD;
    dwServiceFlags2: DWORD;
    dwServiceFlags3: DWORD;
    dwServiceFlags4: DWORD;
    dwProviderFlags: DWORD;
    ProviderId: TGUID;
    dwCatalogEntryId: DWORD;
    ProtocolChain: TWSAProtocolChain;
    iVersion: Integer;
    iAddressFamily: Integer;
    iMaxSockAddr: Integer;
    iMinSockAddr: Integer;
    iSocketType: Integer;
    iProtocol: Integer;
    iProtocolMaxOffset: Integer;
    iNetworkByteOrder: Integer;
    iSecurityScheme: Integer;
    dwMessageSize: DWORD;
    dwProviderReserved: DWORD;
    szProtocol: Array [0 .. WSAPROTOCOL_LEN + 1 - 1] of WideChar;
  end;
{$NODEFINE TWSAProtocol_InfoW}

  TWSAProtocol_InfoW = WSAPROTOCOL_INFOW;
{$NODEFINE PWSAProtocol_InfoW}
  PWSAProtocol_InfoW = ^TWSAProtocol_InfoW;
{$EXTERNALSYM LPWSAPROTOCOL_INFOW}
  LPWSAPROTOCOL_INFOW = PWSAProtocol_InfoW;

{$EXTERNALSYM WSAPROTOCOL_INFO}
{$EXTERNALSYM LPWSAPROTOCOL_INFO}
{$NODEFINE TWSAProtocol_Info}
{$NODEFINE PWSAProtocol_Info}
{$IFDEF UNICODE}
  WSAPROTOCOL_INFO = TWSAProtocol_InfoW;
  TWSAProtocol_Info = TWSAProtocol_InfoW;
  PWSAProtocol_Info = PWSAProtocol_InfoW;
  LPWSAPROTOCOL_INFO = PWSAProtocol_InfoW;
{$ELSE}
  WSAPROTOCOL_INFO = TWSAProtocol_InfoA;
  TWSAProtocol_Info = TWSAProtocol_InfoA;
  PWSAProtocol_Info = PWSAProtocol_InfoA;
  LPWSAPROTOCOL_INFO = PWSAProtocol_InfoA;
{$ENDIF}
  WSAEVENT = THandle;
{$NODEFINE PWSAEVENT}
  PWSAEVENT = ^WSAEVENT;
{$EXTERNALSYM LPWSAEVENT}
  LPWSAEVENT = PWSAEVENT;

  //
  // IP_ADDRESS_STRING - store an IP address as a dotted decimal string
  //

type
  PIP_MASK_STRING = ^IP_MASK_STRING;
{$EXTERNALSYM PIP_MASK_STRING}

  IP_ADDRESS_STRING = record
    S: array [0 .. 15] of AnsiChar;
  end;
{$EXTERNALSYM IP_ADDRESS_STRING}

  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
{$EXTERNALSYM PIP_ADDRESS_STRING}
  IP_MASK_STRING = IP_ADDRESS_STRING;
{$EXTERNALSYM IP_MASK_STRING}
  TIpAddressString = IP_ADDRESS_STRING;
  PIpAddressString = PIP_MASK_STRING;

  //
  // IP_ADDR_STRING - store an IP address with its corresponding subnet mask,
  // both as dotted decimal strings
  //

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
{$EXTERNALSYM PIP_ADDR_STRING}

  _IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;
{$EXTERNALSYM _IP_ADDR_STRING}

  IP_ADDR_STRING = _IP_ADDR_STRING;
{$EXTERNALSYM IP_ADDR_STRING}
  TIpAddrString = IP_ADDR_STRING;
  PIpAddrString = PIP_ADDR_STRING;

  time_t = Longint;

  //
  // ADAPTER_INFO - per-adapter information. All IP addresses are stored as
  // strings
  //

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
{$EXTERNALSYM PIP_ADAPTER_INFO}

  _IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0 .. MAX_ADAPTER_NAME_LENGTH + 3] of Char;
    Description: array [0 .. MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
    AddressLength: UINT;
    Address: array [0 .. MAX_ADAPTER_ADDRESS_LENGTH - 1] of Byte;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;
{$EXTERNALSYM _IP_ADAPTER_INFO}

  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;
{$EXTERNALSYM IP_ADAPTER_INFO}
  TIpAdapterInfo = IP_ADAPTER_INFO;
  PIpAdapterInfo = PIP_ADAPTER_INFO;

const
  MAXLEN_IFDESCR = 256;
{$EXTERNALSYM MAXLEN_IFDESCR}
  MAXLEN_PHYSADDR = 8;
{$EXTERNALSYM MAXLEN_PHYSADDR}
  MIB_IPNET_TYPE_OTHER = 1;
  MIB_IPNET_TYPE_INVALID = 2;
  MIB_IPNET_TYPE_DYNAMIC = 3;
  MIB_IPNET_TYPE_STATIC = 4;
  ANY_SIZE = 1;

type
  PMIB_IPNETROW = ^MIB_IPNETROW;
{$EXTERNALSYM PMIB_IPNETROW}

  _MIB_IPNETROW = record
    dwIndex: DWORD;
    dwPhysAddrLen: DWORD;
    bPhysAddr: array [0 .. MAXLEN_PHYSADDR - 1] of Byte;
    dwAddr: DWORD;
    dwType: DWORD;
  end;
{$EXTERNALSYM _MIB_IPNETROW}

  MIB_IPNETROW = _MIB_IPNETROW;
{$EXTERNALSYM MIB_IPNETROW}
  TMibIpNetRow = MIB_IPNETROW;
  PMibIpNetRow = PMIB_IPNETROW;

type
  PMIB_IPNETTABLE = ^MIB_IPNETTABLE;
{$EXTERNALSYM PMIB_IPNETTABLE}

  _MIB_IPNETTABLE = record
    dwNumEntries: DWORD;
    table: array [0 .. ANY_SIZE - 1] of MIB_IPNETROW;
  end;
{$EXTERNALSYM _MIB_IPNETTABLE}

  MIB_IPNETTABLE = _MIB_IPNETTABLE;
{$EXTERNALSYM MIB_IPNETTABLE}
  TMibIpNetTable = MIB_IPNETTABLE;
  PMibIpNetTable = PMIB_IPNETTABLE;

  IPAddr = ULONG; // An IP address.
  USHORT = Word;
  LPVOID = Pointer;

  IP_OPTION_INFORMATION = record
    Ttl: UCHAR; // Time To Live
    Tos: UCHAR; // Type Of Service
    Flags: UCHAR; // IP header flags
    OptionsSize: UCHAR; // Size in bytes of options data
    OptionsData: PUCHAR; // Pointer to options data
  end;
{$EXTERNALSYM IP_OPTION_INFORMATION}

  PIP_OPTION_INFORMATION = ^IP_OPTION_INFORMATION;
{$EXTERNALSYM PIP_OPTION_INFORMATION}
  TIpOptionInformation = IP_OPTION_INFORMATION;
  PIpOptionInformation = PIP_OPTION_INFORMATION;

  ICMP_ECHO_REPLY = record
    Address: IPAddr; // Replying address
    Status: ULONG; // Reply IP_STATUS
    RoundTripTime: ULONG; // RTT in milliseconds
    DataSize: USHORT; // Reply data size in bytes
    Reserved: USHORT; // Reserved for system use
    Data: LPVOID; // Pointer to the reply data
    Options: IP_OPTION_INFORMATION; // Reply options
  end;
{$EXTERNALSYM ICMP_ECHO_REPLY}

  PICMP_ECHO_REPLY = ^ICMP_ECHO_REPLY;
{$EXTERNALSYM PICMP_ECHO_REPLY}
  TIcmpEchoReply = ICMP_ECHO_REPLY;
  PIcmpEchoReply = PICMP_ECHO_REPLY;

  { --- variables for network communication -------------------------------------- }
var
  WSACreateEvent: function: THandle stdcall;
  WSAResetEvent: function(hEvent: THandle): Boolean stdcall;
  WSACloseEvent: function(hEvent: THandle): Boolean stdcall;
  WSAEventSelect: function(S: TSocket; hEventObject: THandle; lNetworkEvents: Integer): Integer stdcall;
  getaddrinfo: function(NodeName: PAnsiChar; ServiceName: PAnsiChar; Hints: PAddrInfo; ppResult: PPaddrinfo): Integer; stdcall;
  getaddrinfoW: function(NodeName: PWideChar; ServiceName: PWideChar; Hints: PAddrInfoW; ppResult: PPaddrinfoW): Integer; stdcall;
  WsaData: TWSADATA;
  WsockErr: Integer;
  WSARECVFROM: function(const S: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD; var lpNumberOfBytesRecvd: DWORD; var lpFlags: DWORD; lpFrom: PSOCKADDR;
    lpFromlen: PInteger; AOverlapped: Pointer; lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;
  WSASOCKETA: function(af, iType, protocol: Integer; lpProtocolInfo: LPWSAPROTOCOL_INFOA; g: GROUP; dwFlags: DWORD): TSocket; stdcall;
  WSASOCKETW: function(af, iType, protocol: Integer; lpProtocolInfo: LPWSAPROTOCOL_INFOW; g: GROUP; dwFlags: DWORD): TSocket; stdcall;
  WSASOCKET: function(af, iType, protocol: Integer; lpProtocolInfo: LPWSAPROTOCOL_INFOA; g: GROUP; dwFlags: DWORD): TSocket; stdcall;
  WSAWAITFORMULTIPLEEVENTS: function(cEvents: DWORD; lphEvents: PWSAEVENT; fWaitAll: LongBool; dwTimeout: DWORD; fAlertable: LongBool): DWORD; stdcall;
  WSAGETOVERLAPPEDRESULT: function(const S: TSocket; AOverlapped: Pointer; lpcbTransfer: LPDWORD; fWait: BOOL; var lpdwFlags: DWORD): WordBool; stdcall;
  IcmpCreateFile: function: THandle; stdcall;
  IcmpCloseHandle: function(IcmpHandle: THandle): LongBool; stdcall;
  IcmpSendEcho: function(const IcmpHandle: THandle; DestinationAddress: Cardinal; RequestData: Pointer; RequestSize: SmallInt; RequestOptions: Pointer;
    ReplyBuffer: Pointer; ReplySize: DWORD; Timeout: DWORD): Cardinal; stdcall;
  SendARP: function(DestIP: Cardinal; SrcIP: Cardinal; var MacAddr; var PhyAddrLen: Cardinal): Cardinal; stdcall;
  GetAdaptersInfo: function(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;
  CreateIpNetEntry: function(const pArpEntry: MIB_IPNETROW): DWORD; stdcall;
  SetIpNetEntry: function(const pArpEntry: MIB_IPNETROW): DWORD; stdcall;
  DeleteIpNetEntry: function(const pArpEntry: MIB_IPNETROW): DWORD; stdcall;
  FlushIpNetTable: function(dwIfIndex: DWORD): DWORD; stdcall;
  GetIpNetTable: function(pIpNetTable: PMIB_IPNETTABLE; var pdwSize: ULONG; bOrder: BOOL): DWORD; stdcall;

const
  { Network Error Message }
  sWSANOTINITIALISED = 'A successful WSAStartup must occur before using this function.';
  sWSAENETDOWN = 'The network subsystem has failed.';
  sWSAEFAULT = 'The buf argument is not totally contained in a valid part of the user address space.';
  sWSAENOTCONN = 'The socket is not connected.';
  sWSAEINTR = 'The (blocking) call was canceled through WSACancelBlockingCall.';
  sWSAEINPROGRESS = 'A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.';
  sWSAENETRESET = 'The connection has been broken due to the remote host resetting.';
  sWSAENOTSOCK = 'The descriptor is not a socket.';
  sWSAEOPNOTSUPP =
    'MSG_OOB was specified, but the socket is not stream style such as type SOCK_STREAM, out-of-band data is not supported in the communication domain associated with this socket, or the socket is unidirectional and supports only send operations.';
  sWSAESHUTDOWN =
    'The socket has been shut down; it is not possible to recv on a socket after shutdown has been invoked with how set to SD_RECEIVE or SD_BOTH.';
  sWSAEWOULDBLOCK = 'The socket is marked as nonblocking and the receive operation would block.';
  sWSAEMSGSIZE = 'The message was too large to fit into the specified buffer and was truncated.';
  sWSAEINVAL =
    'The socket has not been bound with bind, or an unknown flag was specified, or MSG_OOB was specified for a socket with SO_OOBINLINE enabled or (for byte stream sockets only) len was zero or negative.';
  sWSAECONNABORTED =
    'The virtual circuit was terminated due to a time-out or other failure. The application should close the socket as it is no longer usable.';
  sWSAETIMEDOUT = 'The connection has been dropped because of a network failure or because the peer system failed to respond.';
  sWSAECONNRESET = 'The virtual circuit was reset by the remote side executing a "hard" or "abortive" close.' +
    ' The application should close the socket as it is no longer usable. On a UDP datagram socket this error would indicate that a previous send operation resulted in an ICMP "Port Unreachable" message.';
  iphlpapiDll = 'iphlpapi.dll';

function LoadWinSock2(bWSAStart: Boolean = false): Boolean;
Function WSAGetLastErrorStr: String;

implementation

var
  hWinSock2, hiphlpapi: THandle;

  { --- return the variable with highest value ----------------------------------- }
function Max(AValueOne, AValueTwo: Integer): Integer;
begin
  if AValueOne < AValueTwo then
  begin
    Result := AValueTwo
  end
  else
  begin
    Result := AValueOne;
  end;
end;

{ --- return the variable with lowest value ------------------------------------ }
function Min(AValueOne, AValueTwo: Integer): Integer;
begin
  if AValueOne > AValueTwo then
  begin
    Result := AValueTwo
  end
  else
  begin
    Result := AValueOne;
  end;
end;

{ --- Stop the Network server -------------------------------------------------- }
Procedure StopServer;
begin
  WSACleanup;
end;

{ --- initalize the winsock2 functions to support events for communication ----- }
function LoadWinSock2(bWSAStart: Boolean = false): Boolean;
const
  DLLName = 'ws2_32.dll';
begin
  Result := hWinSock2 > 0;
  if Result then
    Exit;
  if bWSAStart then
  begin
    WsockErr := WSAStartup($0202, WsaData);
    Result := WsockErr = 0;
    if not Result then
      Exit;
  end;
  hWinSock2 := LoadLibraryA(PAnsiChar(DLLName));
  hiphlpapi := LoadLibraryA(PAnsiChar(iphlpapiDll));
  Result := hWinSock2 > 0;
  if Result then
  begin
    WSACreateEvent := GetProcAddress(hWinSock2, 'WSACreateEvent');
    WSAResetEvent := GetProcAddress(hWinSock2, 'WSAResetEvent');
    WSACloseEvent := GetProcAddress(hWinSock2, 'WSACloseEvent');
    WSAEventSelect := GetProcAddress(hWinSock2, 'WSAEventSelect');
    getaddrinfo := GetProcAddress(hWinSock2, 'getaddrinfo');
    getaddrinfoW := GetProcAddress(hWinSock2, 'getaddrinfoW');
    WSARECVFROM := GetProcAddress(hWinSock2, 'WSARecvFrom');
    WSASOCKETA := GetProcAddress(hWinSock2, 'WSASocketA');
    WSASOCKETW := GetProcAddress(hWinSock2, 'WSASocketW');
    WSASOCKET := WSASOCKETA;
    WSAWAITFORMULTIPLEEVENTS := GetProcAddress(hWinSock2, 'WSAWaitForMultipleEvents');
    WSAGETOVERLAPPEDRESULT := GetProcAddress(hWinSock2, 'WSAGetOverlappedResult');
    IcmpCreateFile := GetProcAddress(hiphlpapi, 'IcmpCreateFile');
    IcmpCloseHandle := GetProcAddress(hiphlpapi, 'IcmpCloseHandle');
    IcmpSendEcho := GetProcAddress(hiphlpapi, 'IcmpSendEcho');
    SendARP := GetProcAddress(hiphlpapi, 'SendARP');
    GetAdaptersInfo := GetProcAddress(hiphlpapi, 'GetAdaptersInfo');
    CreateIpNetEntry := GetProcAddress(hiphlpapi, 'CreateIpNetEntry');
    SetIpNetEntry := GetProcAddress(hiphlpapi, 'SetIpNetEntry');
    DeleteIpNetEntry := GetProcAddress(hiphlpapi, 'DeleteIpNetEntry');
    FlushIpNetTable := GetProcAddress(hiphlpapi, 'FlushIpNetTable');
    GetIpNetTable := GetProcAddress(hiphlpapi, 'GetIpNetTable');
  end;
end;

Function WSAGetLastErrorStr: String;
var
  Err: Integer;
begin
  Err := WSAGetLastError;
  case Err of
    WSANOTINITIALISED:
      Result := sWSANOTINITIALISED;
    WSAENETDOWN:
      Result := sWSAENETDOWN;
    WSAEFAULT:
      Result := sWSAEFAULT;
    WSAENOTCONN:
      Result := sWSAENOTCONN;
    WSAEINTR:
      Result := sWSAEINTR;
    WSAEINPROGRESS:
      Result := sWSAEINPROGRESS;
    WSAENETRESET:
      Result := sWSAENETRESET;
    WSAENOTSOCK:
      Result := sWSAENOTSOCK;
    WSAEOPNOTSUPP:
      Result := sWSAEOPNOTSUPP;
    WSAESHUTDOWN:
      Result := sWSAESHUTDOWN;
    WSAEWOULDBLOCK:
      Result := sWSAEWOULDBLOCK;
    WSAEMSGSIZE:
      Result := sWSAEMSGSIZE;
    WSAEINVAL:
      Result := sWSAEINVAL;
    WSAECONNABORTED:
      Result := sWSAECONNABORTED;
    WSAETIMEDOUT:
      Result := sWSAETIMEDOUT;
    WSAECONNRESET:
      Result := sWSAECONNRESET;
  else
    begin
      Str(Err, Result);
      Result := 'Unknown WSA error ' + Result;
    end;
  end;

end;

end.
