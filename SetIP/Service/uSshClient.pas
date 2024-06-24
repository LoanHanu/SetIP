unit uSshClient;

interface

uses
  Windows, Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, System.IOUtils, IdIcmpClient, uDosCommand;

type
  TSshConnectionOption = (coPassword, coIdKey);

  TSshClient = class
  protected
    FProcessPath: string;
    FHostName: string;
    FPort: integer;
    FUser: string;
    FPassword: string;
    FPrivateKey: string; // path to private key file
    FIsConnecting: Boolean; // flag for indicating the state from 'sending connection cmd' to 'receiving login reply'
    FIsConnected: Boolean;
    FConnectionOption: TSshConnectionOption;

    FLockObject: TCriticalSection;

    FDosCommand: TDosCommand;

  

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function TryPing(HostIP: string): Boolean; virtual;
    procedure Connect; virtual;
    procedure DisConnect; virtual;
    function ChangeIP(newIP, newMask, newGate: string): Boolean; virtual;

  public
    property ProcessPath: string read FProcessPath write FProcessPath;
    property HostName: string read FHostName write FHostName;
    property Port: integer read FPort write FPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property IsConnecting: Boolean read FIsConnecting write FIsConnecting;
    property IsConnected: Boolean read FIsConnected write FIsConnected;
    property ConnectionOption: TSshConnectionOption read FConnectionOption write FConnectionOption;

    property DosCommand: TDosCommand read FDosCommand;
  end;

implementation

constructor TSshClient.Create;
begin
  FHostName := 'localhost'; // or host ip address
  FPort := 22;
  FIsConnecting := False;
  FIsConnected := False;
  FConnectionOption := coPassword;

  FLockObject := TCriticalSection.Create;

  FDosCommand := TDosCommand.Create(nil);
  // FDosCommand.MaxTimeAfterBeginning := 5;
  // FDosCommand.MaxTimeAfterLastOutput := 5;
  FDosCommand.InputToOutput := True;

  
end;

destructor TSshClient.Destroy;
begin
  inherited;

  FLockObject.Free;
  FDosCommand.Free;
end;

function TSshClient.TryPing(HostIP: string): Boolean;
var
  res: Boolean;
  Ping: TIdIcmpClient;
begin
  res := False;
  Ping := TIdIcmpClient.Create(nil);
  Ping.ReceiveTimeout := 3000; // 5s

  try
    Ping.Host := HostIP;
    Ping.Ping();
    // Ping.Ping('0000');
    // case Ping.ReplyStatus.ReplyStatusType of
    // rsEcho: ;
    // rsError: ;
    // rsTimeOut: ;
    // rsErrorUnreachable: ;
    // rsErrorTTLExceeded: ;
    // rsErrorPacketTooBig: ;
    // rsErrorParameter: ;
    // rsErrorDatagramConversion: ;
    // rsErrorSecurityFailure: ;
    // rsSourceQuench: ;
    // rsRedirect: ;
    // rsTimeStamp: ;
    // rsInfoRequest: ;
    // rsAddressMaskRequest: ;
    // rsTraceRoute: ;
    // rsMobileHostReg: ;
    // rsMobileHostRedir: ;
    // rsIPv6WhereAreYou: ;
    // rsIPv6IAmHere: ;
    // rsSKIP: ;
    // end;

    if Ping.ReplyStatus.ReplyStatusType = rsEcho then
      // if Ping.ReplyStatus.BytesReceived > 0 then
      res := True
    else
      res := False;
  except
    on E: Exception do
    begin
      res := False;
      // ShowMessage('An exception occurred: ' + E.Message);
    end;
  end;

  Ping.Free;
  Result := res;
end;

procedure TSshClient.Connect;
begin
  Self.FIsConnected := False;
end;

procedure TSshClient.DisConnect;
begin
  Self.FIsConnected := False;
end;

function TSshClient.ChangeIP(newIP, newMask, newGate: string): Boolean;
begin
  Result := False;
end;



end.
