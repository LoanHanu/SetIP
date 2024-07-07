unit uDevice;

interface

uses
  System.Classes, System.IOUtils, System.Generics.Collections, System.Types, System.SysUtils, System.IniFiles,
  uSshClient, uPuttySshClient, uLibSsh2Client, IdModbusClient;

const
  {
    device type on modbus: the address 0x200: 0x20 = IO40, 0x0101 = TR40
    Mac address from IO40 (0x201-0x203)
  }
  MODBUS_DEVTYPE_ADDR = $0200;
  DEVTYPE_TR40 = $0101;
  DEVTYPE_IO40 = $0020;
  MODBUS_MAC_ADDR = $201; // $201 ~ $203 : 3 registers

type
  THostConnectionOption = (hcSsh, hcModbus);

  TDeviceType = (dtNone, dtTR40, dtIO40);

  TDevice = class
  protected
    FIPAddress: string;
    FSubnetMask: string;
    FDefaultGateway: string;
    FSshPort: Integer;
    FModbusPort: Integer;
    FUser: string;
    FPassword: string;

    FHostConnectionOption: THostConnectionOption;

    FNetInterface: string; // eth0 or enp0s3 or other...
    FNetConfigFileName: string;

    FSshClient: TSshClient;
    FModbusClient: TIdModBusClient;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ConnectSsh; virtual;
    function TryPing(hostIP: string): Boolean; virtual;
    function ChangeHostIP(newIP, newMask, newGate: string): Boolean; virtual;
    function ChangeHostPass(newPass: string): Boolean; virtual;

    function GetDevTypeViaModbus(hostIP: string; port: Integer = 502): TDeviceType;
    function GetMacAddrViaModbus(hostIP: string; port: Integer = 502): string;

    property IPAddress: string read FIPAddress write FIPAddress;
    property SubnetMask: string read FSubnetMask write FSubnetMask;
    property DefaultGateway: string read FDefaultGateway write FDefaultGateway;
    property SshPort: Integer read FSshPort write FSshPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property NetInterface: string read FNetInterface write FNetInterface;
    property NetConfigFileName: string read FNetConfigFileName write FNetConfigFileName;

    property SshClient: TSshClient read FSshClient;
    property ModbusClient: TIdModBusClient read FModbusClient;

  end;

  TTR40 = class(TDevice)

  end;

  TIO40 = class(TDevice)

  end;

  TDeviceManager = class
  private
    FTR40: TTR40;
    FIO40: TIO40;

    FST3800IniFile: string;
    FDevConfigFileName: string;

    STIniFile: TIniFile;

  public
    constructor Create;
    destructor Destroy; override;

    property TR40: TTR40 read FTR40;
    property IO40: TIO40 read FIO40;

  public
    procedure LoadFromFile;
    procedure SaveToFile;
  end;

implementation

uses
  IdIcmpClient;

const
  T = '!" #$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ';

Function GetPW: String;
begin
  // hide Password in binary
  Result := T[51] + T[48] + T[51] + T[17] + T[18] + T[16] + T[21] + T[34] + T[45] + T[55] + T[1] + T[52] + T[50] + T[20] + T[16];
end;

{ TDevice }
constructor TDevice.Create;
begin
  Self.FSshClient := TLibSsh2Client.Create;
  Self.FModbusClient := TIdModBusClient.Create(nil);

  // default port numbers
  Self.FSshPort := 22;
  Self.FModbusPort := 502;
end;

destructor TDevice.Destroy;
begin
  Self.FSshClient.Free;
  Self.FModbusClient.Free;

end;

procedure TDevice.ConnectSsh;
begin
  Self.FSshClient.Connect;
end;

function TDevice.TryPing(hostIP: string): Boolean;
var
  res: Boolean;
  Ping: TIdIcmpClient;
begin
  res := False;
  Ping := TIdIcmpClient.Create(nil);
  // Ping.ReceiveTimeout := 3000; // 5s

  try
    Ping.Host := hostIP;
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
      raise E;
      // ShowMessage('An exception occurred: ' + E.Message);
    end;
  end;

  Ping.Free;
  Result := res;
end;

function TDevice.ChangeHostIP(newIP: string; newMask: string; newGate: string): Boolean;
begin
  Result := Self.FSshClient.ChangeIP(newIP, newMask, newGate);
end;

function TDevice.ChangeHostPass(newPass: string): Boolean;
begin
  Result := False;
end;

function TDevice.GetDevTypeViaModbus(hostIP: string; port: Integer = 502): TDeviceType;
var
  devType: Word;
begin
  Self.FModbusClient.Host := hostIP;

  Self.FModbusClient.port := Self.FModbusPort;
  Self.FModbusClient.UnitID := $FF;
  Self.FModbusClient.BaseRegister := 0;
  // Self.FModbusClient.Port := port;

  try
    if Self.FModbusClient.ReadHoldingRegister(MODBUS_DEVTYPE_ADDR, devType) then
    begin
      if devType = DEVTYPE_TR40 then
        Result := dtTR40
      else if devType = DEVTYPE_IO40 then
        Result := dtIO40
      else
        Result := dtNone;
    end;
  except
    on E: Exception do
    begin
      Result := dtNone;
      raise E;
    end;
  end;
end;

function TDevice.GetMacAddrViaModbus(hostIP: string; port: Integer = 502): string;
var
  regArray: array [0 .. 2] of Word;
  hbyte, lbyte: UInt8;
begin
  Self.FModbusClient.Host := hostIP;

  Self.FModbusClient.port := Self.FModbusPort;
  Self.FModbusClient.UnitID := $FF;
  Self.FModbusClient.BaseRegister := 0;
  // Self.FModbusClient.Port := port;

  try
    if Self.FModbusClient.ReadHoldingRegisters(MODBUS_MAC_ADDR, 3, regArray) then
    begin
      // Result := Format('%s%s%s', [IntToHex(regArray[0], 4), IntToHex(regArray[1], 4), IntToHex(regArray[2], 4)]);

      Result := '';
      hbyte := (regArray[0] and $FF00) shr 8;
      lbyte := regArray[0] and $00FF;
      Result := Result + IntToHex(hbyte) + ':' + IntToHex(lbyte) + ':';

      hbyte := (regArray[1] and $FF00) shr 8;
      lbyte := regArray[1] and $00FF;
      Result := Result + IntToHex(hbyte) + ':' + IntToHex(lbyte) + ':';

      hbyte := (regArray[2] and $FF00) shr 8;
      lbyte := regArray[2] and $00FF;
      Result := Result + IntToHex(hbyte) + ':' + IntToHex(lbyte);

    end;

  finally

  end;
end;

{ TTR40 }

{ TIO40 }

{ TDeviceManager }
constructor TDeviceManager.Create;
begin
  Self.FTR40 := TTR40.Create;
  Self.FTR40.FIPAddress := '192.168.0.235';
  Self.FTR40.FSubnetMask := '255.255.255.0';
  Self.FTR40.FDefaultGateway := '0.0.0.0';
  Self.FTR40.FSshPort := 22;
  Self.FTR40.FModbusPort := 502;
  Self.FTR40.FUser := 'root';
  Self.FTR40.FPassword := GetPW;

  Self.FIO40 := TIO40.Create;
  Self.FIO40.FIPAddress := '192.168.0.250';
  Self.FIO40.FSubnetMask := '255.255.255.0';
  Self.FIO40.FDefaultGateway := '0.0.0.0';
  Self.FIO40.FSshPort := 22;
  Self.FIO40.FModbusPort := 502;

  Self.FST3800IniFile := 'ST3800.ini';
  Self.FDevConfigFileName := 'devconfig.ini';

  // STIniFile := TIniFile.Create(ExpandFileName(FST3800IniFile));
  STIniFile := TIniFile.Create(ExpandFileName(FDevConfigFileName));
end;

destructor TDeviceManager.Destroy;
begin
  Self.FTR40.Free;
  Self.FIO40.Free;
  STIniFile.Free;
end;

procedure TDeviceManager.LoadFromFile;
var
  lines: TStringList;
  line: string;
  i: Integer;
begin
  // STIniFile := TIniFile.Create(ExpandFileName('ST3800.ini'));
  // Self.FTR40.FIPAddress := STIniFile.ReadString('TestSystem', 'TR40IP', Self.FTR40.FIPAddress);
  // Self.FIO40.FIPAddress := STIniFile.ReadString('TestSystem', 'IO40Host1', Self.FIO40.FIPAddress);
  // Self.FIO40_2.FIPAddress:=STIniFile.ReadString('TestSystem','IO40Host2',Self.FIO40_2.FIPAddress);
  // Self.FIO40_3.FIPAddress:=STIniFile.ReadString('TestSystem','IO40Host3',Self.FIO40_3.FIPAddress);

  if FileExists(Self.FDevConfigFileName) then
  // if FileExists(Self.FST3800IniFile) then
  begin
    Self.FTR40.FIPAddress := STIniFile.ReadString('TR40', 'TR40IP', Self.FTR40.FIPAddress);
    Self.FTR40.FSubnetMask := STIniFile.ReadString('TR40', 'TR40Mask', Self.FTR40.FSubnetMask);
    Self.FTR40.FDefaultGateway := STIniFile.ReadString('TR40', 'TR40Gate', Self.FTR40.FDefaultGateway);
    Self.FTR40.FSshPort := StrToIntDef(STIniFile.ReadString('TR40', 'TR40SshPort', Self.FTR40.FSshPort.ToString), 22);
    Self.FTR40.FUser := STIniFile.ReadString('TR40', 'TR40User', Self.FTR40.FUser);
    Self.FTR40.FPassword := STIniFile.ReadString('TR40', 'TR40Pass', Self.FTR40.FPassword);

    Self.FIO40.FIPAddress := STIniFile.ReadString('IO40', 'IO40IP', Self.FIO40.FIPAddress);
    Self.FIO40.FSubnetMask := STIniFile.ReadString('IO40', 'IO40Mask', Self.FIO40.FSubnetMask);
    Self.FIO40.FDefaultGateway := STIniFile.ReadString('IO40', 'IO40Gate', Self.FIO40.FDefaultGateway);
    Self.FIO40.FSshPort := StrToIntDef(STIniFile.ReadString('IO40', 'IO40SshPort', Self.FIO40.FSshPort.ToString), 22);
    Self.FIO40.FUser := STIniFile.ReadString('IO40', 'IO40User', Self.FIO40.FUser);
    Self.FIO40.FPassword := STIniFile.ReadString('IO40', 'IO40Pass', Self.FIO40.FPassword);

    // lines := TStringList.Create;
    // lines.LoadFromFile(Self.FDevConfigFileName);
    //
    // i := lines.IndexOf('[TR40]');
    // Self.FTR40.FIPAddress := lines[i + 1].Split([':'])[1];
    // Self.FTR40.FSubnetMask := lines[i + 2].Split([':'])[1];
    // Self.FTR40.FDefaultGateway := lines[i + 3].Split([':'])[1];
    // Self.FTR40.FSshPort := StrToIntDef(lines[i + 4].Split([':'])[1], 22);
    // Self.FTR40.FUser := lines[i + 5].Split([':'])[1];
    // Self.FTR40.FPassword := lines[i + 6].Split([':'])[1];
    //
    // i := lines.IndexOf('[IO40]');
    // Self.FIO40.FIPAddress := lines[i + 1].Split([':'])[1];
    // Self.FIO40.FSubnetMask := lines[i + 2].Split([':'])[1];
    // Self.FIO40.FDefaultGateway := lines[i + 3].Split([':'])[1];
    // Self.FIO40.FSshPort := StrToIntDef(lines[i + 4].Split([':'])[1], 22);
    // Self.FIO40.FUser := lines[i + 5].Split([':'])[1];
    // Self.FIO40.FPassword := lines[i + 6].Split([':'])[1];
    //
    // lines.Free;
  end;
end;

procedure TDeviceManager.SaveToFile;
var
  lines: TStringList;
  line: string;
begin
  STIniFile.WriteString('TR40', 'TR40IP', Self.FTR40.FIPAddress);
  STIniFile.WriteString('TR40', 'TR40Mask', Self.FTR40.FSubnetMask);
  STIniFile.WriteString('TR40', 'TR40Gate', Self.FTR40.FDefaultGateway);
  STIniFile.WriteString('TR40', 'TR40SshPort', Self.FTR40.FSshPort.ToString);
  STIniFile.WriteString('TR40', 'TR40User', Self.FTR40.FUser);
  STIniFile.WriteString('TR40', 'TR40Pass', Self.FTR40.FPassword);

  STIniFile.WriteString('IO40', 'IO40IP', Self.FIO40.FIPAddress);
  STIniFile.WriteString('IO40', 'IO40Mask', Self.FIO40.FSubnetMask);
  STIniFile.WriteString('IO40', 'IO40Gate', Self.FIO40.FDefaultGateway);
  STIniFile.WriteString('IO40', 'IO40SshPort', Self.FIO40.FSshPort.ToString);
  STIniFile.WriteString('IO40', 'IO40User', Self.FIO40.FUser);
  STIniFile.WriteString('IO40', 'IO40Pass', Self.FIO40.FPassword);

  // lines := TStringList.Create;
  //
  // // TR40...
  // lines.Add('[TR40]');
  // lines.Add('IP:' + Self.FTR40.FIPAddress);
  // lines.Add('MASK:' + Self.FTR40.FSubnetMask);
  // lines.Add('GATE:' + Self.FTR40.FDefaultGateway);
  // lines.Add('SSHPORT:' + Self.FTR40.FSshPort.ToString);
  // lines.Add('USER:' + Self.FTR40.FUser);
  // lines.Add('PW:' + Self.FTR40.FPassword);
  //
  // // IO40...
  // lines.Add('[IO40]');
  // lines.Add('IP:' + Self.FIO40.FIPAddress);
  // lines.Add('MASK:' + Self.FIO40.FSubnetMask);
  // lines.Add('GATE:' + Self.FIO40.FDefaultGateway);
  // lines.Add('SSHPORT:' + Self.FIO40.FSshPort.ToString);
  // lines.Add('USER:' + Self.FIO40.FUser);
  // lines.Add('PW:' + Self.FIO40.FPassword);
  //
  // lines.SaveToFile(Self.FDevConfigFileName);
  // lines.Free;
end;

end.
