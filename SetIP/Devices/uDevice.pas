unit uDevice;

interface

uses
  System.Classes, System.IOUtils, System.Generics.Collections, System.Types, System.SysUtils, System.IniFiles;

type
  TDevice = class
  private
    FIPAddress: string;
    FSubnetMask: string;
    FDefaultGateway: string;
    FSshPort: Integer;
    FUser: string;
    FPassword: string;

    FNetInterface: string; // eth0 or enp0s3 or other...
    FNetConfigFileName: string;

  public
    property IPAddress: string read FIPAddress write FIPAddress;
    property SubnetMask: string read FSubnetMask write FSubnetMask;
    property DefaultGateway: string read FDefaultGateway write FDefaultGateway;
    property SshPort: Integer read FSshPort write FSshPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property NetInterface: string read FNetInterface write FNetInterface;
    property NetConfigFileName: string read FNetConfigFileName write FNetConfigFileName;
  end;

  TIO40 = class(TDevice)

  end;

  TTR40 = class(TDevice)

  end;

  TDeviceManager = class
  private
    FTR40: TTR40;
    FIO40: TIO40;

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

const
  T = '!" #$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ';

Function GetPW: String;
begin
  // hide Password in binary
  Result := T[51] + T[48] + T[51] + T[17] + T[18] + T[16] + T[21] + T[34] + T[45] + T[55] + T[1] + T[52] + T[50] + T[20] + T[16];
end;

constructor TDeviceManager.Create;
begin
  Self.FTR40 := TTR40.Create;
  Self.FTR40.FIPAddress := '192.168.0.235';
  Self.FTR40.FSubnetMask := '255.255.255.0';
  Self.FTR40.FDefaultGateway := '0.0.0.0';
  Self.FTR40.FSshPort := 22;
  Self.FTR40.FUser := 'root';
  Self.FTR40.FPassword := GetPW;

  Self.FIO40 := TIO40.Create;
  Self.FIO40.FIPAddress := '192.168.0.250';
  Self.FIO40.FSshPort := 22;

  Self.FDevConfigFileName := 'devconfig.ini';

  STIniFile := TIniFile.Create(ExpandFileName('ST3800.ini'));
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
  STIniFile := TIniFile.Create(ExpandFileName('ST3800.ini'));
  Self.FTR40.FIPAddress := STIniFile.ReadString('TestSystem', 'TR40IP', Self.FTR40.FIPAddress);
  Self.FIO40.FIPAddress := STIniFile.ReadString('TestSystem', 'IO40Host1', Self.FIO40.FIPAddress);
  // Self.FIO40_2.FIPAddress:=STIniFile.ReadString('TestSystem','IO40Host2',Self.FIO40_2.FIPAddress);
  // Self.FIO40_3.FIPAddress:=STIniFile.ReadString('TestSystem','IO40Host3',Self.FIO40_3.FIPAddress);

  if FileExists(Self.FDevConfigFileName) then
  begin
    lines := TStringList.Create;
    lines.LoadFromFile(Self.FDevConfigFileName);

    i := lines.IndexOf('[TR40]');
    Self.FTR40.FIPAddress := lines[i + 1].Split([':'])[1];
    Self.FTR40.FSubnetMask := lines[i + 2].Split([':'])[1];
    Self.FTR40.FDefaultGateway := lines[i + 3].Split([':'])[1];
    Self.FTR40.FSshPort := StrToIntDef(lines[i + 4].Split([':'])[1], 22);
    Self.FTR40.FUser := lines[i + 5].Split([':'])[1];
    Self.FTR40.FPassword := lines[i + 6].Split([':'])[1];

    i := lines.IndexOf('[IO40]');
    Self.FIO40.FIPAddress := lines[i + 1].Split([':'])[1];
    Self.FIO40.FSubnetMask := lines[i + 2].Split([':'])[1];
    Self.FIO40.FDefaultGateway := lines[i + 3].Split([':'])[1];
    Self.FIO40.FSshPort := StrToIntDef(lines[i + 4].Split([':'])[1], 22);
    Self.FIO40.FUser := lines[i + 5].Split([':'])[1];
    Self.FIO40.FPassword := lines[i + 6].Split([':'])[1];

    lines.Free;
  end;
end;

procedure TDeviceManager.SaveToFile;
var
  lines: TStringList;
  line: string;
begin
  lines := TStringList.Create;

  // TR40...
  lines.Add('[TR40]');
  lines.Add('IP:' + Self.FTR40.FIPAddress);
  lines.Add('MASK:' + Self.FTR40.FSubnetMask);
  lines.Add('GATE:' + Self.FTR40.FDefaultGateway);
  lines.Add('SSHPORT:' + Self.FTR40.FSshPort.ToString);
  lines.Add('USER:' + Self.FTR40.FUser);
  lines.Add('PW:' + Self.FTR40.FPassword);

  // IO40...
  lines.Add('[IO40]');
  lines.Add('IP:' + Self.FIO40.FIPAddress);
  lines.Add('MASK:' + Self.FIO40.FSubnetMask);
  lines.Add('GATE:' + Self.FIO40.FDefaultGateway);
  lines.Add('SSHPORT:' + Self.FIO40.FSshPort.ToString);
  lines.Add('USER:' + Self.FIO40.FUser);
  lines.Add('PW:' + Self.FIO40.FPassword);

  lines.SaveToFile(Self.FDevConfigFileName);
  lines.Free;
end;

end.
