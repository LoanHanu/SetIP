unit uDevice;

interface

uses
  System.Classes, System.IOUtils, System.Generics.Collections, System.Types, System.SysUtils;

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

  public
    constructor Create;
    destructor Destroy; override;

    property TR40: TTR40 read FTR40;
    property OP40: TIO40 read FIO40;

  public
    procedure LoadFromFile;
    procedure SaveToFile;
  end;

implementation

constructor TDeviceManager.Create;
begin
  Self.FTR40 := TTR40.Create;
  Self.FTR40.FIPAddress := '10.99.4.24';
  Self.FTR40.FSubnetMask := '255.255.255.0';
  Self.FTR40.FDefaultGateway := '10.99.4.1';
  Self.FTR40.FSshPort := 22;
  Self.FTR40.FUser := 'ubuntu';
  Self.FTR40.FPassword := 'ubuntu';

  Self.FIO40 := TIO40.Create;
  Self.FIO40.FIPAddress := '10.99.4.25';
  Self.FIO40.FSshPort := 22;

  Self.FDevConfigFileName := 'devconfig.ini';
end;

destructor TDeviceManager.Destroy;
begin
  Self.FTR40.Free;
  Self.FIO40.Free;
end;

procedure TDeviceManager.LoadFromFile;
var
  lines: TStringList;
  line: string;
  i: integer;
begin
  if FileExists(Self.FDevConfigFileName) then
  begin
    lines := TStringList.Create;
    lines.LoadFromFile(Self.FDevConfigFileName);

    i := lines.IndexOf('[TR40]');
    Self.FTR40.FIPAddress := lines[i+1].Split([':'])[1];
    Self.FTR40.FSubnetMask := lines[i+2].Split([':'])[1];
    Self.FTR40.FDefaultGateway := lines[i+3].Split([':'])[1];
    Self.FTR40.FSshPort := StrToIntDef(lines[i+4].Split([':'])[1], 22);
    Self.FTR40.FUser := lines[i+5].Split([':'])[1];
    Self.FTR40.FPassword := lines[i+6].Split([':'])[1];

    i := lines.IndexOf('[IO40]');
    Self.FIO40.FIPAddress := lines[i+1].Split([':'])[1];
    Self.FIO40.FSubnetMask := lines[i+2].Split([':'])[1];
    Self.FIO40.FDefaultGateway := lines[i+3].Split([':'])[1];
    Self.FIO40.FSshPort := StrToIntDef(lines[i+4].Split([':'])[1], 22);
    Self.FIO40.FUser := lines[i+5].Split([':'])[1];
    Self.FIO40.FPassword := lines[i+6].Split([':'])[1];

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
