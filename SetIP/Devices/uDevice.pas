unit uDevice;

interface

type
  TDevice = class
  private
    FIPAddress: string;
    FSubnetMask: string;
    FDefaultGateway: string;
    FSshPort: integer;
    FUser: string;
    FPassword: string;
    FNetInterface: string; // eth0 or enp0s3 or other...
    FNetConfigFileName: string;

  public
    property IPAddress: string read FIPAddress write FIPAddress;
    property SubnetMask: string read FSubnetMask write FSubnetMask;
    property DefaultGateway: string read FDefaultGateway write FDefaultGateway;
    property SshPort: integer read FSshPort write FSshPort;
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
end;

destructor TDeviceManager.Destroy;
begin
  Self.FTR40.Free;
  Self.FIO40.Free;
end;

procedure TDeviceManager.LoadFromFile;
begin

end;

procedure TDeviceManager.SaveToFile;
begin

end;

end.
