unit uDevice;

interface

type
  TDevice = class
  private
    FIPAddress: string;
    FSubnetMask: string;
    FDefaultGateway: string;

  public
    property IPAddress: string read FIPAddress write FIPAddress;
    property SubnetMask: string read FSubnetMask write FSubnetMask;
    property DefaultGateway: string read FDefaultGateway write FDefaultGateway;
  end;

  TOP40 = class(TDevice)

  end;

  TTR40 = class(TDevice)

  end;

  TDeviceManager = class
  private
    FTR40: TTR40;
    FOP40: TOP40;

  public
    constructor Create;
    destructor Destroy; override;

    property TR40: TTR40 read FTR40;
    property OP40: TOP40 read FOP40;

  public
    procedure LoadFromFile;
  end;

implementation

constructor TDeviceManager.Create;
begin
  Self.FTR40 := TTR40.Create;
  Self.FOP40 := TOP40.Create;
end;

destructor TDeviceManager.Destroy;
begin
  Self.FTR40.Free;
  Self.FOP40.Free;
end;

procedure TDeviceManager.LoadFromFile;
begin

end;

end.
