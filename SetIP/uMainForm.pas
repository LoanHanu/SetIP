unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
  IdIcmpClient, IdComponent, Vcl.Mask, System.IOUtils, System.Generics.Collections, IdBaseComponent, IdRawBase, IdRawClient,
  Winsock2, Winapi.Winsock,
  uDevice, uSshClient, uPuttySshClient, uOpenSshClient, uLibSsh2Client, uDosCommand;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    SheetTR40: TTabSheet;
    SheetIO40: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ButtonTR40Ping: TButton;
    IconList: TImageList;
    Panel1: TPanel;
    EditTR40CurrIPOctet4: TEdit;
    EditTR40CurrIPDot3: TEdit;
    EditTR40CurrIPOctet3: TEdit;
    EditTR40CurrIPDot2: TEdit;
    EditTR40CurrIPOctet2: TEdit;
    EditTR40CurrIPDot1: TEdit;
    EditTR40CurrIPOctet1: TEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Panel2: TPanel;
    EditTR40NewIPOctet4: TEdit;
    Edit2: TEdit;
    EditTR40NewIPOctet3: TEdit;
    Edit4: TEdit;
    EditTR40NewIPOctet2: TEdit;
    Edit6: TEdit;
    EditTR40NewIPOctet1: TEdit;
    Label3: TLabel;
    Panel3: TPanel;
    EditTR40NewMaskOctet4: TEdit;
    Edit9: TEdit;
    EditTR40NewMaskOctet3: TEdit;
    Edit11: TEdit;
    EditTR40NewMaskOctet2: TEdit;
    Edit13: TEdit;
    EditTR40NewMaskOctet1: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Panel4: TPanel;
    EditTR40NewGateOctet4: TEdit;
    Edit16: TEdit;
    EditTR40NewGateOctet3: TEdit;
    Edit18: TEdit;
    EditTR40NewGateOctet2: TEdit;
    Edit20: TEdit;
    EditTR40NewGateOctet1: TEdit;
    ButtonTR40ChangeIP: TButton;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    ButtonIO40Ping: TButton;
    Panel5: TPanel;
    EditIO40CurrIPOctet4: TEdit;
    Edit3: TEdit;
    EditIO40CurrIPOctet3: TEdit;
    Edit7: TEdit;
    EditIO40CurrIPOctet2: TEdit;
    Edit10: TEdit;
    EditIO40CurrIPOctet1: TEdit;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel6: TPanel;
    EditIO40NewIPOctet4: TEdit;
    Edit15: TEdit;
    EditIO40NewIPOctet3: TEdit;
    Edit19: TEdit;
    EditIO40NewIPOctet2: TEdit;
    Edit22: TEdit;
    EditIO40NewIPOctet1: TEdit;
    Panel7: TPanel;
    EditIO40NewMaskOctet4: TEdit;
    Edit25: TEdit;
    EditIO40NewMaskOctet3: TEdit;
    Edit27: TEdit;
    EditIO40NewMaskOctet2: TEdit;
    Edit29: TEdit;
    EditIO40NewMaskOctet1: TEdit;
    Panel8: TPanel;
    EditIO40NewGateOctet4: TEdit;
    Edit32: TEdit;
    EditIO40NewGateOctet3: TEdit;
    Edit34: TEdit;
    EditIO40NewGateOctet2: TEdit;
    Edit36: TEdit;
    EditIO40NewGateOctet1: TEdit;
    ButtonIO40ChangeIP: TButton;
    GroupBox5: TGroupBox;
    Label10: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    EditTR40HostIP: TEdit;
    EditTR40SshPort: TEdit;
    EditTR40User: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    ButtonTR40SshConnect: TButton;
    EditTR40Pass: TButtonedEdit;
    MemoTR40Output: TMemo;
    Label15: TLabel;
    EditTR40Input: TEdit;
    ButtonTR40Input: TButton;
    Label17: TLabel;
    LabelTR40ConnectionState: TLabel;

    ImageTR40PingLed: TImage;
    ImageTR40ConnectLed: TImage;
    ImageTR40ChangeIPLed: TImage;
    GroupBox6: TGroupBox;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    ImageIO40ConnectLed: TImage;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    EditIO40HostIP: TEdit;
    EditIO40SshPort: TEdit;
    EditIO40User: TEdit;
    ButtonIO40SshConnect: TButton;
    EditIO40Pass: TButtonedEdit;
    MemoIO40Output: TMemo;
    EditIO40Input: TEdit;
    ButtonIO40Input: TButton;
    ImageIO40PingLed: TImage;
    ImageIO40ChangeIPLed: TImage;
    ButtonIO40SshDisconnect: TButton;
    ButtonTR40SshDisconnect: TButton;
    GroupBox7: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    EditIO40Mac: TEdit;
    EditIO40IP: TEdit;
    ButtonIO40SaveSettings: TButton;
    ButtonIO40SetIP: TButton;
    MemoIO40State: TMemo;

    procedure OctetEditExit(Sender: TObject);
    procedure OctetChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure ButtonTR40PingClick(Sender: TObject);
    procedure ButtonTR40ChangeIPClick(Sender: TObject);

    procedure ButtonIO40PingClick(Sender: TObject);
    procedure ButtonIO40ChangeIPClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ButtonTR40SshConnectClick(Sender: TObject);
    procedure EditTR40UserChange(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
    procedure EditTR40PassRightButtonClick(Sender: TObject);

    procedure SshClientDosCommandStarted(Sender: TObject);
    procedure SshClientDosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
    procedure SshClientDosCommandTerminated(Sender: TObject);

    procedure ButtonTR40InputClick(Sender: TObject);

    procedure ButtonTR40SshDisconnectClick(Sender: TObject);
    procedure ButtonIO40SaveSettingsClick(Sender: TObject);
    procedure ButtonIO40SetIPClick(Sender: TObject);
    procedure ButtonIO40SshConnectClick(Sender: TObject);
    procedure ButtonIO40SshDisconnectClick(Sender: TObject);
    procedure EditIO40PassRightButtonClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);

  private
    FDeviceManager: TDeviceManager;
    FTR40SshClient: TSshClient;
    FIO40SshClient: TSshClient;

    TxData: array [0 .. 125] of Cardinal;
    RxData: array [0 .. 125] of Cardinal;

    procedure InitUI;

  public
    { Public declarations }
  end;

procedure RemoveArpEntry(sIP: AnsiString);

function ValidAptAddr(p: pointer): Boolean;

function FindNextAdapterID(const IP: Cardinal; var AdapterID: Cardinal): Boolean;

function FindFirstAdapterID(const IP: Cardinal; var AdapterID: Cardinal): Boolean;

var
  MainForm: TMainForm;

  pAdapterInfo, AdapterInfo: PIP_ADAPTER_INFO;
  BufSize: Cardinal;

implementation

{$R *.dfm}

uses
  uCommonUtils;

procedure RemoveArpEntry(sIP: AnsiString);
var
  bSize, IP: Cardinal;
  IpNetTable: PMIB_IPNETTABLE;
  i: Integer;
begin
  IP := inet_addr(PansiChar(sIP));
  if IP = 0 then
    exit;
  GetIpNetTable(nil, bSize, false);
  IpNetTable := GetMemory(bSize);
  if IpNetTable = nil then
    exit;
  try
    GetIpNetTable(IpNetTable, bSize, false);
    if IpNetTable.dwNumEntries > 0 then
      For i := 0 to IpNetTable.dwNumEntries - 1 do
      begin
        if (IpNetTable.table[i].dwAddr = IP) then
        begin
          DeleteIpNetEntry(IpNetTable.table[i]);
        end;
      end;
  finally
    // Messagebox(0,'End of
    FreeMemory(IpNetTable); // is allready checking for nil
  end;
end;

function ValidAptAddr(p: pointer): Boolean;
begin
  Result := (Cardinal(p) >= Cardinal(pAdapterInfo)) and (Cardinal(p) < (Cardinal(pAdapterInfo) + BufSize));
end;

function FindNextAdapterID(const IP: Cardinal; var AdapterID: Cardinal): Boolean;
var
  Mask: Cardinal;
  IpAddrString: PIpAddrString;
begin
  Result := false;
  While ValidAptAddr(AdapterInfo) do
  begin
    IpAddrString := @AdapterInfo.IpAddressList;
    While ValidAptAddr(IpAddrString) do
    begin
      Mask := inet_addr(IpAddrString.IpMask.S);
      if (Mask > 0) and ((IP and Mask) = (inet_addr(IpAddrString.IpAddress.S) and Mask)) then
      begin
        AdapterID := AdapterInfo.Index;
        AdapterInfo := AdapterInfo.Next;
        Result := True;
        exit;
      end;
      IpAddrString := IpAddrString.Next;
    end;
    AdapterInfo := AdapterInfo.Next;
  end;
end;

function FindFirstAdapterID(const IP: Cardinal; var AdapterID: Cardinal): Boolean;
var
  bSize: Cardinal;
begin
  Result := false;
  GetAdaptersInfo(nil, bSize);
  if bSize > BufSize then
  begin
    FreeMemory(pAdapterInfo); // is allready checking for nil
    BufSize := bSize;
    pAdapterInfo := GetMemory(BufSize);
    if pAdapterInfo = nil then
      exit;
  end;
  GetAdaptersInfo(pAdapterInfo, BufSize);
  AdapterInfo := pAdapterInfo;
  Result := FindNextAdapterID(IP, AdapterID);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Self.EditTR40CurrIPOctet1.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet2.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet3.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet4.OnExit := Self.OctetEditExit;

  EditTR40NewIPOctet1.OnExit := OctetEditExit;
  EditTR40NewIPOctet2.OnExit := OctetEditExit;
  EditTR40NewIPOctet3.OnExit := OctetEditExit;
  EditTR40NewIPOctet4.OnExit := OctetEditExit;

  EditTR40NewMaskOctet1.OnExit := OctetEditExit;
  EditTR40NewMaskOctet2.OnExit := OctetEditExit;
  EditTR40NewMaskOctet3.OnExit := OctetEditExit;
  EditTR40NewMaskOctet4.OnExit := OctetEditExit;

  EditTR40NewGateOctet1.OnExit := OctetEditExit;
  EditTR40NewGateOctet2.OnExit := OctetEditExit;
  EditTR40NewGateOctet3.OnExit := OctetEditExit;
  EditTR40NewGateOctet4.OnExit := OctetEditExit;

  // Self.EditOP40CurrIPOctet1.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet2.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet3.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet4.OnExit := Self.OctetEditExit;

  EditIO40NewIPOctet1.OnExit := OctetEditExit;
  EditIO40NewIPOctet2.OnExit := OctetEditExit;
  EditIO40NewIPOctet3.OnExit := OctetEditExit;
  EditIO40NewIPOctet4.OnExit := OctetEditExit;

  EditIO40NewMaskOctet1.OnExit := OctetEditExit;
  EditIO40NewMaskOctet2.OnExit := OctetEditExit;
  EditIO40NewMaskOctet3.OnExit := OctetEditExit;
  EditIO40NewMaskOctet4.OnExit := OctetEditExit;

  EditIO40NewGateOctet1.OnExit := OctetEditExit;
  EditIO40NewGateOctet2.OnExit := OctetEditExit;
  EditIO40NewGateOctet3.OnExit := OctetEditExit;
  EditIO40NewGateOctet4.OnExit := OctetEditExit;

  // Self.EditTR40CurrIPOctet1.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet2.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet3.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet4.OnChange := Self.OctetChange;

  EditTR40NewIPOctet1.OnChange := OctetChange;
  EditTR40NewIPOctet2.OnChange := OctetChange;
  EditTR40NewIPOctet3.OnChange := OctetChange;
  EditTR40NewIPOctet4.OnChange := OctetChange;

  EditTR40NewMaskOctet1.OnChange := OctetChange;
  EditTR40NewMaskOctet2.OnChange := OctetChange;
  EditTR40NewMaskOctet3.OnChange := OctetChange;
  EditTR40NewMaskOctet4.OnChange := OctetChange;

  EditTR40NewGateOctet1.OnChange := OctetChange;
  EditTR40NewGateOctet2.OnChange := OctetChange;
  EditTR40NewGateOctet3.OnChange := OctetChange;
  EditTR40NewGateOctet4.OnChange := OctetChange;

  // Self.EditOP40CurrIPOctet1.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet2.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet3.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet4.OnChange := Self.OctetChange;

  EditIO40NewIPOctet1.OnChange := OctetChange;
  EditIO40NewIPOctet2.OnChange := OctetChange;
  EditIO40NewIPOctet3.OnChange := OctetChange;
  EditIO40NewIPOctet4.OnChange := OctetChange;

  EditIO40NewMaskOctet1.OnChange := OctetChange;
  EditIO40NewMaskOctet2.OnChange := OctetChange;
  EditIO40NewMaskOctet3.OnChange := OctetChange;
  EditIO40NewMaskOctet4.OnChange := OctetChange;

  EditIO40NewGateOctet1.OnChange := OctetChange;
  EditIO40NewGateOctet2.OnChange := OctetChange;
  EditIO40NewGateOctet3.OnChange := OctetChange;
  EditIO40NewGateOctet4.OnChange := OctetChange;

  EditTR40Pass.RightButton.Enabled := True;
  EditTR40Pass.RightButton.Visible := True;
  EditTR40Pass.RightButton.ImageIndex := 2; // closed eye
  EditTR40Pass.PasswordChar := '*';

  Self.PageControl.ActivePage := Self.SheetTR40;

  Self.FDeviceManager := TDeviceManager.Create;
  // Self.FDeviceManager.LoadFromFile;

  // Self.FSshClient := TPuttySshClient.Create;
  // Self.FSshClient.DosCommand.OnStarted := SshClientDosCommandStarted;
  // Self.FSshClient.DosCommand.OnNewLine := SshClientDosCommandNewLine;
  // Self.FSshClient.DosCommand.OnTerminated := SshClientDosCommandTerminated;

  // Self.FSshClient := TOpenSshClient.Create;
  // Self.FSshClient.DosCommand.OnStarted := SshClientDosCommandStarted;
  // Self.FSshClient.DosCommand.OnNewLine := SshClientDosCommandNewLine;
  // Self.FSshClient.DosCommand.OnTerminated := SshClientDosCommandTerminated;

  Self.FTR40SshClient := TLibSsh2Client.Create;
  Self.FIO40SshClient := TLibSsh2Client.Create;

  { winsock2 }
  if not LoadWinSock2(True) then
    ShowMessage('Error Loading winSock ' + WSAGetLastErrorStr);
  for i := Low(TxData) to High(TxData) - 1 do
    TxData[i] := 0;
  for i := Low(RxData) to High(RxData) - 1 do
    RxData[i] := 0;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //
  FreeAndNil(Self.FDeviceManager);
  FreeAndNil(Self.FTR40SshClient);
  FreeAndNil(Self.FIO40SshClient);

  FreeMemory(pAdapterInfo);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  InitUI;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // save device configuration
  Self.FDeviceManager.SaveToFile;

  Action := TCloseAction.caFree;
end;

procedure TMainForm.InitUI;
var
  octets: TArray<string>;
begin
  // load device info
  Self.FDeviceManager.LoadFromFile;

{$REGION TR40}
  if Self.PageControl.ActivePage = SheetTR40 then
  begin

    octets := Self.FDeviceManager.TR40.IpAddress.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditTR40CurrIPOctet1.Text := octets[0];
      Self.EditTR40CurrIPOctet2.Text := octets[1];
      Self.EditTR40CurrIPOctet3.Text := octets[2];
      Self.EditTR40CurrIPOctet4.Text := octets[3];
    end;

    Self.EditTR40HostIP.Text := Self.FDeviceManager.TR40.IpAddress;
    Self.EditTR40SshPort.Text := Self.FDeviceManager.TR40.SshPort.ToString;
    Self.EditTR40User.Text := Self.FDeviceManager.TR40.User;
    Self.EditTR40Pass.Text := Self.FDeviceManager.TR40.Password;

    octets := Self.FDeviceManager.TR40.IpAddress.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditTR40NewIPOctet1.Text := octets[0];
      Self.EditTR40NewIPOctet2.Text := octets[1];
      Self.EditTR40NewIPOctet3.Text := octets[2];
      Self.EditTR40NewIPOctet4.Text := octets[3];
    end;

    octets := Self.FDeviceManager.TR40.SubnetMask.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditTR40NewMaskOctet1.Text := octets[0];
      Self.EditTR40NewMaskOctet2.Text := octets[1];
      Self.EditTR40NewMaskOctet3.Text := octets[2];
      Self.EditTR40NewMaskOctet4.Text := octets[3];
    end;

    octets := Self.FDeviceManager.TR40.DefaultGateway.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditTR40NewGateOctet1.Text := octets[0];
      Self.EditTR40NewGateOctet2.Text := octets[1];
      Self.EditTR40NewGateOctet3.Text := octets[2];
      Self.EditTR40NewGateOctet4.Text := octets[3];
    end;

  end
{$ENDREGION}
  //
{$REGION OP40 }
  else if Self.PageControl.ActivePage = SheetIO40 then
  begin

    octets := Self.FDeviceManager.IO40.IpAddress.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditIO40CurrIPOctet1.Text := octets[0];
      Self.EditIO40CurrIPOctet2.Text := octets[1];
      Self.EditIO40CurrIPOctet3.Text := octets[2];
      Self.EditIO40CurrIPOctet4.Text := octets[3];
    end;

    Self.EditIO40HostIP.Text := Self.FDeviceManager.IO40.IpAddress;
    Self.EditIO40SshPort.Text := Self.FDeviceManager.IO40.SshPort.ToString;
    Self.EditIO40User.Text := Self.FDeviceManager.IO40.User;
    Self.EditIO40Pass.Text := Self.FDeviceManager.IO40.Password;

    octets := Self.FDeviceManager.IO40.IpAddress.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditIO40NewIPOctet1.Text := octets[0];
      Self.EditIO40NewIPOctet2.Text := octets[1];
      Self.EditIO40NewIPOctet3.Text := octets[2];
      Self.EditIO40NewIPOctet4.Text := octets[3];
    end;

    octets := Self.FDeviceManager.IO40.SubnetMask.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditIO40NewMaskOctet1.Text := octets[0];
      Self.EditIO40NewMaskOctet2.Text := octets[1];
      Self.EditIO40NewMaskOctet3.Text := octets[2];
      Self.EditIO40NewMaskOctet4.Text := octets[3];
    end;

    octets := Self.FDeviceManager.IO40.DefaultGateway.Split(['.']);
    if Length(octets) = 4 then
    begin
      Self.EditIO40NewGateOctet1.Text := octets[0];
      Self.EditIO40NewGateOctet2.Text := octets[1];
      Self.EditIO40NewGateOctet3.Text := octets[2];
      Self.EditIO40NewGateOctet4.Text := octets[3];
    end;

  end;
{$ENDREGION}
end;

procedure TMainForm.SshClientDosCommandStarted(Sender: TObject);
begin
  if Self.PageControl.ActivePage = Self.SheetTR40 then
  begin
    Self.MemoTR40Output.Enabled := True;
    Self.ButtonTR40Input.Enabled := True;
    Self.EditTR40Input.Enabled := True;

    // Self.LabelTR40ConnectionState.Visible := True;
    // Self.LabelTR40ConnectionState.Caption := 'Connecting...';

    Self.MemoTR40Output.Lines.Clear;

  end
  else if Self.PageControl.ActivePage = Self.SheetIO40 then
  begin
  end;
end;

procedure TMainForm.SshClientDosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  if Self.PageControl.ActivePage = Self.SheetTR40 then
  begin
    Self.MemoTR40Output.Lines.Add(ANewLine);

    if FTR40SshClient.IsConnecting and ANewLine.Contains('Store key in cache? (y/n, Return cancels connection, i for more info)') then
    begin
      FTR40SshClient.DosCommand.SendLine('y', True);
      FTR40SshClient.IsConnecting := false;
    end;

    if FTR40SshClient.IsConnecting and ANewLine.Contains('password:') then

      if ANewLine.Contains('Last login:') then
      begin
        Self.FTR40SshClient.IsConnected := True;

        if Self.PageControl.ActivePage = Self.SheetTR40 then
        begin
          FDeviceManager.TR40.IpAddress := Self.EditTR40HostIP.Text;
          FDeviceManager.TR40.SshPort := StrToIntDef(Self.EditTR40SshPort.Text, 22);
          FDeviceManager.TR40.User := Self.EditTR40User.Text;
          FDeviceManager.TR40.Password := Self.EditTR40Pass.Text;
        end
        else if Self.PageControl.ActivePage = Self.SheetIO40 then
        begin

        end;

        Self.FDeviceManager.SaveToFile;

        Self.ButtonTR40SshConnect.ImageIndex := 1;
        Self.IconList.GetBitmap(1, bmp);
        Self.ImageTR40ConnectLed.Picture.Assign(bmp);

      end;

    if Self.FTR40SshClient.IsConnected and ANewLine.Contains('logout') then
    begin
      Self.FTR40SshClient.IsConnected := false;

      Self.ButtonTR40SshConnect.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageTR40ConnectLed.Picture.Assign(bmp);
    end;


    // if ANewLine.Contains('Store key in cache? (y/n, Return cancels connection, i for more info)') then
    // begin
    // Self.EditTR40Input.Text := 'y';
    // ShowMessage('Please type "y" and click button "Input" to cach host key');
    // end;

  end
  else if Self.PageControl.ActivePage = Self.SheetIO40 then
  begin
  end;

  bmp.Free;

end;

procedure TMainForm.SshClientDosCommandTerminated(Sender: TObject);
begin
  if Self.PageControl.ActivePage = Self.SheetTR40 then
  begin
    // Self.MemoTR40Output.Enabled := False;
    Self.EditTR40Input.Enabled := false;
    Self.ButtonTR40Input.Enabled := false;

    Self.LabelTR40ConnectionState.Visible := false;
  end
  else if Self.PageControl.ActivePage = Self.SheetIO40 then
  begin
  end;
end;

procedure TMainForm.ButtonTR40SshConnectClick(Sender: TObject);
var
  OldCursor: TCursor;
  bmp: TBitmap;
begin
  if FTR40SshClient.IsConnected then
  begin
    ShowMessage('The SSH Client already connected to any host');
    exit;
  end;

  Self.MemoTR40Output.Clear;

  Self.FTR40SshClient.HostName := Self.EditTR40HostIP.Text;
  Self.FTR40SshClient.Port := StrToIntDef(Self.EditTR40SshPort.Text, 22);
  Self.FTR40SshClient.User := Self.EditTR40User.Text;
  Self.FTR40SshClient.Password := Self.EditTR40Pass.Text;

  bmp := TBitmap.Create;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    try
      // try ping first
      if not FTR40SshClient.TryPing(FTR40SshClient.HostName) then
      begin
        ShowMessage('Can not ping to host');
        exit;
      end;

      Self.ButtonTR40SshConnect.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageTR40ConnectLed.Picture.Assign(bmp);

      FTR40SshClient.Connect;

      if FTR40SshClient.IsConnected then
      begin
        FDeviceManager.TR40.IpAddress := Self.EditTR40HostIP.Text;
        FDeviceManager.TR40.SshPort := StrToIntDef(Self.EditTR40SshPort.Text, 22);
        FDeviceManager.TR40.User := Self.EditTR40User.Text;
        FDeviceManager.TR40.Password := Self.EditTR40Pass.Text;

        Self.FDeviceManager.SaveToFile;

        Self.ButtonTR40SshConnect.ImageIndex := 1;
        Self.IconList.GetBitmap(1, bmp);
        Self.ImageTR40ConnectLed.Picture.Assign(bmp);
      end
      else
      begin
        Self.ButtonTR40SshConnect.ImageIndex := 0;
        Self.IconList.GetBitmap(0, bmp);
        Self.ImageTR40ConnectLed.Picture.Assign(bmp);
      end;

    finally

    end;

  finally
    Screen.Cursor := OldCursor;
    bmp.Free;
  end;

end;

procedure TMainForm.ButtonIO40SshConnectClick(Sender: TObject);
var
  OldCursor: TCursor;
  bmp: TBitmap;
begin
  if FIO40SshClient.IsConnected then
  begin
    ShowMessage('The SSH Client already connected to any host');
    exit;
  end;

  Self.MemoIO40Output.Clear;

  Self.FIO40SshClient.HostName := Self.EditIO40HostIP.Text;
  Self.FIO40SshClient.Port := StrToIntDef(Self.EditIO40SshPort.Text, 22);
  Self.FIO40SshClient.User := Self.EditIO40User.Text;
  Self.FIO40SshClient.Password := Self.EditIO40Pass.Text;

  bmp := TBitmap.Create;
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    try
      // try ping first
      if not FIO40SshClient.TryPing(FIO40SshClient.HostName) then
      begin
        ShowMessage('Can not ping to host');
        exit;
      end;

      Self.ButtonIO40SshConnect.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageIO40ConnectLed.Picture.Assign(bmp);

      FIO40SshClient.Connect;

      if FIO40SshClient.IsConnected then
      begin
        FDeviceManager.IO40.IpAddress := Self.EditIO40HostIP.Text;
        FDeviceManager.IO40.SshPort := StrToIntDef(Self.EditIO40SshPort.Text, 22);
        FDeviceManager.IO40.User := Self.EditIO40User.Text;
        FDeviceManager.IO40.Password := Self.EditIO40Pass.Text;

        Self.FDeviceManager.SaveToFile;

        Self.ButtonIO40SshConnect.ImageIndex := 1;
        Self.IconList.GetBitmap(1, bmp);
        Self.ImageIO40ConnectLed.Picture.Assign(bmp);
      end
      else
      begin
        Self.ButtonIO40SshConnect.ImageIndex := 0;
        Self.IconList.GetBitmap(0, bmp);
        Self.ImageIO40ConnectLed.Picture.Assign(bmp);
      end;

    finally

    end;

  finally
    Screen.Cursor := OldCursor;
    bmp.Free;
  end;

end;

procedure TMainForm.ButtonIO40SshDisconnectClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  //
  bmp := TBitmap.Create;
  if Self.FIO40SshClient.IsConnected then
  begin
    Self.FIO40SshClient.DisConnect;

    Self.FIO40SshClient.IsConnected := false;

    Self.ButtonIO40SshConnect.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageIO40ConnectLed.Picture.Assign(bmp);
  end;
  bmp.Free;
end;

procedure TMainForm.EditTR40PassRightButtonClick(Sender: TObject);
begin
  //
  if Self.EditTR40Pass.RightButton.ImageIndex = 2 then
  begin
    Self.EditTR40Pass.PasswordChar := #0;
    Self.EditTR40Pass.RightButton.ImageIndex := 3;
  end
  else if Self.EditTR40Pass.RightButton.ImageIndex = 3 then
  begin
    Self.EditTR40Pass.PasswordChar := '*';
    Self.EditTR40Pass.RightButton.ImageIndex := 2;
  end;
end;

procedure TMainForm.ButtonIO40SaveSettingsClick(Sender: TObject);
begin
  WritePrivateProfileString('config', 'IP', PChar(Self.EditIO40IP.Text), PChar(ChangeFileExt(ParamStr(0), '.ini')));
  WritePrivateProfileString('config', 'MAC', PChar(Self.EditIO40Mac.Text), PChar(ChangeFileExt(ParamStr(0), '.ini')));
end;

procedure TMainForm.ButtonIO40SetIPClick(Sender: TObject);
var
  S: AnsiString;
  i: Integer;
  IP: Cardinal;
  ArpEntry: MIB_IPNETROW;
  hPing: THandle;
  lpMsgBuf: PansiChar;
  Args: pointer;
  b: Boolean;
  PECHO: PICMP_ECHO_REPLY;
  Data: Cardinal;
const
  PingLength = 123;
begin
  //
  // 19=192.168.0.51
  hPing := IcmpCreateFile;
  S := '';
  // Precheck of Data.
  for i := 1 to Length(Self.EditIO40Mac.Text) do
    if ((EditIO40Mac.Text[i] >= '0') and (EditIO40Mac.Text[i] <= '9')) or ((EditIO40Mac.Text[i] >= 'A') and (EditIO40Mac.Text[i] <= 'F')) or
      ((EditIO40Mac.Text[i] >= 'a') and (EditIO40Mac.Text[i] <= 'f')) then
      S := S + EditIO40Mac.Text[i];
  if Length(S) <> 12 then
    Messagebox(Handle, 'Invalid MAC Address', 'Input Error', MB_OK)
  else
  begin
    ArpEntry.dwPhysAddrLen := 6;
    HexToBin(PansiChar(S), Addr(ArpEntry.bPhysAddr), 6);
    ArpEntry.dwAddr := inet_addr(PansiChar(EditIO40IP.Text));
    ArpEntry.dwType := MIB_IPNET_TYPE_STATIC; // DYNAMIC;
    if not FindFirstAdapterID(ArpEntry.dwAddr, ArpEntry.dwIndex) then
    begin
      Messagebox(Handle, 'No Adapter matching IP found, check your IP configuration', 'Error', MB_OK);
      exit;
    end;

    repeat
      S := IntToStr(ArpEntry.dwIndex);
      i := CreateIpNetEntry(ArpEntry);
      case i of
        NO_ERROR:
          MemoIO40State.Lines.Add('Arp entry  on adapter ' + S + ' created.');
        ERROR_ACCESS_DENIED:
          MemoIO40State.Lines.Add('CreateIpNetEntry on adapter ' + S + ' failed: access denied, you need admin rights to set IP');
        ERROR_INVALID_PARAMETER:
          MemoIO40State.Lines.Add('CreateIpNetEntry on adapter ' + S + ' failed: The IPv4 transport is not configured on the local computer.');
        ERROR_NOT_SUPPORTED:
          MemoIO40State.Lines.Add('CreateIpNetEntry on adapter ' + S + ' failed: The IPv4 transport is not configured on the local computer.');
        5010:
          MemoIO40State.Lines.Add('Arp entry on Adapter ' + S + 'allready exist');
      else
        MemoIO40State.Lines.Add('CreateIpNetEntry on adapter ' + S + ' failed with errorcode ' + IntToStr(i));
      end;
      MemoIO40State.Lines.Add('sending 123 bytes Ping');
      for i := Low(TxData) to High(TxData) - 1 do
        TxData[i] := Random($FFFFFFFF); // fill Random Data
      if IcmpSendEcho(hPing, ArpEntry.dwAddr, @TxData, PingLength, nil, @RxData, sizeof(RxData), 1000) > 0 then
      begin
        PECHO := Addr(RxData);
        if PECHO.DataSize >= PingLength then
        begin
          Data := Cardinal(PECHO.Data);
          b := True;
          for i := 0 to (PingLength div 4) - 1 do
            // begin
            b := b and (TxData[i] = PCardinal(PTR(Data + i * 4))^);
          // MemoIO40State.Lines.Add(IntToStr(TxData[i])+' = '+IntToStr(PCardinal(PTR(Data+i*4))^));
          // end;
          if b then
            MemoIO40State.Lines.Add('Ping succeed')
          else
            MemoIO40State.Lines.Add('Ping data dont match');
        end
        else
          MemoIO40State.Lines.Add(IntToStr(i) + 'Ping received but too less Data')
      end
      else if i > 0 then
        MemoIO40State.Lines.Add('Ping failed');
      if IcmpSendEcho(hPing, ArpEntry.dwAddr, @TxData, 32, nil, @RxData, sizeof(RxData), 1000) > 0 then
        MemoIO40State.Lines.Add('Normal Ping succeed')
      else
        MemoIO40State.Lines.Add('Normal Ping failed');
      i := DeleteIpNetEntry(ArpEntry);
      if i = 0 then
        MemoIO40State.Lines.Add('removed Arp entry')
      else
        MemoIO40State.Lines.Add('removed Arp entry faild with ' + IntToStr(i));
    until not FindNextAdapterID(ArpEntry.dwAddr, ArpEntry.dwIndex);
    // ReplyBuffer:Pointer; ReplySize,1000);
    // lpMsgBuf:=nil;
    // FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY or FORMAT_MESSAGE_ALLOCATE_BUFFER,nil,i,
    // (SUBLANG_DEFAULT shl 10) or LANG_NEUTRAL, lpMsgBuf,{10}0,@Args);
    // MessageBoxA( Handle, lpMsgBuf, PAnsiChar('CreateIpNetEntry '+IntToStr(i)), MB_OK or MB_ICONINFORMATION );
    // LocalFree(Cardinal(lpMsgBuf));

    // ERROR_ACCESS_DENIED;
    // NO_ERROR;
    //
  end;
  IcmpCloseHandle(hPing);

end;

procedure TMainForm.ButtonIO40ChangeIPClick(Sender: TObject);
var
  OldCursor: TCursor;
  Result: string;
  Lines, fields, names: TArray<string>;
  line, name, ext: string;
  newIP, newMask, newGate, cmd: string;
  // newGate: string;
  i: Integer;
  netConfigContents: TStringList;
  bmp: TBitmap;
begin
  newIP := Format('%s.%s.%s.%s', [EditIO40NewIPOctet1.Text, EditIO40NewIPOctet2.Text, EditIO40NewIPOctet3.Text, EditIO40NewIPOctet4.Text]);
  if newIP = Self.FDeviceManager.IO40.IpAddress then
  begin
    ShowMessage('Please set new other IP address than the current.');
    exit;
  end;

  // newGate := Format('%s.%s.%s.%s', [EditTR40NewGateOctet1.Text, EditTR40NewGateOctet2.Text, EditTR40NewGateOctet3.Text, EditTR40NewGateOctet4.Text]);
  newMask := Format('%s.%s.%s.%s', [EditIO40NewMaskOctet1.Text, EditIO40NewMaskOctet2.Text, EditIO40NewMaskOctet3.Text, EditIO40NewMaskOctet4.Text]);
  newGate := '';

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  bmp := TBitmap.Create;

  if Self.FIO40SshClient.IsConnected then
  begin

    if Self.FIO40SshClient.ChangeIP(newIP, newMask, newGate) then
    begin
      // set set ip led on
      Self.ButtonIO40ChangeIP.ImageIndex := 1;
      Self.IconList.GetBitmap(1, bmp);
      Self.ImageIO40ChangeIPLed.Picture.Assign(bmp);

      Self.FDeviceManager.IO40.IpAddress := newIP;
      Self.FDeviceManager.IO40.SubnetMask := newMask;
      Self.FDeviceManager.SaveToFile;

      Self.FIO40SshClient.IsConnected := false; // set to False on new IP

      // set ping led off
      Self.ButtonIO40Ping.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageIO40PingLed.Picture.Assign(bmp);

      // Set connect led off
      Self.ButtonIO40SshConnect.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageIO40ConnectLed.Picture.Assign(bmp);

      // reload...
      Self.InitUI;
    end;

  end
  else
  begin
    Self.ButtonIO40ChangeIP.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageIO40ChangeIPLed.Picture.Assign(bmp);

    // set ping & connection leds to off
    Self.ButtonIO40Ping.ImageIndex := 0;
    // Self.IconList.GetBitmap(0, bmp);
    Self.ImageIO40PingLed.Picture.Assign(bmp);

    Self.ButtonIO40SshConnect.ImageIndex := 0;
    // Self.IconList.GetBitmap(0, bmp);
    Self.ImageIO40ConnectLed.Picture.Assign(bmp);

    ShowMessage('Not connected yet. Please try connect first.');
  end;

  bmp.Free;
  Screen.Cursor := OldCursor;
end;

procedure TMainForm.ButtonTR40InputClick(Sender: TObject);
begin
  if FTR40SshClient.DosCommand.IsRunning then
  begin
    FTR40SshClient.DosCommand.SendLine(Self.EditTR40Input.Text, True);
  end;
end;

procedure TMainForm.ButtonTR40PingClick(Sender: TObject);
var
  OldCursor: TCursor;
  CurrIP: string;
  bmp: TBitmap;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  bmp := TBitmap.Create;
  CurrIP := Format('%s.%s.%s.%s', [EditTR40CurrIPOctet1.Text, EditTR40CurrIPOctet2.Text, EditTR40CurrIPOctet3.Text, EditTR40CurrIPOctet4.Text]);
  if Self.FTR40SshClient.TryPing(CurrIP) then
  begin
    Self.ButtonTR40Ping.ImageIndex := 1;
    Self.IconList.GetBitmap(1, bmp);
    Self.ImageTR40PingLed.Picture.Assign(bmp);
  end
  else
  begin
    Self.ButtonTR40Ping.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageTR40PingLed.Picture.Assign(bmp);
  end;

  bmp.Free;

  Invalidate;
  Screen.Cursor := OldCursor;
end;

procedure TMainForm.ButtonIO40PingClick(Sender: TObject);
var
  OldCursor: TCursor;
  CurrIP: string;
  bmp: TBitmap;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  bmp := TBitmap.Create;
  CurrIP := Format('%s.%s.%s.%s', [EditIO40CurrIPOctet1.Text, EditIO40CurrIPOctet2.Text, EditIO40CurrIPOctet3.Text, EditIO40CurrIPOctet4.Text]);
  if Self.FIO40SshClient.TryPing(CurrIP) then
  begin
    Self.ButtonIO40Ping.ImageIndex := 1;
    Self.IconList.GetBitmap(1, bmp);
    Self.ImageIO40PingLed.Picture.Assign(bmp);
  end
  else
  begin
    Self.ButtonIO40Ping.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageIO40PingLed.Picture.Assign(bmp);
  end;

  bmp.Free;

  Invalidate;
  Screen.Cursor := OldCursor;
end;

procedure TMainForm.ButtonTR40ChangeIPClick(Sender: TObject);
var
  OldCursor: TCursor;
  Result: string;
  Lines, fields, names: TArray<string>;
  line, name, ext: string;
  newIP, newMask, newGate, cmd: string;
  // newGate: string;
  i: Integer;
  netConfigContents: TStringList;
  bmp: TBitmap;
begin
  newIP := Format('%s.%s.%s.%s', [EditTR40NewIPOctet1.Text, EditTR40NewIPOctet2.Text, EditTR40NewIPOctet3.Text, EditTR40NewIPOctet4.Text]);
  if newIP = Self.FDeviceManager.TR40.IpAddress then
  begin
    ShowMessage('Please set new other IP address than the current.');
    exit;
  end;

  // newGate := Format('%s.%s.%s.%s', [EditTR40NewGateOctet1.Text, EditTR40NewGateOctet2.Text, EditTR40NewGateOctet3.Text, EditTR40NewGateOctet4.Text]);
  newMask := Format('%s.%s.%s.%s', [EditTR40NewMaskOctet1.Text, EditTR40NewMaskOctet2.Text, EditTR40NewMaskOctet3.Text, EditTR40NewMaskOctet4.Text]);
  newGate := '';

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  bmp := TBitmap.Create;

  if Self.FTR40SshClient.IsConnected then
  begin

    if Self.FTR40SshClient.ChangeIP(newIP, newMask, newGate) then
    begin
      // set set ip led on
      Self.ButtonTR40ChangeIP.ImageIndex := 1;
      Self.IconList.GetBitmap(1, bmp);
      Self.ImageTR40ChangeIPLed.Picture.Assign(bmp);

      Self.FDeviceManager.TR40.IpAddress := newIP;
      Self.FDeviceManager.TR40.SubnetMask := newMask;
      Self.FDeviceManager.SaveToFile;

      Self.FTR40SshClient.IsConnected := false; // set to False on new IP

      // set ping led off
      Self.ButtonTR40Ping.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageTR40PingLed.Picture.Assign(bmp);

      // Set connect led off
      Self.ButtonTR40SshConnect.ImageIndex := 0;
      Self.IconList.GetBitmap(0, bmp);
      Self.ImageTR40ConnectLed.Picture.Assign(bmp);

      // reload...
      Self.InitUI;
    end;

  end
  else
  begin
    Self.ButtonTR40ChangeIP.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageTR40ChangeIPLed.Picture.Assign(bmp);

    // set ping & connection leds to off
    Self.ButtonTR40Ping.ImageIndex := 0;
    // Self.IconList.GetBitmap(0, bmp);
    Self.ImageTR40PingLed.Picture.Assign(bmp);

    Self.ButtonTR40SshConnect.ImageIndex := 0;
    // Self.IconList.GetBitmap(0, bmp);
    Self.ImageTR40ConnectLed.Picture.Assign(bmp);

    ShowMessage('Not connected yet. Please try connect first.');
  end;

  bmp.Free;
  Screen.Cursor := OldCursor;
end;

procedure TMainForm.ButtonTR40SshDisconnectClick(Sender: TObject);
var
  bmp: TBitmap;
begin
  //
  bmp := TBitmap.Create;
  if Self.FTR40SshClient.IsConnected then
  begin
    Self.FTR40SshClient.DisConnect;

    Self.FTR40SshClient.IsConnected := false;

    Self.ButtonTR40SshConnect.ImageIndex := 0;
    Self.IconList.GetBitmap(0, bmp);
    Self.ImageTR40ConnectLed.Picture.Assign(bmp);
  end;
  bmp.Free;
end;

procedure TMainForm.EditIO40PassRightButtonClick(Sender: TObject);
begin
  //
  if Self.EditIO40Pass.RightButton.ImageIndex = 2 then
  begin
    Self.EditIO40Pass.PasswordChar := #0;
    Self.EditIO40Pass.RightButton.ImageIndex := 3;
  end
  else if Self.EditIO40Pass.RightButton.ImageIndex = 3 then
  begin
    Self.EditIO40Pass.PasswordChar := '*';
    Self.EditIO40Pass.RightButton.ImageIndex := 2;
  end;
end;

procedure TMainForm.EditPasswordChange(Sender: TObject);
begin
  //
end;

procedure TMainForm.EditTR40UserChange(Sender: TObject);
begin
  //
end;

procedure TMainForm.OctetEditExit(Sender: TObject);
var
  minVal, maxVal, currVal: Integer;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    exit;

  minVal := 0;
  maxVal := 255;

  currVal := StrToIntDef(editBox.Text, 0);
  if currVal < minVal then
    currVal := minVal
  else if currVal > maxVal then
    currVal := maxVal;

  editBox.Text := currVal.ToString;
  //
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  //
  Self.InitUI;
end;

procedure TMainForm.OctetChange(Sender: TObject);
var
  NextCtrl: TControl;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    exit;

  // Move the focus to the next octet after 3 characters are entered
  if Length(editBox.Text) >= 3 then
  begin
    NextCtrl := editBox.Parent.ControlAtPos(Point(editBox.Left + editBox.Width + 10, editBox.Top), True, True, True);
    if (NextCtrl <> nil) and (NextCtrl is TEdit) then
      (NextCtrl as TEdit).SetFocus;
  end;
end;

end.
