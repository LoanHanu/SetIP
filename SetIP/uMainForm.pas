unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
  IdIcmpClient, IdComponent, Vcl.Mask, System.IOUtils, System.Generics.Collections,
  uDevice, uPuttySshClient;

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
    ButtonTR40SetIP: TButton;
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
    ButtonIO40SetIP: TButton;
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
    ButtonSshConnect: TButton;
    EditTR40Pass: TButtonedEdit;

    procedure OctetEditExit(Sender: TObject);
    procedure OctetChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);

    procedure ButtonTR40PingClick(Sender: TObject);
    procedure ButtonTR40SetIPClick(Sender: TObject);

    procedure ButtonIO40PingClick(Sender: TObject);
    procedure ButtonIO40SetIPClick(Sender: TObject);

    procedure FormShow(Sender: TObject);

    // procedure ScSSHClient1ServerKeyValidate(Sender: TObject; NewServerKey: TScKey; var Accept: Boolean);

    procedure ButtonSshConnectClick(Sender: TObject);
    procedure EditTR40UserChange(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
    procedure EditTR40PassRightButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDeviceManager: TDeviceManager;
    FSshClient: TPuttySshClient;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uCommonUtils;

procedure TMainForm.FormCreate(Sender: TObject);
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
  EditTR40Pass.RightButton.ImageIndex := 3; // closed eye
  EditTR40Pass.PasswordChar := '*';

  Self.PageControl.ActivePage := Self.SheetTR40;

  Self.FDeviceManager := TDeviceManager.Create;
  // Self.FDeviceManager.LoadFromFile;
  Self.FSshClient := TPuttySshClient.Create;

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //
  FreeAndNil(Self.FDeviceManager);
  FreeAndNil(Self.FSshClient);

end;

procedure TMainForm.FormShow(Sender: TObject);
var
  octets: TArray<string>;
begin
{$REGION TR40}
  octets := Self.FDeviceManager.TR40.IPAddress.Split(['.']);
  if Length(octets) = 4 then
  begin
    Self.EditTR40CurrIPOctet1.Text := octets[0];
    Self.EditTR40CurrIPOctet2.Text := octets[1];
    Self.EditTR40CurrIPOctet3.Text := octets[2];
    Self.EditTR40CurrIPOctet4.Text := octets[3];
  end;

  Self.EditTR40HostIP.Text := Self.FDeviceManager.TR40.IPAddress;
  Self.EditTR40SshPort.Text := Self.FDeviceManager.TR40.SshPort.ToString;
  Self.EditTR40User.Text := Self.FDeviceManager.TR40.User;
  Self.EditTR40Pass.Text := Self.FDeviceManager.TR40.Password;

  octets := Self.FDeviceManager.TR40.IPAddress.Split(['.']);
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
{$ENDREGION}
  //
{$REGION OP40 }
  octets := Self.FDeviceManager.OP40.IPAddress.Split(['.']);
  if Length(octets) = 4 then
  begin
    Self.EditIO40CurrIPOctet1.Text := octets[0];
    Self.EditIO40CurrIPOctet2.Text := octets[1];
    Self.EditIO40CurrIPOctet3.Text := octets[2];
    Self.EditIO40CurrIPOctet4.Text := octets[3];
  end;
{$ENDREGION}
end;

procedure TMainForm.ButtonSshConnectClick(Sender: TObject);
var
  OldCursor: TCursor;
begin
  // make connection first...
  Self.FSshClient.HostName := Self.EditTR40HostIP.Text;
  Self.FSshClient.Port := StrToIntDef(Self.EditTR40SshPort.Text, 22);
  Self.FSshClient.User := Self.EditTR40User.Text;
  Self.FSshClient.Password := Self.EditTR40Pass.Text;

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if FSshClient.CanConnect then
    begin
      Self.ButtonSshConnect.ImageIndex := 1;
    end
    else
    begin
      Self.ButtonSshConnect.ImageIndex := 0;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;

  { on the case of using SecureBridge lib }
  // Self.ScSSHClient1.HostName := Self.EditSshHost.Text;
  // Self.ScSSHClient1.Port := 22;
  // Self.ScSSHClient1.User := Self.EditUserName.Text;
  // Self.ScSSHClient1.Password := Self.EditPassword.Text;
  /// /   ScSSHClient1.Authentication := atPassword;
  //
  // OldCursor := Screen.Cursor;
  // try
  // Screen.Cursor := crHourGlass;
  // ScSSHClient1.Connect;
  // finally
  // Screen.Cursor := OldCursor;
  // end;
  //
  // if ScSSHClient1.Connected then
  // begin
  // // send cmds to change ip...
  //
  // // change button image
  // Self.ButtonSshConnect.ImageIndex := 1;
  // end
  // else
  // begin
  // Self.ButtonSshConnect.ImageIndex := 0;
  // end;
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

procedure TMainForm.ButtonIO40PingClick(Sender: TObject);
var
  Ping: TIdIcmpClient;

begin
  Ping := TIdIcmpClient.Create(nil);
  try
    Ping.Host := Self.FDeviceManager.OP40.IPAddress;
    Ping.Ping('0000');
    if Ping.ReplyStatus.BytesReceived > 0 then
      Self.ButtonIO40Ping.ImageIndex := 1
    else
      Self.ButtonIO40Ping.ImageIndex := 0;
  finally
    Ping.Free;
  end;
end;

procedure TMainForm.ButtonIO40SetIPClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonTR40PingClick(Sender: TObject);
var
  OldCursor: TCursor;
  Ping: TIdIcmpClient;
  CurrIP: string;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  CurrIP := Format('%s.%s.%s.%s', [EditTR40CurrIPOctet1.Text, EditTR40CurrIPOctet2.Text, EditTR40CurrIPOctet3.Text, EditTR40CurrIPOctet4.Text]);
  if TryPing(CurrIP) then
    Self.ButtonTR40Ping.ImageIndex := 1
  else
    Self.ButtonTR40Ping.ImageIndex := 0;

  Screen.Cursor := OldCursor;
end;

procedure TMainForm.ButtonTR40SetIPClick(Sender: TObject);
var
  OldCursor: TCursor;
  result: string;
  lines, fields, names: TArray<string>;
  line, name, ext: string;
  newIP, newMask, newGate, cmd: string;
  i: integer;
  netConfigContents: TStringList;
begin
  OldCursor := Screen.Cursor;
  if Self.FSshClient.Connected then
  begin
    Screen.Cursor := crHourGlass;
    try
      // get network interface name of ethernet:
      // nmcli device status: get the device status of NetworkManager-controlled interfaces, including their names
      result := FSshClient.ExecuteCommand('nmcli device status');
      result := result.Trim;
      if result.Contains(#13#10) then
      begin
        lines := result.Split([#13#10]);
      end
      else if result.Contains(#10) then
      begin
        lines := result.Split([#10]);
      end;

      if Length(lines) > 0 then
      begin
        fields := lines[0].Split([' '], TStringSplitOptions.ExcludeEmpty);
        for i := 1 to Length(lines) - 1 do
        begin
          fields := lines[i].Split([' '], TStringSplitOptions.ExcludeEmpty);
          if fields[1].ToLower = 'ethernet' then
          begin
            Self.FDeviceManager.TR40.NetInterface := fields[0];
            break;
          end;
        end;
      end;

      // get network config file name with extension ".yaml" : ls /etc/netplan
      result := FSshClient.ExecuteCommand('ls /etc/netplan');
      result := result.Trim;
      names := result.Split([' '], TStringSplitOptions.ExcludeEmpty);
      for name in names do
      begin
        ext := TPath.GetExtension(name).Trim;
        if ext = '.yaml' then
        begin
          Self.FDeviceManager.TR40.NetConfigFileName := name;
          break;
        end;
      end;

      // run netplan config command to change with new ip address
      newIP := Format('%s.%s.%s.%s', [EditTR40NewIPOctet1.Text, EditTR40NewIPOctet2.Text, EditTR40NewIPOctet3.Text, EditTR40NewIPOctet4.Text]);
      newGate := Format('%s.%s.%s.%s', [EditTR40NewGateOctet1.Text, EditTR40NewGateOctet2.Text, EditTR40NewGateOctet3.Text, EditTR40NewGateOctet4.Text]);
      { Here are some common subnet masks and their corresponding CIDR notations:
        255.255.255.0 = /24
        255.255.255.128 = /25
        255.255.255.192 = /26
        255.255.255.224 = /27
        255.255.255.240 = /28
        255.255.255.248 = /29
        255.255.255.252 = /30
        255.255.255.254 = /31
        255.255.255.255 = /32

        for ex, when ip is 10.99.4.24 and subnet mask is 255.255.255.0, full ip addr with the CIDR notations: 10.99.4.24/24
      }
      newMask := Format('%s.%s.%s.%s', [EditTR40NewMaskOctet1.Text, EditTR40NewMaskOctet2.Text, EditTR40NewMaskOctet3.Text, EditTR40NewMaskOctet4.Text]);
      if newMask = '255.255.255.0' then
        newMask := '/24'
      else if newMask = '255.255.255.128' then
        newMask := '/25'
      else if newMask = '255.255.255.192' then
        newMask := '/26'
      else if newMask = '255.255.255.224' then
        newMask := '/27'
      else if newMask = '255.255.255.255' then
        newMask := '/32'
      else
        newMask := '/24';

      netConfigContents := TStringList.Create;
      netConfigContents.Add(Format('network:', []));
      netConfigContents.Add(Format('  version: 2', []));
      netConfigContents.Add(Format('  renderer: networkd', []));
      netConfigContents.Add(Format('  ethernets:', []));
      netConfigContents.Add(Format('    %s:', [Self.FDeviceManager.TR40.NetInterface]));
      netConfigContents.Add(Format('      dhcp4: no', []));
      netConfigContents.Add(Format('      addresses: [%s%s]', [newIP, newMask]));
      // netConfigContents.Add(Format('      addresses:', []));
      // netConfigContents.Add(Format('        - %s%s', [newIP, newMask]));
      netConfigContents.Add(Format('      gateway4: %s', [newGate]));
      netConfigContents.Add(Format('      nameservers:', []));
      netConfigContents.Add(Format('        addresses: [8.8.8.8, 8.8.4.4]', []));

      cmd := Format('echo "%s" | sudo tee /etc/netplan/%s', [netConfigContents.Text, Self.FDeviceManager.TR40.NetConfigFileName]);
      result := Self.FSshClient.ExecuteCommand(cmd);
      Sleep(1000);

      // restart ssh server: sudo systemctl restart sshd
      result := FSshClient.ExecuteCommand('sudo systemctl restart sshd');

      // apply ip change: sudo netplan apply
      result := FSshClient.ExecuteCommand('sudo netplan apply');

      // check if the new ip applied...
      if TryPing(newIP) then
      begin
        Self.FDeviceManager.TR40.IPAddress := newIP;
        // Self.FDeviceManager.TR40.SubnetMask := ;
        // Self.FDeviceManager.TR40.DefaultGateway := ;
        Self.ButtonTR40SetIP.ImageIndex := 1;
      end
      else
      begin
        Self.ButtonTR40SetIP.ImageIndex := 0;
      end;

    finally

    end;

  end
  else
  begin
    Self.ButtonTR40SetIP.ImageIndex := 0;
  end;
  Screen.Cursor := OldCursor;
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
  minVal, maxVal, currVal: integer;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    Exit;

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

// procedure TMainForm.ScSSHClient1ServerKeyValidate(Sender: TObject; NewServerKey: TScKey; var Accept: Boolean);
// begin
// //
// Accept := True;
// end;

procedure TMainForm.OctetChange(Sender: TObject);
var
  NextCtrl: TControl;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    Exit;

  // Move the focus to the next octet after 3 characters are entered
  if Length(editBox.Text) >= 3 then
  begin
    NextCtrl := editBox.Parent.ControlAtPos(Point(editBox.Left + editBox.Width + 10, editBox.Top), True, True, True);
    if (NextCtrl <> nil) and (NextCtrl is TEdit) then
      (NextCtrl as TEdit).SetFocus;
  end;
end;

end.
