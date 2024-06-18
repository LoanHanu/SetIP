unit uPuttySshClient;

interface

uses
  Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, System.IOUtils, DosCommand;

type
  TSshConnectionOption = (coPassword, coPubicKey);

  TSshCmdType = (ctNull, ctCheckPuttyVersion, ctCheckHomeList);

  /// <summary>
  /// ssh client using PuTTy: plink.exe
  /// </summary>
  TPuttySshClient = class
  private
    FPlinkPath: string;
    FHostName: string;
    FPort: integer;
    FUser: string;
    FPassword: string;
    FPrivateKey: string; // path to private key file
    FConnected: Boolean;
    FConnectionOption: TSshConnectionOption;
    FLockObject: TCriticalSection;

    FDosCommand: TDosCommand;
    FDosCommandIsRunning: Boolean;
    FDosCommandOnNewLine: TNewLineEvent;
    FDosCommandOnStarted: TNotifyEvent; // added by Loan
    FDosCommandOnTerminated: TNotifyEvent;

  private
    function IsPuttyInstalled: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function CanConnect: Boolean;

    function ChangeIP(newIP, newMask: string; newGate: string = ''): Boolean;

    /// <summary>
    /// execute cmd on remote machine, ssh server
    /// </summary>
    function ExecuteCommand(cmd: string; inBatch: Boolean = True): string;

  private
    procedure SetDosCommandLine(Value: string);
    function GetDosCommandLine: string;
    function GetDosCommandOutputLines: TStrings;
    function GetDosCommandIsRunning: Boolean;
    procedure SetDosCommandOnNewLine(Value: TNewLineEvent);
    procedure SetDosCommandOnStarted(Value: TNotifyEvent);
    procedure SetDosCommandOnTerminated(Value: TNotifyEvent);

  public
    // procedure ExecuteDosCommand(cmd: string);
    procedure SendLineToDosCommand(input: string; eol: Boolean);

    procedure DosCommandStarted(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
    procedure DosCommandTerminated(Sender: TObject);

  public
    property PlinkPath: string read FPlinkPath write FPlinkPath;
    property HostName: string read FHostName write FHostName;
    property Port: integer read FPort write FPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property Connected: Boolean read FConnected write FConnected;
    property ConnectionOption: TSshConnectionOption read FConnectionOption write FConnectionOption;

    property DosCommandLine: string read GetDosCommandLine write SetDosCommandLine;
    property DosCommandOutputLines: TStrings read GetDosCommandOutputLines;
    property DosCommandIsRunning: Boolean read GetDosCommandIsRunning;
    property DosCommandOnNewLine: TNewLineEvent read FDosCommandOnNewLine write SetDosCommandOnNewLine;
    property DosCommandOnStarted: TNotifyEvent read FDosCommandOnStarted write SetDosCommandOnStarted; // added by Loan
    property DosCommandOnTerminated: TNotifyEvent read FDosCommandOnTerminated write SetDosCommandOnTerminated;

  end;

implementation

uses
  uCommonUtils;

constructor TPuttySshClient.Create;
begin
  FPlinkPath := 'plink.exe'; // C:\Program Files\PuTTY\plink.exe when installed x64 version
  FHostName := 'localhost'; // or host ip address
  FPort := 22;
  FConnected := False;
  FConnectionOption := coPassword;
  FLockObject := TCriticalSection.Create;

  FDosCommand := TDosCommand.Create(nil);
  // FDosCommand.MaxTimeAfterBeginning := 5;
  // FDosCommand.MaxTimeAfterLastOutput := 5;
  FDosCommand.InputToOutput := False;
end;

destructor TPuttySshClient.Destroy;
begin
  inherited;

  FLockObject.Free;
  FDosCommand.Free;
end;

function TPuttySshClient.IsPuttyInstalled: Boolean;
var
  res: Boolean;
  cmdLine: string;
  output: TStrings;
begin
  res := False;

  output := TStringList.Create;

  cmdLine := Format('%s -V', [FPlinkPath]);
  try
    // if not FDosCommand.IsRunning then
    // begin
    // Self.FDosCommand.OutputLines := output;
    // Self.FDosCommand.CommandLine := 'cmd /c ' + cmdLine;
    // Self.FDosCommand.Execute;
    // Sleep(500); // wait for end of command
    //
    // if FDosCommand.IsRunning then
    // begin
    // FDosCommand.SendLine('', True);
    // FDosCommand.Stop
    // end;
    //
    // Sleep(200);
    // while FDosCommand.IsRunning do
    // begin
    // Sleep(100);
    // end;
    //
    // if not FDosCommand.IsRunning then
    // begin
    // output := FDosCommand.OutputLines;
    // end
    // else
    // begin
    // FDosCommand.SendLine('', True); // Enter key
    // output := FDosCommand.Lines;
    // end;
    // end;

    ExecuteCommandLine(cmdLine, output);

    if output.Text.StartsWith('plink:') then
    begin
      res := True;
    end
    else if output.Text.Contains('is not recognized as an internal or external command') then
    begin
      ShowMessage(output.Text + #13#10 + 'Please check if PuTTy installed properly.');
      res := False;
    end;

  finally
    output.Free;
  end;

  Result := res;

end;

function TPuttySshClient.CanConnect: Boolean;
var
  res: Boolean;
  cmdResult: string;
begin
  res := False;

  // if not Self.IsPuttyInstalled then
  // begin
  // Result := False;
  // Exit;
  // end;

  case FConnectionOption of
    // plink -ssh username@hostname -pw user_password command_to_execute
    // plink -ssh hostname -P port -l username -pw user_password -batch command_to_execute
    // for example: plink -ssh 10.99.4.24 -P 22 -l ubuntu -pw ubuntu -batch ls
    // FATAL ERROR: Network error: Connection timed out - invalid ip
    // FATAL ERROR: Network error: Connection refused - invalid port
    // AFATAL ERROR: Configured password was not accepted - invalid user or password
    // 'plink' is not recognized as an internal or external command - PuTTy not installed
    coPassword:
      begin
        cmdResult := ExecuteCommand('ls');
        if cmdResult.ToLower.Contains('error') then // or
        begin
          if cmdResult.Contains('Network error: Connection timed out') then
          begin
            ShowMessage(cmdResult + #13#10 + 'Please check if host name or port number is correct.');
          end
          else if cmdResult.Contains('Network error: Connection refused') then
          begin
            ShowMessage(cmdResult + #13#10 + 'Please check if host name or port number is correct.');
          end
          else if cmdResult.Contains('Configured password was not accepted') then
          begin
            ShowMessage(cmdResult + #13#10 + 'Please check if user name or password is correct.');
          end
          else if cmdResult.Contains('Cannot confirm a host key in batch mode') then
          begin
            cmdResult := ExecuteCommand('ls', False);
            // ShowMessage(cmdResult);
          end
          else
          begin
            ShowMessage(cmdResult);
          end;

          res := False;
        end
        else if cmdResult.Contains('is not recognized as an internal or external command') then
        begin
          ShowMessage(cmdResult + #13#10 + 'Please check if PuTTy installed properly.');
        end
        else
        begin
          res := True;
        end;
      end;
    coPubicKey:
      begin
      end;
  end;

  FConnected := res;
  Result := res;
end;

function TPuttySshClient.ChangeIP(newIP, newMask: string; newGate: string = ''): Boolean;
var
  res: Boolean;
  cmdResult: string;
  netInterface, netConfigFileName: string;
  Lines, fields, names: TArray<string>;
  line, name, ext: string;
  cmd: string;
  // newGate: string;
  i: integer;
  netConfigContents: TStringList;
begin
  res := False;
  if Self.Connected then
  begin
    try
      // get network interface name of ethernet:
      // nmcli device status: get the device status of NetworkManager-controlled interfaces, including their names
      cmdResult := ExecuteCommand('nmcli device status');
      cmdResult := cmdResult.Trim;
      if cmdResult.Contains(#13#10) then
      begin
        Lines := cmdResult.Split([#13#10]);
      end
      else if cmdResult.Contains(#10) then
      begin
        Lines := cmdResult.Split([#10]);
      end;

      if Length(Lines) > 0 then
      begin
        fields := Lines[0].Split([' '], TStringSplitOptions.ExcludeEmpty);
        for i := 1 to Length(Lines) - 1 do
        begin
          fields := Lines[i].Split([' '], TStringSplitOptions.ExcludeEmpty);
          if fields[1].ToLower = 'ethernet' then
          begin
            netInterface := fields[0];
            break;
          end;
        end;
      end;

      // get network config file name with extension ".yaml" : ls /etc/netplan
      cmdResult := ExecuteCommand('ls /etc/netplan');
      cmdResult := cmdResult.Trim;
      names := cmdResult.Split([' '], TStringSplitOptions.ExcludeEmpty);
      for name in names do
      begin
        try
          ext := TPath.GetExtension(name).Trim;
          if ext = '.yaml' then
          begin
            netConfigFileName := name;
            break;
          end;
        finally

        end;
      end;

      // run netplan config command to change with new ip address
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
      if newMask = '255.255.255.0' then
        newMask := '/24'
      else if newMask = '255.255.255.128' then
        newMask := '/25'
      else if newMask = '255.255.255.192' then
        newMask := '/26'
      else if newMask = '255.255.255.224' then
        newMask := '/27'
      else if newMask = '255.255.255.240' then
        newMask := '/28'
      else if newMask = '255.255.255.248' then
        newMask := '/29'
      else if newMask = '255.255.255.252' then
        newMask := '/30'
      else if newMask = '255.255.255.254' then
        newMask := '/31'
      else if newMask = '255.255.255.255' then
        newMask := '/32'
      else
        newMask := '/24';

      netConfigContents := TStringList.Create;
      netConfigContents.Add(Format('network:', []));
      netConfigContents.Add(Format('  version: 2', []));
      netConfigContents.Add(Format('  renderer: networkd', []));
      netConfigContents.Add(Format('  ethernets:', []));
      netConfigContents.Add(Format('    %s:', [netInterface]));
      netConfigContents.Add(Format('      dhcp4: no', []));
      netConfigContents.Add(Format('      addresses: [%s%s]', [newIP, newMask]));
      // netConfigContents.Add(Format('      addresses:', []));
      // netConfigContents.Add(Format('        - %s%s', [newIP, newMask]));
      // netConfigContents.Add(Format('      gateway4: %s', [newGate]));
      netConfigContents.Add(Format('      nameservers:', []));
      netConfigContents.Add(Format('        addresses: [8.8.8.8, 8.8.4.4]', []));

      cmd := Format('echo "%s" | sudo tee /etc/netplan/%s', [netConfigContents.Text, netConfigFileName]);
      cmdResult := Self.ExecuteCommand(cmd);
      Sleep(1000);

      // restart ssh server: sudo systemctl restart sshd
      cmdResult := ExecuteCommand('sudo systemctl restart sshd');

      // apply ip change: sudo netplan apply
      cmdResult := ExecuteCommand('sudo netplan apply');

      // ShowMessage('New IP address applied');

      // check if the new ip applied...
      if TryPing(newIP) then
      begin
        // ShowMessage('Ping succeeded on New IP address');

        res := True;

        Self.FConnected := False;
      end
      else
      begin
        res := False;
      end;

    finally

    end;

  end;

  Result := res;
end;

function TPuttySshClient.ExecuteCommand(cmd: string; inBatch: Boolean = True): string;
var
  res: string;
  CommandLine: string;
  output, input: TStringList;
begin
  res := '';

  output := TStringList.Create;
  input := TStringList.Create;

  output.Text := cmd;
  output.SaveToFile('cmd.txt');

  if inBatch then
  begin
    // commandLine := Format('%s -ssh %s -P %d -l %s -pw %s -batch %s', [FPlinkPath, FHostName, FPort, FUser, FPassword, cmd]);
    CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s -batch -m cmd.txt', [FPlinkPath, FHostName, FPort, FUser, FPassword]);
    try
      output.Clear;
      ExecuteCommandLine(CommandLine, output);
      res := output.Text;
    finally
      output.Free;
    end;
  end
  else
  begin
    input.Add('y'); // for yes
    CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s -m cmd.txt', [FPlinkPath, FHostName, FPort, FUser, FPassword]);
    try
      output.Clear;
      // ExecuteCommandLine2(commandLine, output, input);
      Self.FDosCommand.CommandLine := CommandLine;
      Self.FDosCommand.Execute;

      res := output.Text;
    finally
      output.Free;
    end;
  end;

  Result := res;
end;

procedure TPuttySshClient.SetDosCommandLine(Value: string);
begin
  Self.FDosCommand.CommandLine := Value;
end;

function TPuttySshClient.GetDosCommandLine: string;
begin
  Result := Self.FDosCommand.CommandLine;
end;

function TPuttySshClient.GetDosCommandOutputLines: TStrings;
begin
  Result := Self.FDosCommand.OutputLines;
end;

function TPuttySshClient.GetDosCommandIsRunning: Boolean;
begin
  Result := Self.FDosCommand.IsRunning;
end;

procedure TPuttySshClient.SetDosCommandOnNewLine(Value: TNewLineEvent);
begin
  Self.FDosCommand.OnNewLine := Value;
end;

procedure TPuttySshClient.SetDosCommandOnStarted(Value: TNotifyEvent);
begin
  Self.FDosCommand.OnStarted := Value;
end;

procedure TPuttySshClient.SetDosCommandOnTerminated(Value: TNotifyEvent);
begin
  Self.FDosCommand.OnTerminated := Value;
end;

// procedure TPuttySshClient.ExecuteDosCommand(cmd: string);
// begin
//
// end;
//
procedure TPuttySshClient.SendLineToDosCommand(input: string; eol: Boolean);
begin
  Self.FDosCommand.SendLine(input, eol);
end;

procedure TPuttySshClient.DosCommandStarted(Sender: TObject);
begin

end;

procedure TPuttySshClient.DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
begin

end;

procedure TPuttySshClient.DosCommandTerminated(Sender: TObject);
begin

end;

end.
