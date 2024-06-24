unit uPuttySshClient;

interface

uses
  Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, System.IOUtils, uDosCommand,
  uSshClient;

type

  /// <summary>
  /// ssh client using PuTTy: plink.exe
  /// </summary>
  TPuttySshClient = class(TSshClient)
  private
    function IsPuttyInstalled: Boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure DisConnect; override;

    function ChangeIP(newIP, newMask, newGate: string): Boolean; override;

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

    property DosCommandLine: string read GetDosCommandLine write SetDosCommandLine;
    property DosCommandOutputLines: TStrings read GetDosCommandOutputLines;
    property DosCommandIsRunning: Boolean read GetDosCommandIsRunning;
    property DosCommandOnNewLine: TNewLineEvent write SetDosCommandOnNewLine;
    property DosCommandOnStarted: TNotifyEvent write SetDosCommandOnStarted; // added by Loan
    property DosCommandOnTerminated: TNotifyEvent write SetDosCommandOnTerminated;

  end;

implementation

uses
  uCommonUtils;

constructor TPuttySshClient.Create;
begin
  FProcessPath := 'plink.exe'; // C:\Program Files\PuTTY\plink.exe when installed x64 version

  inherited;
end;

destructor TPuttySshClient.Destroy;
begin
  inherited;
end;

function TPuttySshClient.IsPuttyInstalled: Boolean;
var
  res: Boolean;
  cmdLine: string;
  output: TStrings;
begin
  res := False;

  output := TStringList.Create;

  cmdLine := Format('%s -V', [FProcessPath]);
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

procedure TPuttySshClient.DisConnect;
begin
  if Self.FIsConnected and Self.FDosCommand.IsRunning then
  begin
    FDosCommand.SendLine('exit', True);
  end;

end;

procedure TPuttySshClient.Connect;
var
  cmdResult: string;
  CommandLine: string;
begin
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
        if FDosCommand.IsRunning then
        begin
          Sleep(1000); // wait for some seconds
          FDosCommand.Stop;
        end;

        if not FDosCommand.IsRunning then
        begin
          CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s', [FProcessPath, FHostName, FPort, FUser, FPassword]);
          Self.FDosCommand.CommandLine := CommandLine;
          Self.FDosCommand.Execute;
          Self.FIsConnecting := True;
        end;

        // cmdResult := ExecuteCommand('ls');
        // if cmdResult.ToLower.Contains('error') then // or
        // begin
        // if cmdResult.Contains('Network error: Connection timed out') then
        // begin
        // ShowMessage(cmdResult + #13#10 + 'Please check if host name or port number is correct.');
        // end
        // else if cmdResult.Contains('Network error: Connection refused') then
        // begin
        // ShowMessage(cmdResult + #13#10 + 'Please check if host name or port number is correct.');
        // end
        // else if cmdResult.Contains('Configured password was not accepted') then
        // begin
        // ShowMessage(cmdResult + #13#10 + 'Please check if user name or password is correct.');
        // end
        // else if cmdResult.Contains('Cannot confirm a host key in batch mode') then
        // begin
        // cmdResult := ExecuteCommand('ls', False);
        // // ShowMessage(cmdResult);
        // end
        // else
        // begin
        // ShowMessage(cmdResult);
        // end;
        //
        //
        // end
        // else if cmdResult.Contains('is not recognized as an internal or external command') then
        // begin
        // ShowMessage(cmdResult + #13#10 + 'Please check if PuTTy installed properly.');
        // end
        // else
        // begin
        //
        // end;
      end;
    coIdKey:
      begin
      end;
  end;
end;

function TPuttySshClient.ChangeIP(newIP, newMask, newGate: string): Boolean;
var
  res: Boolean;
  cmdResult: string;
  netInterface, netConfigFileName: string;
  Lines, fields, names: TArray<string>;
  line, name, ext: string;
  cmd: string;
  i: integer;
  netConfigContents: TStringList;
begin
  res := False;
  if Self.FIsConnected then
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

      if netInterface = '' then // error
      begin
        ShowMessage('There is no proper network interface.');
        Result := False;
        Exit;
      end;

      // network config file:
      // the case of modern OS(Ubuntu 17.10 or later) : /etc/netplan/*.yaml
      // the case of old OS(traditional network): /etc/network/interfaces

      netConfigContents := TStringList.Create;

      // get network config file name with extension ".yaml" : ls /etc/netplan
      netConfigFileName := '';
      cmdResult := ExecuteCommand('ls /etc/netplan'); // 'ls: cannot access ''/etc/netplan'': No such file or directory'
      cmdResult := cmdResult.Trim;
      if cmdResult.ToLower.Contains('no such file or directory') or (cmdResult = '') then
      begin
        netConfigFileName := '';
      end
      else
      begin
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
      end;

      if netConfigFileName <> '' then // the case of morden OS
      begin
        netConfigFileName := '/etc/netplan/' + netConfigFileName; // MAKE FULL PATH

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

        netConfigContents.Clear;
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

      end
      else // the case of old OS
      begin
        netConfigFileName := '/etc/network/interfaces';

        {
          auto eth0
          iface eth0 inet static
          address 192.168.1.10
          netmask 255.255.255.0
          gateway 192.168.1.1
        }
        netConfigContents.Clear;
        netConfigContents.Add(Format('auto %s', [netInterface]));
        netConfigContents.Add(Format('iface %s inet static', [netInterface]));
        netConfigContents.Add(Format('address %s', [newIP]));
        netConfigContents.Add(Format('netmask %s', [newMask]));
        // netConfigContents.Add(Format('gateway %s', [newGate]));

      end;

      // run netplan config command to change with new ip address
      cmd := Format('echo "%s" | sudo tee %s', [netConfigContents.Text, netConfigFileName]);
      cmdResult := Self.ExecuteCommand(cmd);
      Sleep(1000);

      if netConfigFileName.Contains('.yaml') then
      begin
        // restart ssh server: sudo systemctl restart sshd
        cmdResult := ExecuteCommand('sudo systemctl restart sshd');

        // apply ip change: sudo netplan apply
        cmdResult := ExecuteCommand('sudo netplan apply');

        // check if the new ip applied...
        if TryPing(newIP) then
        begin
          // ShowMessage('Ping succeeded on New IP address');

          res := True;
          Self.FIsConnected := False;
        end;

      end
      else if netConfigFileName = '/etc/network/interfaces' then
      begin
        cmdResult := ExecuteCommand('sudo systemctl restart networking.service');
        cmdResult := ExecuteCommand(Format('sudo ifdown %s && sudo ifup %s', [netInterface, netInterface]));
        cmdResult := ExecuteCommand(Format('sudo ifdown %s && sudo ifup %s', [netInterface, netInterface])); // need twice, but not sure why
        cmdResult := ExecuteCommand('sudo hostname -I');

        // check if new ip applied
        if cmdResult.Contains(newIP) then
        begin
          res := True;
          Self.FIsConnected := False;

          // finally send cmd for reboot
          cmdResult := ExecuteCommand('sudo reboot');
        end
        else
        begin
          res := False;
          Self.FIsConnected := False;
        end;

      end;

      // ShowMessage('New IP address applied');

    finally

    end;

  end;

  Result := res;
end;

function TPuttySshClient.ExecuteCommand(cmd: string; inBatch: Boolean = True): string;
var
  res: string;
  CommandLine: string;
  output: TStringList;
begin
  res := '';

  output := TStringList.Create;

  if cmd = '' then // the case of connection cmd
  begin
    CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s', [FProcessPath, FHostName, FPort, FUser, FPassword]);

  end;

  if cmd <> '' then
  begin
    output.Text := cmd;
    output.SaveToFile('cmd.txt');

    if inBatch then
    begin
      // commandLine := Format('%s -ssh %s -P %d -l %s -pw %s -batch %s', [FPlinkPath, FHostName, FPort, FUser, FPassword, cmd]);
      CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s -batch -m cmd.txt', [FProcessPath, FHostName, FPort, FUser, FPassword]);
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
      CommandLine := Format('%s -ssh %s -P %d -l %s -pw %s -m cmd.txt', [FProcessPath, FHostName, FPort, FUser, FPassword]);
      try
        output.Clear;

        Self.FDosCommand.CommandLine := CommandLine;
        Self.FDosCommand.Execute;

        res := output.Text;
      finally
        output.Free;
      end;
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
  if Self.FDosCommand.IsRunning then
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
