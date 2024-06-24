unit uLibSsh2Client;

interface

uses
  Winapi.WinSock2, Classes, SysUtils, Winapi.ShellAPI, SyncObjs, Vcl.Dialogs, System.IOUtils,
  uDosCommand, uSshClient, libssh2, LibSsh2Client, SocketUtils;

function LibSsh2KeybIntCallback(const AuthName, AuthInstruction, Prompt: string; Echo: Boolean): string;

type
  TLibSsh2Client = class(TSshClient)
  private
    SshSession: ISshSession;
    SshExec: ISshExec;
    Output: string;
    ErrOutput: string;
    ExitCode: Integer;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure DisConnect; override;

    function ChangeIP(newIP, newMask, newGate: string): Boolean; override;
  end;

implementation

constructor TLibSsh2Client.Create;
begin
  Self.FProcessPath := 'libssh2.dll';

  // SshSession := CreateSession(Self.HostName, Self.FPort);
  // Session.UseCompression := True;
  // SshSession.SetKeybInteractiveCallback(LibSsh2KeybIntCallback);

end;

destructor TLibSsh2Client.Destroy;
begin
  inherited;
end;

procedure TLibSsh2Client.Connect;
begin
  SshSession := CreateSession(Self.HostName, Self.FPort);
  // Session.UseCompression := True;
  SshSession.SetKeybInteractiveCallback(LibSsh2KeybIntCallback);

  SshSession.Connect;
  // WriteLn(SshSession.HostBanner);
  // WriteLn(SshSession.SessionMethods);

  // if not Session.UserAuthInteractive(UserName) then
  // if not Session.UserAuth(UserName) then
  if not SshSession.UserAuthPass(Self.FUser, Self.FPassword) then
  begin
    // WriteLn('Authorization Failure');
    Self.FIsConnected := False;
    Exit;
  end
  else
  begin
    Self.FIsConnected := True;

    SshExec := CreateSshExec(SshSession);
    // SshExec.Exec(Command, Output, ErrOutput, ExitCode);
  end;

end;

procedure TLibSsh2Client.DisConnect;
begin
  SshSession.DisConnect;
  Self.FIsConnected := False;
end;

function TLibSsh2Client.ChangeIP(newIP: string; newMask: string; newGate: string): Boolean;
var
  res: Boolean;
  cmdResult: string;
  netInterface, netConfigFileName: string;
  Lines, fields, names: TArray<string>;
  line, name, ext: string;
  cmd, Output, ErrOutput: string;
  i, ExitCode: Integer;
  netConfigContents: TStringList;
begin
  res := False;
  if Self.FIsConnected then
  begin
    try
      SshExec.Exec('nmcli device status', cmdResult, ErrOutput, ExitCode);
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
      SshExec.Exec('ls /etc/netplan', cmdResult, ErrOutput, ExitCode); // 'ls: cannot access ''/etc/netplan'': No such file or directory'
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
      Self.SshExec.Exec(cmd, cmdResult, ErrOutput, ExitCode);
      Sleep(1000);

      if netConfigFileName.Contains('.yaml') then
      begin
        // restart ssh server: sudo systemctl restart sshd
        SshExec.Exec('sudo systemctl restart sshd', cmdResult, ErrOutput, ExitCode);

        // apply ip change: sudo netplan apply
        SshExec.Exec('sudo netplan apply', cmdResult, ErrOutput, ExitCode);

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
        SshExec.Exec('sudo systemctl restart networking.service', cmdResult, ErrOutput, ExitCode);
        SshExec.Exec(Format('sudo ifdown %s && sudo ifup %s', [netInterface, netInterface]), cmdResult, ErrOutput, ExitCode);
        SshExec.Exec(Format('sudo ifdown %s && sudo ifup %s', [netInterface, netInterface]), cmdResult, ErrOutput, ExitCode); // need twice, but not sure why
        SshExec.Exec('sudo hostname -I', cmdResult, ErrOutput, ExitCode);

        // check if new ip applied
        if cmdResult.Contains(newIP) then
        begin
          res := True;
          Self.FIsConnected := False;

          // finally send cmd for reboot
          SshExec.Exec('sudo reboot', cmdResult, ErrOutput, ExitCode);
        end
        else
        begin
          res := False;
          Self.FIsConnected := False;
        end;

      end;
    except
      on E: Exception do
      begin
        ShowMessage('An exception occurred: ' + E.Message);
        if res then
        begin
          Result := res;
          Exit;
        end;

      end;
    end;

  end
  else
  begin
    res := False;
  end;

  Result := res;
end;

function LibSsh2KeybIntCallback(const AuthName, AuthInstruction, Prompt: string; Echo: Boolean): string;
begin
  // if AuthName <> '' then
  // WriteLn('Authorization Name: ', AuthName);
  // if AuthInstruction <> '' then
  // WriteLn('Authorization Instruction: ', AuthInstruction);

  if Prompt.Contains('Do you want to continue connecting (yes/no)?') then
  begin
    Result := 'yes';
  end;

  // Write(Prompt);
  // // if Echo is False then you should mask the input
  // // See https://stackoverflow.com/questions/3671042/mask-password-input-in-a-console-app
  // ReadLn(Result);
end;

end.
