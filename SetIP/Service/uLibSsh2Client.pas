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
  SshSession := CreateSession(Self.HostIP, Self.FPort);
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
  section: string;
begin
  res := False;
  if Self.FIsConnected then
  begin
    try
      SshExec.Exec('ifconfig', cmdResult, ErrOutput, ExitCode);
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
        for line in Lines do
        begin
          if line.Contains('Link encap:Ethernet') then
          begin
            netInterface := line.Split([' '], TStringSplitOptions.ExcludeEmpty)[0];
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

      cmd := Format('sed -i ''s/%s/%s/'' /etc/network/interfaces', [Self.FHostIP, newIP]);
      SshExec.Exec(cmd, cmdResult, ErrOutput, ExitCode);
      cmdResult := cmdResult.Trim;

      // read net interface file and check the changes
      cmd := 'cat /etc/network/interfaces';
      SshExec.Exec(cmd, cmdResult, ErrOutput, ExitCode);
      cmdResult := cmdResult.Trim;

      if cmdResult.Contains(#13#10) then
      begin
        Lines := cmdResult.Split([#13#10]);
      end
      else if cmdResult.Contains(#10) then
      begin
        Lines := cmdResult.Split([#10]);
      end;

      section := Format('auto %s', [netInterface]);
      if Length(Lines) > 0 then
      begin
        i := 0; // index of line
        for line in Lines do
        begin
          if line.Trim = section then
          begin
            break;
          end
          else
            i := i + 1;
        end;

        i := i + 2;
        if i < Length(Lines) then
        begin
          line := Lines[i]; // get address line
          section := Format('address %s', [newIP]);
          if line.Trim = section then
          begin
            res := True;

            cmd := 'reboot';
            SshExec.Exec(cmd, cmdResult, ErrOutput, ExitCode);

            Self.FIsConnected := False;
          end;
        end
        else
        begin
          res := False;
          Result := res;
          Exit;
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
