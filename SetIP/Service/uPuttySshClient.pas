unit uPuttySshClient;

interface

uses
  Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, DosCommand;

type
  TSshConnectionOption = (scoPassword, scoPubicKey);

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
    FLockConnection: TCriticalSection;
    FDosCommand: TDosCommand;

  public
    constructor Create;
    destructor Destroy; override;

    function CanConnect: Boolean;

    // function ChangeIP(newIP: string): Boolean;

    function IsPuttyInstalled: Boolean;

    /// <summary>
    /// execute cmd on remote machine, ssh server
    /// </summary>
    function ExecuteCommand(cmd: string; inBatch: Boolean = True): string;

  public
    property PlinkPath: string read FPlinkPath write FPlinkPath;
    property HostName: string read FHostName write FHostName;
    property Port: integer read FPort write FPort;
    property User: string read FUser write FUser;
    property Password: string read FPassword write FPassword;
    property PrivateKey: string read FPrivateKey write FPrivateKey;
    property Connected: Boolean read FConnected write FConnected;
    property ConnectionOption: TSshConnectionOption read FConnectionOption write FConnectionOption;

  published
    property DosCommand: TDosCommand read FDosCommand;

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
  FConnectionOption := scoPassword;
  FLockConnection := TCriticalSection.Create;
  FDosCommand := TDosCommand.Create(nil);
end;

destructor TPuttySshClient.Destroy;
begin
  inherited;

  FLockConnection.Free;
  FDosCommand.Free;
end;

function TPuttySshClient.IsPuttyInstalled: Boolean;
var
  res: Boolean;
  commandLine: string;
  output: TStringList;
begin
  res := False;

  output := TStringList.Create;

  commandLine := Format('%s -V', [FPlinkPath]);
  try
    ExecuteCommandLine(commandLine, output);

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

  if not Self.IsPuttyInstalled then
  begin
    Result := False;
    Exit;
  end;

  case FConnectionOption of
    // plink -ssh username@hostname -pw user_password command_to_execute
    // plink -ssh hostname -P port -l username -pw user_password -batch command_to_execute
    // for example: plink -ssh 10.99.4.24 -P 22 -l ubuntu -pw ubuntu -batch ls
    // FATAL ERROR: Network error: Connection timed out - invalid ip
    // FATAL ERROR: Network error: Connection refused - invalid port
    // AFATAL ERROR: Configured password was not accepted - invalid user or password
    // 'plink' is not recognized as an internal or external command - PuTTy not installed
    scoPassword:
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
        else
        begin
          res := True;
        end;
      end;
    scoPubicKey:
      begin
      end;
  end;

  FConnected := res;
  Result := res;
end;

function TPuttySshClient.ExecuteCommand(cmd: string; inBatch: Boolean = True): string;
var
  res: string;
  commandLine: string;
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
    commandLine := Format('%s -ssh %s -P %d -l %s -pw %s -batch -m cmd.txt', [FPlinkPath, FHostName, FPort, FUser, FPassword]);
    try
      output.Clear;
      ExecuteCommandLine(commandLine, output);
      res := output.Text;
    finally
      output.Free;
    end;
  end
  else
  begin
    input.Add('y'); // for yes
    commandLine := Format('%s -ssh %s -P %d -l %s -pw %s -m cmd.txt', [FPlinkPath, FHostName, FPort, FUser, FPassword]);
    try
      output.Clear;
      // ExecuteCommandLine2(commandLine, output, input);
      Self.FDosCommand.commandLine := commandLine;
      Self.FDosCommand.Execute;

      res := output.Text;
    finally
      output.Free;
    end;
  end;

  Result := res;
end;

end.
