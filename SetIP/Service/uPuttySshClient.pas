unit uPuttySshClient;

interface

uses
  Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, DosCommand;

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

    // function ChangeIP(newIP: string): Boolean;

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
