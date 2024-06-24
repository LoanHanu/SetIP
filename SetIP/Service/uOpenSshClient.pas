unit uOpenSshClient;

interface

uses
  Classes, SysUtils, WinAPI.ShellAPI, SyncObjs, Vcl.Dialogs, System.IOUtils,
  uDosCommand, uSshClient;

type
  /// <summary>
  /// ssh client using OpenSSH of WindowsOS: ssh.exe
  /// </summary>
  TOpenSshClient = class(TSshClient)
  private
    procedure DosCommandStarted(Sender: TObject);
    procedure DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
    procedure DosCommandTerminated(Sender: TObject);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure DisConnect; override;

    function ChangeIP(newIP, newMask, newGate: string): Boolean; override;

  end;

implementation

constructor TOpenSshClient.Create;
begin
  inherited;

  FProcessPath := 'ssh.exe'; // default: "C:\Windows\System32\OpenSSH\ssh.exe", when installed separately: "C:\Program Files\OpenSSH\ssh.exe"

  FDosCommand.OnStarted := DosCommandStarted;
  FDosCommand.OnNewLine := DosCommandNewLine;
  FDosCommand.OnTerminated := DosCommandTerminated;

end;

destructor TOpenSshClient.Destroy;
begin
  inherited;
end;

procedure TOpenSshClient.Connect;
var
  CommandLine: string;
begin
  case Self.FConnectionOption of
    coPassword:
      begin
        if FDosCommand.IsRunning then
        begin
          Sleep(1000); // wait for some seconds
          FDosCommand.Stop;
        end;

        if not FDosCommand.IsRunning then
        begin
          {
            ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no ubuntu@10.99.4.24

            -o UserKnownHostsFile=/dev/null //To ignore the user known hosts file
            -o StrictHostKeyChecking=no //To ignore host key checking
          }
          CommandLine := Format('%s -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no %s -p %d -l %s', [FProcessPath, FHostName, FPort, FUser]);
          Self.FDosCommand.CommandLine := CommandLine;
          Self.FDosCommand.Execute;
          Self.FIsConnecting := True;
        end;
      end;
    coIdKey:
      ;
  end;

end;

procedure TOpenSshClient.DisConnect;
begin
  if Self.FIsConnected and Self.FDosCommand.IsRunning then
  begin
    FDosCommand.SendLine('exit', True);
  end;
end;

function TOpenSshClient.ChangeIP(newIP, newMask, newGate: string): Boolean;
begin

end;

procedure TOpenSshClient.DosCommandStarted(Sender: TObject);
begin

end;

procedure TOpenSshClient.DosCommandNewLine(ASender: TObject; const ANewLine: string; AOutputType: TOutputType);
begin
  // if FIsConnecting and FDosCommand.IsRunning and ANewLine.Contains(Format('%s@%s''s password:', [Self.FUser, Self.FHostName])) then
  if FIsConnecting and FDosCommand.IsRunning and ANewLine.Contains('Password:  ubuntu (sudo su -)') then
  begin
    Self.FDosCommand.SendLine(Self.FPassword, True);
  end;

  if FIsConnecting and ANewLine.Contains('Last login:') then
  begin
    Self.FIsConnecting := False;
    Self.FIsConnected := True;
  end;

  if FIsConnected and ANewLine.Contains('logout') then
  begin
    Self.FIsConnected := False; // disconnection
  end;

end;

procedure TOpenSshClient.DosCommandTerminated(Sender: TObject);
begin

end;

end.
