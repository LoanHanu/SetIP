unit uCommonUtils;

interface

uses
  Windows, Classes, SysUtils, IdIcmpClient, Vcl.Dialogs;

procedure ExecuteCommandLine(Command: string; Output: TStrings);

procedure ExecuteCommandLine2(Command: string; Output: TStrings; Input: TStrings);

function TryPing(HostIP: string): Boolean;

implementation

procedure ExecuteCommandLine(Command: string; Output: TStrings);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttributes: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  Buffer: array [0 .. 4095] of AnsiChar;
  BytesRead: Cardinal;
  CommandLine: string;
begin
  // Initialize security attributes
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  // Create pipes for standard output redirection
  if not CreatePipe(ReadPipe, WritePipe, @SecurityAttributes, 0) then
    raise Exception.Create('CreatePipe failed.');

  try
    // Initialize startup info
    ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.hStdOutput := WritePipe;
    StartupInfo.hStdError := WritePipe;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;

    // Prepare command line
    CommandLine := 'cmd.exe /C ' + Command;

    // Create the process
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
      raise Exception.Create('CreateProcess failed.');

    // Close the write end of the pipe
    CloseHandle(WritePipe);

    // Read the output
    while ReadFile(ReadPipe, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) do
    begin
      if BytesRead > 0 then
      begin
        Buffer[BytesRead] := #0; // Null-terminate the buffer
        // WriteLn(string(Buffer));
        Output.Add(Buffer);
      end;
    end;

    // Wait for the process to finish
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);

  finally
    CloseHandle(ReadPipe);
  end;
end;

procedure ExecuteCommandLine2(Command: string; Output: TStrings; Input: TStrings);
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttributes: TSecurityAttributes;
  ReadPipe, WritePipe: THandle;
  InputReadPipe, InputWritePipe: THandle;
  InputLine: string;
  ReadBuffer: array [0 .. 4095] of AnsiChar;
  WriteBuffer: TArray<Char>;
  BytesRead, BytesWrite: Cardinal;
  CommandLine: string;
  PromptReceived: Boolean;
begin
  // Initialize security attributes
  SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
  SecurityAttributes.bInheritHandle := True;
  SecurityAttributes.lpSecurityDescriptor := nil;

  // Create pipes for standard output and input redirection
  if not CreatePipe(ReadPipe, WritePipe, @SecurityAttributes, 0) then
    raise Exception.Create('CreatePipe for output failed.');

  if not CreatePipe(InputReadPipe, InputWritePipe, @SecurityAttributes, 0) then
    raise Exception.Create('CreatePipe for input failed.');

  try
    // Initialize startup info
    ZeroMemory(@StartupInfo, SizeOf(TStartupInfo));
    StartupInfo.cb := SizeOf(TStartupInfo);
    StartupInfo.hStdInput := InputWritePipe; // Set standard input handle
    StartupInfo.hStdOutput := WritePipe;
    StartupInfo.hStdError := WritePipe;
    StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_SHOWNORMAL; // SW_HIDE;

    // Prepare command line
    CommandLine := 'cmd.exe /C ' + Command;

    // Create the process
    if not CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil, nil, StartupInfo, ProcessInfo) then
      raise Exception.Create('CreateProcess failed.');

    PromptReceived := False;

    // Read the output
    while ReadFile(ReadPipe, ReadBuffer, SizeOf(ReadBuffer) - 1, BytesRead, nil) do
    begin
      if BytesRead > 0 then
      begin
        ReadBuffer[BytesRead] := #0; // Null-terminate the buffer
        Output.Add(ReadBuffer);

        if not PromptReceived and (Output.Text.Contains('Update cached key?')) then
        begin
          PromptReceived := True; // Avoid multiple sends

          // Send 'y' followed by newline to accept the key
          WriteFile(InputWritePipe, 'y' + #13#10, 3 * SizeOf(Char), BytesWrite, nil);

          // Close the write end of the pipes
          CloseHandle(WritePipe);
        end;

      end;
    end;

    // Write input to the process if needed
    if Input.Count > 0 then
    begin
      for InputLine in Input do
      begin
        WriteBuffer := InputLine.ToCharArray;
        WriteFile(InputWritePipe, WriteBuffer, Length(WriteBuffer), BytesWrite, nil);
        if BytesWrite > 0 then
        begin
          // Send Enter key after 'y'
          WriteBuffer := [#13];
          WriteFile(InputWritePipe, WriteBuffer, Length(WriteBuffer), BytesWrite, nil);
          WriteFile(InputWritePipe, WriteBuffer, Length(WriteBuffer), BytesWrite, nil);
        end;
      end;
    end;

    CloseHandle(InputWritePipe);

    // Wait for the process to finish
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);

  finally
    CloseHandle(ReadPipe);
  end;
end;

function TryPing(HostIP: string): Boolean;
var
  res: Boolean;
  Ping: TIdIcmpClient;
begin
  res := False;
  Ping := TIdIcmpClient.Create(nil);
  Ping.ReceiveTimeout := 3000; // 5s

  try
    Ping.Host := HostIP;
    Ping.Ping();
    // Ping.Ping('0000');
    // case Ping.ReplyStatus.ReplyStatusType of
    // rsEcho: ;
    // rsError: ;
    // rsTimeOut: ;
    // rsErrorUnreachable: ;
    // rsErrorTTLExceeded: ;
    // rsErrorPacketTooBig: ;
    // rsErrorParameter: ;
    // rsErrorDatagramConversion: ;
    // rsErrorSecurityFailure: ;
    // rsSourceQuench: ;
    // rsRedirect: ;
    // rsTimeStamp: ;
    // rsInfoRequest: ;
    // rsAddressMaskRequest: ;
    // rsTraceRoute: ;
    // rsMobileHostReg: ;
    // rsMobileHostRedir: ;
    // rsIPv6WhereAreYou: ;
    // rsIPv6IAmHere: ;
    // rsSKIP: ;
    // end;

    if Ping.ReplyStatus.ReplyStatusType = rsEcho then
      // if Ping.ReplyStatus.BytesReceived > 0 then
      res := True
    else
      res := False;
  except
    on E: Exception do
    begin
      res := False;
      ShowMessage('An exception occurred: ' + E.Message);
    end;
  end;

  Ping.Free;
  Result := res;
end;

end.
