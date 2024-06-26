program SetIP;







uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uDevice in 'Devices\uDevice.pas',
  uPuttySshClient in 'Service\uPuttySshClient.pas',
  uOpenSshClient in 'Service\uOpenSshClient.pas',
  uCommonUtils in 'uCommonUtils.pas',
  uDosCommand in 'Service\uDosCommand.pas',
  uSshClient in 'Service\uSshClient.pas',
  LibSsh2Client in 'Service\libssh2\LibSsh2Client.pas',
  SocketUtils in 'Service\libssh2\SocketUtils.pas',
  uLibSsh2Client in 'Service\uLibSsh2Client.pas',
  Winsock2 in 'Service\winsock2\Winsock2.pas',
  IdModbusClient in 'Service\Modbus\IdModbusClient.pas',
  ModBusConsts in 'Service\Modbus\ModBusConsts.pas',
  ModbusTypes in 'Service\Modbus\ModbusTypes.pas',
  ModbusUtils in 'Service\Modbus\ModbusUtils.pas',
  libssh2 in 'Service\libssh2\libssh2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
