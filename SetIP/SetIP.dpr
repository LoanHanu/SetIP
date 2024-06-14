program SetIP;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uDevice in 'Devices\uDevice.pas',
  uPuttySshClient in 'Service\uPuttySshClient.pas',
  uOpenSshClient in 'Service\uOpenSshClient.pas',
  uCommonUtils in 'uCommonUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
