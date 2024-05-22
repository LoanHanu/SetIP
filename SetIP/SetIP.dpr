program SetIP;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uDevice in 'Devices\uDevice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
