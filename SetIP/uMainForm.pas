unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
  uDevice;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    SheetTR40: TTabSheet;
    SheetOP40: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ButtonTR40Ping: TButton;
    IconList: TImageList;
    Panel1: TPanel;
    EditTR40CurrIPOctet4: TEdit;
    EditTR40CurrIPDot3: TEdit;
    EditTR40CurrIPOctet3: TEdit;
    EditTR40CurrIPDot2: TEdit;
    EditTR40CurrIPOctet2: TEdit;
    EditTR40CurrIPDot1: TEdit;
    EditTR40CurrIPOctet1: TEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Panel2: TPanel;
    EditTR40NewIPOctet4: TEdit;
    Edit2: TEdit;
    EditTR40NewIPOctet3: TEdit;
    Edit4: TEdit;
    EditTR40NewIPOctet2: TEdit;
    Edit6: TEdit;
    EditTR40NewIPOctet1: TEdit;
    Label3: TLabel;
    Panel3: TPanel;
    EditTR40NewMaskOctet4: TEdit;
    Edit9: TEdit;
    EditTR40NewMaskOctet3: TEdit;
    Edit11: TEdit;
    EditTR40NewMaskOctet2: TEdit;
    Edit13: TEdit;
    EditTR40NewMaskOctet1: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Panel4: TPanel;
    EditTR40NewGateOctet4: TEdit;
    Edit16: TEdit;
    EditTR40NewGateOctet3: TEdit;
    Edit18: TEdit;
    EditTR40NewGateOctet2: TEdit;
    Edit20: TEdit;
    EditTR40NewGateOctet1: TEdit;
    ButtonTR40SetIP: TButton;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    ButtonOP40Ping: TButton;
    Panel5: TPanel;
    EditOP40CurrIPOctet4: TEdit;
    Edit3: TEdit;
    EditOP40CurrIPOctet3: TEdit;
    Edit7: TEdit;
    EditOP40CurrIPOctet2: TEdit;
    Edit10: TEdit;
    EditOP40CurrIPOctet1: TEdit;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel6: TPanel;
    EditOP40NewIPOctet4: TEdit;
    Edit15: TEdit;
    EditOP40NewIPOctet3: TEdit;
    Edit19: TEdit;
    EditOP40NewIPOctet2: TEdit;
    Edit22: TEdit;
    EditOP40NewIPOctet1: TEdit;
    Panel7: TPanel;
    EditOP40NewMaskOctet4: TEdit;
    Edit25: TEdit;
    EditOP40NewMaskOctet3: TEdit;
    Edit27: TEdit;
    EditOP40NewMaskOctet2: TEdit;
    Edit29: TEdit;
    EditOP40NewMaskOctet1: TEdit;
    Panel8: TPanel;
    EditOP40NewGateOctet4: TEdit;
    Edit32: TEdit;
    EditOP40NewGateOctet3: TEdit;
    Edit34: TEdit;
    EditOP40NewGateOctet2: TEdit;
    Edit36: TEdit;
    EditOP40NewGateOctet1: TEdit;
    ButtonOP40SetIP: TButton;

    procedure OctetEditExit(Sender: TObject);
    procedure OctetChange(Sender: TObject);

    procedure FormCreate(Sender: TObject);

    procedure ButtonTR40PingClick(Sender: TObject);
    procedure ButtonTR40SetIPClick(Sender: TObject);

    procedure ButtonOP40PingClick(Sender: TObject);
    procedure ButtonOP40SetIPClick(Sender: TObject);
  private
    FDeviceManager: TDeviceManager;

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Self.EditTR40CurrIPOctet1.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet2.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet3.OnExit := Self.OctetEditExit;
  // Self.EditTR40CurrIPOctet4.OnExit := Self.OctetEditExit;

  EditTR40NewIPOctet1.OnExit := OctetEditExit;
  EditTR40NewIPOctet2.OnExit := OctetEditExit;
  EditTR40NewIPOctet3.OnExit := OctetEditExit;
  EditTR40NewIPOctet4.OnExit := OctetEditExit;

  EditTR40NewMaskOctet1.OnExit := OctetEditExit;
  EditTR40NewMaskOctet2.OnExit := OctetEditExit;
  EditTR40NewMaskOctet3.OnExit := OctetEditExit;
  EditTR40NewMaskOctet4.OnExit := OctetEditExit;

  EditTR40NewGateOctet1.OnExit := OctetEditExit;
  EditTR40NewGateOctet2.OnExit := OctetEditExit;
  EditTR40NewGateOctet3.OnExit := OctetEditExit;
  EditTR40NewGateOctet4.OnExit := OctetEditExit;

  // Self.EditOP40CurrIPOctet1.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet2.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet3.OnExit := Self.OctetEditExit;
  // Self.EditOP40CurrIPOctet4.OnExit := Self.OctetEditExit;

  EditOP40NewIPOctet1.OnExit := OctetEditExit;
  EditOP40NewIPOctet2.OnExit := OctetEditExit;
  EditOP40NewIPOctet3.OnExit := OctetEditExit;
  EditOP40NewIPOctet4.OnExit := OctetEditExit;

  EditOP40NewMaskOctet1.OnExit := OctetEditExit;
  EditOP40NewMaskOctet2.OnExit := OctetEditExit;
  EditOP40NewMaskOctet3.OnExit := OctetEditExit;
  EditOP40NewMaskOctet4.OnExit := OctetEditExit;

  EditOP40NewGateOctet1.OnExit := OctetEditExit;
  EditOP40NewGateOctet2.OnExit := OctetEditExit;
  EditOP40NewGateOctet3.OnExit := OctetEditExit;
  EditOP40NewGateOctet4.OnExit := OctetEditExit;

  // Self.EditTR40CurrIPOctet1.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet2.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet3.OnChange := Self.OctetChange;
  // Self.EditTR40CurrIPOctet4.OnChange := Self.OctetChange;

  EditTR40NewIPOctet1.OnChange := OctetChange;
  EditTR40NewIPOctet2.OnChange := OctetChange;
  EditTR40NewIPOctet3.OnChange := OctetChange;
  EditTR40NewIPOctet4.OnChange := OctetChange;

  EditTR40NewMaskOctet1.OnChange := OctetChange;
  EditTR40NewMaskOctet2.OnChange := OctetChange;
  EditTR40NewMaskOctet3.OnChange := OctetChange;
  EditTR40NewMaskOctet4.OnChange := OctetChange;

  EditTR40NewGateOctet1.OnChange := OctetChange;
  EditTR40NewGateOctet2.OnChange := OctetChange;
  EditTR40NewGateOctet3.OnChange := OctetChange;
  EditTR40NewGateOctet4.OnChange := OctetChange;

  // Self.EditOP40CurrIPOctet1.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet2.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet3.OnChange := Self.OctetChange;
  // Self.EditOP40CurrIPOctet4.OnChange := Self.OctetChange;

  EditOP40NewIPOctet1.OnChange := OctetChange;
  EditOP40NewIPOctet2.OnChange := OctetChange;
  EditOP40NewIPOctet3.OnChange := OctetChange;
  EditOP40NewIPOctet4.OnChange := OctetChange;

  EditOP40NewMaskOctet1.OnChange := OctetChange;
  EditOP40NewMaskOctet2.OnChange := OctetChange;
  EditOP40NewMaskOctet3.OnChange := OctetChange;
  EditOP40NewMaskOctet4.OnChange := OctetChange;

  EditOP40NewGateOctet1.OnChange := OctetChange;
  EditOP40NewGateOctet2.OnChange := OctetChange;
  EditOP40NewGateOctet3.OnChange := OctetChange;
  EditOP40NewGateOctet4.OnChange := OctetChange;

  Self.PageControl.ActivePage := Self.SheetTR40;

  Self.FDeviceManager := TDeviceManager.Create;

end;

procedure TMainForm.ButtonOP40PingClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonOP40SetIPClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.ButtonTR40PingClick(Sender: TObject);
begin
  if Self.ButtonTR40Ping.ImageIndex = 0 then
    Self.ButtonTR40Ping.ImageIndex := 1
  else if Self.ButtonTR40Ping.ImageIndex = 1 then
    Self.ButtonTR40Ping.ImageIndex := 0;
end;

procedure TMainForm.ButtonTR40SetIPClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.OctetEditExit(Sender: TObject);
var
  minVal, maxVal, currVal: Integer;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    Exit;

  minVal := 0;
  maxVal := 255;

  currVal := StrToIntDef(editBox.Text, 0);
  if currVal < minVal then
    currVal := minVal
  else if currVal > maxVal then
    currVal := maxVal;

  editBox.Text := currVal.ToString;
  //
end;

procedure TMainForm.OctetChange(Sender: TObject);
var
  NextCtrl: TControl;
  editBox: TEdit;
begin
  editBox := TEdit(Sender);
  if not Assigned(editBox) then
    Exit;

  // Move the focus to the next octet after 3 characters are entered
  if Length(editBox.Text) >= 3 then
  begin
    NextCtrl := editBox.Parent.ControlAtPos(Point(editBox.Left + editBox.Width + 10, editBox.Top), True, True, True);
    if (NextCtrl <> nil) and (NextCtrl is TEdit) then
      (NextCtrl as TEdit).SetFocus;
  end;
end;

end.
