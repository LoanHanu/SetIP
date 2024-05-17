object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 441
    ActivePage = SheetOP40
    Align = alClient
    TabOrder = 0
    ExplicitTop = 56
    ExplicitHeight = 385
    object SheetTR40: TTabSheet
      Caption = 'TR40'
    end
    object SheetOP40: TTabSheet
      Caption = 'OP40'
      ImageIndex = 1
    end
  end
end
