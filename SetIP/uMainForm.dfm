object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 562
  ClientWidth = 388
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label4: TLabel
    AlignWithMargins = True
    Left = 13
    Top = 32
    Width = 59
    Height = 15
    Margins.Left = 10
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Alignment = taCenter
    Caption = 'Current IP: '
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 388
    Height = 562
    ActivePage = SheetOP40
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 689
    ExplicitHeight = 561
    object SheetTR40: TTabSheet
      Caption = 'TR40'
      DesignSize = (
        380
        532)
      object GroupBox1: TGroupBox
        Left = 3
        Top = 3
        Width = 370
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Origin IP...'
        TabOrder = 0
        ExplicitWidth = 675
        object Label1: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 24
          Width = 59
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Current IP: '
        end
        object ButtonTR40Ping: TButton
          AlignWithMargins = True
          Left = 283
          Top = 20
          Width = 80
          Height = 25
          Margins.Left = 10
          Margins.Top = 1
          Margins.Bottom = 1
          Caption = 'Ping...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ImageIndex = 0
          Images = IconList
          ParentFont = False
          TabOrder = 1
          OnClick = ButtonTR40PingClick
        end
        object Panel1: TPanel
          Left = 76
          Top = 14
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 0
          object EditTR40CurrIPOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 4
            Text = '1'
          end
          object EditTR40CurrIPDot3: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditTR40CurrIPOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 3
            Text = '1'
          end
          object EditTR40CurrIPDot2: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditTR40CurrIPOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 2
            Text = '168'
          end
          object EditTR40CurrIPDot1: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditTR40CurrIPOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
            Text = '192'
          end
        end
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 63
        Width = 370
        Height = 218
        Anchors = [akLeft, akTop, akRight]
        Caption = 'New IP...'
        TabOrder = 1
        ExplicitWidth = 675
        object Label2: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 24
          Width = 58
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'IP Address:'
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 64
          Width = 71
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Subnet Mask:'
        end
        object Label5: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 104
          Width = 92
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Default Gateway: '
        end
        object Panel2: TPanel
          Left = 105
          Top = 14
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 0
          object EditTR40NewIPOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '1'
          end
          object Edit2: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditTR40NewIPOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '1'
          end
          object Edit4: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditTR40NewIPOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '168'
          end
          object Edit6: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditTR40NewIPOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '192'
            ExplicitTop = 9
          end
        end
        object Panel3: TPanel
          Left = 105
          Top = 54
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 1
          object EditTR40NewMaskOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '0'
          end
          object Edit9: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditTR40NewMaskOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '255'
          end
          object Edit11: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditTR40NewMaskOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '255'
          end
          object Edit13: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditTR40NewMaskOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '255'
          end
        end
        object Panel4: TPanel
          Left = 105
          Top = 94
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 2
          object EditTR40NewGateOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '0'
          end
          object Edit16: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditTR40NewGateOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '1'
          end
          object Edit18: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditTR40NewGateOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '168'
          end
          object Edit20: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditTR40NewGateOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '192'
          end
        end
        object ButtonTR40SetIP: TButton
          AlignWithMargins = True
          Left = 283
          Top = 100
          Width = 80
          Height = 25
          Margins.Left = 10
          Margins.Top = 1
          Margins.Bottom = 1
          Caption = 'Set...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ImageIndex = 0
          Images = IconList
          ParentFont = False
          TabOrder = 3
          OnClick = ButtonTR40SetIPClick
        end
      end
    end
    object SheetOP40: TTabSheet
      Caption = 'OP40'
      ImageIndex = 1
      DesignSize = (
        380
        532)
      object GroupBox3: TGroupBox
        Left = 3
        Top = 3
        Width = 370
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Origin IP...'
        TabOrder = 0
        object Label6: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 24
          Width = 59
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Current IP: '
        end
        object ButtonOP40Ping: TButton
          AlignWithMargins = True
          Left = 283
          Top = 20
          Width = 80
          Height = 25
          Margins.Left = 10
          Margins.Top = 1
          Margins.Bottom = 1
          Caption = 'Ping...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ImageIndex = 0
          Images = IconList
          ParentFont = False
          TabOrder = 1
          OnClick = ButtonOP40PingClick
        end
        object Panel5: TPanel
          Left = 76
          Top = 14
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 0
          object EditOP40CurrIPOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 4
            Text = '2'
          end
          object Edit3: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditOP40CurrIPOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 3
            Text = '1'
          end
          object Edit7: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditOP40CurrIPOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 2
            Text = '168'
          end
          object Edit10: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditOP40CurrIPOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            ReadOnly = True
            TabOrder = 1
            Text = '192'
          end
        end
      end
      object GroupBox4: TGroupBox
        Left = 3
        Top = 63
        Width = 370
        Height = 218
        Anchors = [akLeft, akTop, akRight]
        Caption = 'New IP...'
        TabOrder = 1
        object Label7: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 24
          Width = 58
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'IP Address:'
        end
        object Label8: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 64
          Width = 71
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Subnet Mask:'
        end
        object Label9: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 104
          Width = 92
          Height = 15
          Margins.Left = 10
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Alignment = taCenter
          Caption = 'Default Gateway: '
        end
        object Panel6: TPanel
          Left = 105
          Top = 14
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 0
          object EditOP40NewIPOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '2'
          end
          object Edit15: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditOP40NewIPOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '1'
          end
          object Edit19: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditOP40NewIPOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '168'
          end
          object Edit22: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditOP40NewIPOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '192'
          end
        end
        object Panel7: TPanel
          Left = 105
          Top = 54
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 1
          object EditOP40NewMaskOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '0'
          end
          object Edit25: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditOP40NewMaskOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '255'
          end
          object Edit27: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditOP40NewMaskOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '255'
          end
          object Edit29: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditOP40NewMaskOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '255'
          end
        end
        object Panel8: TPanel
          Left = 105
          Top = 94
          Width = 165
          Height = 35
          BevelOuter = bvNone
          BorderWidth = 1
          BorderStyle = bsSingle
          TabOrder = 2
          object EditOP40NewGateOctet4: TEdit
            AlignWithMargins = True
            Left = 118
            Top = 4
            Width = 30
            Height = 23
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 4
            Text = '0'
          end
          object Edit32: TEdit
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = '.'
          end
          object EditOP40NewGateOctet3: TEdit
            AlignWithMargins = True
            Left = 80
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 3
            Text = '1'
          end
          object Edit34: TEdit
            AlignWithMargins = True
            Left = 72
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 5
            Text = '.'
          end
          object EditOP40NewGateOctet2: TEdit
            AlignWithMargins = True
            Left = 42
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 2
            Text = '168'
          end
          object Edit36: TEdit
            AlignWithMargins = True
            Left = 34
            Top = 4
            Width = 5
            Height = 23
            Margins.Left = 0
            Margins.Right = 0
            TabStop = False
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            TabOrder = 6
            Text = '.'
          end
          object EditOP40NewGateOctet1: TEdit
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 30
            Height = 23
            Margins.Right = 0
            Align = alLeft
            Alignment = taCenter
            BorderStyle = bsNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -15
            Font.Name = 'Segoe UI'
            Font.Style = []
            MaxLength = 3
            NumbersOnly = True
            ParentFont = False
            TabOrder = 1
            Text = '192'
          end
        end
        object ButtonOP40SetIP: TButton
          AlignWithMargins = True
          Left = 283
          Top = 100
          Width = 80
          Height = 25
          Margins.Left = 10
          Margins.Top = 1
          Margins.Bottom = 1
          Caption = 'Set...'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = []
          ImageIndex = 0
          Images = IconList
          ParentFont = False
          TabOrder = 3
          OnClick = ButtonOP40SetIPClick
        end
      end
    end
  end
  object IconList: TImageList
    Left = 24
    Top = 504
    Bitmap = {
      494C010102000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AAAAF7555555F0AA1D1DEBE21D1DEBE25555F0AAAAAAF7550000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AAE5B15555CB63AA1DB930E21DB930E255CB63AAAAE5B1550000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E3E3
      FC1C5555F0AA0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF5555
      F0AAE3E3FC1C000000000000000000000000000000000000000000000000E3F6
      E51C55CB63AA00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF55CB
      63AAE3F6E51C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005555
      F0AA0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF5555F0AA00000000000000000000000000000000000000000000000055CB
      63AA00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF55CB63AA0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AAAAF7550000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FFAAAAF75500000000000000000000000000000000AAE5B15500B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FFAAE5B15500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005555F0AA0000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF5555F0AA0000000000000000000000000000000055CB63AA00B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF55CB63AA00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001D1DEBE20000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF1D1DEBE2000000000000000000000000000000001DB930E200B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF1DB930E200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001D1DEBE20000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF1D1DEBE2000000000000000000000000000000001DB930E200B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF1DB930E200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000005555F0AA0000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF5555F0AA0000000000000000000000000000000055CB63AA00B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF55CB63AA00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AAAAF7550000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FFAAAAF75500000000000000000000000000000000AAE5B15500B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FFAAE5B15500000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005555
      F0AA0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF5555F0AA00000000000000000000000000000000000000000000000055CB
      63AA00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF55CB63AA0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E3E3
      FC1C5555F0AA0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF5555
      F0AAE3E3FC1C000000000000000000000000000000000000000000000000E3F6
      E51C55CB63AA00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF55CB
      63AAE3F6E51C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AAAAF7555555F0AA1D1DEBE21D1DEBE25555F0AAAAAAF7550000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AAE5B15555CB63AA1DB930E21DB930E255CB63AAAAE5B1550000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      F81FF81F00000000E007E00700000000E007E00700000000C003C00300000000
      C003C00300000000C003C00300000000C003C00300000000C003C00300000000
      C003C00300000000E007E00700000000E007E00700000000F81FF81F00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
