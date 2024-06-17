object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 562
  ClientWidth = 801
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
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
    Width = 801
    Height = 562
    ActivePage = SheetTR40
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 797
    ExplicitHeight = 561
    object SheetTR40: TTabSheet
      Caption = 'TR40'
      DesignSize = (
        793
        532)
      object GroupBox1: TGroupBox
        Left = 3
        Top = 3
        Width = 775
        Height = 54
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Origin IP...'
        TabOrder = 0
        ExplicitWidth = 771
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
        object ImageTR40PingLed: TImage
          Left = 369
          Top = 20
          Width = 24
          Height = 24
          Center = True
          Transparent = True
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
            TabOrder = 1
            Text = '192'
          end
        end
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 279
        Width = 775
        Height = 138
        Anchors = [akLeft, akTop, akRight]
        Caption = 'New IP...'
        TabOrder = 1
        ExplicitWidth = 771
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
          Visible = False
        end
        object ImageTR40SetIPLed: TImage
          Left = 369
          Top = 60
          Width = 24
          Height = 24
          Center = True
          Transparent = True
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
          Enabled = False
          TabOrder = 2
          Visible = False
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
          Top = 60
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
      object GroupBox5: TGroupBox
        Left = 3
        Top = 63
        Width = 775
        Height = 210
        Anchors = [akLeft, akTop, akRight]
        Caption = 'SSH Connection'
        TabOrder = 2
        ExplicitWidth = 771
        object Label10: TLabel
          Left = 3
          Top = 16
          Width = 112
          Height = 15
          Caption = 'Authentication Kind: '
        end
        object Label11: TLabel
          Left = 13
          Top = 63
          Width = 62
          Height = 15
          Caption = 'SSH Server: '
        end
        object Label12: TLabel
          Left = 13
          Top = 92
          Width = 52
          Height = 15
          Caption = 'SSH Port: '
        end
        object Label13: TLabel
          Left = 13
          Top = 121
          Width = 64
          Height = 15
          Caption = 'User Name: '
        end
        object Label14: TLabel
          Left = 13
          Top = 153
          Width = 53
          Height = 15
          Caption = 'Password:'
        end
        object Label15: TLabel
          Left = 392
          Top = 16
          Width = 187
          Height = 15
          Caption = 'Output(file list in home for testing):'
        end
        object Label17: TLabel
          Left = 392
          Top = 180
          Width = 34
          Height = 15
          Caption = 'Input: '
        end
        object LabelTR40ConnectionState: TLabel
          Left = 264
          Top = 181
          Width = 41
          Height = 15
          Caption = 'Ready...'
          Visible = False
        end
        object ImageTR40ConnectLed: TImage
          Left = 206
          Top = 176
          Width = 24
          Height = 24
          Center = True
          Transparent = True
        end
        object RadioButton1: TRadioButton
          Left = 6
          Top = 37
          Width = 91
          Height = 17
          Caption = 'Password'
          Checked = True
          Enabled = False
          TabOrder = 0
          TabStop = True
        end
        object RadioButton2: TRadioButton
          Left = 103
          Top = 37
          Width = 97
          Height = 17
          Caption = 'Public Key'
          Enabled = False
          TabOrder = 1
        end
        object RadioButton3: TRadioButton
          Left = 206
          Top = 37
          Width = 139
          Height = 17
          Caption = 'Keyboard-interactive'
          Enabled = False
          TabOrder = 2
        end
        object EditTR40HostIP: TEdit
          Left = 105
          Top = 60
          Width = 240
          Height = 23
          TabOrder = 3
          Text = 'localhost'
        end
        object EditTR40SshPort: TEdit
          Left = 105
          Top = 89
          Width = 240
          Height = 23
          TabOrder = 4
          Text = '22'
        end
        object EditTR40User: TEdit
          Left = 105
          Top = 118
          Width = 240
          Height = 23
          TabOrder = 5
          OnChange = EditTR40UserChange
        end
        object ButtonSshConnect: TButton
          Left = 27
          Top = 176
          Width = 173
          Height = 25
          Caption = 'Connect'
          ImageIndex = 0
          Images = IconList
          TabOrder = 6
          OnClick = ButtonSshConnectClick
        end
        object EditTR40Pass: TButtonedEdit
          Left = 105
          Top = 147
          Width = 240
          Height = 23
          Images = IconList
          PasswordChar = '*'
          RightButton.ImageIndex = 2
          RightButton.Visible = True
          TabOrder = 7
          OnRightButtonClick = EditTR40PassRightButtonClick
        end
        object MemoTR40Output: TMemo
          Left = 392
          Top = 37
          Width = 385
          Height = 132
          Enabled = False
          TabOrder = 8
        end
        object EditTR40Input: TEdit
          Left = 439
          Top = 176
          Width = 257
          Height = 23
          Enabled = False
          TabOrder = 9
        end
        object ButtonTR40Input: TButton
          Left = 702
          Top = 175
          Width = 75
          Height = 25
          Caption = 'Input'
          Enabled = False
          TabOrder = 10
          OnClick = ButtonTR40InputClick
        end
      end
    end
    object SheetIO40: TTabSheet
      Caption = 'IO40'
      ImageIndex = 1
      DesignSize = (
        793
        532)
      object GroupBox3: TGroupBox
        Left = 3
        Top = 3
        Width = 767
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
        object ButtonIO40Ping: TButton
          AlignWithMargins = True
          Left = 275
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
          OnClick = ButtonIO40PingClick
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
          object EditIO40CurrIPOctet4: TEdit
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
          object EditIO40CurrIPOctet3: TEdit
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
          object EditIO40CurrIPOctet2: TEdit
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
          object EditIO40CurrIPOctet1: TEdit
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
        Width = 767
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
          Visible = False
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
          object EditIO40NewIPOctet4: TEdit
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
          object EditIO40NewIPOctet3: TEdit
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
          object EditIO40NewIPOctet2: TEdit
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
          object EditIO40NewIPOctet1: TEdit
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
          object EditIO40NewMaskOctet4: TEdit
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
          object EditIO40NewMaskOctet3: TEdit
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
          object EditIO40NewMaskOctet2: TEdit
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
          object EditIO40NewMaskOctet1: TEdit
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
          Visible = False
          object EditIO40NewGateOctet4: TEdit
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
          object EditIO40NewGateOctet3: TEdit
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
          object EditIO40NewGateOctet2: TEdit
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
          object EditIO40NewGateOctet1: TEdit
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
        object ButtonIO40SetIP: TButton
          AlignWithMargins = True
          Left = 283
          Top = 59
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
          OnClick = ButtonIO40SetIPClick
        end
      end
    end
  end
  object IconList: TImageList
    Left = 8
    Top = 512
    Bitmap = {
      494C010104000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
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
      000000000000DADAFB25C7C7FA38B4B4F84BB4B4F84BD0D0FA2FE3E3FC1C0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DAF3DD25C7EDCB38B4E8BA4BB4E8BA4BD0F0D42FE3F6E51C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008E8EF5715555F0AA1D1DEBE21D1DEBE27272F28DAAAAF7550000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008EDC977155CB63AA1DB930E21DB930E272D37E8DAAE5B1550000
      0000000000000000000000000000000000000000000000000000D1D1D12EC4C4
      C43B000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008B8B
      F4743939EDC60000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF6262
      F19DB4B4F84B0000000000000000000000000000000000000000000000008BDB
      957439C24AC600B116FF00B116FF00B116FF00B116FF00B116FF00B116FF62CE
      6F9DB4E8BA4B0000000000000000000000000000000000000000CDCDCD321818
      18E7B8B8B8470000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      EDC60000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF1D1D
      EBE25555F0AA00000000000000000000000000000000000000000000000039C2
      4AC600B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF1DB9
      30E255CB63AA000000000000000000000000000000000000000000000000C8C8
      C837181818E7C7C7C7384E4E4EB1242424DB202020DF4B4B4BB49393936C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009191916E4D4D4DB2222222DD202020DF4B4B4BB49393936C0000
      00000000000000000000000000000000000000000000DADAFB258E8EF5710000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FFB4B4F84B000000000000000000000000DAF3DD258EDC977100B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FFB4E8BA4B00000000000000000000000000000000B3B3B34C1A1A
      1AE5A0A0A05F181818E7BABABA45393939C6141414EB6060609FF6F6F6096262
      629D1F1F1FE0BFBFBF4000000000000000000000000000000000B3B3B34C1B1B
      1BE46F6F6F90F6F6F6096060609F141414EB141414EB6060609FF6F6F6096262
      629D1F1F1FE0BFBFBF40000000000000000000000000C7C7FA385555F0AA0000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF8E8EF571000000000000000000000000C7EDCB3855CB63AA00B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF8EDC977100000000000000000000000000000000202020DFC6C6
      C63900000000C6C6C639181818E7BBBBBB44F5F5F50A595959A66161619E0000
      0000BABABA451C1C1CE3000000000000000000000000000000001F1F1FE0C6C6
      C639000000006161619E595959A6F5F5F50AF5F5F50A595959A66161619E0000
      0000BABABA451C1C1CE3000000000000000000000000B4B4F84B1D1DEBE20000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF6969F296000000000000000000000000B4E8BA4B1DB930E200B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF69D175960000000000000000000000007F7F7F80E7E7E7180000
      000000000000515151AEC6C6C639181818E700000000F5F5F50A131313EC0000
      000000000000E2E2E21D8181817E00000000000000008080807FE7E7E7180000
      000000000000131313ECF5F5F50A0000000000000000F5F5F50A131313EC0000
      000000000000E2E2E21D8181817E0000000000000000B4B4F84B1D1DEBE20000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FF6969F296000000000000000000000000B4E8BA4B1DB930E200B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FF69D17596000000000000000000000000E3E3E31C8E8E8E710000
      000000000000151515EAF8F8F80700000000181818E7BDBDBD423A3A3AC50000
      0000000000008C8C8C73E3E3E31C0000000000000000E3E3E31C8E8E8E710000
      000000000000151515EAF8F8F8070000000000000000F8F8F807151515EA0000
      0000000000008D8D8D72E5E5E51A0000000000000000D0D0FA2F7272F28D0000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FFA1A1F65E000000000000000000000000D0F0D42F72D37E8D00B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FFA1E2A95E000000000000000000000000000000002C2C2CD36F6F
      6F90FAFAFA05565656A966666699FAFAFA05C4C4C43B181818E7BDBDBD42FAFA
      FA056D6D6D922E2E2ED1000000000000000000000000000000002C2C2CD36F6F
      6F90FAFAFA05565656A966666699FAFAFA05FAFAFA0566666699575757A8FAFA
      FA056B6B6B942D2D2DD2000000000000000000000000E3E3FC1CAAAAF7550000
      E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000
      E9FF0000E9FFC7C7FA38000000000000000000000000E3F6E51CAAE5B15500B1
      16FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF00B1
      16FF00B116FFC7EDCB3800000000000000000000000000000000EFEFEF104F4F
      4FB0373737C8F1F1F10E505050AF1D1D1DE2333333CCBBBBBB44181818E74949
      49B6515151AEF1F1F10E00000000000000000000000000000000EFEFEF104F4F
      4FB0373737C8F1F1F10E505050AF1D1D1DE21D1D1DE2505050AFF1F1F10E3434
      34CB525252ADF2F2F20D00000000000000000000000000000000000000006262
      F19D1D1DEBE20000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF3F3F
      EEC08585F47A00000000000000000000000000000000000000000000000062CE
      6F9D1DB930E200B116FF00B116FF00B116FF00B116FF00B116FF00B116FF3FC4
      4FC085D98F7A0000000000000000000000000000000000000000000000000000
      0000000000009C9C9C634A4A4AB5181818E7131313EC474747B8C8C8C8371717
      17E8C0C0C03F0000000000000000000000000000000000000000000000000000
      0000000000009C9C9C634A4A4AB5181818E7141414EB464646B99A9A9A650000
      000000000000000000000000000000000000000000000000000000000000B4B4
      F84B5555F0AA0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF0000E9FF8585
      F47AE3E3FC1C000000000000000000000000000000000000000000000000B4E8
      BA4B55CB63AA00B116FF00B116FF00B116FF00B116FF00B116FF00B116FF85D9
      8F7AE3F6E51C0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C1C1
      C13E171717E8C8C8C83700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B4B4F84B8E8EF5716969F2966969F296A1A1F65EC7C7FA380000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000B4E8BA4B8EDC977169D1759669D17596A1E2A95EC7EDCB380000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C9C9C936D1D1D12E00000000000000000000000000000000000000000000
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
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFF81FF81FFFFFFFFF
      F81FF81FCFFFFFFFE007E007C7FFFFFFE007E007E01FF81F80038003C003C003
      80038003C813C8138003800398999999800380039919999980038003C003C003
      80038003C003C003E007E007F807F81FE007E007FFE3FFFFF81FF81FFFF3FFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end
