object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'FIDS Login'
  ClientHeight = 201
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 5
  Padding.Top = 5
  Padding.Right = 5
  Padding.Bottom = 5
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlContent: TPanel
    Left = 8
    Top = 8
    Width = 313
    Height = 199
    BevelOuter = bvNone
    TabOrder = 0
    object lblJobName: TLabel
      Left = 0
      Top = 0
      Width = 313
      Height = 16
      Align = alTop
      Alignment = taCenter
      Caption = '<Job Name>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 74
    end
    object EUsername: TLabeledEdit
      Left = 12
      Top = 48
      Width = 293
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = 'Username'
      TabOrder = 0
    end
    object EPassword: TLabeledEdit
      Left = 12
      Top = 95
      Width = 293
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      PasswordChar = '*'
      TabOrder = 1
    end
    object Button1: TButton
      Left = 150
      Top = 154
      Width = 77
      Height = 25
      Caption = 'Sign in'
      Default = True
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 231
      Top = 154
      Width = 76
      Height = 25
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = Button2Click
    end
    object CheckBox1: TCheckBox
      Left = 12
      Top = 122
      Width = 189
      Height = 17
      Caption = 'Remember my username'
      TabOrder = 4
    end
  end
  object tmrConnectionChecker: TTimer
    Enabled = False
    Interval = 250
    OnTimer = tmrConnectionCheckerTimer
    Left = 56
    Top = 152
  end
end
