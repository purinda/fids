object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 215
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
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblFidsStatus: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 313
    Height = 16
    Align = alTop
    Alignment = taCenter
    Caption = 'FIDS Host is unavailable'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    ExplicitWidth = 153
  end
  object pnlContent: TPanel
    Left = 8
    Top = 8
    Width = 313
    Height = 201
    BevelOuter = bvNone
    TabOrder = 0
    object EUsername: TLabeledEdit
      Left = 12
      Top = 96
      Width = 295
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = 'Username'
      TabOrder = 0
    end
    object EPassword: TLabeledEdit
      Left = 12
      Top = 143
      Width = 295
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      PasswordChar = '*'
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 313
      Height = 57
      Align = alTop
      Caption = 'DII LOGO GOES HERE'
      TabOrder = 2
    end
    object Button1: TButton
      Left = 150
      Top = 170
      Width = 77
      Height = 25
      Caption = 'Sign in'
      Default = True
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 231
      Top = 170
      Width = 76
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = Button2Click
    end
  end
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 16
    Top = 168
  end
end
