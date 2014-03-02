object frmSearch: TfrmSearch
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 185
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 178
    Top = 8
    Width = 29
    Height = 13
    Caption = 'Under'
  end
  object Label2: TLabel
    Left = 9
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Look for'
  end
  object Panel1: TPanel
    Left = 0
    Top = 146
    Width = 340
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 167
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Find ...'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 256
      Top = 5
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 11
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object cmbFieldList: TComboBox
    Left = 175
    Top = 24
    Width = 157
    Height = 21
    Style = csDropDownList
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 59
    Width = 324
    Height = 78
    Caption = 'Options'
    TabOrder = 2
    object chkCase: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
    end
    object chkWholeWords: TCheckBox
      Left = 16
      Top = 47
      Width = 113
      Height = 17
      Caption = 'Whole words only'
      TabOrder = 1
    end
  end
  object cmbSearch: TComboBox
    Left = 9
    Top = 24
    Width = 157
    Height = 21
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 3
  end
end
