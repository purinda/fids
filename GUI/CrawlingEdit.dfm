object FCrawlingLineEdit: TFCrawlingLineEdit
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Crawling lines properties'
  ClientHeight = 271
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 10
    Height = 13
    Caption = 'IP'
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 40
    Height = 13
    Caption = 'Location'
  end
  object Label3: TLabel
    Left = 8
    Top = 62
    Width = 42
    Height = 13
    Caption = 'Message'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 29
    Height = 13
    Caption = 'Group'
  end
  object Label5: TLabel
    Left = 208
    Top = 104
    Width = 31
    Height = 13
    Caption = 'Status'
  end
  object Label6: TLabel
    Left = 8
    Top = 131
    Width = 35
    Height = 13
    Caption = 'Blinking'
  end
  object Label7: TLabel
    Left = 208
    Top = 131
    Width = 41
    Height = 13
    Caption = 'Crawling'
  end
  object Label8: TLabel
    Left = 8
    Top = 158
    Width = 43
    Height = 13
    Caption = 'MsgType'
  end
  object Label9: TLabel
    Left = 8
    Top = 202
    Width = 56
    Height = 13
    Caption = 'Background'
  end
  object Label10: TLabel
    Left = 208
    Top = 202
    Width = 56
    Height = 13
    Caption = 'Foreground'
  end
  object txtIP: TEdit
    Left = 80
    Top = 5
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object cmbLocation: TComboBox
    Left = 80
    Top = 32
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object cmbGroup: TComboBox
    Left = 80
    Top = 101
    Width = 113
    Height = 21
    TabOrder = 2
  end
  object cmbStatus: TComboBox
    Left = 280
    Top = 101
    Width = 113
    Height = 21
    TabOrder = 3
  end
  object cmbBlinking: TComboBox
    Left = 80
    Top = 128
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 4
    Text = 'No'
    Items.Strings = (
      'Yes'
      'No')
  end
  object cmbCrawling: TComboBox
    Left = 280
    Top = 128
    Width = 113
    Height = 21
    Style = csDropDownList
    ItemIndex = 1
    TabOrder = 5
    Text = 'No'
    Items.Strings = (
      'Yes'
      'No')
  end
  object cmbMsgType: TComboBox
    Left = 80
    Top = 155
    Width = 113
    Height = 21
    TabOrder = 6
  end
  object cmbBg: TComboBox
    Left = 80
    Top = 199
    Width = 113
    Height = 21
    TabOrder = 7
  end
  object cmbFg: TComboBox
    Left = 280
    Top = 199
    Width = 113
    Height = 21
    TabOrder = 8
  end
  object txtMsg: TEdit
    Left = 80
    Top = 59
    Width = 313
    Height = 21
    TabOrder = 9
  end
  object Button1: TButton
    Left = 322
    Top = 238
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 10
  end
  object Button2: TButton
    Left = 241
    Top = 238
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 11
    OnClick = Button2Click
  end
end
