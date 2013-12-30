object fRuleEdit: TfRuleEdit
  Left = 0
  Top = 0
  Caption = 'Rule Edit'
  ClientHeight = 363
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 63
    Top = 49
    Width = 51
    Height = 13
    Alignment = taRightJustify
    Caption = 'Rule Name'
  end
  object Label2: TLabel
    Left = 94
    Top = 22
    Width = 20
    Height = 13
    Alignment = taRightJustify
    Caption = 'Kind'
  end
  object Label3: TLabel
    Left = 92
    Top = 76
    Width = 22
    Height = 13
    Alignment = taRightJustify
    Caption = 'Time'
  end
  object Label4: TLabel
    Left = 64
    Top = 103
    Width = 50
    Height = 13
    Alignment = taRightJustify
    Caption = 'Start Date'
  end
  object Label5: TLabel
    Left = 70
    Top = 130
    Width = 44
    Height = 13
    Alignment = taRightJustify
    Caption = 'End Date'
  end
  object Label6: TLabel
    Left = 62
    Top = 157
    Width = 52
    Height = 13
    Alignment = taRightJustify
    Caption = 'Exceptions'
  end
  object Label7: TLabel
    Left = 89
    Top = 241
    Width = 25
    Height = 13
    Alignment = taRightJustify
    Caption = 'Ports'
  end
  object Label8: TLabel
    Left = 86
    Top = 268
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = 'Gates'
  end
  object Label9: TLabel
    Left = 91
    Top = 295
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'Bays'
  end
  object Label10: TLabel
    Left = 70
    Top = 322
    Width = 44
    Height = 13
    Alignment = taRightJustify
    Caption = 'CheckIns'
  end
  object lbFlightNumber: TLabel
    Left = 48
    Top = 211
    Width = 66
    Height = 13
    Alignment = taRightJustify
    Caption = 'Flight Number'
  end
  object lbFlightNum: TLabel
    Left = 120
    Top = 211
    Width = 55
    Height = 13
    Caption = 'lbFlightNum'
  end
  object ebRuleName: TEdit
    Left = 120
    Top = 46
    Width = 177
    Height = 21
    TabOrder = 0
    Text = 'ebRuleName'
    OnChange = ebRuleNameChange
  end
  object ComboBox1: TComboBox
    Left = 120
    Top = 19
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object ebTime: TEdit
    Left = 120
    Top = 73
    Width = 177
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object ebStart: TEdit
    Left = 120
    Top = 100
    Width = 177
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
  object ebEnd: TEdit
    Left = 120
    Top = 127
    Width = 177
    Height = 21
    TabOrder = 4
    Text = 'Edit1'
  end
  object ebExcept: TEdit
    Left = 120
    Top = 154
    Width = 177
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
  end
  object gbDays: TGroupBox
    Left = 352
    Top = 97
    Width = 281
    Height = 41
    Caption = 'Days'
    TabOrder = 6
    object RadioButton1: TCheckBox
      Tag = 1
      Left = 8
      Top = 21
      Width = 33
      Height = 17
      Caption = 'M'
      TabOrder = 0
    end
    object RadioButton2: TCheckBox
      Tag = 2
      Left = 47
      Top = 21
      Width = 34
      Height = 17
      Caption = 'Tu'
      TabOrder = 1
    end
    object RadioButton3: TCheckBox
      Tag = 4
      Left = 87
      Top = 21
      Width = 34
      Height = 17
      Caption = 'W'
      TabOrder = 2
    end
    object RadioButton4: TCheckBox
      Tag = 8
      Left = 127
      Top = 21
      Width = 33
      Height = 17
      Caption = 'Th'
      TabOrder = 3
    end
    object RadioButton5: TCheckBox
      Tag = 16
      Left = 166
      Top = 21
      Width = 34
      Height = 17
      Caption = 'F'
      TabOrder = 4
    end
    object RadioButton6: TCheckBox
      Tag = 32
      Left = 197
      Top = 21
      Width = 33
      Height = 17
      Caption = 'Sa'
      TabOrder = 5
    end
    object RadioButton7: TCheckBox
      Tag = 64
      Left = 236
      Top = 21
      Width = 34
      Height = 17
      Caption = 'Su'
      TabOrder = 6
    end
  end
  object ebPorts: TEdit
    Left = 120
    Top = 238
    Width = 177
    Height = 21
    TabOrder = 7
    Text = 'Edit1'
  end
  object ebGates: TEdit
    Left = 120
    Top = 265
    Width = 177
    Height = 21
    TabOrder = 8
    Text = 'Edit1'
  end
  object ebBays: TEdit
    Left = 120
    Top = 292
    Width = 177
    Height = 21
    TabOrder = 9
    Text = 'Edit1'
  end
  object ebCheckIns: TEdit
    Left = 120
    Top = 319
    Width = 177
    Height = 21
    TabOrder = 10
    Text = 'Edit1'
  end
  object btOK: TButton
    Left = 549
    Top = 317
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 11
    OnClick = btOKClick
  end
  object gbDaysExcept: TGroupBox
    Left = 352
    Top = 144
    Width = 281
    Height = 41
    Caption = 'Days Except'
    TabOrder = 12
    object CheckBox2: TCheckBox
      Tag = 1
      Left = 8
      Top = 21
      Width = 33
      Height = 17
      Caption = 'M'
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Tag = 2
      Left = 47
      Top = 21
      Width = 34
      Height = 17
      Caption = 'Tu'
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Tag = 4
      Left = 87
      Top = 21
      Width = 34
      Height = 17
      Caption = 'W'
      TabOrder = 2
    end
    object CheckBox5: TCheckBox
      Tag = 8
      Left = 127
      Top = 21
      Width = 33
      Height = 17
      Caption = 'Th'
      TabOrder = 3
    end
    object CheckBox6: TCheckBox
      Tag = 16
      Left = 166
      Top = 21
      Width = 34
      Height = 17
      Caption = 'F'
      TabOrder = 4
    end
    object CheckBox7: TCheckBox
      Tag = 32
      Left = 197
      Top = 21
      Width = 33
      Height = 17
      Caption = 'Sa'
      TabOrder = 5
    end
    object CheckBox8: TCheckBox
      Tag = 64
      Left = 236
      Top = 21
      Width = 34
      Height = 17
      Caption = 'Su'
      TabOrder = 6
    end
  end
  object btNew: TButton
    Left = 352
    Top = 268
    Width = 75
    Height = 25
    Caption = 'New'
    TabOrder = 13
    OnClick = btNewClick
  end
  object btDelete: TButton
    Left = 352
    Top = 317
    Width = 75
    Height = 25
    Caption = 'DELETE'
    TabOrder = 14
    OnClick = btDeleteClick
  end
  object Button1: TButton
    Left = 456
    Top = 268
    Width = 89
    Height = 25
    Caption = 'add Code Share'
    TabOrder = 15
  end
end
