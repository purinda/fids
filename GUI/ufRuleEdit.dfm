object fRuleEdit: TfRuleEdit
  Left = 0
  Top = 0
  Caption = 'Rule Edit'
  ClientHeight = 389
  ClientWidth = 585
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
    Left = 22
    Top = 173
    Width = 51
    Height = 13
    AutoSize = False
    Caption = 'Rule Name'
  end
  object Label2: TLabel
    Left = 22
    Top = 146
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Kind'
  end
  object Label3: TLabel
    Left = 22
    Top = 200
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Time'
  end
  object Label4: TLabel
    Left = 22
    Top = 227
    Width = 50
    Height = 13
    AutoSize = False
    Caption = 'Start Date'
  end
  object Label5: TLabel
    Left = 22
    Top = 254
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'End Date'
  end
  object Label6: TLabel
    Left = 22
    Top = 281
    Width = 52
    Height = 13
    AutoSize = False
    Caption = 'Exceptions'
    Visible = False
  end
  object Label7: TLabel
    Left = 22
    Top = 38
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Ports'
  end
  object Label8: TLabel
    Left = 22
    Top = 65
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Gates'
  end
  object Label9: TLabel
    Left = 22
    Top = 92
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Bays'
  end
  object Label10: TLabel
    Left = 22
    Top = 119
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'CheckIns'
  end
  object lbFlightNumber: TLabel
    Left = 22
    Top = 8
    Width = 44
    Height = 13
    AutoSize = False
    Caption = 'Flight'
  end
  object lbFlightNum: TLabel
    Left = 87
    Top = 8
    Width = 55
    Height = 13
    Caption = 'lbFlightNum'
  end
  object ebRuleName: TEdit
    Left = 87
    Top = 170
    Width = 177
    Height = 21
    TabOrder = 0
    OnChange = ebRuleNameChange
  end
  object cmbFKind: TComboBox
    Left = 87
    Top = 143
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object ebExcept: TEdit
    Left = 87
    Top = 278
    Width = 177
    Height = 21
    TabOrder = 2
    Visible = False
  end
  object gbDays: TGroupBox
    Left = 287
    Top = 15
    Width = 281
    Height = 41
    Caption = 'Days'
    TabOrder = 3
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
    Left = 87
    Top = 35
    Width = 177
    Height = 21
    TabOrder = 4
  end
  object ebGates: TEdit
    Left = 87
    Top = 62
    Width = 177
    Height = 21
    TabOrder = 5
  end
  object ebBays: TEdit
    Left = 87
    Top = 89
    Width = 177
    Height = 21
    TabOrder = 6
  end
  object ebCheckIns: TEdit
    Left = 87
    Top = 116
    Width = 177
    Height = 21
    TabOrder = 7
  end
  object gbDaysExcept: TGroupBox
    Left = 287
    Top = 72
    Width = 281
    Height = 41
    Caption = 'Days Except'
    TabOrder = 8
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
  object btDelete: TButton
    Left = 22
    Top = 317
    Width = 75
    Height = 25
    Caption = 'DELETE'
    TabOrder = 9
    Visible = False
    OnClick = btDeleteClick
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 358
    Width = 585
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    object btnClose: TButton
      AlignWithMargins = True
      Left = 507
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Close'
      ModalResult = 11
      TabOrder = 0
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 426
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = Button3Click
    end
  end
  object ebStart: TDateTimePicker
    Left = 87
    Top = 224
    Width = 177
    Height = 21
    Date = 40662.431299444440000000
    Time = 40662.431299444440000000
    TabOrder = 11
  end
  object ebEnd: TDateTimePicker
    Left = 87
    Top = 251
    Width = 177
    Height = 21
    Date = 40662.431299444440000000
    Time = 40662.431299444440000000
    TabOrder = 12
  end
  object GroupBox1: TGroupBox
    Left = 287
    Top = 123
    Width = 281
    Height = 214
    Caption = 'Exceptions'
    TabOrder = 13
    object Label11: TLabel
      Left = 14
      Top = 23
      Width = 62
      Height = 13
      Caption = 'Exclude from'
    end
    object Label12: TLabel
      Left = 14
      Top = 47
      Width = 21
      Height = 13
      Caption = 'Until'
    end
    object Label13: TLabel
      Left = 14
      Top = 69
      Width = 21
      Height = 13
      Caption = 'Sets'
    end
    object lstExclusions: TListBox
      Left = 88
      Top = 69
      Width = 181
      Height = 60
      ItemHeight = 13
      TabOrder = 0
    end
    object btnAdd: TButton
      Left = 238
      Top = 131
      Width = 32
      Height = 23
      Caption = '+'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRem: TButton
      Left = 200
      Top = 132
      Width = 32
      Height = 23
      Caption = '-'
      TabOrder = 2
      OnClick = btnRemClick
    end
    object dtExFrom: TDateTimePicker
      Left = 89
      Top = 18
      Width = 114
      Height = 21
      Date = 40662.431299444440000000
      Time = 40662.431299444440000000
      TabOrder = 3
    end
    object dtUntill: TDateTimePicker
      Left = 89
      Top = 42
      Width = 114
      Height = 21
      Date = 40662.431299444440000000
      Time = 40662.431299444440000000
      TabOrder = 4
    end
  end
  object ebTime: TDateTimePicker
    Left = 87
    Top = 197
    Width = 177
    Height = 21
    Date = 40662.431299444440000000
    Time = 40662.431299444440000000
    Kind = dtkTime
    TabOrder = 14
  end
end
