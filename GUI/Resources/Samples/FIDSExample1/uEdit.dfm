object fEdit: TfEdit
  Left = 0
  Top = 0
  Caption = 'Edit'
  ClientHeight = 75
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 297
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object btCancel: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btCancelClick
  end
  object btOK: TButton
    Left = 224
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = btOKClick
  end
end
