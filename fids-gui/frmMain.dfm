object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 386
  ClientWidth = 791
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object table: TStringGrid
    Left = 0
    Top = 39
    Width = 791
    Height = 347
    Align = alBottom
    BevelOuter = bvNone
    BorderStyle = bsNone
    ColCount = 12
    FixedCols = 0
    RowCount = 100
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnDrawCell = tableDrawCell
    OnSelectCell = tableSelectCell
    ColWidths = (
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64
      64)
  end
  object Button1: TButton
    Left = 166
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Change Flight Status'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 85
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 4
    Top = 8
    Width = 75
    Height = 25
    Caption = 'New flight'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 686
    Top = 8
    Width = 97
    Height = 25
    Caption = 'is Connected?'
    TabOrder = 4
    OnClick = Button4Click
  end
end
