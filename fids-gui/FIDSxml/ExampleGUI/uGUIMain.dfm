object fGUIMain: TfGUIMain
  Left = 0
  Top = 0
  Caption = 'Example FIDS GUI'
  ClientHeight = 405
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TMemo
    Left = 0
    Top = 8
    Width = 682
    Height = 397
    Align = alBottom
    Lines.Strings = (
      '')
    TabOrder = 0
    OnChange = memoLogChange
  end
  object MainMenu1: TMainMenu
    Left = 560
    Top = 352
    object Departures1: TMenuItem
      Caption = '&Departures'
      OnClick = Departures1Click
    end
    object imeTable1: TMenuItem
      Caption = '&Time Table'
      OnClick = imeTable1Click
    end
    object DataTree1: TMenuItem
      Caption = 'D&ata Tree'
      OnClick = DataTree1Click
    end
  end
end
