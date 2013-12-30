object fFeedMain: TfFeedMain
  Left = 0
  Top = 0
  Caption = 'FIDS Feed and Timetable handler'
  ClientHeight = 536
  ClientWidth = 682
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object memoLog: TMemo
    Left = 0
    Top = 0
    Width = 682
    Height = 536
    Align = alLeft
    Lines.Strings = (
      '')
    TabOrder = 0
  end
  object MainMenu1: TMainMenu
    Left = 272
    Top = 168
    object imetable1: TMenuItem
      Caption = '&Timetable'
      object Reset1: TMenuItem
        Caption = '&Reset'
        OnClick = Reset1Click
      end
    end
    object UDCs1: TMenuItem
      Caption = '&UDCs'
      OnClick = UDCs1Click
    end
  end
end
