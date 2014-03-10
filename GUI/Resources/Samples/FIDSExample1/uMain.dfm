object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 559
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object sgFlights: TStringGrid
    Left = 0
    Top = 0
    Width = 643
    Height = 559
    Align = alClient
    RowCount = 20
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
    OnClick = sgFlightsClick
    OnDblClick = sgFlightsDblClick
    OnSelectCell = sgFlightsSelectCell
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object Departures1: TMenuItem
      Caption = 'Departures'
      OnClick = Departures1Click
    end
    object Gates1: TMenuItem
      Caption = 'Gates'
      OnClick = Gates1Click
    end
    object Bays1: TMenuItem
      Caption = 'Bays'
      OnClick = Bays1Click
    end
    object CheckIns1: TMenuItem
      Caption = 'CheckIns'
      OnClick = CheckIns1Click
    end
  end
end
