object fDepartures: TfDepartures
  Left = 0
  Top = 0
  Caption = 'Departures'
  ClientHeight = 305
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sgFlights: TStringGrid
    Left = 0
    Top = 0
    Width = 643
    Height = 305
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
    OnDblClick = sgFlightsDblClick
    OnSelectCell = sgFlightsSelectCell
  end
  object MainMenu1: TMainMenu
    object AddFlight1: TMenuItem
      Caption = '&Add Flight'
      OnClick = AddFlight1Click
    end
    object AddCodeShare1: TMenuItem
      Caption = 'Add &Code Share'
      OnClick = AddCodeShare1Click
    end
    object DeleteFlight1: TMenuItem
      Caption = '&Delete Flight'
      OnClick = DeleteFlight1Click
    end
  end
end
