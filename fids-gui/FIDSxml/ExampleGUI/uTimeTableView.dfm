object fTimeTable: TfTimeTable
  Left = 0
  Top = 0
  Caption = 'Time Table'
  ClientHeight = 500
  ClientWidth = 484
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vstTTV: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 484
    Height = 500
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoFreeOnCollapse]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    OnEditing = vstTTVEditing
    OnGetText = vstTTVGetText
    Columns = <
      item
        Position = 0
        Width = 80
        WideText = 'Flight'
      end
      item
        Position = 1
        Width = 100
        WideText = 'Kind'
      end
      item
        Position = 2
        WideText = 'Time'
      end
      item
        Position = 3
        Width = 200
        WideText = 'Ports'
      end
      item
        Position = 4
        WideText = 'Days'
      end>
  end
end
