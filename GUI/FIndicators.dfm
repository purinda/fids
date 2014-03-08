object frmManageIndicators: TfrmManageIndicators
  Left = 0
  Top = 0
  Caption = 'Indicator Management'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VST: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 635
    Height = 300
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    OnChange = VSTChange
    OnGetText = VSTGetText
    OnPaintText = VSTPaintText
    OnGetNodeDataSize = VSTGetNodeDataSize
    Columns = <>
  end
  object mnuMain: TMainMenu
    Left = 472
    Top = 120
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Cut1: TMenuItem
        Caption = 'Cut'
        ShortCut = 16472
      end
      object Copy1: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
      end
      object Past1: TMenuItem
        Caption = 'Paste'
        ShortCut = 16470
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        ShortCut = 46
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Filter1: TMenuItem
        Caption = 'Filter'
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Refresh1: TMenuItem
        Caption = '&Refresh'
        ShortCut = 116
      end
    end
  end
  object mnuContext: TPopupMenu
    Left = 544
    Top = 120
  end
end
