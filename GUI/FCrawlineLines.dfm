object FCrawlineLinesAllocator: TFCrawlineLinesAllocator
  Left = 0
  Top = 0
  Caption = 'Crawling lines'
  ClientHeight = 336
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object VST: TVirtualStringTree
    Left = 0
    Top = 33
    Width = 515
    Height = 303
    Align = alClient
    Header.AutoSizeIndex = -1
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    OnDblClick = VSTDblClick
    OnGetText = VSTGetText
    OnPaintText = VSTPaintText
    OnGetNodeDataSize = VSTGetNodeDataSize
    ExplicitTop = 37
    Columns = <
      item
        Position = 0
        WideText = 'Label'
      end
      item
        Position = 1
        WideText = 'Status'
      end
      item
        Position = 2
        WideText = 'Message'
      end
      item
        Position = 3
        WideText = 'Blinking'
      end
      item
        Position = 4
        WideText = 'Crawling'
      end
      item
        Position = 5
        WideText = 'BG'
      end
      item
        Position = 6
        WideText = 'FG'
      end>
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 515
    Height = 33
    Align = alTop
    Anchors = [akTop, akRight]
    AutoSize = True
    BevelInner = bvNone
    BevelOuter = bvNone
    BevelKind = bkNone
    RowSize = 50
    TabOrder = 1
    object ToolBar1: TToolBar
      Left = 12
      Top = 2
      Width = 205
      Height = 29
      AutoSize = True
      ButtonHeight = 29
      ButtonWidth = 65
      EdgeInner = esNone
      EdgeOuter = esNone
      GradientEndColor = 13288646
      ShowCaptions = True
      TabOrder = 0
      Transparent = True
      Wrapable = False
      object ToolButton2: TToolButton
        Left = 0
        Top = 0
        Caption = 'Details'
        ImageIndex = 0
        OnClick = VSTDblClick
      end
      object ToolButton3: TToolButton
        Left = 65
        Top = 0
        Width = 8
        Caption = 'Delete'
        ImageIndex = 11
        Style = tbsDivider
      end
      object ToolButton6: TToolButton
        Left = 73
        Top = 0
        Caption = 'Activate'
        ImageIndex = 13
        OnClick = ToolButton6Click
      end
      object ToolButton5: TToolButton
        Left = 138
        Top = 0
        Caption = 'Deactivate'
        ImageIndex = 13
        OnClick = ToolButton5Click
      end
      object ToolButton4: TToolButton
        Left = 203
        Top = 0
        ImageIndex = 12
        Style = tbsDropDown
        Visible = False
      end
    end
  end
end
