object frmVTCheckList: TfrmVTCheckList
  Left = 403
  Top = 173
  Caption = 'Generic CheckListbox selection Form (no node data used)'
  ClientHeight = 308
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 385
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    Caption = 'Panel1'
    TabOrder = 0
    object VT: TVirtualStringTree
      Left = 7
      Top = 7
      Width = 371
      Height = 263
      Align = alClient
      CheckImageKind = ckFlat
      Header.AutoSizeIndex = 0
      Header.DefaultHeight = 17
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'MS Shell Dlg 2'
      Header.Font.Style = []
      Header.MainColumn = -1
      Header.Options = [hoColumnResize, hoDrag]
      HintAnimation = hatNone
      RootNodeCount = 7
      TabOrder = 0
      TreeOptions.PaintOptions = [toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect, toCenterScrollIntoView]
      OnGetNodeDataSize = VTGetNodeDataSize
      OnInitNode = VTInitNode
      Columns = <>
    end
    object panBase: TPanel
      Left = 7
      Top = 270
      Width = 371
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btnOk: TButton
        Left = 225
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Ok'
        TabOrder = 0
        OnClick = btnOkClick
      end
      object btnCancel: TButton
        Left = 304
        Top = 6
        Width = 75
        Height = 25
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
