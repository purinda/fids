object fDataTree: TfDataTree
  Left = 0
  Top = 0
  Caption = 'Data Tree'
  ClientHeight = 304
  ClientWidth = 643
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
  object vstData: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 643
    Height = 304
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 0
    OnExpanding = vstDataExpanding
    OnGetText = vstDataGetText
    OnInitNode = vstDataInitNode
    Columns = <>
  end
end
