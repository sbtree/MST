object frmProdDict: TfrmProdDict
  Left = 0
  Top = 0
  Caption = 'Product Dictionary'
  ClientHeight = 597
  ClientWidth = 846
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object trvProduct: TTreeView
    Left = 8
    Top = 8
    Width = 413
    Height = 457
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
  end
  object lsvConfig: TListView
    Left = 427
    Top = 8
    Width = 410
    Height = 457
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Value'
        Width = 250
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object btnLoad: TButton
    Left = 321
    Top = 481
    Width = 47
    Height = 21
    Caption = '&Load'
    TabOrder = 2
    OnClick = btnLoadClick
  end
  object btnOpenIni: TButton
    Left = 296
    Top = 481
    Width = 19
    Height = 21
    Caption = '...'
    TabOrder = 3
  end
  object txtConfigFile: TEdit
    Left = 8
    Top = 481
    Width = 287
    Height = 21
    TabOrder = 4
    Text = '..\config\ARS21xx.INI'
  end
end
