object frmProdTester: TfrmProdTester
  Left = 0
  Top = 0
  Caption = 'ProductTester'
  ClientHeight = 510
  ClientWidth = 858
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
    Height = 449
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = trvProductChange
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 473
    Width = 317
    Height = 21
    TabOrder = 1
    Text = 'ARS21xx.INI'
  end
  object btnOpenIni: TButton
    Left = 331
    Top = 473
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnOpenIniClick
  end
  object btnLoad: TButton
    Left = 366
    Top = 471
    Width = 55
    Height = 25
    Caption = '&Load'
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object lstVar: TListBox
    Left = 441
    Top = 8
    Width = 408
    Height = 449
    ItemHeight = 13
    TabOrder = 4
  end
  object chkShowAll: TCheckBox
    Left = 752
    Top = 475
    Width = 97
    Height = 17
    Caption = 'Show All Settings'
    TabOrder = 5
    OnClick = chkShowAllClick
  end
end
