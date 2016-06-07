object frmProdTester: TfrmProdTester
  Left = 0
  Top = 0
  Caption = 'ProductTester'
  ClientHeight = 504
  ClientWidth = 631
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
  object btnTest: TButton
    Left = 568
    Top = 471
    Width = 55
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object trvProduct: TTreeView
    Left = 8
    Top = 8
    Width = 615
    Height = 441
    Indent = 19
    TabOrder = 1
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 473
    Width = 405
    Height = 21
    TabOrder = 2
    Text = 'ARS21xx.INI'
  end
  object btnOpenIni: TButton
    Left = 419
    Top = 473
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnOpenIniClick
  end
  object btnLoad: TButton
    Left = 507
    Top = 471
    Width = 55
    Height = 25
    Caption = '&Load'
    TabOrder = 4
    OnClick = btnLoadClick
  end
end
