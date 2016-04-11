object FrmStarterMain: TFrmStarterMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Test Starter'
  ClientHeight = 364
  ClientWidth = 537
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object sgdTestStations: TStringGrid
    Left = 8
    Top = 8
    Width = 519
    Height = 305
    ColCount = 3
    DefaultColWidth = 128
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goTabs, goRowSelect]
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 352
    Top = 329
    Width = 75
    Height = 25
    Caption = '&Start'
    Default = True
    TabOrder = 1
    OnClick = btnStartClick
  end
  object btnCancel: TButton
    Left = 452
    Top = 329
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 2
  end
end
