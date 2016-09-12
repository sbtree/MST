object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 0
  Caption = 'DeviceTester'
  ClientHeight = 337
  ClientWidth = 671
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
  object btnFR: TButton
    Left = 588
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Flash Runner'
    TabOrder = 0
    OnClick = btnFRClick
  end
  object btnCreateTree: TButton
    Left = 507
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Create Tree'
    TabOrder = 1
  end
  object btnTest: TButton
    Left = 426
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 2
    OnClick = btnTestClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 655
    Height = 281
    ScrollBars = ssBoth
    TabOrder = 3
  end
end
