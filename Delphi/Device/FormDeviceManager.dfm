object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 0
  Caption = 'DeviceTester'
  ClientHeight = 301
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnFR: TButton
    Left = 472
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Flash Runner'
    TabOrder = 0
    OnClick = btnFRClick
  end
  object trvDevices: TTreeView
    Left = 8
    Top = 8
    Width = 441
    Height = 285
    Indent = 19
    ReadOnly = True
    TabOrder = 1
  end
  object btnCreateTree: TButton
    Left = 472
    Top = 268
    Width = 75
    Height = 25
    Caption = 'Create Tree'
    TabOrder = 2
    OnClick = btnCreateTreeClick
  end
end
