object frmGeneralTester: TfrmGeneralTester
  Left = 0
  Top = 0
  Caption = 'GeneralTester'
  ClientHeight = 300
  ClientWidth = 635
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
  object imgTest: TImage
    Left = 560
    Top = 32
    Width = 41
    Height = 33
    Stretch = True
  end
  object btnTest: TButton
    Left = 536
    Top = 168
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
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
