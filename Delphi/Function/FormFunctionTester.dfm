object FormFTMain: TFormFTMain
  Left = 0
  Top = 0
  Caption = 'FunctionTester'
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
  object btnTest: TButton
    Left = 479
    Top = 268
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 546
    Height = 241
    TabOrder = 1
  end
end
