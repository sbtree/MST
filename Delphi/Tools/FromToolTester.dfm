object frmToolTester: TfrmToolTester
  Left = 0
  Top = 0
  Caption = 'ToolTester'
  ClientHeight = 575
  ClientWidth = 799
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
    Left = 716
    Top = 542
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object memTest: TMemo
    Left = 8
    Top = 8
    Width = 783
    Height = 513
    Lines.Strings = (
      '')
    TabOrder = 1
  end
end
