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
    Left = 56
    Top = 40
    Width = 41
    Height = 33
    Stretch = True
  end
  object btnTest: TButton
    Left = 544
    Top = 256
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
end
