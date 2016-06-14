object frmCommTester: TfrmCommTester
  Left = 0
  Top = 0
  Caption = 'CommunicationTester'
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
  object btnRS232: TButton
    Left = 456
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Test &RS232'
    TabOrder = 0
    OnClick = btnRS232Click
  end
  object btnCan: TButton
    Left = 456
    Top = 233
    Width = 75
    Height = 25
    Caption = 'Test &CAN'
    TabOrder = 1
    OnClick = btnCanClick
  end
end
