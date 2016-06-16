object frmCommTester: TfrmCommTester
  Left = 0
  Top = 0
  Caption = 'CommunicationTester'
  ClientHeight = 298
  ClientWidth = 460
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
  object lblCount: TLabel
    Left = 0
    Top = 277
    Width = 117
    Height = 13
    Caption = 'Sending: 0; Receiving: 0'
  end
  object btnRS232: TButton
    Left = 296
    Top = 268
    Width = 75
    Height = 25
    Caption = 'Test &RS232'
    TabOrder = 0
    OnClick = btnRS232Click
  end
  object btnCan: TButton
    Left = 382
    Top = 268
    Width = 75
    Height = 25
    Caption = 'Test &CAN'
    TabOrder = 1
    OnClick = btnCanClick
  end
  object memLog: TMemo
    Left = 0
    Top = 8
    Width = 457
    Height = 254
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btnCount: TButton
    Left = 240
    Top = 268
    Width = 50
    Height = 25
    Caption = 'Count'
    TabOrder = 3
    OnClick = btnCountClick
  end
end
