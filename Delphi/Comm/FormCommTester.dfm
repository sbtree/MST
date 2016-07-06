object frmCommTester: TfrmCommTester
  Left = 0
  Top = 0
  Caption = 'CommunicationTester'
  ClientHeight = 329
  ClientWidth = 510
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
    Left = 4
    Top = 268
    Width = 114
    Height = 13
    Caption = 'Messages: Tx(0); Rx(0)'
  end
  object btnRS232: TButton
    Left = 4
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Test &RS232'
    TabOrder = 0
    OnClick = btnRS232Click
  end
  object btnCan: TButton
    Left = 85
    Top = 296
    Width = 75
    Height = 25
    Caption = 'Test &CAN'
    TabOrder = 1
    OnClick = btnCanClick
  end
  object memLog: TMemo
    Left = 4
    Top = 8
    Width = 501
    Height = 254
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object btnUSB: TButton
    Left = 166
    Top = 296
    Width = 75
    Height = 25
    Caption = '&USB'
    TabOrder = 3
    OnClick = btnUSBClick
  end
  object txtSending: TEdit
    Left = 349
    Top = 269
    Width = 156
    Height = 21
    TabOrder = 4
    Text = 'OR:1:0008'
  end
  object btnSend: TButton
    Left = 349
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 5
    OnClick = btnSendClick
  end
  object btnRecv: TButton
    Left = 430
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Receive'
    TabOrder = 6
    OnClick = btnRecvClick
  end
  object tmrUpdate: TTimer
    Interval = 500
    OnTimer = tmrUpdateTimer
    Left = 16
    Top = 24
  end
end
