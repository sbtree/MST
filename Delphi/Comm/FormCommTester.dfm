object frmCommTester: TfrmCommTester
  Left = 0
  Top = 0
  Caption = 'CommunicationTester'
  ClientHeight = 329
  ClientWidth = 590
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
    Left = 247
    Top = 303
    Width = 60
    Height = 13
    Caption = 'Tx(0); Rx(0)'
  end
  object btnRS232: TButton
    Left = 4
    Top = 296
    Width = 75
    Height = 25
    Caption = '&RS232'
    TabOrder = 0
    OnClick = btnRS232Click
  end
  object btnCan: TButton
    Left = 85
    Top = 296
    Width = 75
    Height = 25
    Caption = '&CAN'
    TabOrder = 1
    OnClick = btnCanClick
  end
  object memLog: TMemo
    Left = 4
    Top = 8
    Width = 580
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
  object btnSend: TButton
    Left = 428
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 4
    OnClick = btnSendClick
  end
  object btnRecv: TButton
    Left = 509
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Receive'
    TabOrder = 5
    OnClick = btnRecvClick
  end
  object cmbConf: TComboBox
    Left = 4
    Top = 269
    Width = 317
    Height = 21
    TabOrder = 6
    Text = 
      'Port:1|Baudrate:9600|PARITY:None|DATABITS:8|STOPBITS:1|FLOWCONTR' +
      'OL:NONE'
    Items.Strings = (
      
        'Port:1|Baudrate:9600|PARITY:None|DATABITS:8|STOPBITS:1|FLOWCONTR' +
        'OL:NONE'
      'HWT:USB1CH|PCANDLL:PCAN_USB.dll|baudrate:1M|CANVER:STD'
      'VID:$1B97|PID:$2|PSN:1234')
  end
  object cmbSending: TComboBox
    Left = 368
    Top = 269
    Width = 121
    Height = 21
    TabOrder = 7
    Text = 'OR:1:0008'
    Items.Strings = (
      'OR:1:0008'
      '664:40842000')
  end
  object chkCr: TCheckBox
    Left = 495
    Top = 271
    Width = 42
    Height = 17
    Caption = '+CR'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
  object chkLF: TCheckBox
    Left = 546
    Top = 271
    Width = 38
    Height = 17
    Caption = '+LF'
    TabOrder = 9
  end
  object tmrUpdate: TTimer
    Interval = 500
    OnTimer = tmrUpdateTimer
    Left = 16
    Top = 24
  end
end
