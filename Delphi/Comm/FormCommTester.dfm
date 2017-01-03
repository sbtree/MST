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
  object memLog: TMemo
    Left = 4
    Top = 8
    Width = 580
    Height = 254
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object btnSend: TButton
    Left = 428
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object btnRecv: TButton
    Left = 509
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Receive'
    TabOrder = 2
    OnClick = btnRecvClick
  end
  object cmbConf: TComboBox
    Left = 4
    Top = 269
    Width = 317
    Height = 21
    TabOrder = 3
    Text = 
      'Port:6|Baudrate:9600|PARITY:None|DATABITS:8|STOPBITS:1|FLOWCONTR' +
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
    TabOrder = 4
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
    TabOrder = 5
  end
  object chkLF: TCheckBox
    Left = 546
    Top = 271
    Width = 38
    Height = 17
    Caption = '+LF'
    TabOrder = 6
  end
  object btnConnect: TButton
    Left = 4
    Top = 296
    Width = 75
    Height = 25
    Caption = '&Connect'
    TabOrder = 7
    OnClick = btnConnectClick
  end
  object tmrUpdate: TTimer
    Interval = 500
    Left = 16
    Top = 24
  end
end
