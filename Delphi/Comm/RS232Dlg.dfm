object SerialDialog: TSerialDialog
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Serial Port Settings'
  ClientHeight = 176
  ClientWidth = 351
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblBaud: TLabel
    Left = 103
    Top = 14
    Width = 54
    Height = 13
    Caption = 'Baud Rate:'
  end
  object bvlCom: TBevel
    Left = 8
    Top = 8
    Width = 257
    Height = 160
  end
  object lblCom: TLabel
    Left = 16
    Top = 13
    Width = 35
    Height = 16
    Caption = 'COM?'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold, fsUnderline]
    ParentFont = False
  end
  object btnOK: TButton
    Left = 271
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 271
    Top = 38
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object cmbBaud: TComboBox
    Left = 164
    Top = 11
    Width = 91
    Height = 21
    TabOrder = 2
    Text = 'Baudrate'
    Items.Strings = (
      '110'
      '300'
      '600'
      '1200'
      '2400'
      '4800'
      '9600'
      '14400'
      '19200'
      '38400'
      '56000'
      '57600'
      '115200'
      '128000'
      '256000')
  end
  object rdgParity: TRadioGroup
    Left = 16
    Top = 38
    Width = 74
    Height = 120
    Caption = 'Parity'
    Items.Strings = (
      'None'
      'Odd'
      'Even'
      'Mark'
      'Space')
    TabOrder = 3
  end
  object rdgDataBits: TRadioGroup
    Left = 96
    Top = 38
    Width = 74
    Height = 59
    Caption = 'Data Bits'
    Items.Strings = (
      '7 bits'
      '8 bits')
    TabOrder = 4
  end
  object rdgStopBits: TRadioGroup
    Left = 96
    Top = 103
    Width = 74
    Height = 55
    Caption = 'Stop Bits'
    Items.Strings = (
      '1 bit'
      '2 bits')
    TabOrder = 5
  end
  object rdgFlowMode: TRadioGroup
    Left = 176
    Top = 38
    Width = 74
    Height = 120
    Caption = 'Flow Mode'
    Items.Strings = (
      'None'
      'RTS/CTS'
      'DTR/DSR'
      'Xon/Xoff')
    TabOrder = 6
  end
end
