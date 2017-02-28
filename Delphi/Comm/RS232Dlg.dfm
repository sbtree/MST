object SerialDialog: TSerialDialog
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Serial Port Settings'
  ClientHeight = 195
  ClientWidth = 353
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
    Height = 179
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
  object grpParity: TGroupBox
    Left = 16
    Top = 38
    Width = 81
    Height = 140
    Caption = 'Parity'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    object rdbNone: TRadioButton
      Left = 13
      Top = 18
      Width = 49
      Height = 17
      Caption = 'None'
      TabOrder = 0
    end
    object rdbOdd: TRadioButton
      Left = 13
      Top = 41
      Width = 49
      Height = 17
      Caption = 'Odd'
      TabOrder = 1
    end
    object rdbEven: TRadioButton
      Left = 13
      Top = 64
      Width = 49
      Height = 17
      Caption = 'Even'
      TabOrder = 2
    end
    object rdbMark: TRadioButton
      Left = 13
      Top = 87
      Width = 49
      Height = 17
      Caption = 'Mark'
      TabOrder = 3
    end
    object rdbSpace: TRadioButton
      Left = 13
      Top = 110
      Width = 49
      Height = 17
      Caption = 'Space'
      TabOrder = 4
    end
  end
  object cmbBaud: TComboBox
    Left = 164
    Top = 11
    Width = 91
    Height = 21
    TabOrder = 3
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
  object grpDataBits: TGroupBox
    Left = 103
    Top = 38
    Width = 73
    Height = 67
    Caption = 'Data Bits'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 4
    object rdbD8Bit: TRadioButton
      Left = 10
      Top = 18
      Width = 49
      Height = 17
      Caption = '8 bits'
      TabOrder = 0
    end
    object rdbD7Bit: TRadioButton
      Left = 10
      Top = 37
      Width = 49
      Height = 17
      Caption = '7 bits'
      TabOrder = 1
    end
  end
  object grpStopBits: TGroupBox
    Left = 182
    Top = 38
    Width = 73
    Height = 67
    Caption = 'Stop Bits'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 5
    object rdbStopBit1: TRadioButton
      Left = 10
      Top = 18
      Width = 49
      Height = 17
      Caption = '1 bit'
      TabOrder = 0
    end
    object rdbStopBit2: TRadioButton
      Left = 10
      Top = 37
      Width = 49
      Height = 17
      Caption = '2 bits'
      TabOrder = 1
    end
  end
  object grpFlowMode: TGroupBox
    Left = 103
    Top = 111
    Width = 152
    Height = 67
    Caption = 'Flow Mode'
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 6
    object rdbFcNone: TRadioButton
      Left = 10
      Top = 18
      Width = 49
      Height = 17
      Caption = 'None'
      TabOrder = 0
    end
    object rdbFcRtsCts: TRadioButton
      Left = 10
      Top = 37
      Width = 63
      Height = 17
      Caption = 'RTS/CTS'
      TabOrder = 1
    end
    object rdbFcXonXoff: TRadioButton
      Left = 85
      Top = 18
      Width = 64
      Height = 17
      Caption = 'XonXoff'
      TabOrder = 2
    end
    object rdbFcDtrDsr: TRadioButton
      Left = 85
      Top = 37
      Width = 64
      Height = 17
      Caption = 'DTR/DSR'
      TabOrder = 3
    end
  end
end
