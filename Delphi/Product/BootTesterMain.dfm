object FrmBootTester: TFrmBootTester
  Left = 0
  Top = 0
  Caption = 'BootTester'
  ClientHeight = 472
  ClientWidth = 772
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
  object grpRS232: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 89
    Caption = 'RS232 Settings'
    TabOrder = 3
    object lblPort: TLabel
      Left = 43
      Top = 27
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object lblBaudrate: TLabel
      Left = 166
      Top = 26
      Width = 48
      Height = 13
      Caption = 'Baudrate:'
    end
    object Label1: TLabel
      Left = 43
      Top = 62
      Width = 64
      Height = 13
      Caption = 'Flow Control:'
    end
    object cmbPort: TComboBox
      Left = 73
      Top = 23
      Width = 48
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '1'
      OnChange = cmbPortChange
      Items.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
    end
    object cmbBaudrate: TComboBox
      Left = 220
      Top = 24
      Width = 66
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = '9600'
      OnChange = cmbBaudrateChange
      Items.Strings = (
        '9600'
        '19200'
        '115200')
    end
    object btnClose: TButton
      Left = 220
      Top = 57
      Width = 66
      Height = 24
      Caption = '&Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
    object chkXonXoff: TCheckBox
      Left = 113
      Top = 61
      Width = 59
      Height = 17
      Caption = 'Xon/Xoff'
      TabOrder = 3
      OnClick = chkXonXoffClick
    end
  end
  object memRecv: TMemo
    Left = 360
    Top = 8
    Width = 401
    Height = 426
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 557
    Top = 440
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnExport: TButton
    Left = 678
    Top = 440
    Width = 75
    Height = 25
    Caption = 'E&xport...'
    TabOrder = 2
    OnClick = btnExportClick
  end
  object grpSendCommand: TGroupBox
    Left = 8
    Top = 103
    Width = 337
    Height = 257
    Caption = 'Send Command'
    TabOrder = 4
    object lblLoop: TLabel
      Left = 17
      Top = 225
      Width = 27
      Height = 13
      Caption = 'Loop:'
    end
    object btnSend: TButton
      Left = 256
      Top = 191
      Width = 57
      Height = 25
      Caption = '&Send'
      TabOrder = 0
      OnClick = btnSendClick
    end
    object txtSend: TEdit
      Left = 17
      Top = 193
      Width = 233
      Height = 22
      TabOrder = 1
      Text = 'BOOT?'
    end
    object chbRecv: TCheckBox
      Left = 111
      Top = 222
      Width = 60
      Height = 21
      Caption = 'Receive'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chbRecvClick
    end
    object txtLoop: TEdit
      Left = 44
      Top = 222
      Width = 39
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object lstSending: TListBox
      Left = 17
      Top = 24
      Width = 296
      Height = 161
      ItemHeight = 13
      TabOrder = 4
      OnDblClick = lstSendingDblClick
    end
    object chbVerify: TCheckBox
      Left = 177
      Top = 224
      Width = 49
      Height = 17
      Caption = 'Verify:'
      TabOrder = 5
      OnClick = chbVerifyClick
    end
    object txtVerify: TEdit
      Left = 232
      Top = 222
      Width = 81
      Height = 21
      Enabled = False
      TabOrder = 6
      OnEnter = txtVerifyEnter
    end
  end
  object grpService: TGroupBox
    Left = 8
    Top = 366
    Width = 337
    Height = 99
    Caption = 'Service'
    TabOrder = 5
    object lblSendFile: TLabel
      Left = 288
      Top = 80
      Width = 25
      Height = 11
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnReset: TButton
      Left = 17
      Top = 24
      Width = 50
      Height = 25
      Caption = '&Reset To'
      TabOrder = 0
      OnClick = btnResetClick
    end
    object cmbTo: TComboBox
      Left = 71
      Top = 26
      Width = 79
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Application'
      Items.Strings = (
        'Application'
        'Service')
    end
    object btnSendFile: TButton
      Left = 177
      Top = 24
      Width = 51
      Height = 25
      Caption = 'Send &File'
      TabOrder = 2
      OnClick = btnSendFileClick
    end
    object txtFile: TEdit
      Left = 16
      Top = 57
      Width = 266
      Height = 22
      TabOrder = 3
    end
    object btnFile: TButton
      Left = 288
      Top = 57
      Width = 25
      Height = 22
      Caption = '...'
      TabOrder = 4
      OnClick = btnFileClick
    end
    object pgbSendFile: TProgressBar
      Left = 16
      Top = 80
      Width = 266
      Height = 9
      TabOrder = 5
    end
    object chkMetronix: TCheckBox
      Left = 232
      Top = 28
      Width = 83
      Height = 17
      Caption = 'MTX Protocol'
      TabOrder = 6
    end
  end
end
