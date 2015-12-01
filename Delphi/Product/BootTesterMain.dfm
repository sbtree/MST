object FrmBootTester: TFrmBootTester
  Left = 0
  Top = 0
  Caption = 'BootTester'
  ClientHeight = 455
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
    Width = 400
    Height = 65
    Caption = 'RS232 Settings'
    TabOrder = 3
    object lblPort: TLabel
      Left = 19
      Top = 27
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object lblBaudrate: TLabel
      Left = 107
      Top = 26
      Width = 48
      Height = 13
      Caption = 'Baudrate:'
    end
    object cmbPortTest: TComboBox
      Left = 49
      Top = 23
      Width = 48
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '1'
      OnChange = cmbPortTestChange
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
      Left = 161
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
      Left = 325
      Top = 22
      Width = 57
      Height = 24
      Caption = '&Close'
      TabOrder = 2
      OnClick = btnCloseClick
    end
    object chkXonXoff: TCheckBox
      Left = 240
      Top = 25
      Width = 59
      Height = 17
      Caption = 'Xon/Xoff'
      TabOrder = 3
      OnClick = chkXonXoffClick
    end
  end
  object memRecv: TMemo
    Left = 416
    Top = 8
    Width = 345
    Height = 393
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 565
    Top = 418
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnExport: TButton
    Left = 686
    Top = 418
    Width = 75
    Height = 25
    Caption = 'E&xport...'
    TabOrder = 2
    OnClick = btnExportClick
  end
  object grpSendCommand: TGroupBox
    Left = 8
    Top = 79
    Width = 400
    Height = 258
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
      Left = 325
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
      Width = 302
      Height = 21
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
      Width = 365
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
      Width = 150
      Height = 21
      Enabled = False
      TabOrder = 6
      OnEnter = txtVerifyEnter
    end
  end
  object grpService: TGroupBox
    Left = 8
    Top = 343
    Width = 400
    Height = 100
    Caption = 'Service'
    TabOrder = 5
    object lblSendFile: TLabel
      Left = 357
      Top = 47
      Width = 15
      Height = 11
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblBootCmd: TLabel
      Left = 16
      Top = 66
      Width = 76
      Height = 13
      Caption = 'Boot Command:'
    end
    object txtFile: TEdit
      Left = 16
      Top = 24
      Width = 335
      Height = 21
      TabOrder = 0
    end
    object btnFile: TButton
      Left = 357
      Top = 24
      Width = 25
      Height = 22
      Caption = '...'
      TabOrder = 1
      OnClick = btnFileClick
    end
    object pgbSendFile: TProgressBar
      Left = 16
      Top = 47
      Width = 335
      Height = 10
      TabOrder = 2
    end
    object btnStateQue: TButton
      Left = 243
      Top = 64
      Width = 56
      Height = 25
      Caption = 'State?'
      TabOrder = 3
      OnClick = btnStateQueClick
    end
    object btnDownload: TButton
      Left = 325
      Top = 64
      Width = 57
      Height = 25
      Caption = '&Download'
      TabOrder = 4
      OnClick = btnDownloadClick
    end
    object txtBootCmd: TEdit
      Left = 98
      Top = 63
      Width = 63
      Height = 21
      TabOrder = 5
      Text = 'Reset!'
    end
    object cmbPortProd: TComboBox
      Left = 167
      Top = 63
      Width = 48
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 6
      Text = '1'
      OnChange = cmbPortProdChange
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
  end
end
