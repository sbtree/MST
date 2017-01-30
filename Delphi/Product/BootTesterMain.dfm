object FrmBootTester: TFrmBootTester
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'BootTester'
  ClientHeight = 471
  ClientWidth = 774
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
  object memRecv: TMemo
    Left = 416
    Top = 8
    Width = 345
    Height = 414
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 565
    Top = 438
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object btnExport: TButton
    Left = 686
    Top = 438
    Width = 75
    Height = 25
    Caption = 'E&xport...'
    TabOrder = 2
    OnClick = btnExportClick
  end
  object grpSendCommand: TGroupBox
    Left = 8
    Top = 8
    Width = 400
    Height = 282
    Caption = 'Single Command'
    TabOrder = 3
    object lblLoop: TLabel
      Left = 17
      Top = 252
      Width = 27
      Height = 13
      Caption = 'Loop:'
    end
    object lblBaudTest: TLabel
      Left = 107
      Top = 26
      Width = 48
      Height = 13
      Caption = 'Baudrate:'
    end
    object lblPortTest: TLabel
      Left = 19
      Top = 27
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object btnSend: TButton
      Left = 325
      Top = 218
      Width = 57
      Height = 25
      Caption = '&Send'
      TabOrder = 0
      OnClick = btnSendClick
    end
    object txtSend: TEdit
      Left = 17
      Top = 220
      Width = 302
      Height = 21
      TabOrder = 1
      Text = 'BOOT?'
    end
    object chbRecv: TCheckBox
      Left = 111
      Top = 249
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
      Top = 249
      Width = 39
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object lstSending: TListBox
      Left = 17
      Top = 52
      Width = 365
      Height = 160
      ItemHeight = 13
      TabOrder = 4
      OnDblClick = lstSendingDblClick
    end
    object chbVerify: TCheckBox
      Left = 177
      Top = 251
      Width = 49
      Height = 17
      Caption = 'Verify:'
      TabOrder = 5
      OnClick = chbVerifyClick
    end
    object txtVerify: TEdit
      Left = 232
      Top = 249
      Width = 150
      Height = 21
      Enabled = False
      TabOrder = 6
      OnEnter = txtVerifyEnter
    end
    object btnCloseTest: TButton
      Left = 325
      Top = 22
      Width = 57
      Height = 24
      Caption = '&Close'
      TabOrder = 7
      OnClick = btnCloseTestClick
    end
    object chkXonXoffTest: TCheckBox
      Left = 240
      Top = 25
      Width = 59
      Height = 17
      Caption = 'Xon/Xoff'
      TabOrder = 8
      OnClick = chkXonXoffTestClick
    end
    object cmbBaudTest: TComboBox
      Left = 161
      Top = 24
      Width = 66
      Height = 21
      ItemIndex = 0
      TabOrder = 9
      Text = '9600'
      OnChange = cmbBaudTestChange
      Items.Strings = (
        '9600'
        '19200'
        '115200')
    end
    object cmbPortTest: TComboBox
      Left = 49
      Top = 23
      Width = 48
      Height = 21
      ItemIndex = 0
      TabOrder = 10
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
  end
  object grpService: TGroupBox
    Left = 8
    Top = 296
    Width = 400
    Height = 169
    Caption = 'Service'
    TabOrder = 4
    object lblBootCmd: TLabel
      Left = 19
      Top = 138
      Width = 76
      Height = 13
      Caption = 'Boot Command:'
    end
    object lblPortProd: TLabel
      Left = 43
      Top = 28
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object lblBaudProd: TLabel
      Left = 19
      Top = 52
      Width = 48
      Height = 13
      Caption = 'Baudrate:'
    end
    object txtFile: TEdit
      Left = 19
      Top = 82
      Width = 335
      Height = 21
      TabOrder = 0
    end
    object btnFile: TButton
      Left = 360
      Top = 80
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnFileClick
    end
    object btnStateQue: TButton
      Left = 246
      Top = 131
      Width = 56
      Height = 28
      Caption = 'State?'
      TabOrder = 2
      OnClick = btnStateQueClick
    end
    object btnDownload: TButton
      Left = 328
      Top = 131
      Width = 57
      Height = 28
      Caption = '&Download'
      TabOrder = 3
      OnClick = btnDownloadClick
    end
    object txtBootCmd: TEdit
      Left = 101
      Top = 135
      Width = 63
      Height = 21
      TabOrder = 4
      Text = 'Reset!'
    end
    object cmbPortProd: TComboBox
      Left = 73
      Top = 25
      Width = 48
      Height = 21
      ItemIndex = 6
      TabOrder = 5
      Text = '7'
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
    object chkXonXoffProd: TCheckBox
      Left = 145
      Top = 27
      Width = 59
      Height = 17
      Caption = 'Xon/Xoff'
      TabOrder = 6
      OnClick = chkXonXoffProdClick
    end
    object cmbBaudProd: TComboBox
      Left = 73
      Top = 50
      Width = 66
      Height = 21
      ItemIndex = 0
      TabOrder = 7
      Text = '9600'
      OnChange = cmbBaudProdChange
      Items.Strings = (
        '9600'
        '19200'
        '115200')
    end
    object btnCloseProd: TButton
      Left = 328
      Top = 22
      Width = 57
      Height = 24
      Caption = '&Close'
      TabOrder = 8
      OnClick = btnCloseProdClick
    end
    object chkBaudFactor: TCheckBox
      Left = 145
      Top = 51
      Width = 104
      Height = 17
      Caption = 'Baudrate Factor:'
      TabOrder = 9
      OnClick = chkBaudFactorClick
    end
    object txtBaudFactor: TEdit
      Left = 255
      Top = 50
      Width = 62
      Height = 21
      Enabled = False
      TabOrder = 10
      Text = '0.9216'
    end
    object pgbSendFile: TProgressBar
      Left = 19
      Top = 112
      Width = 366
      Height = 13
      TabOrder = 11
    end
  end
  object btnTest: TButton
    Left = 432
    Top = 438
    Width = 75
    Height = 25
    Caption = '&Test'
    TabOrder = 5
    OnClick = btnTestClick
  end
end
