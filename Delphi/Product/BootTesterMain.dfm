object FrmBootTester: TFrmBootTester
  Left = 0
  Top = 0
  Caption = 'BootTester'
  ClientHeight = 611
  ClientWidth = 674
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
  object lblPort: TLabel
    Left = 378
    Top = 206
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lblBaudrate: TLabel
    Left = 480
    Top = 206
    Width = 48
    Height = 13
    Caption = 'Baudrate:'
  end
  object lblLoop: TLabel
    Left = 89
    Top = 205
    Width = 27
    Height = 13
    Caption = 'Loop:'
  end
  object memRecv: TMemo
    Left = 8
    Top = 231
    Width = 657
    Height = 337
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnStart: TButton
    Left = 8
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object cmbPort: TComboBox
    Left = 408
    Top = 203
    Width = 55
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 2
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
    Left = 530
    Top = 203
    Width = 64
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = '9600'
    OnChange = cmbBaudrateChange
    Items.Strings = (
      '9600'
      '19200'
      '115200')
  end
  object txtSend: TEdit
    Left = 8
    Top = 175
    Width = 337
    Height = 21
    TabOrder = 4
    Text = 'Read?'
  end
  object chbRecv: TCheckBox
    Left = 375
    Top = 175
    Width = 60
    Height = 21
    Caption = 'Receive'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = chbRecvClick
  end
  object btnClear: TButton
    Left = 468
    Top = 574
    Width = 75
    Height = 25
    Caption = '&Clear'
    TabOrder = 6
    OnClick = btnClearClick
  end
  object btnExport: TButton
    Left = 589
    Top = 574
    Width = 75
    Height = 25
    Caption = 'E&xport...'
    TabOrder = 7
    OnClick = btnExportClick
  end
  object txtLoop: TEdit
    Left = 116
    Top = 202
    Width = 47
    Height = 21
    TabOrder = 8
    Text = '1'
  end
  object lstSending: TListBox
    Left = 8
    Top = 8
    Width = 657
    Height = 161
    ItemHeight = 13
    TabOrder = 9
    OnDblClick = lstSendingDblClick
  end
  object chbVerify: TCheckBox
    Left = 433
    Top = 177
    Width = 49
    Height = 17
    Caption = 'Verify:'
    TabOrder = 10
    OnClick = chbVerifyClick
  end
  object txtVerify: TEdit
    Left = 488
    Top = 176
    Width = 178
    Height = 21
    Enabled = False
    TabOrder = 11
    OnEnter = txtVerifyEnter
  end
  object btnReset: TButton
    Left = 169
    Top = 200
    Width = 75
    Height = 25
    Caption = '&Reset To'
    TabOrder = 12
    OnClick = btnResetClick
  end
  object cmbTo: TComboBox
    Left = 250
    Top = 202
    Width = 79
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 13
    Text = 'Application'
    Items.Strings = (
      'Application'
      'Service')
  end
  object btnClose: TButton
    Left = 600
    Top = 200
    Width = 66
    Height = 25
    Caption = '&Close'
    TabOrder = 14
    OnClick = btnCloseClick
  end
end
