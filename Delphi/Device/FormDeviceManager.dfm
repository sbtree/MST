object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 0
  Caption = 'DeviceTester'
  ClientHeight = 337
  ClientWidth = 671
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
  object Label1: TLabel
    Left = 8
    Top = 309
    Width = 36
    Height = 13
    Caption = 'Relays:'
  end
  object btnReset: TButton
    Left = 588
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Reset'
    TabOrder = 0
    OnClick = btnResetClick
  end
  object btnMeasure: TButton
    Left = 498
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Measure'
    TabOrder = 1
    OnClick = btnMeasureClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 655
    Height = 281
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object cmbMeasure: TComboBox
    Left = 376
    Top = 306
    Width = 116
    Height = 21
    ItemHeight = 13
    ItemIndex = 1
    TabOrder = 3
    Text = 'VOLT:DC'
    Items.Strings = (
      'RES'
      'VOLT:DC'
      'VOLT:AC'
      'CURR:DC'
      'CURR:AC'
      'FREQ'
      'PER  '
      'TEMP')
  end
  object txtRelays: TEdit
    Left = 50
    Top = 306
    Width = 143
    Height = 21
    TabOrder = 4
    Text = '101,102,205'
  end
  object btnClose: TButton
    Left = 199
    Top = 304
    Width = 42
    Height = 25
    Caption = '&Close'
    TabOrder = 5
    OnClick = btnCloseClick
  end
  object btnOpen: TButton
    Left = 247
    Top = 304
    Width = 42
    Height = 25
    Caption = '&Open'
    TabOrder = 6
    OnClick = btnOpenClick
  end
  object btnOpenAll: TButton
    Left = 295
    Top = 304
    Width = 65
    Height = 25
    Caption = 'Open &All'
    TabOrder = 7
    OnClick = btnOpenAllClick
  end
end
