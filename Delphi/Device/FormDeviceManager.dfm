object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Keithley Tester'
  ClientHeight = 497
  ClientWidth = 667
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
    Top = 472
    Width = 92
    Height = 13
    Caption = 'Relays Description:'
  end
  object btnInit: TButton
    Left = 59
    Top = 437
    Width = 60
    Height = 25
    Caption = '&Initialize'
    TabOrder = 0
    OnClick = btnInitClick
  end
  object btnMeasure: TButton
    Left = 506
    Top = 437
    Width = 75
    Height = 25
    Caption = '&Measure'
    TabOrder = 1
    OnClick = btnMeasureClick
  end
  object cmbMeasure: TComboBox
    Left = 424
    Top = 439
    Width = 76
    Height = 21
    ItemIndex = 1
    TabOrder = 2
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
  object btnClose: TButton
    Left = 257
    Top = 437
    Width = 42
    Height = 25
    Caption = '&Close'
    Enabled = False
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object btnOpen: TButton
    Left = 305
    Top = 437
    Width = 42
    Height = 25
    Caption = '&Open'
    Enabled = False
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object btnOpenAll: TButton
    Left = 353
    Top = 437
    Width = 65
    Height = 25
    Caption = 'Open &All'
    TabOrder = 5
    OnClick = btnOpenAllClick
  end
  object pgcMain: TPageControl
    Left = 8
    Top = 4
    Width = 652
    Height = 431
    ActivePage = tabRelays
    TabOrder = 6
    object tabRelays: TTabSheet
      Caption = 'Relays'
      ImageIndex = 1
      ExplicitWidth = 645
    end
    object pgInfo: TTabSheet
      Caption = 'Info'
      ImageIndex = 5
      ExplicitHeight = 272
      object memInfo: TMemo
        Left = -4
        Top = -2
        Width = 652
        Height = 409
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object btnConnect: TButton
    Left = 8
    Top = 437
    Width = 50
    Height = 25
    Caption = 'Co&nnet'
    TabOrder = 7
    OnClick = btnConnectClick
  end
  object chkSelection: TCheckBox
    Left = 184
    Top = 441
    Width = 67
    Height = 17
    Caption = '&Selection'
    TabOrder = 8
    OnClick = chkSelectionClick
  end
  object txtMeasure: TEdit
    Left = 582
    Top = 439
    Width = 78
    Height = 21
    ReadOnly = True
    TabOrder = 9
  end
  object btnUpdate: TButton
    Left = 582
    Top = 467
    Width = 78
    Height = 25
    Caption = '&Update'
    TabOrder = 10
  end
  object txtFile: TEdit
    Left = 106
    Top = 469
    Width = 447
    Height = 21
    ReadOnly = True
    TabOrder = 11
  end
  object btnFile: TButton
    Left = 554
    Top = 467
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 12
  end
end
