object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 66
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Keithley Tester'
  ClientHeight = 513
  ClientWidth = 989
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblRelaysDesc: TLabel
    Left = 8
    Top = 489
    Width = 92
    Height = 13
    Caption = 'Relays Description:'
  end
  object btnInit: TButton
    Left = 79
    Top = 453
    Width = 60
    Height = 25
    Caption = '&Initialize'
    TabOrder = 0
    OnClick = btnInitClick
  end
  object btnMeasure: TButton
    Left = 681
    Top = 453
    Width = 75
    Height = 25
    Caption = '&Measure'
    TabOrder = 1
    OnClick = btnMeasureClick
  end
  object cmbMeasure: TComboBox
    Left = 597
    Top = 455
    Width = 84
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
    Left = 365
    Top = 453
    Width = 42
    Height = 25
    Caption = '&Close'
    Enabled = False
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object btnOpen: TButton
    Left = 413
    Top = 453
    Width = 42
    Height = 25
    Caption = '&Open'
    Enabled = False
    TabOrder = 4
    OnClick = btnOpenClick
  end
  object btnOpenAll: TButton
    Left = 461
    Top = 453
    Width = 65
    Height = 25
    Caption = 'Open &All'
    TabOrder = 5
    OnClick = btnOpenAllClick
  end
  object btnConnect: TButton
    Left = 8
    Top = 453
    Width = 65
    Height = 25
    Caption = 'Co&nnect'
    TabOrder = 6
    OnClick = btnConnectClick
  end
  object chkSelection: TCheckBox
    Left = 292
    Top = 457
    Width = 67
    Height = 17
    Caption = '&Selection'
    TabOrder = 7
    OnClick = chkSelectionClick
  end
  object txtMeasure: TEdit
    Left = 759
    Top = 455
    Width = 78
    Height = 21
    ReadOnly = True
    TabOrder = 8
  end
  object btnUpdate: TButton
    Left = 678
    Top = 484
    Width = 78
    Height = 25
    Caption = '&Update'
    TabOrder = 9
    OnClick = btnUpdateClick
  end
  object txtFile: TEdit
    Left = 106
    Top = 486
    Width = 543
    Height = 21
    ReadOnly = True
    TabOrder = 10
  end
  object btnFile: TButton
    Left = 650
    Top = 484
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 11
    OnClick = btnFileClick
  end
  object btnView: TButton
    Left = 906
    Top = 484
    Width = 75
    Height = 25
    Caption = '&View >>'
    TabOrder = 12
    OnClick = btnViewClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 515
    Width = 973
    Height = 186
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 13
    Visible = False
  end
  object pgcMain: TPageControl
    Left = 8
    Top = 4
    Width = 973
    Height = 431
    ActivePage = tabRelays
    TabOrder = 14
    object tabRelays: TTabSheet
      Caption = 'Relays'
      ImageIndex = 1
    end
  end
  object chkAutoMeasure: TCheckBox
    Left = 840
    Top = 457
    Width = 141
    Height = 17
    Caption = 'Auto after relay changing'
    TabOrder = 15
  end
end
