object frmDeviceManager: TfrmDeviceManager
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Keithley Tester'
  ClientHeight = 335
  ClientWidth = 658
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
    Left = 58
    Top = 309
    Width = 36
    Height = 13
    Caption = 'Relays:'
  end
  object btnInit: TButton
    Left = 579
    Top = 304
    Width = 75
    Height = 25
    Caption = '&Initialize'
    TabOrder = 0
    OnClick = btnInitClick
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
  object cmbMeasure: TComboBox
    Left = 416
    Top = 306
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
  object txtRelays: TEdit
    Left = 100
    Top = 306
    Width = 143
    Height = 21
    TabOrder = 3
    Text = '101,102,205'
  end
  object btnClose: TButton
    Left = 249
    Top = 304
    Width = 42
    Height = 25
    Caption = '&Close'
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object btnOpen: TButton
    Left = 297
    Top = 304
    Width = 42
    Height = 25
    Caption = '&Open'
    TabOrder = 5
    OnClick = btnOpenClick
  end
  object btnOpenAll: TButton
    Left = 345
    Top = 304
    Width = 65
    Height = 25
    Caption = 'Open &All'
    TabOrder = 6
    OnClick = btnOpenAllClick
  end
  object pgcMain: TPageControl
    Left = 2
    Top = 0
    Width = 652
    Height = 300
    ActivePage = pgR100
    TabOrder = 7
    object pgR100: TTabSheet
      Caption = 'R101-140'
      Enabled = False
      object CheckBox1: TCheckBox
        Left = 24
        Top = 56
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
    end
    object pgR200: TTabSheet
      Caption = 'R201-240'
      Enabled = False
      ImageIndex = 1
    end
    object pgR300: TTabSheet
      Caption = 'R301-340'
      Enabled = False
      ImageIndex = 2
    end
    object pgR400: TTabSheet
      Caption = 'R401-440'
      Enabled = False
      ImageIndex = 3
      object CheckBox2: TCheckBox
        Left = 184
        Top = 96
        Width = 97
        Height = 17
        Caption = 'CheckBox2'
        TabOrder = 0
      end
    end
    object pgR500: TTabSheet
      Caption = 'R501-540'
      Enabled = False
      ImageIndex = 4
    end
    object pgInfo: TTabSheet
      Caption = 'Info'
      ImageIndex = 5
      object memInfo: TMemo
        Left = -4
        Top = -2
        Width = 652
        Height = 276
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object btnConnect: TButton
    Left = 2
    Top = 304
    Width = 50
    Height = 25
    Caption = '&Connet'
    TabOrder = 8
    OnClick = btnConnectClick
  end
end
