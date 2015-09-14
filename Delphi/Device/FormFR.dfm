object FRTester: TFRTester
  Left = 0
  Top = 0
  Caption = 'FRTester'
  ClientHeight = 374
  ClientWidth = 565
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
  object btnStart: TButton
    Left = 8
    Top = 142
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object memDisplay: TMemo
    Left = 8
    Top = 171
    Width = 546
    Height = 195
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object txtCmd: TEdit
    Left = 104
    Top = 144
    Width = 207
    Height = 21
    TabOrder = 2
    Text = ' $0 $0C940057'
  end
  object btnSend: TButton
    Left = 479
    Top = 142
    Width = 75
    Height = 25
    Caption = 'Send'
    TabOrder = 3
    OnClick = btnSendClick
  end
  object btnSetDM: TButton
    Left = 317
    Top = 142
    Width = 75
    Height = 25
    Caption = 'fr_set_dm'
    TabOrder = 4
    OnClick = btnSetDMClick
  end
  object btnRunScript: TButton
    Left = 398
    Top = 142
    Width = 75
    Height = 25
    Caption = 'fr_run_script'
    TabOrder = 5
    OnClick = btnRunScriptClick
  end
  object lstSending: TListBox
    Left = 8
    Top = 8
    Width = 546
    Height = 121
    ItemHeight = 13
    TabOrder = 6
    OnDblClick = lstSendingDblClick
  end
end
