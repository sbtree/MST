object frmScriptTester: TfrmScriptTester
  Left = 0
  Top = 0
  Caption = 'ScriptTester'
  ClientHeight = 301
  ClientWidth = 562
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
  object btnGetStepByNr: TButton
    Left = 63
    Top = 262
    Width = 90
    Height = 25
    Caption = 'GetStepByNr'
    TabOrder = 0
    OnClick = btnGetStepByNrClick
  end
  object txtStepNr: TEdit
    Left = 8
    Top = 264
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '895.68'
  end
  object btnGetStepByIndex: TButton
    Left = 63
    Top = 233
    Width = 90
    Height = 25
    Caption = 'GetStepByIndex'
    TabOrder = 2
    OnClick = btnGetStepByIndexClick
  end
  object txtStepIndex: TEdit
    Left = 8
    Top = 235
    Width = 49
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 8
    Width = 353
    Height = 21
    TabOrder = 4
    Text = 'PST_BT_FT_PS_liste.txt'
  end
  object btnOpenScript: TButton
    Left = 366
    Top = 6
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = btnOpenScriptClick
  end
  object btnReadScript: TButton
    Left = 399
    Top = 6
    Width = 75
    Height = 25
    Caption = 'ReadScript'
    TabOrder = 6
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 480
    Top = 6
    Width = 75
    Height = 25
    Caption = 'SaveScript'
    TabOrder = 7
    OnClick = btnSaveScriptClick
  end
end
