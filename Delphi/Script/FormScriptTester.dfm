object frmScriptTester: TfrmScriptTester
  Left = 0
  Top = 0
  Caption = 'ScriptTester'
  ClientHeight = 330
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
    Left = 62
    Top = 270
    Width = 90
    Height = 25
    Caption = 'GetStepByNr'
    TabOrder = 0
    OnClick = btnGetStepByNrClick
  end
  object txtStepNr: TEdit
    Left = 7
    Top = 272
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '895.68'
  end
  object btnGetStepByIndex: TButton
    Left = 62
    Top = 297
    Width = 90
    Height = 25
    Caption = 'GetStepByIndex'
    TabOrder = 2
    OnClick = btnGetStepByIndexClick
  end
  object txtStepIndex: TEdit
    Left = 7
    Top = 299
    Width = 49
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object txtScriptFile: TEdit
    Left = 7
    Top = 240
    Width = 353
    Height = 21
    TabOrder = 4
    Text = 'PST_BT_FT_PS_liste.txt'
  end
  object btnOpenScript: TButton
    Left = 365
    Top = 238
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = btnOpenScriptClick
  end
  object btnReadScript: TButton
    Left = 398
    Top = 238
    Width = 75
    Height = 25
    Caption = 'ReadScript'
    TabOrder = 6
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 479
    Top = 238
    Width = 75
    Height = 25
    Caption = 'SaveScript'
    TabOrder = 7
    OnClick = btnSaveScriptClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 546
    Height = 224
    TabOrder = 8
  end
  object txtField: TEdit
    Left = 205
    Top = 302
    Width = 74
    Height = 21
    TabOrder = 9
    Text = 'T'
  end
  object btnGetField: TButton
    Left = 285
    Top = 300
    Width = 75
    Height = 25
    Caption = 'GetField'
    TabOrder = 10
    OnClick = btnGetFieldClick
  end
  object btnPrevious: TButton
    Left = 204
    Top = 269
    Width = 75
    Height = 25
    Caption = 'Previous'
    TabOrder = 11
    OnClick = btnPreviousClick
  end
  object btnNext: TButton
    Left = 285
    Top = 269
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 12
    OnClick = btnNextClick
  end
  object txtCase: TEdit
    Left = 399
    Top = 272
    Width = 74
    Height = 21
    TabOrder = 13
    Text = '11'
  end
  object btnGetCase: TButton
    Left = 479
    Top = 269
    Width = 75
    Height = 25
    Caption = 'GetCase'
    TabOrder = 14
    OnClick = btnGetCaseClick
  end
  object txtSequence: TEdit
    Left = 399
    Top = 300
    Width = 74
    Height = 21
    TabOrder = 15
    Text = '10-80'
  end
  object btnSequence: TButton
    Left = 479
    Top = 297
    Width = 75
    Height = 25
    Caption = 'GetSequence'
    TabOrder = 16
    OnClick = btnSequenceClick
  end
end
