object frmScriptTester: TfrmScriptTester
  Left = 0
  Top = 0
  Caption = 'ScriptTester'
  ClientHeight = 356
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
  object lblInclusive: TLabel
    Left = 361
    Top = 303
    Width = 25
    Height = 13
    Caption = 'Incl.:'
  end
  object lblExclusive: TLabel
    Left = 361
    Top = 332
    Width = 27
    Height = 13
    Caption = 'Excl.:'
  end
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
    Top = 298
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
    Width = 405
    Height = 21
    TabOrder = 4
    Text = 'PST_BT_FT_PS_liste.txt'
  end
  object btnOpenScript: TButton
    Left = 418
    Top = 240
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 5
    OnClick = btnOpenScriptClick
  end
  object btnReadScript: TButton
    Left = 383
    Top = 270
    Width = 50
    Height = 25
    Caption = 'Read'
    TabOrder = 6
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 448
    Top = 270
    Width = 50
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = btnSaveScriptClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 546
    Height = 224
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object txtField: TEdit
    Left = 184
    Top = 300
    Width = 74
    Height = 21
    TabOrder = 9
    Text = 'FKT'
  end
  object btnGetField: TButton
    Left = 263
    Top = 298
    Width = 90
    Height = 25
    Caption = 'GetField'
    TabOrder = 10
    OnClick = btnGetFieldClick
  end
  object btnPrevious: TButton
    Left = 8
    Top = 326
    Width = 48
    Height = 25
    Hint = 'previous test step'
    Caption = '<'
    TabOrder = 11
    OnClick = btnPreviousClick
  end
  object btnNext: TButton
    Left = 104
    Top = 326
    Width = 48
    Height = 25
    Hint = 'next test step'
    Caption = '>'
    TabOrder = 12
    OnClick = btnNextClick
  end
  object txtCase: TEdit
    Left = 184
    Top = 328
    Width = 74
    Height = 21
    TabOrder = 13
    Text = '11'
  end
  object btnGetCase: TButton
    Left = 263
    Top = 326
    Width = 90
    Height = 25
    Caption = 'GetCase'
    TabOrder = 14
    OnClick = btnGetCaseClick
  end
  object txtInclusive: TEdit
    Left = 392
    Top = 300
    Width = 71
    Height = 21
    Hint = 'inclusive test cases'
    TabOrder = 15
    Text = 'all'
  end
  object btnSequenceCases: TButton
    Left = 469
    Top = 298
    Width = 85
    Height = 25
    Caption = 'Sequence Cases'
    TabOrder = 16
    OnClick = btnSequenceCasesClick
  end
  object txtExclusive: TEdit
    Left = 391
    Top = 328
    Width = 72
    Height = 21
    Hint = 'exclusive test cases'
    TabOrder = 17
    Text = '10-80'
  end
  object txtVariable: TEdit
    Left = 184
    Top = 272
    Width = 74
    Height = 21
    TabOrder = 18
    Text = 'Include'
  end
  object btnVariable: TButton
    Left = 263
    Top = 270
    Width = 90
    Height = 25
    Caption = 'GetVariable'
    TabOrder = 19
    OnClick = btnVariableClick
  end
  object btnClear: TButton
    Left = 504
    Top = 270
    Width = 50
    Height = 25
    Caption = 'Clear'
    TabOrder = 20
    OnClick = btnClearClick
  end
  object chkForce: TCheckBox
    Left = 448
    Top = 242
    Width = 45
    Height = 17
    Caption = 'Force'
    TabOrder = 21
  end
  object chkAppend: TCheckBox
    Left = 500
    Top = 242
    Width = 54
    Height = 17
    Caption = 'Append'
    TabOrder = 22
  end
  object btnSequenceSteps: TButton
    Left = 469
    Top = 327
    Width = 85
    Height = 25
    Caption = 'Sequence Steps'
    TabOrder = 23
    OnClick = btnSequenceStepsClick
  end
end
