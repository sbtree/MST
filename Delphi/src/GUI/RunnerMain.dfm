object FrmTestRunner: TFrmTestRunner
  Left = 0
  Top = 0
  Caption = 'Test Runner'
  ClientHeight = 418
  ClientWidth = 683
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
    Left = 8
    Top = 356
    Width = 25
    Height = 13
    Caption = 'Incl.:'
  end
  object lblExclusive: TLabel
    Left = 8
    Top = 385
    Width = 27
    Height = 13
    Caption = 'Excl.:'
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 301
    Width = 344
    Height = 21
    TabOrder = 0
    Text = '..\..\ini\PS_lists\test_list.txt'
  end
  object btnOpenScript: TButton
    Left = 358
    Top = 301
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnOpenScriptClick
  end
  object btnReadScript: TButton
    Left = 513
    Top = 299
    Width = 50
    Height = 25
    Caption = 'Read'
    TabOrder = 2
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 569
    Top = 299
    Width = 50
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveScriptClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 667
    Height = 285
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object txtInclusive: TEdit
    Left = 38
    Top = 353
    Width = 144
    Height = 21
    Hint = 'inclusive test cases'
    TabOrder = 5
    Text = 'all'
  end
  object btnSequence: TButton
    Left = 190
    Top = 351
    Width = 81
    Height = 25
    Caption = 'Sequence'
    TabOrder = 6
    OnClick = btnSequenceClick
  end
  object txtExclusive: TEdit
    Left = 38
    Top = 382
    Width = 144
    Height = 21
    Hint = 'exclusive test cases'
    TabOrder = 7
    Text = '10-80'
  end
  object btnClear: TButton
    Left = 625
    Top = 299
    Width = 50
    Height = 25
    Caption = 'Clear'
    TabOrder = 8
    OnClick = btnClearClick
  end
  object chkForce: TCheckBox
    Left = 389
    Top = 303
    Width = 45
    Height = 17
    Caption = 'Force'
    TabOrder = 9
  end
  object chkAppend: TCheckBox
    Left = 440
    Top = 303
    Width = 54
    Height = 17
    Caption = 'Append'
    TabOrder = 10
  end
  object btnRunSequence: TButton
    Left = 190
    Top = 380
    Width = 32
    Height = 25
    Caption = 'Run'
    TabOrder = 11
    OnClick = btnRunSequenceClick
  end
  object btnRepeatSequence: TButton
    Left = 228
    Top = 380
    Width = 43
    Height = 25
    Caption = 'Repeat'
    TabOrder = 12
    OnClick = btnRepeatSequenceClick
  end
  object GroupBox1: TGroupBox
    Left = 466
    Top = 330
    Width = 97
    Height = 84
    Caption = 'Step Mode'
    TabOrder = 13
    object btnRunFirstStep: TButton
      Left = 5
      Top = 21
      Width = 87
      Height = 25
      Caption = 'First Step'
      TabOrder = 0
      OnClick = btnRunFirstStepClick
    end
    object btnRepeatStep: TButton
      Left = 36
      Top = 49
      Width = 25
      Height = 25
      Caption = '='
      TabOrder = 1
      OnClick = btnRepeatStepClick
    end
    object btnPreviousStep: TButton
      Left = 5
      Top = 49
      Width = 25
      Height = 25
      Hint = 'run previous test step'
      Caption = '<'
      TabOrder = 2
      OnClick = btnPreviousStepClick
    end
    object btnNextStep: TButton
      Left = 67
      Top = 49
      Width = 25
      Height = 25
      Hint = 'run next test step'
      Caption = '>'
      TabOrder = 3
      OnClick = btnNextStepClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 578
    Top = 330
    Width = 97
    Height = 82
    Caption = 'Case Mode'
    TabOrder = 14
    object btnRepeatCase: TButton
      Left = 36
      Top = 49
      Width = 25
      Height = 25
      Caption = '='
      TabOrder = 0
      OnClick = btnRepeatCaseClick
    end
    object btnRunPrevCase: TButton
      Left = 6
      Top = 49
      Width = 25
      Height = 25
      Hint = 'run previous test case'
      Caption = '<'
      TabOrder = 1
      OnClick = btnRunPrevCaseClick
    end
    object btnRunNextCase: TButton
      Left = 66
      Top = 49
      Width = 25
      Height = 25
      Hint = 'run next test case'
      Caption = '>'
      TabOrder = 2
      OnClick = btnRunNextCaseClick
    end
    object btnRunFirstCase: TButton
      Left = 6
      Top = 21
      Width = 85
      Height = 25
      Caption = 'First Case'
      TabOrder = 3
      OnClick = btnRunFirstCaseClick
    end
  end
  object chkJump: TCheckBox
    Left = 277
    Top = 383
    Width = 81
    Height = 17
    Caption = 'Jump Minus'
    Checked = True
    State = cbChecked
    TabOrder = 15
  end
  object chkDiagnose: TCheckBox
    Left = 277
    Top = 355
    Width = 64
    Height = 17
    Caption = 'Diagnose'
    TabOrder = 16
    OnClick = chkDiagnoseClick
  end
end
