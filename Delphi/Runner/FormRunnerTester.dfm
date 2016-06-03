object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Test Runner'
  ClientHeight = 358
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblInclusive: TLabel
    Left = 136
    Top = 303
    Width = 25
    Height = 13
    Caption = 'Incl.:'
  end
  object lblExclusive: TLabel
    Left = 136
    Top = 332
    Width = 27
    Height = 13
    Caption = 'Excl.:'
  end
  object btnRunStepByNr: TButton
    Left = 64
    Top = 298
    Width = 64
    Height = 25
    Caption = 'RunStep'
    TabOrder = 0
    OnClick = btnRunStepByNrClick
  end
  object txtStepNr: TEdit
    Left = 8
    Top = 300
    Width = 49
    Height = 21
    TabOrder = 1
    Text = '895.68'
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 240
    Width = 404
    Height = 21
    TabOrder = 2
    Text = 'PST_BT_FT_PS_liste.txt'
  end
  object btnOpenScript: TButton
    Left = 418
    Top = 240
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnOpenScriptClick
  end
  object btnReadScript: TButton
    Left = 8
    Top = 267
    Width = 50
    Height = 25
    Caption = 'Read'
    TabOrder = 4
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 64
    Top = 267
    Width = 50
    Height = 25
    Caption = 'Save'
    TabOrder = 5
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 546
    Height = 224
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 6
  end
  object txtCase: TEdit
    Left = 8
    Top = 329
    Width = 49
    Height = 21
    TabOrder = 7
    Text = '11'
  end
  object btnRunCase: TButton
    Left = 64
    Top = 327
    Width = 64
    Height = 25
    Caption = 'RunCase'
    TabOrder = 8
    OnClick = btnRunCaseClick
  end
  object txtInclusive: TEdit
    Left = 167
    Top = 300
    Width = 92
    Height = 21
    Hint = 'inclusive test cases'
    TabOrder = 9
    Text = 'all'
  end
  object btnSequence: TButton
    Left = 265
    Top = 298
    Width = 81
    Height = 25
    Caption = 'Sequence'
    TabOrder = 10
    OnClick = btnSequenceClick
  end
  object txtExclusive: TEdit
    Left = 167
    Top = 329
    Width = 92
    Height = 21
    Hint = 'exclusive test cases'
    TabOrder = 11
    Text = '10-80'
  end
  object btnClear: TButton
    Left = 129
    Top = 267
    Width = 50
    Height = 25
    Caption = 'Clear'
    TabOrder = 12
    OnClick = btnClearClick
  end
  object chkForce: TCheckBox
    Left = 449
    Top = 242
    Width = 45
    Height = 17
    Caption = 'Force'
    TabOrder = 13
  end
  object chkAppend: TCheckBox
    Left = 500
    Top = 242
    Width = 54
    Height = 17
    Caption = 'Append'
    TabOrder = 14
  end
  object btnRunSequence: TButton
    Left = 265
    Top = 327
    Width = 32
    Height = 25
    Caption = 'Run'
    TabOrder = 15
    OnClick = btnRunSequenceClick
  end
  object btnRepeatSequence: TButton
    Left = 303
    Top = 327
    Width = 43
    Height = 25
    Caption = 'Repeat'
    TabOrder = 16
    OnClick = btnRepeatSequenceClick
  end
  object GroupBox1: TGroupBox
    Left = 352
    Top = 274
    Width = 97
    Height = 82
    Caption = 'Step Mode'
    TabOrder = 17
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
    Left = 457
    Top = 274
    Width = 97
    Height = 82
    Caption = 'Case Mode'
    TabOrder = 18
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
    Left = 265
    Top = 275
    Width = 81
    Height = 17
    Caption = 'Jump Minus'
    Checked = True
    State = cbChecked
    TabOrder = 19
    OnClick = chkJumpClick
  end
end
