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
    Left = 449
    Top = 274
    Width = 25
    Height = 13
    Caption = 'Incl.:'
  end
  object lblExclusive: TLabel
    Left = 449
    Top = 301
    Width = 27
    Height = 13
    Caption = 'Excl.:'
  end
  object btnRunStepByNr: TButton
    Left = 321
    Top = 267
    Width = 75
    Height = 25
    Caption = 'RunStepByNr'
    TabOrder = 0
    OnClick = btnRunStepByNrClick
  end
  object txtStepNr: TEdit
    Left = 266
    Top = 269
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
  end
  object btnReadScript: TButton
    Left = 8
    Top = 267
    Width = 75
    Height = 25
    Caption = 'ReadScript'
    TabOrder = 4
    OnClick = btnReadScriptClick
  end
  object btnSaveScript: TButton
    Left = 89
    Top = 267
    Width = 75
    Height = 25
    Caption = 'SaveScript'
    TabOrder = 5
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 546
    Height = 224
    TabOrder = 6
  end
  object txtField: TEdit
    Left = 8
    Top = 298
    Width = 74
    Height = 21
    TabOrder = 7
    Text = 'FKT'
  end
  object btnGetField: TButton
    Left = 89
    Top = 298
    Width = 90
    Height = 25
    Caption = 'GetField'
    TabOrder = 8
    OnClick = btnGetFieldClick
  end
  object btnPrevious: TButton
    Left = 410
    Top = 267
    Width = 16
    Height = 25
    Hint = 'previous test step'
    Caption = '<'
    TabOrder = 9
  end
  object btnNext: TButton
    Left = 426
    Top = 267
    Width = 16
    Height = 25
    Hint = 'next test step'
    Caption = '>'
    TabOrder = 10
  end
  object txtCase: TEdit
    Left = 266
    Top = 298
    Width = 96
    Height = 21
    TabOrder = 11
    Text = '11'
  end
  object btnRunCase: TButton
    Left = 368
    Top = 298
    Width = 74
    Height = 25
    Caption = 'RunCase'
    TabOrder = 12
  end
  object txtInclusive: TEdit
    Left = 480
    Top = 271
    Width = 74
    Height = 21
    Hint = 'inclusive test cases'
    TabOrder = 13
    Text = 'all'
  end
  object btnSequence: TButton
    Left = 449
    Top = 327
    Width = 105
    Height = 25
    Caption = 'RunSequence'
    TabOrder = 14
  end
  object txtExclusive: TEdit
    Left = 480
    Top = 297
    Width = 74
    Height = 21
    Hint = 'exclusive test cases'
    TabOrder = 15
    Text = '10-80'
  end
  object txtVariable: TEdit
    Left = 8
    Top = 329
    Width = 74
    Height = 21
    TabOrder = 16
    Text = 'Include'
  end
  object btnVariable: TButton
    Left = 88
    Top = 327
    Width = 90
    Height = 25
    Caption = 'GetVariable'
    TabOrder = 17
  end
  object btnClear: TButton
    Left = 167
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 18
  end
  object chkForce: TCheckBox
    Left = 449
    Top = 242
    Width = 45
    Height = 17
    Caption = 'Force'
    TabOrder = 19
  end
  object chkAppend: TCheckBox
    Left = 500
    Top = 242
    Width = 54
    Height = 17
    Caption = 'Append'
    TabOrder = 20
  end
end
