object FormFTMain: TFormFTMain
  Left = 0
  Top = 0
  Caption = 'FunctionTester'
  ClientHeight = 364
  ClientWidth = 630
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
  object lblFuncName: TLabel
    Left = 8
    Top = 328
    Width = 75
    Height = 13
    Caption = 'Function Name:'
  end
  object lblFuncPara: TLabel
    Left = 277
    Top = 328
    Width = 54
    Height = 13
    Caption = 'Parameter:'
  end
  object btnTest: TButton
    Left = 564
    Top = 323
    Width = 58
    Height = 25
    Caption = '&Test'
    TabOrder = 0
    OnClick = btnTestClick
  end
  object memInfo: TMemo
    Left = 8
    Top = 8
    Width = 614
    Height = 265
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object txtScriptFile: TEdit
    Left = 8
    Top = 287
    Width = 409
    Height = 21
    TabOrder = 2
    Text = '..\Script\PST_BT_FT_PS_liste.txt'
  end
  object btnOpenScript: TButton
    Left = 419
    Top = 287
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnOpenScriptClick
  end
  object txtFuncName: TEdit
    Left = 85
    Top = 325
    Width = 180
    Height = 21
    TabOrder = 4
    Text = 'JumpIfTrue'
  end
  object txtFuncPara: TEdit
    Left = 332
    Top = 325
    Width = 226
    Height = 21
    TabOrder = 5
    Text = '3>4 PS_41.00'
  end
  object btnReadScript: TButton
    Left = 449
    Top = 285
    Width = 50
    Height = 25
    Caption = 'Read'
    TabOrder = 6
    OnClick = btnReadScriptClick
  end
  object btnGoTo: TButton
    Left = 508
    Top = 285
    Width = 50
    Height = 25
    Caption = 'Go to'
    TabOrder = 7
    OnClick = btnGoToClick
  end
  object txtStepNr: TEdit
    Left = 564
    Top = 287
    Width = 58
    Height = 21
    TabOrder = 8
    Text = '11.02'
  end
end
