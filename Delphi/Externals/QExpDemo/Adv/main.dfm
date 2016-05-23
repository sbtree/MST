object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QExp Advance Demo'
  ClientHeight = 316
  ClientWidth = 537
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 316
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 328
    Top = 0
    Width = 209
    Height = 316
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnCreateButton: TButton
      Left = 14
      Top = 8
      Width = 180
      Height = 25
      Caption = 'Object(TButton)'
      TabOrder = 0
      OnClick = btnCreateButtonClick
    end
    object btnFuncAssign: TButton
      Left = 14
      Top = 40
      Width = 180
      Height = 25
      Caption = 'Function Assign'
      TabOrder = 1
      OnClick = btnFuncAssignClick
    end
    object btnJSFuncAssign: TButton
      Left = 14
      Top = 72
      Width = 180
      Height = 25
      Caption = 'JS Style Function Assign(No Name)'
      TabOrder = 2
      OnClick = btnJSFuncAssignClick
    end
    object btnBlockAssign: TButton
      Left = 14
      Top = 103
      Width = 180
      Height = 25
      Caption = 'Block Assign'
      TabOrder = 3
      OnClick = btnBlockAssignClick
    end
    object btnCallScriptFunction: TButton
      Left = 14
      Top = 134
      Width = 180
      Height = 25
      Caption = 'Call Script Function'
      TabOrder = 4
      OnClick = btnCallScriptFunctionClick
    end
    object btnVarArgs: TButton
      Left = 14
      Top = 165
      Width = 180
      Height = 25
      Caption = 'Var Args'
      TabOrder = 5
      OnClick = btnVarArgsClick
    end
  end
end
