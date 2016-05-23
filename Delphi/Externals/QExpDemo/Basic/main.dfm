object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'QExp basic demo'
  ClientHeight = 262
  ClientWidth = 575
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lblResult: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 244
    Width = 565
    Height = 13
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    WordWrap = True
    ExplicitTop = 242
    ExplicitWidth = 3
  end
  object mmScript: TMemo
    Left = 8
    Top = 8
    Width = 337
    Height = 208
    TabOrder = 0
  end
  object Button3: TButton
    Left = 351
    Top = 191
    Width = 75
    Height = 25
    Caption = 'ExecScript'
    TabOrder = 1
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 351
    Top = 8
    Width = 194
    Height = 177
    Caption = 'Predefine'
    TabOrder = 2
    object Button1: TButton
      Left = 14
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Assign'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 14
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Math'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button4: TButton
      Left = 14
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Compare'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 14
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Bit Operation'
      TabOrder = 3
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 96
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Logical'
      TabOrder = 4
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 96
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Complex'
      TabOrder = 5
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 96
      Top = 80
      Width = 75
      Height = 25
      Caption = 'Function'
      TabOrder = 6
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 95
      Top = 111
      Width = 75
      Height = 25
      Caption = 'User Function'
      TabOrder = 7
      OnClick = Button9Click
    end
    object Button10: TButton
      Left = 14
      Top = 143
      Width = 75
      Height = 25
      Caption = 'ChineseVars'
      TabOrder = 8
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 95
      Top = 142
      Width = 75
      Height = 25
      Caption = 'Block Function'
      TabOrder = 9
      OnClick = Button11Click
    end
  end
  object Button12: TButton
    Left = 432
    Top = 191
    Width = 118
    Height = 25
    Caption = 'Internal Functions'
    TabOrder = 3
    OnClick = Button12Click
  end
end
