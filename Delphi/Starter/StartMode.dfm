object frmStart: TfrmStart
  Left = 0
  Top = 0
  Caption = 'Pr'#252'fung Starter'
  ClientHeight = 273
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 120
    Top = 32
    Width = 147
    Height = 29
    Caption = 'PP ARS21xx'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 80
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Start'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 240
    Top = 224
    Width = 75
    Height = 25
    Caption = '&Cancel'
    TabOrder = 1
  end
  object RadioGroup1: TRadioGroup
    Left = 88
    Top = 96
    Width = 217
    Height = 97
    Caption = 'Systemauswahl zu  starten'
    TabOrder = 2
  end
  object RadioButton1: TRadioButton
    Left = 112
    Top = 119
    Width = 177
    Height = 26
    Caption = 'Aktuelles System'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    TabStop = True
  end
  object RadioButton2: TRadioButton
    Left = 112
    Top = 151
    Width = 177
    Height = 25
    Caption = 'Ersatzsystem'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
  end
end
