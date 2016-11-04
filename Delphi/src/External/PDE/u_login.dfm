object fo_login: Tfo_login
  Left = 0
  Top = 0
  Caption = 'Login f'#252'r PDE Datenbank'
  ClientHeight = 208
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 32
    Top = 48
    Width = 40
    Height = 16
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 32
    Top = 80
    Width = 66
    Height = 16
    Caption = 'Passwort:'
  end
  object ed_user: TEdit
    Left = 128
    Top = 40
    Width = 289
    Height = 24
    TabOrder = 0
    OnChange = ed_Change
  end
  object ed_password: TEdit
    Left = 128
    Top = 77
    Width = 289
    Height = 24
    PasswordChar = '*'
    TabOrder = 1
    OnChange = ed_Change
  end
  object but_OK: TBitBtn
    Left = 32
    Top = 128
    Width = 185
    Height = 41
    Caption = '&OK'
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = but_OKClick
  end
  object BitBtn2: TBitBtn
    Left = 248
    Top = 128
    Width = 169
    Height = 41
    Caption = '&Abbrechen'
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
end
