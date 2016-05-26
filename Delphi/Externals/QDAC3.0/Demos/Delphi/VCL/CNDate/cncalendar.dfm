object frmCnCalendar: TfrmCnCalendar
  Left = 0
  Top = 0
  BorderStyle = bsNone
  BorderWidth = 1
  Caption = 'frmCnCalendar'
  ClientHeight = 335
  ClientWidth = 456
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 192
    Top = 16
    Width = 24
    Height = 12
    Caption = '2009'
    Font.Charset = GB2312_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
  end
  object dgCalendar: TDrawGrid
    Tag = 99
    AlignWithMargins = True
    Left = 1
    Top = 41
    Width = 454
    Height = 253
    Margins.Left = 1
    Margins.Top = 0
    Margins.Right = 1
    Margins.Bottom = 0
    Align = alClient
    BorderStyle = bsNone
    ColCount = 7
    DefaultRowHeight = 35
    DefaultDrawing = False
    FixedColor = 4210816
    FixedCols = 0
    RowCount = 7
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
    ParentShowHint = False
    PopupMenu = pmExtends
    ScrollBars = ssNone
    ShowHint = True
    TabOrder = 0
    OnClick = dgCalendarClick
    OnDblClick = dgCalendarDblClick
    OnDrawCell = dgCalendarDrawCell
    OnMouseMove = dgCalendarMouseMove
    OnSelectCell = dgCalendarSelectCell
    ExplicitLeft = 0
    ExplicitWidth = 457
    ExplicitHeight = 255
  end
  object pnlTitle: TPanel
    Tag = 99
    Left = 0
    Top = 0
    Width = 456
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = clActiveCaption
    ParentBackground = False
    TabOrder = 1
    ExplicitWidth = 457
    object lblYear: TLabel
      Left = 163
      Top = 15
      Width = 28
      Height = 12
      Alignment = taRightJustify
      Caption = '2009'
      Font.Charset = GB2312_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = lblYearClick
    end
    object Label3: TLabel
      Left = 197
      Top = 15
      Width = 13
      Height = 12
      Caption = #24180
      Font.Charset = GB2312_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMonth: TLabel
      Left = 218
      Top = 15
      Width = 14
      Height = 12
      Caption = '09'
      Font.Charset = GB2312_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
      PopupMenu = pmMonthes
      OnClick = lblMonthClick
    end
    object lblCnAnimal: TLabel
      Left = 56
      Top = 15
      Width = 84
      Height = 12
      Alignment = taRightJustify
      AutoSize = False
      Font.Charset = GB2312_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
    end
    object sbPriorMonth: TSpeedButton
      Left = 11
      Top = 5
      Width = 32
      Height = 32
      Caption = '3'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnFace
      Font.Height = -24
      Font.Name = 'Marlett'
      Font.Style = []
      ParentFont = False
      OnClick = sbPriorMonthClick
    end
    object sbNextMonth: TSpeedButton
      Left = 338
      Top = 5
      Width = 32
      Height = 32
      Caption = '4'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnFace
      Font.Height = -24
      Font.Name = 'Marlett'
      Font.Style = []
      ParentFont = False
      OnClick = sbNextMonthClick
    end
    object pnlTypes: TPanel
      Tag = 99
      Left = 370
      Top = 0
      Width = 86
      Height = 41
      Align = alRight
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      ExplicitLeft = 371
      object cbxTypes: TComboBox
        Left = 8
        Top = 11
        Width = 73
        Height = 21
        BevelInner = bvNone
        BevelOuter = bvNone
        Style = csDropDownList
        ImeName = #20013#25991' - QQ'#20116#31508#36755#20837#27861
        ItemIndex = 0
        TabOrder = 0
        Text = #38452#38451#21382
        OnChange = cbxTypesChange
        Items.Strings = (
          #38452#38451#21382
          #38451#38452#21382
          #38451#21382
          #38452#21382)
      end
    end
    object cseYear: TSpinEdit
      Left = 109
      Top = 11
      Width = 48
      Height = 22
      MaxValue = 2049
      MinValue = 1901
      TabOrder = 1
      Value = 2009
      Visible = False
      OnChange = cseYearChange
    end
  end
  object pnlToday: TPanel
    Tag = 99
    Left = 0
    Top = 294
    Width = 456
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 296
    ExplicitWidth = 457
    object lblToday: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 12
      Align = alClient
      Alignment = taCenter
      Font.Charset = GB2312_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      OnDblClick = lblTodayDblClick
    end
  end
  object pmMonthes: TPopupMenu
    Left = 103
    Top = 10
  end
  object tmHintTimer: TTimer
    OnTimer = tmHintTimerTimer
    Left = 76
    Top = 8
  end
  object pmExtends: TPopupMenu
    Left = 203
    Top = 31
  end
end