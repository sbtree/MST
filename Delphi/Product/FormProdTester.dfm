object frmProdTester: TfrmProdTester
  Left = 0
  Top = 0
  Caption = 'ProductTester'
  ClientHeight = 510
  ClientWidth = 858
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
  object trvProduct: TTreeView
    Left = 8
    Top = 8
    Width = 413
    Height = 417
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = trvProductChange
  end
  object txtConfigFile: TEdit
    Left = 8
    Top = 436
    Width = 317
    Height = 21
    TabOrder = 1
    Text = 'ARS21xx.INI'
  end
  object btnOpenIni: TButton
    Left = 331
    Top = 436
    Width = 24
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnOpenIniClick
  end
  object btnLoad: TButton
    Left = 366
    Top = 436
    Width = 55
    Height = 21
    Caption = '&Load'
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object chkShowAll: TCheckBox
    Left = 744
    Top = 475
    Width = 105
    Height = 17
    Caption = 'Show Full Settings'
    TabOrder = 4
    OnClick = chkShowAllClick
  end
  object chkFilter: TCheckBox
    Left = 72
    Top = 475
    Width = 45
    Height = 17
    Caption = 'Filter:'
    TabOrder = 5
    OnClick = chkFilterClick
  end
  object txtFilter: TEdit
    Left = 127
    Top = 473
    Width = 198
    Height = 21
    Enabled = False
    TabOrder = 6
    OnExit = txtFilterExit
    OnKeyPress = txtFilterKeyPress
  end
  object lsvConfig: TListView
    Left = 440
    Top = 8
    Width = 410
    Height = 449
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Value'
        Width = 250
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 7
    ViewStyle = vsReport
  end
  object chkSorted: TCheckBox
    Left = 681
    Top = 475
    Width = 57
    Height = 17
    Caption = 'Sorted'
    TabOrder = 8
    OnClick = chkSortedClick
  end
  object cmbFilterName: TComboBox
    Left = 331
    Top = 473
    Width = 90
    Height = 21
    Enabled = False
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 9
    Text = 'ID_STRING'
    OnChange = cmbFilterNameChange
    Items.Strings = (
      'ID_STRING'
      'TYP_CODE'
      'TYP_NAME'
      'CUSTOMER_ID'
      'Description'
      'PS_LIST'
      'ID_PDE_PRUEFPLATZ'
      'ID_PDE_PRODUKTART')
  end
  object btnCollapse: TButton
    Left = 8
    Top = 474
    Width = 20
    Height = 20
    Caption = '+'
    TabOrder = 10
    OnClick = btnCollapseClick
  end
  object btnExpand: TButton
    Left = 34
    Top = 474
    Width = 20
    Height = 20
    Caption = '-'
    TabOrder = 11
    OnClick = btnExpandClick
  end
end
