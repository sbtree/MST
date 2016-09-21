object frmProdTester: TfrmProdTester
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ProductTester'
  ClientHeight = 576
  ClientWidth = 860
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
    Height = 457
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnChange = trvProductChange
  end
  object txtConfigFile: TEdit
    Left = 8
    Top = 481
    Width = 287
    Height = 21
    TabOrder = 1
    Text = '..\config\ARS21xx.INI'
  end
  object btnOpenIni: TButton
    Left = 296
    Top = 481
    Width = 19
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnOpenIniClick
  end
  object btnLoad: TButton
    Left = 321
    Top = 481
    Width = 47
    Height = 21
    Caption = '&Load'
    TabOrder = 3
    OnClick = btnLoadClick
  end
  object chkShowAll: TCheckBox
    Left = 770
    Top = 483
    Width = 79
    Height = 17
    Caption = 'Full Settings'
    TabOrder = 4
    OnClick = chkShowAllClick
  end
  object chkFilter: TCheckBox
    Left = 72
    Top = 514
    Width = 45
    Height = 17
    Caption = 'Filter:'
    TabOrder = 5
    OnClick = chkFilterClick
  end
  object txtFilter: TEdit
    Left = 127
    Top = 512
    Width = 188
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
    Height = 457
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
    OnCustomDrawItem = lsvConfigCustomDrawItem
    OnSelectItem = lsvConfigSelectItem
  end
  object chkSorted: TCheckBox
    Left = 712
    Top = 483
    Width = 52
    Height = 17
    Caption = 'Sorted'
    TabOrder = 8
    OnClick = chkSortedClick
  end
  object cmbFilterName: TComboBox
    Left = 321
    Top = 512
    Width = 100
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 9
    Text = 'ID_STRING'
    OnChange = cmbFilterNameChange
    Items.Strings = (
      'ID_STRING'
      'TYP_CODE'
      'TYP_NAME'
      'CUSTOMER_ID'
      'PS_LIST'
      'BOOTLOADER_0001'
      'FIRMWARE_0001'
      'PARA_0001'
      'TX_FILE_0001'
      'Description'
      'ID_PDE_PRUEFPLATZ'
      'ID_PDE_PRODUKTART')
  end
  object btnCollapse: TButton
    Left = 8
    Top = 513
    Width = 20
    Height = 20
    Caption = '+'
    TabOrder = 10
    OnClick = btnCollapseClick
  end
  object btnExpand: TButton
    Left = 34
    Top = 513
    Width = 20
    Height = 20
    Caption = '-'
    TabOrder = 11
    OnClick = btnExpandClick
  end
  object btnClean: TButton
    Left = 440
    Top = 481
    Width = 41
    Height = 21
    Caption = 'Clean'
    TabOrder = 12
    OnClick = btnCleanClick
  end
  object btnCleanAll: TButton
    Left = 487
    Top = 481
    Width = 58
    Height = 21
    Caption = 'Clean All'
    TabOrder = 13
    OnClick = btnCleanAllClick
  end
  object btnSave: TButton
    Left = 374
    Top = 481
    Width = 47
    Height = 21
    Caption = '&Save'
    TabOrder = 14
    OnClick = btnSaveClick
  end
  object txtCurConfig: TEdit
    Left = 8
    Top = 545
    Width = 109
    Height = 21
    TabOrder = 15
  end
  object txtRefConfig: TEdit
    Left = 141
    Top = 545
    Width = 260
    Height = 21
    TabOrder = 16
  end
  object btnMoveText: TButton
    Left = 116
    Top = 545
    Width = 24
    Height = 21
    Caption = '->'
    Default = True
    TabOrder = 17
    OnClick = btnMoveTextClick
  end
  object btnUpdate: TButton
    Left = 551
    Top = 481
    Width = 54
    Height = 21
    Caption = '&Update'
    TabOrder = 18
    OnClick = btnUpdateClick
  end
  object btnMove: TButton
    Left = 490
    Top = 545
    Width = 50
    Height = 21
    Caption = '&Move To'
    TabOrder = 19
    OnClick = btnMoveClick
  end
  object btnNew: TButton
    Left = 440
    Top = 545
    Width = 50
    Height = 21
    Caption = '&New'
    TabOrder = 20
    OnClick = btnNewClick
  end
  object btnRemove: TButton
    Left = 590
    Top = 545
    Width = 50
    Height = 21
    Caption = '&Remove'
    TabOrder = 21
    OnClick = btnRemoveClick
  end
  object txtVarNames: TEdit
    Left = 440
    Top = 512
    Width = 391
    Height = 21
    Hint = 'names of selected variables'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 22
  end
  object btnPromote: TButton
    Left = 540
    Top = 545
    Width = 50
    Height = 21
    Caption = 'Up&grade'
    TabOrder = 23
    OnClick = btnPromoteClick
  end
  object btnClearVarNames: TButton
    Left = 830
    Top = 513
    Width = 20
    Height = 20
    Hint = 'clear the names of selected variables'
    Caption = 'X'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 24
    OnClick = btnClearVarNamesClick
  end
  object btnDefault: TButton
    Left = 640
    Top = 545
    Width = 50
    Height = 21
    Caption = '&Default'
    TabOrder = 25
    OnClick = btnDefaultClick
  end
  object btnClearRef: TButton
    Left = 401
    Top = 546
    Width = 20
    Height = 20
    Hint = 'clear the names of selected variables'
    Caption = 'X'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 26
    OnClick = btnClearRefClick
  end
end
