unit MtxGUIWrapper;

// =============================================================================
// =============================================================================
//
//     Module name       : $RCSfile: MtxGUIWrapper.pas,v $
//     Actual version    : $Revision: 1.38 $
//     Short description : Wrapper für diverse GUI-Komponenten
//                         MSC setzt die TntLibrary ein. Aus diesen Komponenten
//                         werden Metronix-Komponenten erstellt (GUI-Wrapper),
//                         so dass Anpassungen einfacher durchgeführt werden können.
//     Project           : ServoCommander
//     Compiler          : Delphi 2007
//     First author      : 2013-12-20 /gsv/
//     Copyright         : (c) Metronix GmbH 2013
// -----------------------------------------------------------------------------
//     History           : --
// -----------------------------------------------------------------------------
//     Descriptions
// =============================================================================

{$INCLUDE MtxCompilers.inc}

{$TYPEINFO ON}

interface

{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
uses Windows, Types, Classes, Commctrl, Messages, ComCtrls, Forms, RTLConsts,
     Graphics, UxTheme, Themes, PngBitBtn, Buttons, PngSpeedButton, MtxTypes,
     Controls, PngImage, PngFunctions, PngImageList, TypInfo, Consts,
     StdCtrls, ExtCtrls, Menus, Grids, System.UITypes, SysUtils, StrUtils,
     System.Generics.Collections;
{$ELSE}
uses Windows, Types, Classes, Commctrl, Messages, ComCtrls, Forms, RTLConsts,
     Graphics, UxTheme, Themes, Buttons, MtxTypes, Controls, Consts,
     TypInfo, TntStdCtrls, TntControls, TntClasses, TntExtCtrls,
     TntButtons, TntMenus, TntComCtrls, TntForms, TntGrids, PngImage,
     PngFunctions, PngImageList, PngBitBtn, PngSpeedButton, SysUtils, StrUtils,
     System.Generics.Collections;
{$ENDIF}



const
  MAPVK_VK_TO_VSC    = 0;
  {$EXTERNALSYM MAPVK_VK_TO_VSC}
  MAPVK_VSC_TO_VK    = 1;
  {$EXTERNALSYM MAPVK_VSC_TO_VK}
  MAPVK_VK_TO_CHAR   = 2;
  {$EXTERNALSYM MAPVK_VK_TO_CHAR}
  MAPVK_VSC_TO_VK_EX = 3;
  {$EXTERNALSYM MAPVK_VSC_TO_VK_EX}
  MAPVK_VK_TO_VSC_EX = 4;
  {$EXTERNALSYM MAPVK_VK_TO_VSC_EX}

  C_COL_MTX_INFO_PANEL_BLUE = $00A4671E;    // 2016-02-01 /who/ Blaue Farbe für Infopanele
  C_COL_MTX_ERROR_RED       = $00000DC5;    // 2016-02-04 /who/ Rote Farbe zum Signalisieren von Fehlern
  C_COL_MTX_OK_GREEN        = $00009952;    // 2016-02-04 /who/ Grüne Farbe zum Signalisieren von Fehlerfreiheit
  C_COL_MTX_ACT_GREEN       = clMoneyGreen; // 2016-05-19 /who/ Grüne Farbe für Istwertepanels
  C_COL_MTX_YELLOW          = clYellow;     // 2016-04-26 /gsv/: Konstante für gelbe Farbe definiert, Farbcode muss noch festgelegt werden!

  C_RG_BUTTON_FIXED_WIDTH_DEF_VAL = 50;     // Default-Wert der Eigenschaft TCustomRadioGroup.ButtonFixedWidth

  // Konstanten für Windows Messages
  WM_MTX_FORM_STARTUP       = WM_USER;      // Message: Form erzeugt und angezeigt.
  WM_MTX_USER               = WM_USER+10;


// ===========================================================================
// Klasse: TMtxGUIIntegerList
// Integer-Liste für published Properties in GUI-Komponenten
// ===========================================================================
  type TMtxGUIIntegerList = class ( TPersistent )
    protected
      FList     : TList<Integer>;
      FUpdating : Integer;
      FUpdated  : Boolean;
      FOnChange : TNotifyEvent;
      function  GetItems(nIndex: Integer): Integer; virtual;
      procedure SetItems(nIndex: Integer; const Value: Integer); virtual;
      procedure DefineProperties(Filer: TFiler); override;
      procedure ReadData(Reader: TReader); virtual;
      procedure WriteData(Writer: TWriter); virtual;
      procedure ListNotifyHandler ( Sender     : TObject;
                                    const Item : Integer;
                                    Action     : TCollectionNotification);
      procedure   TriggerOnChange; virtual;
      function    getCount : Integer; virtual;
    public
      constructor Create(); virtual;
      destructor  Destroy(); override;
      function    Add ( const Value : Integer ) : Integer; virtual;
      procedure   Assign ( Source : TPersistent ); override;
      procedure   BeginUpdate(); virtual;
      procedure   EndUpdate(); virtual;
      procedure   Clear(); virtual;
      procedure   Delete ( Index: Integer ); virtual;
      procedure   FromString ( const S : String ); virtual;
      function    IndexOf ( const Value: Integer ): Integer; virtual;
      procedure   Insert ( Index: Integer; const Value: Integer ); virtual;
      function    Remove ( const Value: Integer ): Integer; virtual;
      function    ToString(): String; override;

      property    Count : Integer read getCount;
      property    Items[nIndex : Integer] : Integer read  GetItems  write SetItems; default;
      property    OnChange     : TNotifyEvent       read  FOnChange write FOnChange;
  end;

// ===========================================================================
// Klasse: TMtxCustomControl
// Wrapper-Klasse, um einige protected Eigenschaften als public zu definieren.
// ===========================================================================
type TMtxWinControl = class ( TWinControl )
  public
    property Color;
    property ParentBackground;
    property ParentColor;
end;


// ===========================================================================
// Klasse: TMtxLabel
// Label-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxLabel          = class ( TLabel )
{$ELSE}
  type TMtxLabel          = class ( TTntLabel )
{$ENDIF}
    private
      FTrailingSpaceAdded : boolean; // Flag: Leerzeichen am Ende des Textes angefügt?
      FBorder             : boolean; // Flag: Rahmen um das Label zeichnen?
      FOnPaint            : TNotifyEvent;
      FAutoAppendColon    : boolean; // 2016-09-28 /gsv/: Flag: ":" ans Ende der Caption automatisch hinzufügen?
      FPadding            : TPadding;
    protected
      procedure Paint; override;
      procedure DoDrawText ( var Rect: TRect; Flags: Longint ); override;

      // Caption-Property der Basisklasse überschreiben
      function  GetCaption(): TWideCaption;
      procedure SetCaption ( const Value: TWideCaption );

      // Getter/Setter für die Property Border
      function  getBorder() : boolean;
      procedure setBorder ( newValue : boolean );

      procedure setAutoAppendColon ( newValue : boolean );

      procedure setPadding ( const Value: TPadding );
      procedure doPaddingChange ( Sender: TObject );

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

    published
      // 2016-09-28 /gsv/: Flag: ":" ans Ende der Caption automatisch hinzufügen?
      property AutoAppendColon : boolean      read FAutoAppendColon write setAutoAppendColon default false;

      // Caption-Property der Basisklasse überschreiben
      property Caption         : TWideCaption read GetCaption       write SetCaption;

      // Flag: Rahmen um das Label zeichnen?
      property Border          : boolean      read getBorder        write setBorder       default false;

      // OnPaint-Ereignis
      property OnPaint         : TNotifyEvent read FOnPaint         write FOnPaint;

      property Padding         : TPadding     read FPadding         write SetPadding;
  end; // type TMtxLabel    = class ( TTntLabel )





// ===========================================================================
// Klasse: TTntStaticText
// StaticText-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxStaticText     = class ( TStaticText )
{$ELSE}
  type TMtxStaticText     = class ( TTntStaticText )
{$ENDIF}
  end; // type TMtxStaticText     = class ( TTntStaticText )


// ===========================================================================
// Klasse: TMtxCustomEdit
// Edit-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxCustomEdit     = class ( TCustomEdit )
{$ELSE}
  type TMtxCustomEdit     = class ( TTntCustomEdit )
{$ENDIF}
  end; // type TMtxCustomEdit = class ( TTntCustomEdit )

// ===========================================================================
// Klasse: TMtxEdit
// Edit-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxEdit           = class ( TEdit )
{$ELSE}
  type TMtxEdit           = class ( TTntEdit )
{$ENDIF}
  end; // type TMtxEdit           = class ( TTntEdit )


// ===========================================================================
// Klasse: TMtxRichEdit
// RichEdit-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxRichEdit       = class ( TRichEdit )
{$ELSE}
  type TMtxRichEdit       = class ( TTntRichEdit )
{$ENDIF}
  end; // type TMtxRichEdit       = class ( TTntRichEdit )


// ===========================================================================
// Klasse: TMtxMemo
// Memo-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxMemo           = class ( TMemo )
{$ELSE}
  type TMtxMemo           = class ( TTntMemo )
{$ENDIF}
  end; // type TMtxMemo           = class ( TTntMemo )


// ===========================================================================
// Klasse: TMtxListBox
// ListBox-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxListBox        = class ( TListBox )
{$ELSE}
  type TMtxListBox        = class ( TTntListBox )
{$ENDIF}
  end; // type TMtxListBox        = class ( TTntListBox )


// ===========================================================================
// Klasse: TMtxTreeNode
// TreeNode-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxTreeNode       = TTreeNode;
{$ELSE}
  type TMtxTreeNode       = TTntTreeNode;
{$ENDIF}



// ===========================================================================
// Klasse: TMtxTreeView
// TreeView-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxTreeView       = class ( TTreeView )
{$ELSE}
  type TMtxTreeView       = class ( TTntTreeView )
{$ENDIF}
  end; // type TMtxTreeView       = class ( TTntTreeView )


// ===========================================================================
// Klasse: TMtxListView
// ListView-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxListView       = class ( TListView )
{$ELSE}
  type TMtxListView       = class ( TTntListView )
{$ENDIF}
  end; // type TMtxListView       = class ( TTntListView )


// ===========================================================================
// Klasse: TMtxComboBox
// ComboBox-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxComboBox       = class ( TComboBox )
{$ELSE}
  type TMtxComboBox       = class ( TTntComboBox )
{$ENDIF}
  end; // type TMtxComboBox       = class ( TTntComboBox )

// ===========================================================================
// Klasse: TMtxComboBoxEx
// ComboBoxEx-Komponente
// ===========================================================================
  type TMTxComboExItemType =
  (
    enComboItemExStandard, // Standard-Eintrag
    enComboItemExHeading,  // Überschrift, der Eintrag kann vom Benutzer nicht ausgewählt werden.
    enComboItemExSubling   // "Unterknoten"

  );
  type TMtxComboExItemData = class
    private
      FUserData  : TCustomData;
      FItemType  : TMTxComboExItemType;
    public
      constructor Create(); overload;
      constructor Create ( item_type : TMTxComboExItemType ); overload;
      constructor Create ( user_data : TCustomData; item_type : TMTxComboExItemType ); overload;
      //destructor  Destroy();

      property UserData : TCustomData         read FUserData write FUserData;
      property ItemType : TMTxComboExItemType read FItemType write FItemType;
  end;

  type TMtxComboBoxEx     = class ( TComboBoxEx )
    private
      FOldItemIndex : integer;
    protected
      function GetItemHt(): Integer; override;

      procedure DoBeforeItemChange ( oldIndex, newIndex : integer; var changeAllowed : boolean );

      function  IsItemSelectable ( inx : integer ) : boolean;
      function  IsNextItemSelectable(): boolean;
      function  IsPrevItemSelectable(): boolean;
      procedure SelectNextSelectableItem();
      procedure SelectPrevSelectableItem();

      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      function  isItemIndexValid ( inx : integer ) : boolean;
    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;
      procedure   KeyPress(var Key: Char); override;
      function    AddItemEx ( const Caption: string; Data: TMtxComboExItemData ): TComboExItem; overload;
  end; // type TMtxComboBoxEx       = class ( TTntComboBoxEx )


// ===========================================================================
// Klasse: TMtxPanel
// Panel-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxPanel          = class ( TPanel )
{$ELSE}
  type TMtxPanel          = class ( TTntPanel )
{$ENDIF}
  end; // type TMtxPanel    = class ( TTntPanel )


// ===========================================================================
// Klasse: TMtxGroupBox
// GroupBox-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxGroupBox       = class ( TGroupBox )
{$ELSE}
  type TMtxGroupBox       = class ( TTntGroupBox )
{$ENDIF}
    protected
      procedure AdjustClientRect ( var Rect: TRect ); override;
    published
      // 2016-08-18 /gsv/: AutoSize-Property aktiviert
      property AutoSize;
  end; // type TMtxGroupBox = class ( TTntGroupBox )


// ===========================================================================
// Klasse: TMtxButton
// Button-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxButton         = class ( TButton )
{$ELSE}
  type TMtxButton         = class ( TTntButton )
{$ENDIF}
  end; // type TMtxButton   = class ( TTntButton )


// ===========================================================================
// Klasse: TMtxSpeedButton
// SpeedButton-Komponente
// ===========================================================================
{$IFDEF PNG_SUPPORT_ON}
  type TMtxSpeedButton    = class ( TPngSpeedButton )
  end; // type TMtxSpeedButton = class ( TTntSpeedButton )
{$ELSE}
  // Kein PNG-Support
  // Trotzdem hier die Eigenschaften PngImage und PngOptions definieren,
  // damit diese, falls in der dfm-Datei angegebenen sind, nicht automatisch
  // durch Delphi entfernt werden.
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxSpeedButton    = class ( TSpeedButton )
{$ELSE}
  type TMtxSpeedButton    = class ( TTntSpeedButton )
{$ENDIF}
    private
      FPngImage  : TPngImage;
      FPngOptions: TPngOptions;

    protected
      procedure SetPngImage   ( const Value: TPngImage  );
      procedure SetPngOptions ( const Value: TPngOptions );

    published
      property PngImage   : TPngImage  read FPngImage   write SetPngImage;
      property PngOptions : TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];

    public
      constructor Create ( AOwner: TComponent ); override;
      destructor  Destroy(); override;
  end; // type TMtxSpeedButton = class ( TTntSpeedButton )
{$ENDIF}


// ===========================================================================
// Klasse: TMtxBitBtn
// BitButton-Komponente
// ===========================================================================
{$IFDEF PNG_SUPPORT_ON}
  type TMtxBitBtn         = class ( TPngBitBtn )
    procedure Loaded(); override;
  end; // type TMtxBitBtn      = class ( TPngBitBtn )
{$ELSE}
  // Kein PNG-Support
  // Trotzdem hier die Eigenschaften PngImage und PngOptions definieren,
  // damit diese, falls in der dfm-Datei angegebenen sind, nicht automatisch
  // durch Delphi entfernt werden.
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxBitBtn    = class ( TBitBtn )
{$ELSE}
  type TMtxBitBtn    = class ( TTntBitBtn )
{$ENDIF}
    private
      FPngImage  : TPngImage;
      FPngOptions: TPngOptions;

    protected
      procedure SetPngImage   ( const Value: TPngImage  );
      procedure SetPngOptions ( const Value: TPngOptions );

    published
      property PngImage   : TPngImage  read FPngImage   write SetPngImage;
      property PngOptions : TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];

    public
      constructor Create ( AOwner: TComponent ); override;
      destructor  Destroy(); override;

      procedure Loaded(); override;
  end; // type TMtxBitBtn = class ( TTntBitBtn )
{$ENDIF}


// ===========================================================================
// Klasse: TMtxCheckbox
// Checkbox-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxCheckbox       = class ( TCheckBox )
{$ELSE}
  type TMtxCheckbox       = class ( TTntCheckBox )
{$ENDIF}
    private
      FUserData         : TObject;
      FReadOnly         : boolean;

      // Getter/Setter für die Property ReadOnly
      procedure setReadOnly ( AValue : boolean );

    protected
      procedure Toggle(); override;
    public
      constructor Create ( AOwner : TComponent ); override;
      // 2016-02-24 /gsv/: Beliebige Anwenderdaten
      property UserData : TObject read FUserData write FUserData;
    published
      // 2016-10-04 /gsv/: ReadOnly-Modus
      property ReadOnly : boolean read FReadOnly write setReadOnly default false;
  end; // type TMtxCheckbox = class ( TTntCheckBox )


// ===========================================================================
// Klasse: TMtxRadioButton
// RadioButton-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxRadioButton    = class ( TRadioButton )
{$ELSE}
  type TMtxRadioButton    = class ( TTntRadioButton )
{$ENDIF}
  end; // type TMtxRadioButton = class ( TTntRadioButton )


// ===========================================================================
// Klasse: TMtxCustomRadioGroup
// RadioGroup-Komponente
// ===========================================================================
  TMtxCustomRadioGroup = class(TCustomGroupBox)
  private
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FWordWrap: Boolean;
    FBorderDraw           : boolean; // Flag: Border zeichnen: ja/nein
    FButtonTopMargin      : integer; // Feste Top-Margin der Buttons (-1 = default/automatisch)
    FButtonFixedHeightUse : boolean; // Feste Höhe der Buttons verwenden? (abhängig von der Schriftart, Höhe nicht von außen einstellbar)
    FColumnsFixedWidthUse : boolean; // Feste Spaltenbreite verwenden?
    FColumnsFixedWidth    : TMtxGUIIntegerList; // Feste Spaltenbreite

    function GetButtons(Index: Integer): TRadioButton;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure SetWordWrap(Value: Boolean);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure setBorderDraw ( b_on : boolean );
    procedure setButtonTopMargin   ( ATopMargin : integer );
    procedure setButtonFixedHeightUse ( AUseFixedHeight : boolean );
    function  getButtonFixedHeight() : integer;
    procedure setColumnsFixedWidthUse ( AValue : boolean );
    function  getColumnsFixedWidthCount() : integer;
    procedure setColumnsFixedWidth    ( AValueList : TMtxGUIIntegerList );


  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function  CanModify: Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   FlipChildren(AllLevels: Boolean); override;
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure   Paint(); override;
    procedure   ClearColumnsFixedWidth();

    property    Buttons[Index: Integer]: TRadioButton read GetButtons;
    property    ColumnsFixedWidthCount : integer read getColumnsFixedWidthCount;

  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    // Flag: Border zeichnen: ja/nein
    property BorderDraw             : boolean read FBorderDraw           write setBorderDraw           default true;
    // Feste Top-Margin der Buttons (-1 = default/automatisch)
    property ButtonTopMargin        : integer read FButtonTopMargin      write setButtonTopMargin      default -1;
    // Feste Höhe der Buttons verwenden (abhängig von der aktuellen Schriftart, vonaußen nicht einstellbar)
    property ButtonFixedHeightUse   : boolean read FButtonFixedHeightUse write setButtonFixedHeightUse default false;
    // Rückgabe der ermittelten festen Button-Höhe (abhängig von der aktuellen Schriftart)
    property ButtonFixedHeight      : integer read getButtonFixedHeight;
    // Feste Breite der Buttons verwenden?
    property ColumnsFixedWidthUse   : boolean read FColumnsFixedWidthUse write setColumnsFixedWidthUse   default false;
    // Feste Breite der Buttons
    property ColumnsFixedWidth      : TMtxGUIIntegerList read FColumnsFixedWidth write setColumnsFixedWidth;
    property Font;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property Constraints;
    property ParentBiDiMode;
    property ParentBackground default True;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property StyleElements;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnStartDock;
    property OnStartDrag;
  end;


// ===========================================================================
// Klasse: TMtxRadioGroup
// RadioGroup-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxRadioGroup = class ( TMtxCustomRadioGroup )
{$ELSE}
  type TMtxRadioGroup = class ( TTntRadioGroup )
{$ENDIF}
  end; // type TMtxRadioGroup = class ( TRadioGroup )


// ===========================================================================
// Klasse: TMtxMainMenu
// MainMenu-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxMainMenu       = class ( TMainMenu )
{$ELSE}
  type TMtxMainMenu       = class ( TTntMainMenu )
{$ENDIF}
  end; // type TMtxMainMenu      = class ( TTntMainMenu )


// ===========================================================================
// Klasse: TMtxMenuItem
// MenuItem-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxMenuItem       = class ( TMenuItem )
{$ELSE}
  type TMtxMenuItem       = class ( TTntMenuItem )
{$ENDIF}
  end; // type TMtxMenuItem      = class ( TTntMenuItem )


// ===========================================================================
// Klasse: TMtxPopupMenu
// PopupMenu-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxPopupMenu      = class ( TPopupMenu )
{$ELSE}
  type TMtxPopupMenu      = class ( TTntPopupMenu )
{$ENDIF}
  end; // type TMtxPopupMenu      = class ( TTntPopupMenu )


// ===========================================================================
// Klasse: TMtxPageControl
// PageControl-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxPageControl    = class ( TPageControl )
{$ELSE}
  type TMtxPageControl    = class ( TTntPageControl )
{$ENDIF}
    published
      // 2016-09-02 /gsv/: AutoSize-Property aktiviert
      property AutoSize;
  end; // type TMtxPageControl      = class ( TTntPageControl )


// ===========================================================================
// Klasse: TTntTabControl
// TabControl-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxTabControl     = class ( TTabControl )
{$ELSE}
  type TMtxTabControl     = class ( TTntTabControl )
{$ENDIF}
  end; // type TMtxTabControl     = class ( TTntTabControl )


// ===========================================================================
// Klasse: TMtxTabSheet
// TabSheet-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxTabSheet       = class ( TTabSheet )
{$ELSE}
  type TMtxTabSheet       = class ( TTntTabSheet )
{$ENDIF}
    published
      // 2016-09-02 /gsv/: AutoSize-Property aktiviert
      property AutoSize;
  end; // type TMtxTabSheet      = class ( TTntTabSheet )


// ===========================================================================
// Klasse: TMtxSplitter
// Splitter-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxSplitter       = class ( TSplitter )
{$ELSE}
  type TMtxSplitter       = class ( TTntSplitter )
{$ENDIF}
  end; // type TMtxSplitter       = class ( TTntSplitter )


// ===========================================================================
// Klasse: TMtxForm
// Hilfsklasse, um auf die Font-Property zuzugreifen
// (In TControl ist die Font-Property protected
// ===========================================================================
  type TMtxControlPublicFont = class(TControl)
    public
      property Font;
  end;

// ===========================================================================
// Klasse: TMtxForm
// Form-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxForm           = class ( TForm )
{$ELSE}
  type TMtxForm           = class ( TTntForm )
{$ENDIF}
    protected
      procedure Loaded; override;
    public
      constructor Create ( AOwner : TComponent ); override;
  end; // type TMtxForm           = class ( TTntForm )


// ===========================================================================
// Klasse: TMtxStringGrid
// StringGrid-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxStringGrid     = class ( TStringGrid )
{$ELSE}
  type TMtxStringGrid     = class ( TTntStringGrid )
{$ENDIF}
  end; // type TMtxStringGrid     = class ( TTntStringGrid )


// ===========================================================================
// Klasse: TMtxImage
// Image-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxImage          = class ( TImage )
{$ELSE}
  type TMtxImage          = class ( TTntImage )
{$ENDIF}
  end; // type TMtxImage          = class ( TTntImage )

{$IFDEF PNG_SUPPORT_ON}
  type TMtxImageList      = class ( TPngImageList )
  end;
{$ELSE}
  type TMtxImageList      = class ( TImageList )
  end;
{$ENDIF}

// ===========================================================================
// Klasse: TMtxTrackBar
// TrackBar-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxTrackBar       = class ( TTrackBar )
{$ELSE}
  type TMtxTrackBar       = class ( TTntTrackBar )
{$ENDIF}
  end; // type TMtxTrackBar       = class ( TTntTrackBar )


// ===========================================================================
// Klasse: TMtxScrollBar
// ScrollBar-Komponente
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxScrollBar       = class ( TScrollBar )
{$ELSE}
  type TMtxScrollBar       = class ( TTntScrollBar )
{$ENDIF}
  end; // type TMtxScrollBar      = class ( TScrollBar )

// ===========================================================================
// Klasse: TMtxStrings
// StringListe
// ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  type TMtxStrings        = class ( TStrings )
{$ELSE}
  type TMtxStrings        = class ( TTntStrings )
{$ENDIF}
  end; // type TMtxStrings        = class ( TTntStrings )


// ===========================================================================
// Klasse: TMtxBalloonHint
// BallonHint-Komponente
// ===========================================================================
  type TMtxBalloonHint    = class
    protected
      FParent      : TWinControl;
      FHandle      : HWND;
      FIsShowing   : boolean;
      FText        : string;
      FTitle       : string;
      FTooltipInfo : TToolInfoW;

      procedure setText  ( ws : string );
      procedure setTitle ( ws : string );

    public
      constructor Create ( AParent : TWinControl );
      destructor  Destroy(); override;

      property  Handle    : HWND        read FHandle;
      property  IsShowing : boolean     read FIsShowing;
      property  Parent    : TWinControl read FParent;

      procedure Show();
      procedure Hide();

    published
      property Text  : string read FText  write setText;
      property Title : string read FTitle write setTitle;
  end; // type TMtxBalloonHint        = class


  // Prüft, ob das neue oder das alte Look&Feel verwendet wird
  function checkMtxUseNewLookAndFeel() : boolean;
  // Name  der zu verwendeten Schriftart ermitteln
  function getMtxDefaultFontName()  : string;
  // Name der monospaced Font
  function getMtxMonospacedFontName() : string;
  // Größe der zu verwendeten Schriftart ermitteln
  function getMtxDefaultFontSize()  : integer;
  // Stilart  der zu verwendeten Schriftart ermitteln
  function getMtxDefaultFontStyle() : TFontStyles;

  // Schriftart eines Fensters und des dazugehörigen Komponenten setzen
  procedure setFormAndControlsDefaultMtxFont ( mtx_form : TMtxForm );

  // Blaue Metronix-Farbe für Texte zurückgeben
  function getMtxDefaultColorBlueForText() : TColor;
  // Graue Metronix-Farbe für Texte zurückgeben
  function getMtxDefaultColorGrayForText() : TColor;

  // Blaue Metronix-Farbe zurückgeben
  function getMtxDefaultColorBlue()     : TColor;
  // Rote Metronix-Farbe zurückgeben
  function getMtxDefaultColorRed()      : TColor;
  // Grüne Metronix-Farbe zurückgeben
  function getMtxDefaultColorGreen()    : TColor;
  function getMtxDefaultColorActGreen() : Tcolor;
  // Gelbe Metronix-Farbe zurückgeben
  function getMtxDefaultColorYellow()   : TColor;
  // Textfarbe für Hintergrundfarbe auswählen
  function calcTxtColorForBackground(c: TColor): TColor;

implementation

uses MtxUtils;


// =============================================================================
//    Class        : --
//    Function     : checkMtxUseNewLookAndFeel()
//                   Prüft, ob das neue oder das alte Look&Feel verwendet wird
//                   Neues Look&Feel:
//                     PNG-Images mit Alpha-Kanal, OK/Abbruch-Buttons ohne Glyph,
//                     Schriftart "Segoe UI", nicht fett, Größe 9
//                   Altes Look&Feel:
//                     Bitmaps ohne Alpha-Kanal, OK/Abbruch-Buttons mit Glyph,
//                     Schriftart "Arial Unicode MS", fett, Größe 8
//    Parameter    : --
//    Return       : true  - Neues Look&Feel verwenden (s.o.)
//                   false - Altes Look&Feel verwenden (s.o.)
//    Exceptions   : --
//    First author : 2016-03-11 /gsv/
//    History      : --
// =============================================================================
function checkMtxUseNewLookAndFeel() : boolean;
begin
{$IFDEF DIS_2_FAMILY}
  // DIS-2 SC ==> Es wird immer die alte (fette) Schriftart verwendet.
  result := false;
{$ELSE}
  // MSC ==> Unterscheidung zwischen alter und neuer Schriftart anhand des
  // Compilerschalters PNG_SUPPORT_ON
  // PNG-Support gibt es erst mit dem neuen Look&Feel. Die Einführung weiterer
  // Compilerschalter wird das Ganze nur noch verkomplizieren.
  {$IFDEF PNG_SUPPORT_ON}
    // ==> Neues Look&Feel, also neue (nicht fette) Schriftart.
    result := true;
  {$ELSE}
    // ==> Altes Look&Feel, also alte (fette) Schriftart
    result := false;
  {$ENDIF}
{$ENDIF}
end;


// =============================================================================
//    Class        : --
//    Function     : getMtxDefaultFontName()
//                   Ermittelt den Namen der zu verwendeten Schriftart für die
//                   GUI-Komponenten
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2016-03-10 /gsv/
//    History      : --
// =============================================================================
function getMtxDefaultFontName()  : string;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := 'Segoe UI';
  end
  else
  begin
    // Altes Look&Feel
    result := 'Arial Unicode MS';
  end;
end; // function getMtxDefaultFontName()  : string;

// =============================================================================
//    Class        : --
//    Function     : getMtxMonospacedFontName()
//                   Ermittelt den Namen der zu verwendeten monospaced Schriftart 
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2016-03-23 /who/
//    History      : --
// =============================================================================
function getMtxMonospacedFontName() : string;
begin
  result := 'Courier New';
end;

// =============================================================================
//    Class        : --
//    Function     : getMtxDefaultFontName()
//                   Ermittelt die Größe der zu verwendeten Schriftart für die
//                   GUI-Komponenten
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2016-03-10 /gsv/
//    History      : --
// =============================================================================
function getMtxDefaultFontSize() : integer;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := 9;
  end
  else
  begin
    // Altes Look&Feel
    result := 8;
  end;
end; // function getMtxDefaultFontSize() : integer;


// =============================================================================
//    Class        : --
//    Function     : getMtxDefaultFontStyle()
//                   Ermittelt die Stilart/Formatierung der zu verwendeten
//                   Schriftart für die GUI-Komponenten
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2016-03-10 /gsv/
//    History      : --
// =============================================================================
function getMtxDefaultFontStyle() : TFontStyles;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := [];
  end
  else
  begin
    // Altes Look&Feel
    result := [fsBold];
  end;
end; // function getMtxDefaultFontStyle() : TFontStyles;


// =============================================================================
//    Class        : --
//    Function     : setFormAndControlsDefaultMtxFont()
//                   Schriftart eines Fensters und des dazugehörigen Komponenten setzen
//    Parameter    : mtx_form - Anzupassendes Fenster
//    Return       : --
//    Exceptions   : --
//    First author : 2016-03-10 /gsv/: Funktion TUpdateForm.Loaded von who
//                                     hierher verschoben und für DIS-2 und MSC
//                                    (MSC alte / neue Schriftart) angepasst.
//    History      : --
// =============================================================================
procedure setFormAndControlsDefaultMtxFont ( mtx_form : TMtxForm );
var
  i           : Integer;
  PropInfo    : PPropInfo;
  component   : TComponent;
  mtx_control : TMtxControlPublicFont;
begin
  // 2016-03-24 /gsv/: Hier auch Font des Fensters und der mtx_form.Canvas setzen.
  // Die Canvas-Schriftart weicht sonst ggf. von der Schriftart des Fensters ab
  // und führt z.B. bei Berechnungen der Textbreite über Canvas.TextWidth zu einem falschen Wert.

  // Monospaced-Font (Courier New) nicht überschreiben
  if ( mtx_form.Font.Name <> getMtxMonospacedFontName() ) then
  begin
    mtx_form.Font.Name  := getMtxDefaultFontName();
  end;
  // Größe und Erscheinung immer setzen
  mtx_form.Font.Size  := getMtxDefaultFontSize();
  mtx_form.Font.Style := getMtxDefaultFontStyle();

  // Schriftart der Canvas entsprechend setzen
  mtx_form.Canvas.Font.Name  := mtx_form.Font.Name;
  mtx_form.Canvas.Font.Size  := mtx_form.Font.Size;
  mtx_form.Canvas.Font.Style := mtx_form.Font.Style;


  // Font für jede Komponente einzeln setzen.
  for i := 0 to mtx_form.ComponentCount - 1 do
  begin
    component := mtx_form.Components[i];

    PropInfo := GetPropInfo ( component, 'Font' );

    // Hat die Komponente eine Font-Property und ist ein TControl?
    if ( Assigned(PropInfo) and (component is TControl) ) then
    begin
      mtx_control := TMtxControlPublicFont(component);

      // Monospaced-Font (Courier New) nicht überschreiben
      if ( mtx_control.Font.Name <> getMtxMonospacedFontName()) then
      begin
        mtx_control.Font.Name  := getMtxDefaultFontName();
      end;

      // Größe und Erscheinung immer setzen
      mtx_control.Font.Size  := getMtxDefaultFontSize();
      mtx_control.Font.Style := getMtxDefaultFontStyle();
    end;
  end;
end; // procedure setFormAndControlsDefaultMtxFont ( mtx_form : TMtxForm );


// =============================================================================
//    Class        :
//    Function     : getMtxDefaultColorBlue / Red / Green
//                   Ermittelt den Farbcode der jeweilgen zu verwendeten Farbe
//                   blau, rot, grün oder gelb.
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2016-04-26 /gsv/
//    History      : --
// =============================================================================
// Blaue Metronix-Farbe zurückgeben
function getMtxDefaultColorBlue() : TColor;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := C_COL_MTX_INFO_PANEL_BLUE;
  end
  else
  begin
    // Altes Look&Feel
    result := clBlue;
  end;
end; // function getMtxDefaultColorBlue () : TColor;

// Rote Metronix-Farbe zurückgeben
function getMtxDefaultColorRed() : TColor;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := C_COL_MTX_ERROR_RED;
  end
  else
  begin
    // Altes Look&Feel
    result := clRed;
  end;
end; // function getMtxDefaultColorRed  () : TColor;

// Blaue Metronix-Farbe für Texte zurückgeben
function getMtxDefaultColorBlueForText() : TColor;
begin
  result := RGB ( 10, 0, 100 );
end;
// Graue Metronix-Farbe für Texte zurückgeben
function getMtxDefaultColorGrayForText() : TColor;
begin
  result := RGB ( 159, 159, 159 );
end;

// Grüne Metronix-Farbe zurückgeben
function getMtxDefaultColorGreen() : TColor;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := C_COL_MTX_OK_GREEN;
  end
  else
  begin
    // Altes Look&Feel
    result := clLime;
  end;
end; // function getMtxDefaultColorGreen() : TColor;

// Grüne Metronix-Farbe zurückgeben
function getMtxDefaultColorActGreen() : TColor;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := C_COL_MTX_ACT_GREEN;
  end
  else
  begin
    // Altes Look&Feel
    result := clMoneyGreen;
  end;
end; // function getMtxDefaultColorGreen() : TColor;

// Gelbe Metronix-Farbe zurückgeben
function getMtxDefaultColorYellow() : TColor;
begin
  if ( checkMtxUseNewLookAndFeel() ) then
  begin
    // Neues Look&Feel
    result := C_COL_MTX_YELLOW;
  end
  else
  begin
    // Altes Look&Feel
    result := clYellow;
  end;
end; // function getMtxDefaultColorYellow() : TColor;

// =============================================================================
//    Class        :
//    Function     : calcTxtColorForBackground
//                   Wähle passende Textfarbe zu einer Hintergrundfarbe
//    Parameter    : c: Hintergrundfarbe
//    Return       : clWhite, wenn Hintergrund zu dunkel, clBlack sonst
//    Exceptions   : --
//    First author : 2016-05-09 /who/
//    History      : --
// =============================================================================
function calcTxtColorForBackground(c: TColor): TColor;
const
  // Willkürlich gewählter Schwellwert
  th_brightness = $99;
var
  a: double;
begin
  // Ganz rudimentär die "Helligkeit" berechnen und gegebenenfalls Schriftfarbe
  // für den Text auf Weiß setzen, sonst Schwarz. 
  // Gewichtungswerte von www.w3.org (relative luminance)
  a := (c and $FF)*0.2126 + ((c shr 8) and $FF)*0.7152 + ((c shr 16) and $FF)*0.0722;

  if (round(a) < th_brightness) then result := clWhite
  else result := clBlack;
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxGUIIntegerList
// Integer-Liste für published-Properties in GUI-Komponenten
// =============================================================================
// =============================================================================
function TMtxGUIIntegerList.Add(const Value: Integer): Integer;
begin
    Result := FList.Add(Value);
end;

procedure TMtxGUIIntegerList.Assign ( Source: TPersistent );
var
  I : Integer;
begin
  if ( Source is TMtxGUIIntegerList ) then
  begin
    FList.Clear;
    for I in TMtxGUIIntegerList(Source).FList do
    begin
      FList.Add(I);
    end;
  end
  else
  begin
    inherited Assign ( Source );
  end;
end;

procedure TMtxGUIIntegerList.BeginUpdate;
begin
  Inc ( FUpdating );
end;

procedure TMtxGUIIntegerList.Clear;
begin
  FList.Clear();
end;

function TMtxGUIIntegerList.getCount() : Integer;
begin
  result := FList.Count;
end;

constructor TMtxGUIIntegerList.Create;
begin
  inherited Create;
  FList          := TList<Integer>.Create;
  FList.OnNotify := ListNotifyHandler;
end;

procedure TMtxGUIIntegerList.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while ( not Reader.EndOfList ) do
    begin
      Add(Reader.ReadInteger);
    end;
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TMtxGUIIntegerList.WriteData(Writer: TWriter);
var
  I : Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
  begin
    Writer.WriteInteger(FList[I]);
  end;

  Writer.WriteListEnd;
end;

function TMtxGUIIntegerList.Remove(const Value: Integer): Integer;
begin
  Result := FList.Remove(Value);
end;

procedure TMtxGUIIntegerList.DefineProperties(Filer: TFiler);

  function HasData: Boolean;
  begin
    if ( Filer.Ancestor <> nil ) then
    begin
      Result := TRUE;
      if ( Filer.Ancestor is TMtxGUIIntegerList)  then
      begin
        Result := not Equals(TMtxGUIIntegerList(Filer.Ancestor))
      end;
    end
    else
    begin
      Result := FList.Count > 0;
    end;
  end;

begin
  Filer.DefineProperty ( 'Integers', ReadData, WriteData, HasData );
end;

procedure TMtxGUIIntegerList.Delete ( Index: Integer );
begin
  FList.Delete(Index);
end;

destructor TMtxGUIIntegerList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMtxGUIIntegerList.EndUpdate;
begin
  Dec(FUpdating);
  if FUpdating = 0 then
  begin
    if ( FUpdated ) then
    begin
      TriggerOnChange;
    end;
    FUpdated := FALSE;
  end;
end;

procedure TMtxGUIIntegerList.FromString(const S: String);
var
  SArray : TStringDynArray;
  N      : String;
begin
  SArray := SplitString(S, ';');
  Clear;
  for N in SArray do
    Add(StrToIntDef(N, 0));
end;

function TMtxGUIIntegerList.GetItems(nIndex: Integer): Integer;
begin
  Result := FList[nIndex];
end;

function TMtxGUIIntegerList.IndexOf(const Value: Integer): Integer;
begin
  Result := FList.IndexOf(Value);
end;

procedure TMtxGUIIntegerList.Insert(Index: Integer; const Value: Integer);
begin
  FList.Insert(Index, Value);
end;

procedure TMtxGUIIntegerList.ListNotifyHandler ( Sender     : TObject;
                                                 const Item : Integer;
                                                 Action     : TCollectionNotification);
begin
  if FUpdating <> 0 then
    FUpdated := TRUE
  else
    TriggerOnChange;
end;

procedure TMtxGUIIntegerList.SetItems(nIndex: Integer; const Value: Integer);
begin
  FList[nIndex] := Value;
end;

function TMtxGUIIntegerList.ToString: String;
var
  N : Integer;
begin
  result := self.ClassName;
  Result := '';
  for N in FList do
    Result := Result + IntToStr(N) + ';';
  if Length(Result) > 0 then
    SetLength(Result, Length(Result) - 1);
end;

procedure TMtxGUIIntegerList.TriggerOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxForm
//
// =============================================================================
// =============================================================================


// =============================================================================
//    Class        : TMtxForm
//    Function     : Create
//                   Konstruktor: Neues Objekt mit Default-Werten erzeugen
//                   Hinweis:
//                   Der Konstruktor von TForm musste übernommen werden!
//                   Andernfalls, z.B. durch Aufruf von inherited, wurde die
//                   Exception "Resource TMtxForm nicht gefunden" ausgelöst.
//    Parameter    : AOwner - Besitzer der Komponente
//    Return       : --
//    Exceptions   : --
//    First author : 2013-12-23 /gsv/
//    History      : --
// =============================================================================
constructor TMtxForm.Create ( AOwner : TComponent );
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    if ( (ClassType <> TMtxForm) and (not (csDesigning in ComponentState)) ) then
    begin
      Include ( FFormState, fsCreating );
      try
        if ( not InitInheritedComponent ( Self, TMtxForm ) ) then
          raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      finally
        Exclude ( FFormState, fsCreating );
      end;
      if ( OldCreateOrder ) then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end; // constructor TMtxForm.Create ( AOwner : TComponent );


// =============================================================================
//    Class        : TMtxForm
//    Function     : Loaded
//                   Wird aufgerufen, nachdem alle Componenten aus der DFM geladen
//                   wurden. Somit kann man jetzt z.B. nachträglich die Font anpassen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2016-03-02 /who/
//    History      : 2016-03-10 /gsv/: Von wclas.TUpdateForm hierher verschoben
//                                     und auf zentrale Funktion umgestellt.   
// =============================================================================
procedure TMtxForm.Loaded;
begin
  inherited;
  setFormAndControlsDefaultMtxFont ( self );
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxLabel
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TMtxLabel
//    Function     : Create()
//                   Konstruktor: Neues Objekt mit Default-Werten erzeugen
//    Parameter    : AOwner - Besitzer der Komponente
//    Return       : --
//    Exceptions   : --
//    First author : 2013-12-23 /gsv/
//    History      : --
// =============================================================================
constructor TMtxLabel.Create ( AOwner : TComponent );
begin
  FTrailingSpaceAdded := false;
  FBorder             := false;   // Flag: Rahmen um das Label zeichnen?
  FOnPaint            := nil;
  FAutoAppendColon    := false;
  FPadding            := TPadding.Create ( self );
  FPadding.OnChange   := doPaddingChange;
  inherited Create ( AOwner );
end; // constructor TMtxLabel.Create ( AOwner : TComponent );

destructor TMtxLabel.Destroy();
begin
  self.FPadding.Free();
  inherited Destroy();
end;

// =============================================================================
//    Class        : TMtxLabel
//    Function     : GetCaption()
//                   Auslesen der Caption
//                   Falls beim Setzen der Caption ein Leerzeichen am Ende
//                   des Textes angefügt wurde, wird es hier beim Auslesen
//                   wieder entfernt.   
//    Parameter    : --
//    Return       : Caption als string
//    Exceptions   : --
//    First author : 2013-12-23 /gsv/
//    History      : --
// =============================================================================
function TMtxLabel.GetCaption(): TWideCaption;
var
  i_len : integer;
begin
  result := inherited Caption;

  // Ggf. Leerzeichen am Ende des Textes entfernen, falls es zuvor bei SetCaption()
  // automatisch angefügt wurde. 
  i_len := Length ( result );
  if ( FTrailingSpaceAdded and (i_len > 0) ) then
  begin
    Delete ( result, i_len, 1 );
  end;
end; // function TMtxLabel.GetCaption(): TWideCaption;


procedure TMtxLabel.setAutoAppendColon ( newValue : boolean );
begin
  self.FAutoAppendColon := newValue;
  self.Caption          := self.Caption; // Caption aktualisieren
end;

procedure TMtxLabel.setPadding ( const Value: TPadding );
begin
  self.FPadding.Assign ( Value );
end;

procedure TMtxLabel.doPaddingChange ( Sender: TObject );
begin
  self.AdjustBounds();
  self.Repaint();
end;


// =============================================================================
//    Class        : TMtxLabel
//    Function     : SetCaption()
//                   Setzen der Caption
//                   Falls die Caption rechtsbündig ist und das letzte Zeichen
//                   kein Leerzeichen ist, so wird am Ende des Textes noch ein
//                   Leerzeichen angefügt. Dadurch hat man einen kleinen Abstand
//                   zwischen Text und Label-Rand.   
//    Parameter    : Value - neue Caption
//    Return       : -- 
//    Exceptions   : --
//    First author : 2013-12-23 /gsv/
//    History      : --
// =============================================================================
procedure TMtxLabel.SetCaption ( const Value: TWideCaption );
var
  i_len      : integer;
  newCaption : TWideCaption;
begin
  newCaption := Value;
  i_len      := Length ( newCaption );

  // 2016-09-28 /gsv/: Ggf. ":" ans Ende der Caption automatisch anfügen.
  if ( (self.FAutoAppendColon) and (i_len > 0) ) then
  begin
    if ( newCaption[i_len] <> ':' ) then
    begin
      newCaption := newCaption + ':';
    end;
  end;

  // Flag allgemein zurücksetzen
  FTrailingSpaceAdded := false;

  // Das Leerzeichen wird nur bei rechtsbündigen Texten angefügt.
  if ( Alignment = taRightJustify ) then
  begin
    // Leerzeichen nur bei gültigen Texten anfügen, bei denen das letzte Zeichen
    // nicht bereits ein Leerzeichen ist.
    if ( (i_len >= 1) and
         (newCaption[i_len] <> ' ') ) then
    begin
      newCaption := newCaption + ' ' ;
      FTrailingSpaceAdded := true;
    end;
  end; // if ( Alignment = taRightJustify ) then

  inherited Caption := newCaption;
end; // procedure TMtxLabel.SetCaption ( const Value: TWideCaption );


// Getter/Setter für die Property Border
function TMtxLabel.getBorder() : boolean;
begin
  result := FBorder;
end;
procedure TMtxLabel.setBorder ( newValue : boolean );
begin
  if ( FBorder <> newValue ) then
  begin
    FBorder := newValue;
    self.Invalidate();
  end; 
end;


// =============================================================================
//    Class        : TMtxLabel
//    Function     : Paint()
//                   Label neu zeichnen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2014-01-07 /gsv/
//    History      : --
// =============================================================================
procedure TMtxLabel.Paint();
var
  rect : TRect;
begin
  // Hinweis: Zuerst Rahmen zeichnen, dann inherited aufrufen,
  // da sonst der Text überschrieben wird!

  // Rahmen zeichnen?
  if ( FBorder ) then
  begin
    rect.Left   := 0;
    rect.Top    := 0;
    rect.Right  := self.Width;
    rect.Bottom := self.Height;
    // Das Label mit dem Theme eines TEdit zeichnen.
  {$IFDEF DELPHI_XE_UP}
    DrawThemeBackground ( StyleServices.Theme[teEdit],
                          Canvas.Handle, EP_EDITBORDER_NOSCROLL, ETS_NORMAL, rect, @rect );
  {$ELSE}
    DrawThemeBackground ( ThemeServices.Theme[teEdit],
                          Canvas.Handle, EP_EDITBORDER_NOSCROLL, ETS_NORMAL, rect, @rect );
  {$ENDIF}
  end;


  inherited;

  // Ggf. Event-Handler aufrufen
  if ( Assigned ( FOnPaint ) ) then FOnPaint ( self );
end; // procedure TMtxLabel.Paint;

procedure TMtxLabel.DoDrawText ( var Rect: TRect; Flags: Longint );
begin
  // 2016-10-10 /gsv/: Padding berücksichtigen
  Inc ( Rect.Left,   Padding.Left   );
  Inc ( Rect.Top,    Padding.Top    );
  Dec ( Rect.Right,  Padding.Right  );
  Dec ( Rect.Bottom, Padding.Bottom );

  inherited DoDrawText ( Rect, Flags );
end;



constructor TMtxComboExItemData.Create();
begin
  self.Create ( nil, enComboItemExStandard );
end;
constructor TMtxComboExItemData.Create ( item_type : TMTxComboExItemType );
begin
  self.Create ( nil, item_type );
end;
constructor TMtxComboExItemData.Create ( user_data : TCustomData; item_type : TMTxComboExItemType );
begin
  self.FUserData := user_data;
  self.FItemType := item_type;
end;


// =============================================================================
//    Class        : TMtxComboBoxEx
//    Function     : GetItemHt()
//                   ItemHeight ermitteln
//                   In der Basisklasse ist die ItemHeight 16 fest hinterlegt und
//                   kann nicht modifiziert werden. Sobald man Bilder hat,
//                   die größer sind als 16 Pixel, wird die Dropdown-Liste nicht
//                   vollständig angezeigt. Daher wurde diese Funktion überschrieben!
//                   Hier wird die maximale Bild-Höhe ermittelt und als ItemHeight
//                   angenommen. Dadurch stimmt die Dropdown-Liste wieder und
//                   zeigt alle Einträge an.
//    Parameter    : --
//    Return       : Größe eines Eintrags in der Liste
//    Exceptions   : --
//    First author : 2014-08-26 /gsv/
//    History      : --
// =============================================================================
function TMtxComboBoxEx.GetItemHt: Integer;
var
  i   : integer;
  bmp : TBitmap;
begin
  // Vorinitialisierung mit Default-Wert aus der Basisklasse
  result := inherited GetItemHt();

  if ( Assigned ( Images ) ) then
  begin
    // Bilder vorhanden ==> Max. Bildhöhe ermitteln und als ItemHeight übernehmen
    for i := 0 to Images.Count-1 do
    begin
      bmp := TBitmap.Create();
      if ( Images.GetBitmap ( i, bmp ) ) then
      begin
        if ( result < Images.Height ) then
        begin
          result := Images.Height;
        end;
      end;

      bmp.Free();
    end; // for i := 0 to Images.Count-1 do
       
  end; // if ( Assigned ( Images ) ) then
end;


procedure TMtxComboBoxEx.DoBeforeItemChange ( oldIndex, newIndex : integer; var changeAllowed : boolean );
begin
  changeAllowed := IsItemSelectable ( newIndex );
end;


constructor TMtxComboBoxEx.Create ( AOwner : TComponent );
begin
  inherited Create ( AOwner );
  Style         := csExDropDownList;
  FOldItemIndex := -1;
end;

destructor TMtxComboBoxEx.Destroy();
begin
  inherited Destroy();
end;



function TMtxComboBoxEx.isItemIndexValid ( inx : integer ) : boolean;
begin
  result := (inx = -1) or ((inx >= 0) and (inx < self.ItemsEx.Count));
end; // function TUpdateComboBox.isItemIndexValid();



function TMtxComboBoxEx.IsItemSelectable ( inx : integer ) : boolean;
begin
  if ( (inx < 0) or (inx >= ItemsEx.Count) ) then
  begin
    result := false;
    exit;
  end;

  if ( not Assigned (ItemsEx[inx].Data ) ) then
  begin
    result := true;
    exit;
  end;

  try
    if ( not (TObject(ItemsEx[inx].Data) is TMtxComboExItemData) ) then
    begin
      result := true;
      exit;
    end;

    // Eintrag selektierbar, wenn keine Überschrift
    result := TMtxComboExItemData(ItemsEx[inx].Data).ItemType <> enComboItemExHeading;
  except on E:Exception do result := true;
  end;
end;


function TMtxComboBoxEx.IsNextItemSelectable(): boolean;
begin
  if ( ItemIndex >= ItemsEx.Count-1 ) then
  begin
    result := false;
    exit;
  end;

  result := IsItemSelectable ( ItemIndex+1 );
end;


function TMtxComboBoxEx.IsPrevItemSelectable(): boolean;
begin
  if ( ItemIndex < 1 ) then
  begin
    result := false;
    exit;
  end;

  result := IsItemSelectable ( ItemIndex-1 );
end;


procedure TMtxComboBoxEx.SelectNextSelectableItem();
var
  i : integer;
begin
  for i := ItemIndex+1 to ItemsEx.Count-1 do
  begin
    if ( IsItemSelectable ( i ) ) then
    begin
      ItemIndex := i;
      exit;
    end;
  end;
end;


procedure TMtxComboBoxEx.SelectPrevSelectableItem();
var
  i : integer;
begin
  for i := ItemIndex-1 downto 0 do
  begin
    if ( IsItemSelectable ( i ) ) then
    begin
      ItemIndex := i;
      exit;
    end;
  end;
end;

procedure TMtxComboBoxEx.KeyDown ( var Key: Word; Shift: TShiftState );
begin
  case ( Key ) of
    VK_DOWN:
    begin
      SelectNextSelectableItem();
      Key := 0;
    end;
    VK_UP:
    begin
      SelectPrevSelectableItem();
      Key := 0;
    end;

    else inherited KeyDown ( Key, Shift );
  end;
end;


procedure TMtxComboBoxEx.KeyPress(var Key: Char);
var
  LItemIndex, Idx, I,
  FirstIndex, LastIndex: Integer;
  bChangeAllowed : boolean;
begin
  if (Style <> csExDropDownList) or (Ord(Key) in [$00, VK_RETURN, VK_BACK, VK_ESCAPE, VK_TAB]) then
    Exit;

  LItemIndex := ItemIndex;
  Idx        := LItemIndex;
  FirstIndex := 0;
  LastIndex  := Items.Count - 1;

  if (LItemIndex >= 0) and (LItemIndex < LastIndex) and StartsText(Key, Items[LItemIndex]) then
    FirstIndex := LItemIndex + 1;

  repeat
    for I := FirstIndex to LastIndex do
    begin
      if StartsText(Key, Items[I]) then
      begin
        Idx := I;
        Break;
      end
    end;
    if FirstIndex > 0 then
    begin
      FirstIndex := 0;
      LastIndex  := LItemIndex;
    end
    else
      Break;
  until LItemIndex <> Idx;

  if LItemIndex <> Idx then
  begin
    DoBeforeItemChange ( LItemIndex, Idx, bChangeAllowed );
    if ( bChangeAllowed ) then
    begin
      SendMessage ( Handle, CB_SETCURSEL, Idx, 0 );
      Text := Items[ItemIndex];
      Click;
      Select;
    end;
  end;

  Key := #0;
end;


function TMtxComboBoxEx.AddItemEx ( const Caption: string; Data: TMtxComboExItemData ): TComboExItem;
var
  i_indent : integer;
begin
  if ( Assigned ( Data ) ) then
  begin
    case ( Data.ItemType ) of
      enComboItemExStandard: i_indent := -1;
      enComboItemExHeading : i_indent := -1;
      enComboItemExSubling : i_indent :=  2;
      else                   i_indent := -1;
    end;
  end
  else
  begin
    i_indent := -1;
  end;

  result := self.ItemsEx.AddItem ( Caption, -1, -1, -1, i_indent, Data );
end;


// ===========================================================================
// Klasse: TMtxBalloonHint
// BallonHint-Komponente
// ===========================================================================
constructor TMtxBalloonHint.Create ( AParent : TWinControl );
begin
  FText      := '';
  FIsShowing := false;
  FParent    := AParent;
  FHandle    := 0;
  FTitle     := '';
end;


destructor TMtxBalloonHint.Destroy();
begin
  self.Hide();

  if ( Handle <> 0 ) then
  begin
    DestroyWindow ( Handle );
  end;

  inherited Destroy();
end;

procedure TMtxBalloonHint.Show();
begin
  if ( IsShowing      ) then exit;
  if ( self.Text = '' ) then exit;
  
  if ( FHandle = 0 ) then
  begin
    FHandle := CreateWindow ( TOOLTIPS_CLASS, nil, WS_POPUP or TTS_NOPREFIX or TTS_BALLOON or TTS_ALWAYSTIP,// or TTS_CLOSE,
                              0, 0, 0, 0, Parent.Handle, 0, HInstance, nil );
  end;

  if ( FHandle <> 0 ) then
  begin
    SetWindowPos ( Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE );

    FTooltipInfo.cbSize   := SizeOf(FTooltipInfo);
    FTooltipInfo.uFlags   := TTF_CENTERTIP or TTF_TRANSPARENT or TTF_SUBCLASS;
    FTooltipInfo.hwnd     := Parent.Handle;
    FTooltipInfo.lpszText := PChar(FText);
    Windows.GetClientRect ( Parent.Handle, FTooltipInfo.rect );
    SendMessage ( Handle, TTM_SETTITLEW, TTI_NONE, LPARAM(PChar(FTitle)) );
    SendMessage ( Handle, TTM_SETMAXTIPWIDTH, 0, LPARAM(1000) ); // Notwendig, damit der Hint mehrzeilig wird!
    SendMessage ( Handle, TTM_ADDTOOLW, 1, LPARAM(@FTooltipInfo) );
    SendMessage ( Handle, TTM_TRACKACTIVATE, Ord(True), LPARAM(@FTooltipInfo) );
  end;

  FIsShowing := true;
end;


procedure TMtxBalloonHint.Hide();
begin
  if ( (not IsShowing) or (Handle = 0) ) then
  begin
    exit;
  end;

  SendMessage ( Handle, TTM_POP, 0, 0 );
  SendMessage ( Handle, TTM_TRACKACTIVATE, Ord(False), LPARAM(@FTooltipInfo) );
  SendMessage ( Handle, TTM_DELTOOLW, 1, LPARAM(@FTooltipInfo) );  
  FIsShowing := false;
end;


procedure TMtxBalloonHint.setText ( ws : string );
begin
  if ( FText <> ws ) then
  begin
    FText := ws;
  end;
end;


procedure TMtxBalloonHint.setTitle ( ws : string );
begin
  if ( FTitle <> ws ) then
  begin
    FTitle := ws;
  end;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxGroupBox
//
// =============================================================================
// =============================================================================
procedure TMtxGroupBox.AdjustClientRect ( var Rect: TRect );
begin
  inherited AdjustClientRect ( Rect );

  // 2016-10-10 /gsv/: Die Funktion TCustomGroupBox.AdjustClientRect() addiert immer
  // Canvas.TextHeight ( '0' ) zu Rect.Top, so dass bei GroupBox ohne Caption
  // das erste GUI-Element mit alTop an Y-Position 17 positioniert wird.
  // Daher wird hier bei einer leeren Caption die Text-Höhe wieder abgezogen.
  if ( Caption = '' ) then
  begin
    Dec ( Rect.Top, Canvas.TextHeight ( '0' ) );
    if ( Rect.Top < 2 ) then Rect.Top := 2;
  end;
end; // procedure TMtxGroupBox.AdjustClientRect ( var Rect: TRect );




{$IFNDEF PNG_SUPPORT_ON}
  constructor TMtxSpeedButton.Create ( AOwner: TComponent );
  begin
    FPngImage   := TPngImage.Create;
    FPngOptions := [pngBlendOnDisabled];

    inherited Create ( AOwner );
  end; // constructor TMtxSpeedButton.Create ( AOwner: TComponent );

  destructor TMtxSpeedButton.Destroy();
  begin
    FPngImage.Free();
    inherited Destroy();
  end;

  procedure TMtxSpeedButton.SetPngImage ( const Value: TPngImage );
  begin
    if ( Value = nil ) then
    begin
      // PNG-Image wieder löschen (also freigeben und neu erzeugen)
      FPngImage.Free();
      FPngImage := TPngImage.Create;
    end
    else
    begin
      // Neues PNG-Image übernehmen
      FPngImage.Assign ( Value );
    end;
  end;

  procedure TMtxSpeedButton.SetPngOptions ( const Value: TPngOptions );
  begin
    if ( FPngOptions <> Value ) then
    begin
      FPngOptions := Value;
    end; 
  end;
{$ENDIF}

{$IFDEF PNG_SUPPORT_ON}
  // PNG unterstützt
  procedure TMtxBitBtn.Loaded;
  begin
    inherited Loaded;
    if(PngImage.Empty and (Kind in [bkOk, bkCancel, bkHelp])) then
    begin
      self.Margin := -1;
      self.Spacing := -1;
    end;
  end;
{$ELSE}
  // PNG nicht unterstützt
  constructor TMtxBitBtn.Create ( AOwner: TComponent );
  begin
    FPngImage   := TPngImage.Create;
    FPngOptions := [pngBlendOnDisabled];

    inherited Create ( AOwner );
  end; // constructor TMtxSpeedButton.Create ( AOwner: TComponent );

  destructor TMtxBitBtn.Destroy();
  begin
    FPngImage.Free();
    inherited Destroy();
  end;

  procedure TMtxBitBtn.Loaded;
  begin
    inherited Loaded;
    if ( (not Glyph.Empty) and (Kind in [bkOk, bkCancel, bkHelp])) then
    begin
      if ( (self.Spacing < 0) and (self.Margin < 0) ) then
      begin
        self.Margin  :=  2;
        self.Spacing := -1;
      end;
    end;
  end;

  procedure TMtxBitBtn.SetPngImage ( const Value: TPngImage );
  begin
    if ( Value = nil ) then
    begin
      // PNG-Image wieder löschen (also freigeben und neu erzeugen)
      FPngImage.Free();
      FPngImage := TPngImage.Create;
    end
    else
    begin
      // Neues PNG-Image übernehmen
      FPngImage.Assign ( Value );
    end;
  end;

  procedure TMtxBitBtn.SetPngOptions ( const Value: TPngOptions );
  begin
    if ( FPngOptions <> Value ) then
    begin
      FPngOptions := Value;
    end; 
  end;
{$ENDIF}


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCheckbox
//
// =============================================================================
// =============================================================================

constructor TMtxCheckbox.Create ( AOwner : TComponent );
begin
  FUserData         := nil;
  FReadOnly         := false;
  inherited Create ( AOwner );
end;


procedure TMtxCheckbox.setReadOnly ( AValue : boolean );
begin
  self.FReadOnly      := AValue;
end;


procedure TMtxCheckbox.Toggle();
begin
  if ( not self.ReadOnly ) then
  begin
    inherited Toggle();
  end;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxGroupButton
//
// (Quellcode von Vcl.ExtCtrls übernommen und wegen der Button-Position angepasst)
// =============================================================================
// =============================================================================
type
  TMtxGroupButton = class(TRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor InternalCreate(RadioGroup: TMtxCustomRadioGroup);
    destructor Destroy; override;
  end;

constructor TMtxGroupButton.InternalCreate(RadioGroup: TMtxCustomRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  Parent := RadioGroup;
end;

destructor TMtxGroupButton.Destroy;
begin
  TMtxCustomRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TMtxGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TMtxCustomRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TMtxGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  TMtxCustomRadioGroup(Parent).KeyPress(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TMtxCustomRadioGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TMtxGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TMtxCustomRadioGroup(Parent).KeyDown(Key, Shift);
end;

procedure TMtxGroupButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and not (CS_HREDRAW or CS_VREDRAW);
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCustomRadioGroup
//
// (Quellcode von Vcl.ExtCtrls übernommen und wegen der Button-Position angepasst)
// =============================================================================
// =============================================================================
constructor TMtxCustomRadioGroup.Create(AOwner: TComponent);
begin
  FButtonTopMargin        := -1;
  FButtonFixedHeightUse   := false;
  FColumnsFixedWidthUse   := false;
  FBorderDraw             := true;
  self.FColumnsFixedWidth := TMtxGUIIntegerList.Create();

  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks, csParentBackground];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
end;

destructor TMtxCustomRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  FColumnsFixedWidth.Free();
  inherited Destroy;
end;

procedure TMtxCustomRadioGroup.FlipChildren(AllLevels: Boolean);
begin
  { The radio buttons are flipped using BiDiMode }
end;

procedure TMtxCustomRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I, j: Integer;
  ButtonWidthAuto : integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
  ButtonColumnIndex : integer;
begin
  if (FButtons.Count <> 0) and not FReading then
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol   := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidthAuto := (Width - 10) div FColumns;

    // 2016-09-27 /gsv/: Falls eine feste TopMargin für die Buttons angegeben ist,
    // dann diese für die Berechnung der Höhe und der Top-Position der Buttons verwenden.
    // Dadurch werden die
    I := Height - 5;
    if ( FButtonTopMargin >= 0 ) then
    begin
      I         := I - FButtonTopMargin;
      TopMargin := FButtonTopMargin;
    end
    else
    begin
      I         := I - Metrics.tmHeight;
      TopMargin := Metrics.tmHeight + 1 + (I mod ButtonsPerCol) div 2;
    end;

    // 2016-09-27 /gsv/: Ggf. feste Button-Höhe verwenden, die unabhängig von der
    // Größe der RadioGroup ist.
    if ( FButtonFixedHeightUse ) then
         ButtonHeight := Metrics.tmHeight + 1
    else ButtonHeight := I div ButtonsPerCol;

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
      begin
        with TMtxGroupButton(FButtons[I]) do
        begin
          BiDiMode          := Self.BiDiMode;
          ButtonColumnIndex := I div ButtonsPerCol;

          // 2016-09-29 /gsv/: Ggf. feste Spaltenbreite berücksichtigen
          if ( Self.ColumnsFixedWidthUse and (self.Items.Count <= self.ColumnsFixedWidthCount) ) then
          begin
            ButtonWidth := ColumnsFixedWidth.Items[ButtonColumnIndex];
            ALeft       := 8;
            for j := 1 to ButtonColumnIndex do
            begin
              ALeft := ALeft + ColumnsFixedWidth.Items[j-1] + 8;
            end;
          end
          else
          begin
            ButtonWidth := ButtonWidthAuto;
            ALeft       := ButtonColumnIndex * ButtonWidth + 8;
          end;

          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
                         ALeft, (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
                         ButtonWidth, ButtonHeight,
                         SWP_NOZORDER or SWP_NOACTIVATE );
          Visible := True;
        end; // with TMtxGroupButton(FButtons[I]) do
      end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TMtxCustomRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;

procedure TMtxCustomRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TMtxCustomRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TMtxCustomRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  if HandleAllocated then
    UpdateButtons;
end;

procedure TMtxCustomRadioGroup.CreateWnd;
begin
  inherited CreateWnd;
  UpdateButtons;
end;

procedure TMtxCustomRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TMtxGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do
    TMtxGroupButton(FButtons.Last).Free;
end;

procedure TMtxCustomRadioGroup.SetColumns(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 16 then Value := 16;
  if FColumns <> Value then
  begin
    FColumns := Value;
    ArrangeButtons;
    Invalidate;
  end;
end;

procedure TMtxCustomRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then FItemIndex := Value else
  begin
    HandleNeeded;
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
        TMtxGroupButton(FButtons[FItemIndex]).Checked := False;
      FItemIndex := Value;
      if FItemIndex >= 0 then
        TMtxGroupButton(FButtons[FItemIndex]).Checked := True;
    end;
  end;
end;

procedure TMtxCustomRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TMtxCustomRadioGroup.SetWordWrap(Value: Boolean);
begin
  if Value <> FWordWrap then
  begin
    FWordWrap := Value;
    UpdateButtons;
  end;
end;

procedure TMtxCustomRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
  begin
    TMtxGroupButton(FButtons[I]).Caption := FItems[I];
    TMtxGroupButton(FButtons[I]).WordWrap := FWordWrap;
  end;
  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TMtxGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TMtxCustomRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do
    TMtxGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TMtxCustomRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TMtxCustomRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

procedure TMtxCustomRadioGroup.setBorderDraw ( b_on : boolean );
begin
  if ( b_on <> FBorderDraw ) then
  begin
    self.FBorderDraw := b_on;
    self.Invalidate();
  end;
end;

procedure TMtxCustomRadioGroup.setButtonTopMargin ( ATopMargin : integer );
begin
  if ( (ATopMargin < -1) or (ATopMargin > self.Height) ) then
    raise EPropertyError.CreateResFmt ( @SOutOfRange, [-1, self.Height] );

  self.FButtonTopMargin := ATopMargin;
  UpdateButtons();
end;

procedure TMtxCustomRadioGroup.setButtonFixedHeightUse ( AUseFixedHeight : boolean );
begin
  self.FButtonFixedHeightUse := AUseFixedHeight;
  UpdateButtons();
end;

function TMtxCustomRadioGroup.getButtonFixedHeight() : integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  result := Metrics.tmHeight + 1;
end;

procedure TMtxCustomRadioGroup.setColumnsFixedWidthUse ( AValue : boolean );
begin
  self.FColumnsFixedWidthUse := AValue;
  UpdateButtons();
end;

function TMtxCustomRadioGroup.getColumnsFixedWidthCount() : integer;
begin
  result := self.FColumnsFixedWidth.Count;
end;


procedure TMtxCustomRadioGroup.setColumnsFixedWidth ( AValueList : TMtxGUIIntegerList );
begin
  if ( not Assigned ( FColumnsFixedWidth ) ) then exit;

  FColumnsFixedWidth.Assign ( AValueList );
  UpdateButtons();
end;


procedure TMtxCustomRadioGroup.ClearColumnsFixedWidth();
begin
  self.FColumnsFixedWidth.Clear();
end;


function TMtxCustomRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TMtxCustomRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TMtxCustomRadioGroup.GetButtons(Index: Integer): TRadioButton;
begin
  Result := TRadioButton(FButtons[Index]);
end;


procedure TMtxCustomRadioGroup.Paint;
const C_BORDER_WIDTH = 1;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  CaptionRect,
  OuterRect: TRect;
  Size: TSize;
  Box: TThemedButton;
  Details: TThemedElementDetails;
begin
  with Canvas do
  begin
    Font := Self.Font;

    if ThemeControl(Self) then
    begin
      if Text <> '' then
      begin
        GetTextExtentPoint32(Handle, Text, Length(Text), Size);
        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect, 8, 0)
        else
          OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(Handle, Left, Top, Right, Bottom);
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;

      if ( BorderDraw ) then
      begin
        Details := StyleServices.GetElementDetails(Box);
        StyleServices.DrawElement(Handle, Details, OuterRect);
      end
      else
      begin
        // Border nicht zeichnen, also nur mit der Hintergrundsfarbe zeichnen
        Details := StyleServices.GetElementDetails ( tpPanelBackground );
        StyleServices.DrawElement(Handle, Details, OuterRect);
      end;

      SelectClipRgn(Handle, 0);
      Brush.Style := bsClear;
    {$WARN SYMBOL_DEPRECATED OFF}
      if Text <> '' then
        if IsRightToLeft then
        begin
          Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
          StyleServices.DrawText(Handle, Details, Text, CaptionRect, Flags, 0);
        end
        else
          StyleServices.DrawText(Handle, Details, Text, CaptionRect, [tfLeft]);
    {$WARN SYMBOL_DEPRECATED ON}
    end
    else
    begin
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if Ctl3D then
      begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        if ( BorderDraw ) then
        begin
          FrameRect(R);
        end;
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end else
        Brush.Color := clWindowFrame;
        if ( BorderDraw ) then
        begin
          FrameRect(R);
        end;
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        DrawText(Handle, Text, Length(Text), R, Flags or DT_CALCRECT);
        Brush.Color := Color;
        DrawText(Handle, Text, Length(Text), R, Flags);
      end;
    end;
  end;
end;


end.

