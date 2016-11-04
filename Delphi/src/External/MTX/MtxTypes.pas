unit MtxTypes;

// =============================================================================
// =============================================================================
//
//     Module name       : $RCSfile: MtxTypes.pas,v $
//     Short description : Contains global data types, which are used
//                         by the library components and/or MSC.
//     Project           : MSC 2.8
//     Compiler          : Delphi 2007
//     First author      : 2007-03-22 /gsv/
//     Author            : $Author: gsv $ 
//     Version           : $Revision: 1.55 $
//     Copyright         : (c) Metronix GmbH 2007
// -----------------------------------------------------------------------------
//     History           : --
// -----------------------------------------------------------------------------
//     Descriptions
// =============================================================================

{$INCLUDE MtxCompilers.inc}

interface

{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
uses SysUtils, Classes, Contnrs, SyncObjs, Windows, System.Generics.Collections,
     System.Generics.Defaults, Types, StrUtils, Controls;
{$ELSE}
uses SysUtils, Classes, TntSysUtils, TntDialogs, Contnrs, SyncObjs, Windows,
     System.Generics.Collections, System.Generics.Defaults, Types, StrUtils,
     Controls;
{$ENDIF}

// 2008-10-22 /gsv/: Bei Delphi 2007 muss dieser Compiler-Schalter gesetzt werden,
// damit es keine Warnung beim Compilieren gibt (Konflikt mit dem published-Abschnitt).
// Den Schalter gibt es auch schon unter Delphi 7, also keine Sonderbehandlung für Delphi 2007. 
// Scope of $TYPEINFO = local
{$TYPEINFO ON}

const
  C_DEF_BIT_INV_LOGIC = false; // Defaultwert für die Eigenschaft InvLogic von TBitListentry

  // Konstanten für die Länge des Datentyps TDataType in Bits
  C_BITS_PER_BYTE     = 8;     // Ein Byte hat 8 Bits!
  C_BIT_SIZE_INT8     = C_BITS_PER_BYTE*1;
  C_BIT_SIZE_INT16    = C_BITS_PER_BYTE*2;
  C_BIT_SIZE_INT32    = C_BITS_PER_BYTE*4;

  C_CHAR_COMMENT_LINE = ';';

  // Konstanten für Key-Codes, die in Windows.pas nicht definiert sind.
  VK_0      = $30; // Taste 0 (Nicht Numpad0!)
  VK_1      = $31; // Taste 1 (Nicht Numpad1!)
  VK_2      = $32; // Taste 2 (Nicht Numpad2!)
  VK_3      = $33; // Taste 3 (Nicht Numpad3!)
  VK_4      = $34; // Taste 4 (Nicht Numpad4!)
  VK_5      = $35; // Taste 5 (Nicht Numpad5!)
  VK_6      = $36; // Taste 6 (Nicht Numpad6!)
  VK_7      = $37; // Taste 7 (Nicht Numpad7!)
  VK_8      = $38; // Taste 8 (Nicht Numpad8!)
  VK_9      = $39; // Taste 9 (Nicht Numpad9!)
  VK_PERIOD = $BE; // Punkt

  C_TEXT_ID_DEFAULT_VALUE                 = -1; // Default-Wert der TextID-Property
  C_TEXT_ID_FORMAT_TEXT_CUSTOMER_SPECIFIC = 3236; // Default-Wert der FormatTextID-Property für die ComboBox
                                                  // TEXT_3236 = kundenspezifisch 0x%.08X


  // 2016-09-14 /gsv/: Konstanten für die Default-Position der MSC-Fenster
  C_MTX_DEF_WIN_POS_LEFT = 4;
  C_MTX_DEF_WIN_POS_TOP  = 4;
type // Because of the forward declarations do not use other "type" keywords
     // until the end of all type declarations!


  // ===========================================================================
  //                 F O R W A R D   D E C L A R A T I O N S
  // ===========================================================================
  TMaskedTableEntry = class;


  // ===========================================================================
  //                     T Y P E     D E C L A R A T I O N S
  // ===========================================================================
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  TWideCaption = type string;
{$ENDIF}


  // ===========================================================================
  //                 E X C E P T I O N   D E C L A R A T I O N S
  // ===========================================================================

  // ===========================================================================
  // Class MtxException
  // Oberklasse für alle MTX spezifischen Exceptions.
  // ===========================================================================
  MtxException = class ( Exception )
  end; // MtxException = class ( Exception )

  // ===========================================================================
  // Class EFileError
  // A MTX defined exception. This exception is thrown,
  // when an error occurs during a file operation required.
  // ===========================================================================
  EFileError = class ( MtxException )
  end; // type EFileError = class ( MtxException )

  // ===========================================================================
  // Class EFileNotFound
  // Diese Exception wird beim lesenden Zugriff auf eine nicht existierende
  // Datei ausgelöst.
  // ===========================================================================
  EFileNotFound = class ( MtxException )
  end; // type EFileNotFound = class ( MtxException )

  // ===========================================================================
  // Class EObjectNotExist
  // A MTX defined exception. This exception is thrown,
  // when a required object cannot be found, e.g. searching for an object in
  // an array, which doesn't contain the desired object.
  // ===========================================================================
  EObjectNotExist = class ( MtxException )
  end; // type EObjectNotExist = class ( MtxException )

  // ===========================================================================
  // Class EObjectAlreadyExist
  // Diese Exception wird ausgelöst, wenn ein Objekt bereits existiert,
  // z.B. wenn in eine Liste ein Objekt eingefügt werden soll, welches in der
  // Liste bereits enthalten ist (Duplikate ausschließen).
  // ===========================================================================
  EObjectAlreadyExist = class ( MtxException )
  end; // type EObjectAlreadyExist = class ( MtxException )

  // ===========================================================================
  // Class ETooManyObjects
  // Diese Exception wird beim Einfügen eines Objektes in eine Liste,
  // die bereits voll ist und keine weiteren Objekte aufnehmen kann.
  // z.B. Hinzufügen eines CAN-Objektes in ein PDO, welches bereits die maximale
  // Anzahl von CAN-Objekten enthält.
  // ===========================================================================
  ETooManyObjects = class ( MtxException )
  end; // type ETooManyObjects = class ( MtxException )

  // ===========================================================================
  // Class ETooMuchData
  // Diese Exception wird bei zu vielen Daten ausgelöst.
  // z.B. Hinzufügen eines CAN-Objektes in ein PDO,
  // bei dem die 64-Bits bereits belegt sind bzw. durch das Einfügen des Objektes
  // diese Datenlänge von 64-Bits überschritten wird. 
  // ===========================================================================
  ETooMuchData = class ( MtxException )
  end; // type ETooMuchData = class ( MtxException )


  // ===========================================================================
  // Class EIndexOutOfBounds
  // A MTX defined exception. This exception is thrown,
  // when an array/list element is indexed with an invalid index,
  // e.g. index < 0 OR index >= number of elements
  // ===========================================================================
  EIndexOutOfBounds = class ( MtxException )
  end; // EIndexOutOfBounds = class ( MtxException )


  // ===========================================================================
  // Class EInvalidState
  // This exception is thrown, when an invalid state is reached,
  // e.g. the default state in a switch-statement.
  // ===========================================================================
  EInvalidState = class ( MtxException )
  end; // EInvalidState = class ( MtxException )

  // ===========================================================================
  // Class EInvalidArgument
  // This exception is thrown, when function/method is called with an invalid
  // argument(s). 
  // ===========================================================================
  EInvalidArgument = class ( MtxException )
  end; // EInvalidArgument = class ( MtxException )


  // ===========================================================================
  // 2010-07-07 /gsv/: SWAE 2010-169
  // Class MtxDCOException
  // Exception für Fehler in der DCO-Datei 
  // ===========================================================================
  EDCOException = class ( MtxException )
  end; // MtxDCOException = class ( MtxException )

  // ===========================================================================
  // 2010-07-07 /gsv/: SWAE 2010-169
  // Class EDCOInvalidCompNr
  // Exception für eine ungültige Komponentennummer eines KOs in der DCO-Datei
  // z.B. nicht unterstützte Komp-Nr / Textdarstellung der Komp-Nr unbekannt
  // ===========================================================================
  EDCOInvalidCompNr = class ( EDCOException )
  end; // type EDCOInvalidCompNr = class ( EDCOException )


  // ===========================================================================
  // 2010-07-07 /gsv/: SWAE 2010-169
  // Class EDCOInvalidCONr
  // Exception für eine ungültige KO-Nummer in der DCO-Datei
  // z.B. Fehler bei der Konvertierung der KO-Nummer String --> Zahl
  // ===========================================================================
  EDCOInvalidCONr = class ( EDCOException )
  end; // type EDCOInvalidCONr = class ( EDCOException )

  // ===========================================================================
  // 2010-07-07 /gsv/: SWAE 2010-169
  // Class EDCOAbortUser
  // Diese Exception wird ausgelöst, fall der DCO-Download durch den
  // Benutzer abgebrochen wird.
  // ===========================================================================
  EDCOAbortUser = class ( EDCOException )
  end; // type EDCOAbortUser = class ( EDCOException )


  // ===========================================================================
  //                    E V E N T   D E C L A R A T I O N S
  // ===========================================================================



  // ===========================================================================
  // Data type TMaskedTableEvent
  // This is the data type used by the class TMaskedTable for some of the events,
  // the class generates.
  // Parameters: sender - the table object, generated the event
  //             entry  - the table entry, currently participated
  //                      in the corresponding operation, as the event was fired.
  // ===========================================================================
  TMaskedTableEvent = procedure ( Sender : TObject; entry : TMaskedTableEntry ) of object;

  
  // ===========================================================================
  // Data type TItemChangedEvent
  // Used for OnItemChanged events generated by RadioGroups and ComboBoxes
  // (in the following called just "GUI component").
  // The OnItemChanged event is generated, whenever the item index of the
  // GUI component is changed by the user or in the source code.
  // When using a GUI component with an initialized TMaskedTable object,
  // the GUI component automatically sets the corresponding item index.
  // With this event the software developer has still the possibility to do any
  // further actions, whenever the item index of the GUI component changes.
  // For example further GUI components may be enabled/disabled or shown/hidden.
  // The information about the last and new selections is passed as arguments,
  // when the event is generated, so that the SW developer has just to evaluate the
  // passed arguments, in order to determine any further actions.
  //
  // Parameters: sender       - The GUI component (RadioGroup, Combobox), generated the event
  //             oldItemIndex - The index of the last selected item in the GUI component
  //             newItemIndex - The index of the new/currently selected item in the GUI component
  //             oldEntry     - The table entry of the last selection. This parameter may be nil,
  //                            if the last selection was invalid!
  //             newEntry     - The table entry of the new/current selection.
  //                            This parameter may be nil, if the new/current selection is invalid!
  // ===========================================================================
  TItemChangedEvent = procedure ( Sender : TObject; oldItemIndex, newItemIndex : integer;
                                  oldEntry, newEntry : TMaskedTableEntry ) of object;


  // ===========================================================================
  //                    E N U M   D E C L A R A T I O N S
  // ===========================================================================


  // ===========================================================================
  // Aufzählungstyp: TDataType
  // Definition für Datentypen eines Objektes (KO, CAN-Objekt, etc.),
  // die z.B. in externen Dateien hinterlegt sind.
  // Dieser Datentyp wird benutzt, um den in der Datei hinterlegten Datentyp
  // intern darzustellen. (s. z.B. Einlesen der EDS-Datei im PDO-Editor)
  // ===========================================================================
  TDataType = (
    dtINT8,    // signed    8-Bit
    dtUINT8,   // unsigned  8-Bit
    dtINT16,   // signed   16-Bit
    dtUINT16,  // unsigned 32-Bit
    dtINT32,   // signed   32-Bit
    dtUINT32   // unsigned 32-Bit
  ); // TDataType


  // ===========================================================================
  // Aufzählungstyp: TAccessType
  // Definiert die Zugriffsart auf ein KO, CAN-Objekt, etc.
  // Dieser Datentyp wird benutzt, um z.B. die in der Datei hinterlegten Zugriffsart
  // intern darzustellen. (s. z.B. Einlesen der EDS-Datei im PDO-Editor)
  // ===========================================================================
  TAccessType = (
    atNoReadWrite, // kein Zugriff
    atReadOnly,    // nur Lesezugriff
    atWriteOnly,   // nur Schreibzugriff
    atReadWrite    // Lese- und Schreibzugriff
  ); // TAccessType
    

  // ===========================================================================
  //                   R E C O R D   D E C L A R A T I O N S
  // ===========================================================================


  // ===========================================================================
  // Record TMaskedTableMask
  // A record describing the mask for the class TMaskedTable.
  // ===========================================================================
  TMaskedTableMask = record
    s_alias : string;   // symbolic identifier (alias) of the mask
    lw_mask : longword; // the value of the mask
  end; // type TMaskedTableMask = record


  // ===========================================================================
  // Record: TLimitInfo
  // Dieser Rekord bildet die Grenzen eines Parameters ab.
  // ===========================================================================
  TIntLimitInfo = record
    i64_min : Int64; // untere Grenze des Parameters
    i64_max : Int64; // obere Grenze des Parameters
  end; // TIntLimitInfo = record


  // ===========================================================================
  //                    I N T E R F A C E   D E C L A R A T I O N S
  // ===========================================================================

  // ===========================================================================
  // Klasse: TMtxCloneableObject
  // Von allen Klassen zu implementieren, die zentral geklont werden können sollen,
  // z.B. als UserData in TMaskedTableObject, d.h. wenn der eigentliche Datentyp
  // nicht wirklich bekannt ist, sondern die Instanz als TObject da ist.  
  // ===========================================================================
  TMtxCloneableObject = class ( TObject )
    function Clone() : TMtxCloneableObject; virtual; abstract; 
  end;


  // ===========================================================================
  //                    C L A S S   D E C L A R A T I O N S
  // ===========================================================================

  // Forward declarations
  TMtxCOComponentControlCOReadWrite = class;
  TMtxCOComponentSingleCOReadOnly   = class;
  TMtxExtCOAssociatedComponentList  = class;
  TMtxCOComponentSingleCOReadWrite  = class;



  // ===========================================================================
  // Class TBoolean
  // A class representating the simple data type "boolean".
  // This class is used to store boolean user data in classes such as
  // TMaskedTableEntry.
  // ===========================================================================
  TBoolean = class ( TMtxCloneableObject )
    protected
      b_value : boolean;

    public
      property    Value   : boolean read b_value       write b_value;

      constructor create(); overload; virtual;
      constructor create ( b_val : boolean ); overload; virtual;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TBoolean = class



  // ===========================================================================
  // Class TDouble
  // A class representating the simple data type "double".
  // This class is used to store double user data in classes such as
  // TMaskedTableEntry.
  // ===========================================================================
  TDouble = class ( TMtxCloneableObject )
    protected
      d_value : double;

    public
      property Value   : double  read d_value       write d_value;

      constructor create(); overload; virtual;
      constructor create ( d_val : double ); overload; virtual;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TDouble = class


  // ===========================================================================
  // Class TAcknowledgedDouble
  // A data container consisting of a double and a boolean value.
  // The boolean value defines, whether the double value is valid or not.
  // ===========================================================================
  TAcknowledgedDouble = class ( TDouble )
    private
      b_value_valid : boolean;
    public
      property isValid : boolean read b_value_valid write b_value_valid;

      constructor create(); override;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TAcknowledgedDouble = class
  PAcknowledgedDouble = ^TAcknowledgedDouble;

  // ===========================================================================
  // Class TInteger
  // A class representating the simple data type "Int64".
  // This class is used to store integer user data in classes such as
  // TMaskedTableEntry.
  // ===========================================================================
  TInteger = class ( TMtxCloneableObject )
    protected
      li_value : longint;

    public
      property Value : longint read li_value write li_value;

      constructor Create   (); overload; virtual;
      constructor Create   ( i   : longint  ); overload; virtual;
      procedure   copyFrom ( src : TInteger );

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TInteger = class


  // ===========================================================================
  // Class TInt64
  // A class representating the simple data type "Int64".
  // This class is used to store integer user data in classes such as
  // TMaskedTableEntry.
  // ===========================================================================
  TInt64 = class ( TMtxCloneableObject )
    protected
      i64_value : Int64;

    public
      property Value : int64 read i64_value write i64_value;

      constructor create(); overload; virtual;
      constructor create ( i64 : int64 ); overload; virtual;
      procedure   copyFrom ( src : TInt64 );

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TInt64 = class


  // ===========================================================================
  // Class TAcknowledgedInt64
  // A data container consisting of an int64 and a boolean value.
  // The boolean value defines, whether the int64 value is valid or not.
  // ===========================================================================
  TAcknowledgedInt64 = class ( TInt64 )
    private
      b_value_valid : boolean;
    public
      property isValid : boolean read b_value_valid write b_value_valid;

      constructor create(); override;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TAcknowledgedInt64 = class


  // ===========================================================================
  // Class: TBitListEntry
  // This class represents an entry of a TBitList.
  // ===========================================================================
  TBitListEntry = class ( TMtxCloneableObject )
    private
      lw_bit          : longword;   // Bit value, e.g. 0x80000000 = Bit 31, 0x00000001 = Bit 0
      str_alias       : string;     // An unique alias for the list, containing this bit entry.
                                    // Alias = abstraction of the underlying bit number.
      str_text        : string; // Description: Meaning of the bit
      b_inv_logic     : boolean;    // Flag: The bit has inverted logic?
      i64_user_data   : int64;      // Platzhalter für benutzerdefinierte Daten
    public
      property Alias    : string     read str_alias     write str_alias;
      property Bit      : longword   read lw_bit        write lw_bit;
      property Text     : string read str_text      write str_text;
      property InvLogic : boolean    read b_inv_logic   write b_inv_logic;
      property UserData : int64      read i64_user_data write i64_user_data;

      constructor create(); overload;
      constructor create ( s_alias : string; bit : longword; s_text : string;
                           inv_logic : boolean = C_DEF_BIT_INV_LOGIC ); overload;
      destructor  Destroy(); override;
      function    equals   ( e : TBitListEntry ) : boolean; {$IFDEF DELPHI_XE_UP} reintroduce; {$ENDIF}
      procedure   copyFrom ( entry : TBitListEntry );

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TBitListEntry = class


  // ===========================================================================
  // Class: TBitList
  // Implementation of a list, containing bit information.
  // This list can be used for storing the bit meaning of a statusword or
  // controlword.
  // ===========================================================================
  TBitList = class ( TMtxCloneableObject )
    private
      entries : TList;
      function indexOf ( entry : TBitListEntry ) : integer;
      function getCount() : integer;

    public
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;
      procedure   clear();
      procedure   add           ( entry : TBitListEntry ); overload;
      procedure   add           ( alias : string; bit : longword; text : string;
                                  inv_logic : boolean = C_DEF_BIT_INV_LOGIC ); overload;
      procedure   delete        ( entry : TBitListEntry );
      function    getEntry      ( inx   : integer    ) : TBitListEntry;
      function    getByAlias    ( alias : string     ) : TBitListEntry;
      function    getByBit      ( bit   : longword   ) : TBitListEntry;
      function    getByText     ( text  : string ) : TBitListEntry;
      function    isEmpty() : boolean;
      function    contains      ( entry : TBitListEntry ) : boolean;
      procedure   copyFrom      ( list  : TBitList );

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TBitList = class


  // ===========================================================================
  // Class: TDoubleList
  // Implementation of a list, which contains only double values.
  // ===========================================================================
  TDoubleList = class ( TMtxCloneableObject )
    private
      list_values : TList; // the internal list containing the values

    protected
                  // Number of elements in the list
      function    getCount() : integer;

    public
      // Number of elements in the list
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;

                  // Copies the elements of list to the calling object.
      procedure   copyFrom ( list : TDoubleList );

                  // Adds a new value to the list
      procedure   add ( value : double ); overload;
                  // Adds a new value to the list at the specified index
      procedure   add ( index : integer; value : double ); overload;
                  // Raturns the value at the specified index
      function    get ( index : integer ) : double;

                  // Updates the entry at the specified index
      procedure   update ( index : integer; value : double );

                  // Clears the contents of the list
      procedure   clear();

                  // Tests whether the list is empty or not.
      function    isEmpty() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;

  end; // type TDoubleList = class


  // ===========================================================================
  // Class: TAcknowledgedDoubleList
  // Implementation of a list, which contains only variables of type TAcknowledgedDouble.
  // ===========================================================================
  TAcknowledgedDoubleList = class ( TMtxCloneableObject )
    private
      list_values : TList; // the internal list containing the values

    protected
                  // Number of elements in the list
      function    getCount() : integer;

    public
                  // Number of elements in the list
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;

                  // Copies the elements of list to the calling object.
      procedure   copyFrom ( list : TAcknowledgedDoubleList );

                  // Adds a new value to the list
      procedure   add ( value : double; b_valid : boolean ); overload;
                  // Adds a new value to the list
      procedure   add ( value : TAcknowledgedDouble ); overload;
                  // Adds a new value to the list at the specified index
      procedure   add ( index : integer; value : TAcknowledgedDouble ); overload;

                  // Returns the value at the specified index
      function    get ( index : integer ) : TAcknowledgedDouble;

                  // Updates the entry at the specified index
      procedure   update ( index : integer; entry : TAcknowledgedDouble ); overload;

                  // Updates the double value of the entry at the specified index
      procedure   update ( index : integer; d_value : double ); overload;

                  // Updates the boolean value of the entry at the specified index
      procedure   update ( index : integer; b_valid : boolean ); overload;

                  // Clears the contents of the list
      procedure   clear();

                  // Tests whether the list is empty or not.
      function    isEmpty() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // type TAcknowledgedDoubleList = class


  // ===========================================================================
  // Class: TByteList
  // Implementation of a list, which contains only byte values.
  // ===========================================================================
  TByteList = class ( TMtxCloneableObject )
    private
      list_values : TList; // the internal list containing the values

    protected
                  // Number of elements in the list
      function    getCount() : integer;

    public
                  // Number of elements in the list
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;

                  // Copies the elements of list to the calling object.
      procedure   copyFrom ( list : TByteList );

                  // Adds a new value to the list
      procedure   add ( value : byte ); overload;
                  // Adds a new value to the list at the specified index
      procedure   add ( index : integer; value : byte ); overload;
                  // Raturns the value at the specified index
      function    get ( index : integer ) : byte;
                  // Updates the entry at the specified index
      procedure   update ( index : integer; value : byte );
                  // Clears the contents of the list
      procedure   clear();

                  // Tests whether the list is empty or not.
      function    isEmpty() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // type TByteList = class


  // ===========================================================================
  // Class: TInt64List
  // Implementation of a list, which contains only int64 values.
  // ===========================================================================
  TInt64List = class ( TMtxCloneableObject )
    private
      list_values : TList; // the internal list containing the values

    protected
                  // Number of elements in the list
      function    getCount() : integer;

    public
                  // Number of elements in the list
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;

                  // Copies the elements of list to the calling object.
      procedure   copyFrom ( list : TInt64List );

                  // Adds a new value to the list
      procedure   add ( value : int64 ); overload;
                  // Adds a new value to the list at the specified index
      procedure   add ( index : integer; value : int64 ); overload;
                  // Raturns the value at the specified index
      function    get ( index : integer ) : int64;
                  // Updates the entry at the specified index
      procedure   update ( index : integer; value : int64 );
                  // Clears the contents of the list
      procedure   clear();

                  // Tests whether the list is empty or not.
      function    isEmpty() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // type TInt64List = class


  // ===========================================================================
  // Class: TLongwordList
  // Implementation of a list, which contains only longword values.
  // ===========================================================================
  TLongwordList = class ( TMtxCloneableObject )
    private
      list_values : TList; // the internal list containing the values
      FAllowDuplicates : boolean; // 2009-10-05 /gsv/: Duplikate zulassen?

    protected
                  // Number of elements in the list
      function    getCount() : integer;

    public
                  // Number of elements in the list
      property    Count : integer read getCount;
                  // 2009-10-05 /gsv/: Duplikate zulassen?
      property    AllowDuplicates : boolean read FAllowDuplicates write FAllowDuplicates;

      constructor create();
      destructor  Destroy(); override;

                  // Copies the elements of list to the calling object.
      procedure   copyFrom ( list : TLongwordList );

                  // Adds a new value to the list
      procedure   add ( value : longword ); overload;
                  // Adds a new value to the list at the specified index
      procedure   add ( index : integer; value : longword ); overload;
                  // Raturns the value at the specified index
      function    get ( index : integer ) : longword;
                  // Updates the entry at the specified index
      procedure   update ( index : integer; value : longword );
                  // Clears the contents of the list
      procedure   clear();

      // 2009-10-05 /gsv/: Prüft, ob ein Longword-Wert in der Liste bereits enthalten ist.
      function    contains ( lw : longword ) : boolean;

                  // Tests whether the list is empty or not.
      function    isEmpty() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // type TLongwordList = class

  
  // ===========================================================================
  // Class TMaskedTableEntry
  // An entry for the class TMaskedTable
  // ===========================================================================
  TMaskedTableEntry = class ( TMtxCloneableObject )
    private
      s_alias     : string;     // symbolic identifier (alias) of the bitfield option
      lw_bitfield : longword;   // the value of the bitfield option
      s_text      : string;     // the text to be displayed in the RadioGroup/Combobox
      i_text_id   : integer;    // 2015-12-02 /gsv/: TextID als Platzhalter für den sprachabhängigen Text
      objUserData : TObject;    // "pointer" to an user defined data (optional, nil if not set!)
                                // 2009-10-05 /gsv/: Manchmal soll bei unterschiedlichen
                                // KO-Werten der gleiche Text angezeigt / die gleiche
                                // Option ausgewählt werden. Dabei soll der Eintrag / Text
                                // in einer RadioGroup/Combobox nur einmal vorhanden sein.
                                // Deshalb wird eine optionale Liste mit alternativen Bitfeldern
                                // implementiert, die nur für die Richtung Servo --> Para-SW benutzt wird.
                                // Beim Setzen des KO-Wertes wird die Property Bitfield verwendet.
      list_alt_bitfields : TLongwordList;

    public
      constructor create();
      destructor  Destroy(); override;
      procedure   copyFrom ( entry : TMaskedTableEntry );
      function    equals ( e : TMaskedTableEntry ) : boolean; {$IFDEF DELPHI_XE_UP} reintroduce; {$ENDIF}

      // 2009-10-05 /gsv/: Alternative Bitfeld-Definitionen zurückgeben
      // Existieren alternative Bitfeld-Definitionen?
      function    hasAltBitfields() : boolean;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;      

    published
      property Alias    : string              read s_alias     write s_alias;
      property Bitfield : longword            read lw_bitfield write lw_bitfield;
      property Text     : string              read s_text      write s_text;
      property UserData : TObject             read objUserData write objUserData;
      // 2009-10-05 /gsv/: Liste mit den alternativen Bitfeld-Werten
      property AltBitfields : TLongwordList   read list_alt_bitfields;
      // 2015-12-02 /gsv/: TextID als Platzhalter für den sprachabhängigen Text
      property TextID   : integer             read i_text_id   write i_text_id default C_TEXT_ID_DEFAULT_VALUE;

  end; // type TMaskedTableEntry = record


  // ===========================================================================
  // Class TMaskedTable
  // A class implementing a table with entries of data type TMaskedTableEntry.
  // This class is used by radio groups and combo boxes and provides a
  // translation table bitfield <-> displayed text (==> ItemIndex).
  // Such a table is always associated with one mask!
  // ===========================================================================
  TMaskedTable = class ( TMtxCloneableObject )
    protected
      mask      : TMaskedTableMask;
      entries   : TObjectList;
      FOnAdd    : TMaskedTableEvent;
      FOnClear  : TNotifyEvent;
      FOnDelete : TMaskedTableEvent;
      FCapitalizeFirstLetter : boolean; // 2016-09-28 /gsv/: Flag: ersten Buchstaben immer in Großbuchstaben umwandeln?
      function    indexOf ( entry : TMaskedTableEntry ) : integer;
      function    getCount() : integer;

    public
      property    Count : integer read getCount;
      // 2016-09-28 /gsv/: Flag: ersten Buchstaben immer in Großbuchstaben umwandeln?
      // Diese Property hat keine Auswirkung auf bereits vorhandene Einträge!
      property    CapitalizeFirstLetter : boolean read FCapitalizeFirstLetter write FCapitalizeFirstLetter;

      constructor create();
      destructor  Destroy(); override;
      procedure   clear();
      function    add           ( entry : TMaskedTableEntry ) : boolean; overload;
      function    add           ( alias : string; bitfield : longword; text : string ) : boolean; overload;
      function    add           ( alias : string; bitfield : longword; text : string; userData : TMtxCloneableObject ) : boolean; overload;
      function    add           ( alias : string; bitfield : longword; text : string; userData : TObject             ) : boolean; overload;
      // 2015-12-02 /gsv/: Neue add-Funktionen mit TextID (Platzhalter für den sprachabhängigen Text) 
      function    add           ( alias : string; bitfield : longword; text : string; text_id : integer ) : boolean; overload;
      function    add           ( alias : string; bitfield : longword; text : string; text_id : integer; userData : TObject ) : boolean; overload;

      procedure   delete        ( entry : TMaskedTableEntry );
      function    getEntry      ( inx      : integer    ) : TMaskedTableEntry;
      function    getByAlias    ( alias    : string     ) : TMaskedTableEntry;
      function    getByBitfield ( bitfield : longword   ) : TMaskedTableEntry; overload;

      function    getIndexByBitfield ( bitfield : longword ) : integer;

      function    getByBitfield ( bitfield : longword; var index : integer ) : boolean; overload;
      function    getByBitfield ( bitfield : longword; var index : integer; var entry : TMaskedTableEntry ) : boolean; overload;

      function    getByText     ( text     : string ) : TMaskedTableEntry;
      function    getMask() : TMaskedTableMask;
      procedure   setMask       ( new_mask :  TMaskedTableMask );     overload;
      procedure   setMask       ( alias : string; value : longword ); overload;
      function    isEmpty() : boolean;
      function    contains      ( entry : TMaskedTableEntry ) : boolean;
      procedure   copyFrom      ( table : TMaskedTable );

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;

    published
      property OnAdd    : TMaskedTableEvent read FOnAdd    write FOnAdd;
      property OnClear  : TNotifyEvent      read FOnClear  write FOnClear;
      property OnDelete : TMaskedTableEvent read FOnDelete write FOnDelete;
  end; // TMaskedTable = class


  // ===========================================================================
  // Class: TMaskedTableList
  // A list containing TMaskedTable-Objects
  // ===========================================================================
  TMaskedTableList = class ( TMtxCloneableObject )
    protected
      entries : TObjectList;
      function    getCount() : integer;

    public
      property    Count : integer read getCount;

      constructor create();
      destructor  Destroy(); override;

      procedure   clear();
      procedure   add ( table : TMaskedTable );
      function    getTable ( inx : integer ) : TMaskedTable;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;      
  end; // TMaskedTableList = class


  // ===========================================================================
  // class TFraction
  // Implementation of a fraction numerator/divisor.
  // ===========================================================================
  TFraction = class ( TMtxCloneableObject )
    private
      i64_numerator      : int64;
      i64_divisor        : int64;
      d_decimal_fraction : double;
    protected
      procedure setNumerator ( new_numerator : int64 );
      procedure setDivisor   ( new_divisor   : int64 );
      procedure refreshDecimalFraction();

    public
      property Numerator       : int64  read i64_numerator write setNumerator;
      property Divisor         : int64  read i64_divisor   write setDivisor;
      property DecimalFraction : double read d_decimal_fraction;

      constructor create();
      function equals ( fraction : TFraction ) : boolean; {$IFDEF DELPHI_XE_UP} reintroduce; {$ENDIF}

      // 2009-11-21 /gsv/: Größten gemeinsamen Teiler ermitteln
      function  getGCD() : int64;
      // 2009-11-21 /gsv/: Bruch kürzen
      procedure shorten();

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;
  end; // TFraction = class

  
  // ===========================================================================
  // Klasse: TDataCmdCO / DatenCluster Oszilloskop
  // Datencontainer für das Auslesen der Oszi-Daten
  // Ein Datencluster nimmt einen ROD-Befehl auf
  // ===========================================================================
  TOszCluster = class ( TMtxCloneableObject )
    protected
      iChannel      : longword;  // Kanal
      iCluster      : longword;  // Cluster / Paketnummer
      str_answer    : string;    // Antwortdaten / Messwerte
    public
      // Kanal
      property Channel : longword read iChannel      write iChannel;
      // Cluster / Paketnummer
      property Cluster : longword read iCluster      write iCluster;
      // Antwortdaten / Messwerte
      property Answer  : string   read str_answer    write str_answer;

      constructor create();
      // Kopierfunktion: Kopiert osz in self
      procedure copyFrom ( const osz : TOszCluster );
      // Rückgabe eines Messwertes
      function  getLong  ( i : integer ) : longword;

      // 2014-08-21 /gsv/: Kopie des Objektes erstellen
      function    Clone() : TMtxCloneableObject; override;      
  end; // TOszCluster = class




  TMtxGUICODataBase = class ( TPersistent )
    protected
      i64Value           : int64;   // KO-Wert (OR, High- + Low-Anteil)
      i64ValueBackup     : int64;   // Backup-Wert des KOs (1. ausgelesener Wert)
      i64ValueOI         : int64;   // OI-Wert des KOs, falls das KO OI-Wert hat (High- + Low-Anteil)
      bHasOIValue        : boolean; // Flag: KO hat OI-Wert?
      bReadCyclic        : boolean; // Flag: KO auch in prog_update zyklisch auslesen?
      bReadORValue       : boolean; // Flag: OR-Wert auslesen?
      bReadOIValue       : boolean; // Flag: OI-Wert auslesen? (falls vorhanden)
      bRestoreOldValue   : boolean; // Flag: ursprünglichen KO-Wert bei Abbruch restaurieren?


    public
      constructor Create(); virtual;
      procedure   copyFrom ( obj : TMtxGUICODataBase ); virtual;

    published
      // KO-Wert (OR, High- + Low-Anteil)
      property Value              : int64   read i64Value         write i64Value         default 0;
      // Backup-Wert des KOs (1. ausgelesener Wert)
      property ValueBackup        : int64   read i64ValueBackup   write i64ValueBackup   default 0;
      // OI-Wert des KOs, falls das KO OI-Wert hat (High- + Low-Anteil)
      property ValueOI            : int64   read i64ValueOI       write i64ValueOI       default 0;

      // Flag: KO hat OI-Wert?
      property HasOIValue         : boolean read bHasOIValue      write bHasOIValue      default false;
      // Flag: KO auch in prog_update zyklisch auslesen?
      property ReadCyclic         : boolean read bReadCyclic      write bReadCyclic      default false;
      // Flag: OR-Wert auslesen?
      property ReadORValue        : boolean read bReadORValue     write bReadORValue     default true;
      // Flag: OI-Wert auslesen? (falls vorhanden)
      property ReadOIValue        : boolean read bReadOIValue     write bReadOIValue     default false;
      // Flag: ursprünglichen KO-Wert bei Abbruch restaurieren?
      property RestoreOldValue    : boolean read bRestoreOldValue write bRestoreOldValue default true;
  end;


  TMtxGUICODataSingleCO = class ( TMtxGUICODataBase )
    protected
      sName              : string;  // Name des KOs

    public
      constructor Create(); override;
      procedure   copyFrom ( obj : TMtxGUICODataBase ); override;

    published
      // Name des KOs
      property Name               : string  read sName            write sName;
  end;


  TMtxGUICODataInt64 = class ( TMtxGUICODataBase )
    protected
      sNameHigh          : string;  // Name des KOs für den High-Anteil
      sNameLow           : string;  // Name des KOs für den Low-Anteil

    public
      constructor Create(); override;
      procedure   copyFrom ( obj : TMtxGUICODataBase ); override;

    published
      // Name des KOs für den High-Anteil
      property NameHigh           : string  read sNameHigh        write sNameHigh;
      // Name des KOs für den Low-Anteil
      property NameLow            : string  read sNameLow         write sNameLow;
  end;


  // ===========================================================================
  // Klasse: TMtxGUICODataSimpleMultiCO
  // Einfaches Multi-KO
  // ===========================================================================
  TMtxGUICODataSimpleMultiCO = class ( TPersistent )
    private
      FPointerCOName              : string;
      FPointerCOMin               : int64;
      FPointerCOMax               : int64;
      FPointerCOVal               : int64;

      FDataCOInfo                 : TMtxGUICODataSingleCO;
      FDataCOValues               : TInt64List;
      FDataCOValuesBackup         : TInt64List;

    public
      constructor Create(); virtual;
      procedure   copyFrom ( obj : TMtxGUICODataSimpleMultiCO );

      property PointerCOMin       : int64                 read FPointerCOMin       write FPointerCOMin default 0;
      property PointerCOMax       : int64                 read FPointerCOMax       write FPointerCOMax default 0;
      property PointerCOVal       : int64                 read FPointerCOVal       write FPointerCOVal default 0;

      property DataCOValues       : TInt64List            read FDataCOValues       write FDataCOValues;
      property DataCOValuesBackup : TInt64List            read FDataCOValuesBackup write FDataCOValuesBackup;

    published
      // Name des Zeiger-KOs
      property PointerCOName      : string                read FPointerCOName      write FPointerCOName;
      // Info über das Daten-KO (Name, OR-/OI-Wert lesen, etc.)
      property DataCOInfo         : TMtxGUICODataSingleCO read FDataCOInfo         write FDataCOInfo;
  end;


  // ===========================================================================
  // Klasse: TMtxThread
  // Basisklasse für die von Thread abgeleiteten Klassen.
  // Die Funktionen Suspend() und Resume() sind veraltet. Diese Basisklasse
  // bietet alternative Funktionen.
  // ===========================================================================
  TMtxThread = class ( TThread )
    protected
      FSuspendEvent       : TEvent;
      FCreateSuspendedMtx : boolean; // Flag: Thread als Suspended erzeugt?
    public
    {$IFDEF DELPHI_XE_UP}
      constructor Create(); overload;
    {$ENDIF}
      constructor Create ( CreateSuspended: Boolean ); {$IFDEF DELPHI_XE_UP} overload; {$ENDIF}
      destructor  Destroy(); override;

    {$IFDEF DELPHI_XE_UP}
      procedure   TerminatedSet(); override;
    {$ENDIF}

      procedure   SuspendWork();
      procedure   ResumeWork();
      procedure   CheckHandleSuspended();
      function    isSuspended() : boolean;
  end;






  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponent
  // Basis-Interface, über welches eine GUI-Klasse als Metronix GUI-Klasse
  // gekennzeichnet werden kann.
  // ===========================================================================
  IMtxGUIComponent = interface
    ['{05DB41AF-A94F-4402-8BA1-DB2BB5FB76A2}']
  end;

  IMtxGUIComponentWithHint = interface (IMtxGUIComponent)
    ['{FD0D86BB-7D27-43EF-BDE3-4D06911CBD08}']
    procedure CancelHint();
  end;

  IMtxGUIComponentMultiLanguage = interface ( IMtxGUIComponent )
    ['{F8EE35A8-F075-4A2D-82AB-9DF464C36227}']

    // Getter/Setter der Property TextID
    procedure setTextID ( id : integer );
    function  getTextID () : integer;

    // Getter/Setter der Property TextRemoveChars
    procedure setTextRemoveChars ( s : string );
    function  getTextRemoveChars() : string;

    // Funktion, um den Text/die Caption zu aktualisieren
    // ws: sprachabhängiger Text
    procedure updateText ( ws : string );

    // Text-ID für die Sprachunterstützung. Über die Text-ID wird in der Klasse
    // TUpdateFormWithAutomation der Text / die Caption aktualisiert.    
    property  TextID          : integer read getTextID          write setTextID;
    // Zeichen, die aus dem Text (s. TextID) entfernt werden.
    // Die Zeichen werden durch die Klasse TUpdateFormWithAutomation entfernt.
    property  TextRemoveChars : string  read getTextRemoveChars write setTextRemoveChars;
  end;


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxUpdatedGUIComponent
  // Dieses Interface definiert die Schnittstelle für eine Metronix GUI-Komponente,
  // die mit dem Regler interagiert und minimale Funktionen / Eigenschaften
  // für die Darstellung der Werte unterstützt.
  // ===========================================================================
  IMtxUpdatedGUIComponent = interface ( IMtxGUIComponent )
    ['{CC2A0A27-69DD-416E-8926-81CA761B6F7E}']

    // Getter / Setter für die Enabled-Eigenschaft
    function  GetEnabled() : boolean;
    procedure SetEnabled ( b_on : boolean );

    // Komponente zurücksetzen (bei win_reset()).
    procedure reset();

    // Berechnung Servo --> GUI (==> COValue anzeigen)
    procedure calcValueFromServo();

    // Komponente aktiv / inaktiv
    property Enabled : boolean read GetEnabled write SetEnabled;

  end;

  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxUpdatedGUIComponentReadOnly
  // Dieses Interface definiert die Schnittstelle für eine Metronix GUI-Komponente,
  // die mit dem Regler interagiert und readonly ist.
  // ===========================================================================
  IMtxUpdatedGUIComponentReadOnly = interface ( IMtxUpdatedGUIComponent )
    ['{5A6E2C6E-D2DC-4204-B0A3-6963EF6EC86B}']
  end; // IMtxUpdatedGUIComponentReadOnly = interface

  
  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxUpdatedGUIComponent
  // Dieses Interface definiert die Schnittstelle für eine Metronix GUI-Komponente,
  // die mit dem Regler interagiert und die is_changed()-Eigenschaft unterstützt.
  // ===========================================================================
  IMtxUpdatedGUIComponentReadWrite = interface ( IMtxUpdatedGUIComponent )
    ['{15CF819A-C817-4D20-B774-B886E66FA4A2}']

    // Getter/Setter für die Property is_changed
    function  getIsChanged() : boolean;
    procedure setIsChanged ( b : boolean );

    // Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
    procedure calcValueToServo();

    // is_changed-Property: Sollte nur bei Änderung durch den Benutzer gesetzt werden.
    property  is_changed        : boolean read getIsChanged          write setIsChanged;
  end; // IMtxUpdatedGUIComponentReadWrite = interface


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxUpdatedGUIComponentWithLimitsAndPhysUnit
  // Metronix GUI-Komponente, die zusätztlich die Eingabegrenzen entsprechend
  // den KO-Grenzen setzt und physikalische Einheit unterstützt. 
  // ===========================================================================  
  IMtxUpdatedGUIComponentWithLimitsAndPhysUnit = interface (IMtxUpdatedGUIComponent)
    ['{CEECE5F3-DA7D-40A6-BC96-CC175182B90A}']

    // Grenzen in Basiseinheiten setzen
    procedure setLimitsInBaseUnits      ( min, max : int64 );
    // Schrittweiten zur Änderung des Wertes in Benutzereinheiten setzen (s. auch TInbox)
    procedure setChangeStepsInUserUnits ( d_small, d_large : double );

    // Getter/Setter für die Property prop_factor (Umrechnungsfaktor)
    procedure setPropFactor ( fac : extended );
    function  getPropFactor() : extended;

    // Getter/Setter für die Property PostString (Physikalische Einheit)
    procedure setPostString ( ws : string );
    function  getPostString() : string;

    // Getter/Setter für die Property Decimals (Anzahl Nachkommastellen)
    procedure setDecimals ( i : integer );
    function  getDecimals() : integer;

    // Anzahl der anzuzeigenden signifikanten Nachkommastellen (bei Wert, der nach der Rundung 0,00.. ergibt)
    procedure setDecimals_signif ( i : integer );
    function  getDecimals_signif() : integer;

    // Übersetzt den Text bei einem Wert außerhalb des Bereichs
    procedure set_sprach_err ( i : integer );

    // Umrechnungsfaktor (Basiseinheiten <-> Benutzereinheiten)
    property prop_factor     : extended   read getPropFactor      write setPropFactor;
    // Physikalische Einheit (string)
    property PostString      : string read getPostString      write setPostString;
    // Anzahl der anzuzeigenden Nachkommastellen
    property Decimals        : integer    read getDecimals        write setDecimals;
    // Anzahl der anzuzeigenden signifikanten Nachkommastellen (bei Wert, der nach der Rundung 0,00.. ergibt)
    property Decimals_signif : integer    read getDecimals_signif write setDecimals_signif;

  end;


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponentWithCO
  // Allgemeines Interface, um erkennen zu können, dass die GUI-Komponente eines
  // der nachfolgenden Interfaces mit Angabe des auszulesenden bzw. zu beschreibenden
  // KOs implementiert.
  // In der Klasse TUpdateFormWithAutomation werden diese KOs automatisch ermittelt,
  // so dass Standard-Aktionen wie "KO auslesen", "KO anzeigen" und
  // "durch Benutzer modifizierten Wert schreiben" automatisiert werden können. 
  // ===========================================================================
  IMtxGUIComponentWithCO = interface ( IMtxUpdatedGUIComponent )
    ['{848BB55C-46D3-4180-BB6C-E593712FB82B}']
    // Getter/Setter für die Property WriteAccessRunning
    function  getWriteAccessRunning() : boolean;
    procedure setWriteAccessRunning ( b : boolean );
    // Flag: KO-Schreibzugriff aktiv?
    property WriteAccessRunning : boolean read getWriteAccessRunning write setWriteAccessRunning;    
  end;

  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponentWithSingleCO
  // Schnittstellenbeschreibung für eine GUI-Komponente, die ein einziges
  // normales KO (also kein Multi-KO)  ausliest bzw. beschreibt.
  // ===========================================================================
  IMtxGUIComponentWithSingleCO = interface ( IMtxGUIComponentWithCO )
    ['{84E1FB69-B349-4CDE-99E3-57565FCF8CE8}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxGUICODataSingleCO;
    procedure setCOData ( obj : TMtxGUICODataSingleCO );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxGUICODataSingleCO read getCOData write setCOData;

  end; // IMtxGUIComponentWithSingleCO = interface ( IMtxGUIComponentWithCO )


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponentWithControlCO
  // Schnittstellenbeschreibung für eine GUI-Komponente, die ein einziges
  // Control-KO ausliest bzw. beschreibt.
  // ===========================================================================
  IMtxGUIComponentWithControlCO = interface ( IMtxGUIComponentWithSingleCO )
    ['{637D0A82-7076-47E8-9FA5-49D38096BFA9}']
  end;


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponentWithCO64Bit
  // Schnittstellenbeschreibung für eine GUI-Komponente, die zwei KOs
  // (insgesamt ein 64-Bit Wert, 1. KO High-Anteil, 2. KO Low-Anteil)
  // ausliest bzw. beschreibt. Typischer Anwendungsfall: Positionsdaten 64-Bit
  // ===========================================================================
  IMtxGUIComponentWithCO64Bit = interface ( IMtxGUIComponentWithCO )
    ['{8936A264-91A0-41BE-9776-DE790FB169F9}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxGUICODataInt64;
    procedure setCOData ( obj : TMtxGUICODataInt64 );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxGUICODataInt64 read getCOData write setCOData;
  end; // IMtxGUIComponentWithCO64Bit = interface ( IMtxGUIComponentWithCO )


  // ===========================================================================
  // 2015-11-27 /gsv/:
  // Interface: IMtxGUIComponentWithSimpleMultiCO
  // Schnittstellenbeschreibung für eine GUI-Komponente, die zwei KOs
  // (insgesamt ein 64-Bit Wert, 1. KO High-Anteil, 2. KO Low-Anteil)
  // ausliest bzw. beschreibt. Typischer Anwendungsfall: Positionsdaten 64-Bit
  // ===========================================================================
  IMtxGUIComponentWithSimpleMultiCO = interface ( IMtxGUIComponentWithCO )
    ['{FF192D41-9921-47AA-90BA-BE474EFFF186}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxGUICODataSimpleMultiCO;
    procedure setCOData ( obj : TMtxGUICODataSimpleMultiCO );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxGUICODataSimpleMultiCO read getCOData write setCOData;
  end; // IMtxGUIComponentWithSimpleMultiCO = interface ( IMtxGUIComponentWithCO )


  // ===========================================================================
  // 2016-09-27 /gsv/:
  // Interface: IMtxGUIComponentWithExternalCO
  // Basis-Interface für eine GUI-Komponente, die ein KO ausliest bzw. beschreibt.
  // Die GUI-Komponente hat in diesem Fall nur eine Objekt-Referenz auf
  // das zu verwendete KO (externes KO, z.B. sinnvoll, wenn in einem Fenster mehrere
  // Checkboxen / Comboboxen auf das gleiche KO zugreifen).
  // ===========================================================================
  IMtxGUIComponentWithExternalCO = interface ( IMtxGUIComponentWithCO )
    ['{297B92E6-E90E-465F-89AF-CDF6BC4D7582}']
  end;


  // ===========================================================================
  // 2016-09-27 /gsv/:
  // Interface: IMtxGUIComponentWithExternalSingleCO
  // Basis-Interface für eine GUI-Komponente, die ein KO ausliest bzw. beschreibt.
  // Die GUI-Komponente hat in diesem Fall nur eine Objekt-Referenz auf
  // das zu verwendete KO (externes KO, z.B. sinnvoll, wenn in einem Fenster mehrere
  // Checkboxen / Comboboxen auf das gleiche KO zugreifen).
  // ===========================================================================
  IMtxGUIComponentWithExternalSingleCOReadOnly = interface ( IMtxGUIComponentWithExternalCO )
    ['{B2089444-9534-4F77-AA20-210C5DE406E2}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxCOComponentSingleCOReadOnly;
    procedure setCOData ( obj : TMtxCOComponentSingleCOReadOnly );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxCOComponentSingleCOReadOnly read getCOData write setCOData;
  end;


  // ===========================================================================
  // 2016-09-27 /gsv/:
  // Interface: IMtxGUIComponentWithExternalSingleCOReadWrite
  // Basis-Interface für eine GUI-Komponente, die ein KO ausliest bzw. beschreibt.
  // Die GUI-Komponente hat in diesem Fall nur eine Objekt-Referenz auf
  // das zu verwendete KO (externes KO, z.B. sinnvoll, wenn in einem Fenster mehrere
  // Checkboxen / Comboboxen auf das gleiche KO zugreifen).
  // ===========================================================================
  IMtxGUIComponentWithExternalSingleCOReadWrite = interface ( IMtxGUIComponentWithExternalCO )
    ['{10079DE4-FA2A-4C96-8D5A-CF6C0B007BA6}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxCOComponentSingleCOReadWrite;
    procedure setCOData ( obj : TMtxCOComponentSingleCOReadWrite );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxCOComponentSingleCOReadWrite read getCOData write setCOData;
  end;



  // ===========================================================================
  // 2016-09-27 /gsv/:
  // Interface: IMtxGUIComponentWithExternalControlCO
  // Schnittstellenbeschreibung für eine GUI-Komponente, die ein einziges
  // Control-KO ausliest bzw. beschreibt.
  // Das zu verwendete KO ist extern definiert (hier nur als Referenz).
  // ===========================================================================
  IMtxGUIComponentWithExternalControlCO = interface ( IMtxGUIComponentWithExternalCO )
    ['{82D7DFDE-30AF-4448-BB38-36F3A7EE8C24}']

    // Getter/Setter für die Property COData
    function  getCOData() : TMtxCOComponentControlCOReadWrite;
    procedure setCOData ( obj : TMtxCOComponentControlCOReadWrite );

    // KO-Daten (Name, Wert, etc.)
    property COData : TMtxCOComponentControlCOReadWrite read getCOData write setCOData;
  end;



  // ===========================================================================
  // 2016-09-27 /gsv/:
  // Interface: IMtxCOComponentWithAssociatedGUIComponents
  // Schnittstellenbeschreibung für eine KO-Komponente, der weitere GUI-Komponenten
  // zugewiesen sind.
  // ===========================================================================
  IMtxCOComponentWithAssociatedGUIComponents = interface
    ['{883C2AF6-0FFA-470F-B9FD-D2D2AA4F7DC7}']
    // Getter/Setter für die Property AssociatedComponents
    function getAssociatedComponents() : TMtxExtCOAssociatedComponentList;

    // Liste mit den Komponenten, die mit diesem KO verlinkt sind.
    property AssociatedComponents : TMtxExtCOAssociatedComponentList read getAssociatedComponents;
  end;


  // ===========================================================================
  // Klasse: TMtxCOComponentSingleCOBase
  // Basisklasse für eine Metronix-Komponente mit KO-Unterstützung
  // Die Unterklasse kann einem Fenster vom Typ TUpdateFormWithAutomation
  // hinzugefügt werden. Dadurch kann der automatisierte Mechanismus zum
  // Lesen/Schreiben von KOs benutzt werden.
  // ===========================================================================
  TMtxCOComponentBase = class abstract ( TComponent, IMtxUpdatedGUIComponent,
                                                     IMtxGUIComponentWithCO,
                                                     IMtxCOComponentWithAssociatedGUIComponents )
    protected
      FEnabled              : boolean;
      FWriteAccessRunning   : boolean;
      FOnReset              : TNotifyEvent;
      FOnCalcValueFromServo : TNotifyEvent;
      FAssociatedComponents : TMtxExtCOAssociatedComponentList; // Liste mit den Komponenten, die mit diesem KO verlinkt sind.

      // Getter / Setter für die Enabled-Eigenschaft
      function  GetEnabled() : boolean;
      // Der Aufrufparameter muss wegen TControl "Value" heißen!
      procedure SetEnabled ( b_on : boolean );

      // Getter/Setter für die Property WriteAccessRunning
      function  getWriteAccessRunning() : boolean;
      procedure setWriteAccessRunning ( b : boolean );

      // Getter/Setter für die Property AssociatedComponents
      function getAssociatedComponents() : TMtxExtCOAssociatedComponentList;

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

      // Berechnung Servo --> GUI (==> COValue anzeigen)
      procedure calcValueFromServo();

      // Komponente zurücksetzen (bei win_reset()).
      procedure reset();

      property OnReset              : TNotifyEvent read FOnReset              write FOnReset;
      property OnCalcValueFromServo : TNotifyEvent read FOnCalcValueFromServo write FOnCalcValueFromServo;
      // Komponente aktiv / inaktiv
      property Enabled              : boolean read GetEnabled write SetEnabled;
      // Flag: KO-Schreibzugriff aktiv?
      property WriteAccessRunning   : boolean read getWriteAccessRunning write setWriteAccessRunning;

      // Liste mit den Komponenten, die mit diesem KO verlinkt sind.
      property AssociatedComponents : TMtxExtCOAssociatedComponentList read getAssociatedComponents;
  end;


  // ===========================================================================
  // Klasse: TMtxCOComponentSingleCOBase
  // Basisklasse für ein einfaches KO
  // ===========================================================================
  TMtxCOComponentSingleCOBase = class abstract ( TMtxCOComponentBase, IMtxUpdatedGUIComponent,
                                                                      IMtxGUIComponentWithCO,
                                                                      IMtxGUIComponentWithSingleCO,
                                                                      IMtxCOComponentWithAssociatedGUIComponents )
    protected
      FCOData : TMtxGUICODataSingleCO;

      // Getter/Setter für die Property COData
      function  getCOData() : TMtxGUICODataSingleCO;
      procedure setCOData ( obj : TMtxGUICODataSingleCO );

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

    published
      // KO-Daten (Name, Wert, etc.)
      property COData : TMtxGUICODataSingleCO read getCOData write setCOData;
  end; // TMtxCOComponentSingleCOBase


  // ===========================================================================
  // Klasse: TMtxCOComponentSingleCOReadOnly
  // Klasse für das Lesen eines einfachen readonly KOs
  // ===========================================================================
  TMtxCOComponentSingleCOReadOnly = class ( TMtxCOComponentSingleCOBase, IMtxUpdatedGUIComponent,
                                                                         IMtxUpdatedGUIComponentReadOnly,
                                                                         IMtxGUIComponentWithCO,
                                                                         IMtxGUIComponentWithSingleCO,
                                                                         IMtxCOComponentWithAssociatedGUIComponents )
    public
      constructor Create ( AOwner : TComponent ); override;
  end; // TMtxCOComponentSingleCOReadOnly


  // ===========================================================================
  // Klasse: TMtxCOComponentSingleCOReadWrite
  // Klasse für das Lesen/Schreiben eines einfachen KOs
  // ===========================================================================
  TMtxCOComponentSingleCOReadWrite = class ( TMtxCOComponentSingleCOBase, IMtxUpdatedGUIComponent,
                                                                          IMtxUpdatedGUIComponentReadWrite,
                                                                          IMtxGUIComponentWithCO,
                                                                          IMtxGUIComponentWithSingleCO,
                                                                          IMtxCOComponentWithAssociatedGUIComponents )
    protected
      b_is_changed        : boolean;
      FOnCalcValueToServo : TNotifyEvent;

      // Getter / Setter der Property is_changed
      function  getIsChanged() : boolean;
      procedure setIsChanged ( b : boolean );

    public
      constructor Create ( AOwner : TComponent ); override;

      // Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
      procedure calcValueToServo();

    published
      // is_changed-Property: Sollte nur bei Änderung durch den Benutzer gesetzt werden.
      property    is_changed         : boolean      read getIsChanged        write setIsChanged     default false;
      property    OnCalcValueToServo : TNotifyEvent read FOnCalcValueToServo write FOnCalcValueToServo;

  end; // TMtxCOComponentSingleCOReadWrite


  
  // ===========================================================================
  // Klasse: TMtxCOComponentControlCOBase
  // Basisklasse für ein einfaches Control-KO
  // ===========================================================================
  TMtxCOComponentControlCOBase = class abstract ( TMtxCOComponentBase, IMtxUpdatedGUIComponent,
                                                                       IMtxGUIComponentWithCO,
                                                                       IMtxGUIComponentWithControlCO,
                                                                       IMtxCOComponentWithAssociatedGUIComponents )
    protected
      FCOData : TMtxGUICODataSingleCO;

      // Getter/Setter für die Property COData
      function  getCOData() : TMtxGUICODataSingleCO;
      procedure setCOData ( obj : TMtxGUICODataSingleCO );

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

    published
      // KO-Daten (Name, Wert, etc.)
      property COData : TMtxGUICODataSingleCO read getCOData write setCOData;
  end; // TMtxCOComponentControlCOBase


  // ===========================================================================
  // Klasse: TMtxCOComponentControlCOReadOnly
  // Klasse für das Lesen eines einfachen readonly Control-KOs
  // ===========================================================================
  TMtxCOComponentControlCOReadOnly = class ( TMtxCOComponentControlCOBase, IMtxUpdatedGUIComponent,
                                                                           IMtxUpdatedGUIComponentReadOnly,
                                                                           IMtxGUIComponentWithCO,
                                                                           IMtxGUIComponentWithControlCO,
                                                                           IMtxCOComponentWithAssociatedGUIComponents )
    public
      constructor Create ( AOwner : TComponent ); override;                                                                           
  end; // TMtxCOComponentControlCOReadOnly


  // ===========================================================================
  // Klasse: TMtxCOComponentSingleCOBase
  // Klasse für das Lesen/Schreiben eines einfachen KOs
  // ===========================================================================
  TMtxCOComponentControlCOReadWrite = class ( TMtxCOComponentControlCOBase, IMtxUpdatedGUIComponent,
                                                                            IMtxUpdatedGUIComponentReadWrite,
                                                                            IMtxGUIComponentWithCO,
                                                                            IMtxGUIComponentWithControlCO,
                                                                            IMtxCOComponentWithAssociatedGUIComponents )
    protected
      b_is_changed        : boolean;
      FOnCalcValueToServo : TNotifyEvent;

      // Getter / Setter der Property is_changed
      function  getIsChanged() : boolean;
      procedure setIsChanged ( b : boolean );

    public
      constructor Create ( AOwner : TComponent ); override;

      // Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
      procedure calcValueToServo();

    published
      // is_changed-Property: Sollte nur bei Änderung durch den Benutzer gesetzt werden.
      property    is_changed         : boolean      read getIsChanged        write setIsChanged     default false;
      property    OnCalcValueToServo : TNotifyEvent read FOnCalcValueToServo write FOnCalcValueToServo;
  end; // TMtxCOComponentControlCOReadWrite




  // ===========================================================================
  // Klasse: TMtxCOComponentInt64COBase
  // Basisklasse für ein INT64-KO (Abbildung zweier UINT32-KOs, High + Low Anteil)
  // ===========================================================================
  TMtxCOComponentInt64COBase = class abstract ( TMtxCOComponentBase, IMtxUpdatedGUIComponent,
                                                                     IMtxGUIComponentWithCO,
                                                                     IMtxGUIComponentWithCO64Bit,
                                                                     IMtxCOComponentWithAssociatedGUIComponents )
    protected
      FCOData : TMtxGUICODataInt64;

      // Getter/Setter für die Property COData
      function  getCOData() : TMtxGUICODataInt64;
      procedure setCOData ( obj : TMtxGUICODataInt64 );

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

    published
      // KO-Daten (Name, Wert, etc.)
      property COData : TMtxGUICODataInt64 read getCOData write setCOData;
  end; // TMtxCOComponentInt64COBase


  // ===========================================================================
  // Klasse: TMtxCOComponentInt64COReadOnly
  // Klasse für das Lesen eines readonly INT64-KO (Abbildung zweier UINT32-KOs, High + Low Anteil)
  // ===========================================================================
  TMtxCOComponentInt64COReadOnly = class ( TMtxCOComponentInt64COBase, IMtxUpdatedGUIComponent,
                                                                       IMtxUpdatedGUIComponentReadOnly,
                                                                       IMtxGUIComponentWithCO,
                                                                       IMtxGUIComponentWithCO64Bit,
                                                                       IMtxCOComponentWithAssociatedGUIComponents )
    public
      constructor Create ( AOwner : TComponent ); override;
  end; // TMtxCOComponentInt64COReadOnly


  // ===========================================================================
  // Klasse: TMtxCOComponentInt64COReadWrite
  // Klasse für das Lesen/Schreiben eines INT64-KOs (Abbildung zweier UINT32-KOs, High + Low Anteil)
  // ===========================================================================
  TMtxCOComponentInt64COReadWrite = class ( TMtxCOComponentInt64COBase, IMtxUpdatedGUIComponent,
                                                                        IMtxUpdatedGUIComponentReadWrite,
                                                                        IMtxGUIComponentWithCO,
                                                                        IMtxGUIComponentWithCO64Bit,
                                                                        IMtxCOComponentWithAssociatedGUIComponents )
    protected
      b_is_changed        : boolean;
      FOnCalcValueToServo : TNotifyEvent;

      // Getter / Setter der Property is_changed
      function  getIsChanged() : boolean;
      procedure setIsChanged ( b : boolean );

    public
      constructor Create ( AOwner : TComponent ); override;

      // Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
      procedure calcValueToServo();

    published
      // is_changed-Property: Sollte nur bei Änderung durch den Benutzer gesetzt werden.
      property    is_changed         : boolean      read getIsChanged        write setIsChanged     default false;
      property    OnCalcValueToServo : TNotifyEvent read FOnCalcValueToServo write FOnCalcValueToServo;
  end; // TMtxCOComponentInt64COReadWrite


  // ===========================================================================
  // Klasse: TMtxCOComponentSimpleMultiCOBase
  // Basisklasse für ein einfaches Multi-KO (1x Zeiger-KO + 1x Daten-KO)
  // ===========================================================================
  TMtxCOComponentSimpleMultiCOBase = class abstract ( TMtxCOComponentBase, IMtxUpdatedGUIComponent,
                                                                           IMtxGUIComponentWithCO,
                                                                           IMtxGUIComponentWithSimpleMultiCO,
                                                                           IMtxCOComponentWithAssociatedGUIComponents )
    protected
      FCOData : TMtxGUICODataSimpleMultiCO;

      // Getter/Setter für die Property COData
      function  getCOData() : TMtxGUICODataSimpleMultiCO;
      procedure setCOData ( obj : TMtxGUICODataSimpleMultiCO );

    public
      constructor Create ( AOwner : TComponent ); override;
      destructor  Destroy(); override;

    published
      // KO-Daten (Name, Wert, etc.)
      property COData : TMtxGUICODataSimpleMultiCO read getCOData write setCOData;
  end; // TMtxCOComponentSimpleMultiCOBase


  // ===========================================================================
  // Klasse: TMtxCOComponentSimpleMultiCOReadWrite
  // Klasse für das Lesen/Schreiben eines einfachen Multi-KOs (1x Zeiger-KO + 1x Daten-KO)
  // ===========================================================================
  TMtxCOComponentSimpleMultiCOReadWrite = class ( TMtxCOComponentSimpleMultiCOBase, IMtxUpdatedGUIComponent,
                                                                                    IMtxUpdatedGUIComponentReadWrite,
                                                                                    IMtxGUIComponentWithCO,
                                                                                    IMtxGUIComponentWithSimpleMultiCO,
                                                                                    IMtxCOComponentWithAssociatedGUIComponents )
    protected
      b_is_changed        : boolean;
      FOnCalcValueToServo : TNotifyEvent;

      // Getter / Setter der Property is_changed
      function  getIsChanged() : boolean;
      procedure setIsChanged ( b : boolean );

    public
      constructor Create ( AOwner : TComponent ); override;

      // Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
      procedure calcValueToServo();

    published
      // is_changed-Property: Sollte nur bei Änderung durch den Benutzer gesetzt werden.
      property    is_changed         : boolean      read getIsChanged        write setIsChanged     default false;
      property    OnCalcValueToServo : TNotifyEvent read FOnCalcValueToServo write FOnCalcValueToServo;
  end; // TMtxCOComponentSimpleMultiCOReadWrite


  TMtxGUICheckedInfo = class ( TPersistent )
    protected
      FMask              : int64;
      FBitfieldChecked   : int64;
      FBitfieldUnchecked : int64;

    public
      constructor Create(); virtual;

    published
      // Für die Eigenschaft Checked: Maske für das auszuwertende KO
      property Mask              : int64 read FMask              write FMask              default 0;
      // Für die Eigenschaft Checked: Bitfeld innerhalb der Maske, welches zum Setzen der Checkbox führt bzw. bei
      // gesetzter Checkbox in das KO geschrieben wird.
      property BitfieldChecked   : int64 read FBitfieldChecked   write FBitfieldChecked   default 0;
      // Für die Eigenschaft Checked: Bitfeld innerhalb der Maske, welches zum Löschen der Checkbox führt bzw. bei
      // gelöschter Checkbox in das KO geschrieben wird.
      property BitfieldUnchecked : int64 read FBitfieldUnchecked write FBitfieldUnchecked default 0;
  end;



  TMtxStringList = class ( TStringList )
    public
      function findString                   ( str : string; var i_inx : integer ) : boolean;
      function getNextLine                  ( var i_inx : integer; var s_line : string ) : boolean;
      function getNextLineUntilSectionBegin ( var i_inx : integer; var s_line : string ) : boolean;


      function  isEmptyString  ( str : string ) : boolean;
      function  isCommentLine  ( str : string ) : boolean;
      function  isSectionBegin ( str : string ) : boolean;
      procedure addNewLine();
  end;



  TMtxWindowPositionEntry = class
    private
      FWinName : string;  // Name des Fensters
      FLeft    : integer; // Fenster.Left
      FTop     : integer; // Fenster.Top

    public
      constructor Create();

      // Name des Fensters
      property WinName : string  read FWinName write FWinName;
      // Fenster.Left
      property Left    : integer read FLeft    write FLeft;
      // Fenster.Top
      property Top     : integer read FTop     write FTop;
  end;

  TMtxWindowPositionEntryComparer = class ( TComparer<TMtxWindowPositionEntry> )
    public
      function Compare ( const Left, Right: TMtxWindowPositionEntry ): Integer; override;
  end;


  TMtxWindowPositionList = class ( TObjectList<TMtxWindowPositionEntry> )
    public
      constructor Create();
      function    getByName       ( AWinName : string; var AEntry : TMtxWindowPositionEntry ) : boolean;
      function    getWinPosByName ( AWinName : string; var ALeft, ATop : integer ) : boolean;
      procedure   updateWinPos    ( AWinName : string; ALeft, ATop : integer );
      procedure   Add             ( AWinName : string; ALeft, ATop : integer ); reintroduce; overload;
  end;



  // Comparator für TMtxExtCOAssociatedComponentList
  TMtxExtCOAssociatedComponentComparer = class ( TComparer<TWinControl> )
    function Compare ( const Left, Right: TWinControl ): Integer; override;
  end;

  // Liste mit den GUI-Komponenten, die mit einem extern definierten KO verlinkt sind.
  TMtxExtCOAssociatedComponentList = class ( TObjectList<TWinControl> )
    public
      constructor Create();
  end;


//procedure mmsg (s : string );

implementation

uses Math, MtxUtils;

//procedure mmsg (s : string ); begin WideShowMessage (s); end;


// =============================================================================
// =============================================================================
//               C L A S S   TBoolean
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TBoolean
//    Function     : create()
//                   Creates a default object with value set to false.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2014-08-20 /gsv/
//    History      : --
// =============================================================================
constructor TBoolean.create();
begin
  Create ( false );
end; // constructor TBoolean.create();


// =============================================================================
//    Class        : TBoolean
//    Function     : create()
//                   Creates a new object with the boolean value b_val.
//    Parameter    : b_val - The initial boolean value of the object.
//    Return       : --
//    Exceptions   : --
//    First author : 2014-08-20 /gsv/
//    History      : --
// =============================================================================
constructor TBoolean.create ( b_val : boolean );
begin
  self.b_value := b_val;

  inherited Create();
end; // constructor TBoolean.create ( b_val : boolean );


// =============================================================================
//    Class        : TBoolean
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TBoolean.Clone() : TMtxCloneableObject;
begin
  result                 := TBoolean.create();
  TBoolean(result).Value := self.Value;
end; // function TBoolean.Clone() : TMtxCloneableObject;

// =============================================================================
// =============================================================================
//               C L A S S   TDouble
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TDouble
//    Function     : create()
//                   Creates a default object with value set to 0.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-11-30 /gsv/
//    History      : --
// =============================================================================
constructor TDouble.create();
begin
  Create ( 0 );
end; // constructor TDouble.create();


// =============================================================================
//    Class        : TDouble
//    Function     : create()
//                   Creates a new object with the double value d_val.
//    Parameter    : d_val - The initial double value of the object.
//    Return       : --
//    Exceptions   : --
//    First author : 2007-11-30 /gsv/
//    History      : --
// =============================================================================
constructor TDouble.create ( d_val : double );
begin
  self.d_value := d_val;

  inherited Create();
end; // constructor TDouble.create ( d_val : double );


// =============================================================================
//    Class        : TDouble
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TDouble.Clone() : TMtxCloneableObject;
begin
  result                 := TDouble.create();
  TDouble(result).Value  := self.Value;
end; // function TDouble.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//               C L A S S   TAcknowledgedDouble
// =============================================================================
// =============================================================================

// =============================================================================
//   Class      : TAcknowledgedDouble
//   Function   : create()
//                Creates a new TAcknowledgedDouble object, initialized with default values.
//   Parameter  : --
//   Return     : --
//   History    : 2007-11-30 /gsv/: d_value is initialized in the inherited constructor!
//                ==> Initialization removed.
// =============================================================================
constructor TAcknowledgedDouble.create();
begin
  self.b_value_valid := false;

  inherited Create();
end; // constructor TAcknowledgedDouble.create();


// =============================================================================
//    Class        : TAcknowledgedDouble
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TAcknowledgedDouble.Clone() : TMtxCloneableObject;
begin
  result                              := TAcknowledgedDouble.create();
  TAcknowledgedDouble(result).Value   := self.Value;
  TAcknowledgedDouble(result).isValid := self.isValid;
end; // function TAcknowledgedDouble.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//               C L A S S   TInteger
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TInteger
//    Function     : create()
//                   Konstruktor: Neue Instanz mit Defaultwert 0 erzeugen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2012-10-05 /gsv/
//    History      : --
// =============================================================================
constructor TInteger.Create();
begin
  Create ( 0 );
end; // constructor TInteger.create();


// =============================================================================
//    Class        : TInteger
//    Function     : create()
//                   Konstruktor: Neue Instanz mit dem vorgegebenen Wert i
//                   erzeugen
//    Parameter    : i - Initialwert des Objektes
//    Return       : --
//    Exceptions   : --
//    First author : 2012-10-05 /gsv/
//    History      : --
// =============================================================================
constructor TInteger.create ( i : longint );
begin
  self.li_value := i;

  inherited Create();
end; // constructor TInteger.create ( i : longint );


// =============================================================================
//    Class        : TInteger
//    Function     : copyFrom()
//                   Kopierfunktion - kopiert den Inhalt von src nach self
//    Parameter    : src - zu kopierendes Objekt
//    Return       : --
//    Exceptions   : EInvalidArgument, falls src nicht initialisiert ist.
//    First author : 2012-10-05 /gsv/
//    History      : --
// =============================================================================
procedure TInteger.copyFrom ( src : TInteger );
begin
  // Übergabeparameter prüfen
  if ( not Assigned ( src ) ) then raise EInvalidArgument.Create ( 'src is not assigned!' );

  self.li_value := src.Value;
end; // procedure TInteger.copyFrom ( src : TInteger );


// =============================================================================
//    Class        : TInteger
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TInteger.Clone() : TMtxCloneableObject;
begin
  result                  := TInteger.create();
  TInteger(result).copyFrom ( self );
end; // function TInteger.Clone() : TMtxCloneableObject;




// =============================================================================
// =============================================================================
//               C L A S S   TInt64
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TInt64
//    Function     : create()
//                   Creates a default object with value set to 0.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-11-30 /gsv/
//    History      : --
// =============================================================================
constructor TInt64.create();
begin
  Create ( 0 );
end; // constructor TInt64.create();


// =============================================================================
//    Class        : TInt64
//    Function     : create()
//                   Creates a new object with the integer value i64.
//    Parameter    : i64 - The initial integer value of the object. 
//    Return       : --
//    Exceptions   : --
//    First author : 2007-11-30 /gsv/
//    History      : --
// =============================================================================
constructor TInt64.create ( i64 : int64 );
begin
  self.i64_value := i64;

  inherited Create();
end; // constructor TInt64.create ( i64 : int64 );


// =============================================================================
//    Class        : TInt64
//    Function     : copyFrom()
//                   Kopierfunktion - kopiert den Inhalt von src nach self
//    Parameter    : src - zu kopierendes Objekt
//    Return       : --
//    Exceptions   : EInvalidArgument, falls src nicht initialisiert ist.
//    First author : 2008-12-02 /gsv/
//    History      : --
// =============================================================================
procedure TInt64.copyFrom ( src : TInt64 );
begin
  // Übergabeparameter prüfen
  if ( not Assigned ( src ) ) then raise EInvalidArgument.Create ( 'src is not assigned!' );

  self.i64_value := src.Value;
end; // procedure TInt64.copyFrom ( src : TInt64 );


// =============================================================================
//    Class        : TInt64
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TInt64.Clone() : TMtxCloneableObject;
begin
  result                := TInt64.create();
  TInt64(result).copyFrom ( self );
end; // function TInt64.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//               C L A S S   TAcknowledgedInt64
// =============================================================================
// =============================================================================

// =============================================================================
//   Class      : TAcknowledgedInt64
//   Function   : create()
//                Creates a new TAcknowledgedInt64 object, initialized with default values.
//   Parameter  : --
//   Return     : --
//   Exceptions : --
//   History    : 2007-11-30 /gsv/: i64_value is initialized in the inherited constructor!
//                ==> Initialization removed.
// =============================================================================
constructor TAcknowledgedInt64.create();
begin
  self.b_value_valid := false;

  inherited Create();
end; // constructor TAcknowledgedInt64.create();


// =============================================================================
//    Class        : TAcknowledgedInt64
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TAcknowledgedInt64.Clone() : TMtxCloneableObject;
begin
  result                             := TAcknowledgedInt64.create();
  TAcknowledgedInt64(result).copyFrom ( self );
  TAcknowledgedInt64(result).isValid := self.isValid;
end; // function TAcknowledgedInt64.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//               C L A S S   T M a s k e d T a b l e E n t r y
// =============================================================================
// =============================================================================

// =============================================================================
//   Class      : TMaskedTableEntry
//   Function   : create()
//                Creates a new TMaskedTableEntry object,
//                initialized with default values
//   Parameter  : --
//   Return     : --
//   Exceptions : --
//   History    : 2007-11-30 /gsv/: New property "UserData"
//                2009-10-05 /gsv/: Erweiterung list_alt_bitfields
// =============================================================================
constructor TMaskedTableEntry.create();
begin
  self.s_alias     := '';
  self.lw_bitfield := 0;
  self.s_text      := '';
  self.i_text_id   := C_TEXT_ID_DEFAULT_VALUE;
  self.objUserData := nil;
  // 2009-10-05 /gsv/: Liste mit alternativen Bitfeldern
  self.list_alt_bitfields := TLongwordList.create();
  self.list_alt_bitfields.clear();
  self.list_alt_bitfields.AllowDuplicates := false; // Keine Duplikate zulassen
end; // constructor TMaskedTableEntry.create();


// =============================================================================
//    Class        : TMaskedTableEntry
//    Function     : destroy
//                   Destruktor: Gibt den belegten Speicherplatz wieder frei.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2009-10-05 /gsv/
//    History      : --
// =============================================================================
destructor TMaskedTableEntry.Destroy();
begin
  try
    if ( Assigned ( self.objUserData ) ) then self.objUserData.Free();
    except on E:Exception do ;
  end;
  
  self.list_alt_bitfields.Free();
  inherited;
end; // destructor TMaskedTableEntry.Destroy();


// =============================================================================
//   Class      : TMaskedTableEntry
//   Function   : copyFrom()
//                Copies the entry to this object.
//   Parameter  : entry - The entry to be copied.
//   Return     : --
//   Exceptions : --
//   History    : 2007-11-30 /gsv/: New property "UserData"
//                2009-10-05 /gsv/: Erweiterung list_alt_bitfields
// =============================================================================
procedure TMaskedTableEntry.copyFrom ( entry : TMaskedTableEntry );
begin
  self.s_alias     := entry.s_alias;
  self.lw_bitfield := entry.lw_bitfield;
  self.s_text      := entry.s_text;
  self.i_text_id   := entry.i_text_id;


  // 2014-08-21 /gsv/: Kopiervorgang von UserData verbessert!
  // Es wurde eine neue Basisklasse eingeführt, die eine Clone-Funktion
  // unterstützt, so dass nicht mehr die Objektinstanz zugewiesen wird,
  // sondern eine neue Kopie des Objektes angelegt wird.

  // Aktuelles Objekt ggf. vorher freigeben.
  if ( Assigned ( self.objUserData ) ) then self.objUserData.Free();

  if ( Assigned ( entry.objUserData ) ) then
  begin
    // Prüfen, ob das neue Object die Clone-Funktion unterstützt
    if ( entry.objUserData is TMtxCloneableObject ) then
    begin
      // Objekt klonen
      self.objUserData := TMtxCloneableObject(entry.objUserData).Clone();
    end
    else
    begin
      // Keine Clone-Funktion ==> Instanzzuweisung
      self.objUserData := entry.objUserData;
    end;
  end // if ( Assigned ( entry.objUserData ) ) then
  else
  begin
    // entry.UserData nicht initialisiert/zugewiesen
    self.objUserData := entry.objUserData;
  end; // else, if ( Assigned ( entry.objUserData ) ) then                                     

  self.list_alt_bitfields.copyFrom ( entry.list_alt_bitfields );
end; // procedure TMaskedTableEntry.copyFrom()


// =============================================================================
//   Class      : TMaskedTableEntry
//   Function   : equals()
//                Tests wether this object equals the object.
//   Parameter  : e - The object to compare this object to.
//   Return     : true  - this object equals e
//                false - this object is not equal to e
//   Exceptions : --
// =============================================================================
function TMaskedTableEntry.equals ( e : TMaskedTableEntry ) : boolean;
begin
  if ( CompareText ( self.s_alias, e.s_alias ) <> 0 ) then
  begin
    // Bitfeld-Aliases sind unterschiedlich ==> Die Bitfelder sind nicht gleich!
    result := false;
    exit;
  end; // if ( CompareText ( self.s_alias, e.s_alias ) <> 0 ) then

  result := (self.lw_bitfield = e.lw_bitfield);

  // 2009-10-05 /gsv/: ggf. Alternative Bitfeld-Werte prüfen
  if ( (not result) and self.hasAltBitfields() ) then
  begin
    result := self.AltBitfields.contains ( e.lw_bitfield );      
  end; // if ( (not result) and self.hasAltBitfields() ) then
  
end; // function TMaskedTableEntry.equals()


// =============================================================================
//    Class        : TMaskedTableEntry
//    Function     : hasAltBitfields()
//                   Existieren alternative Bitfeld-Definitionen?
//    Parameter    : --
//    Return       : TRUE  - alternative Bitfeld-Definitionen vorhanden
//                   FALSE - keine alternative Bitfeld-Definitionen vorhanden
//    Exceptions   : --
//    First author : 2009-10-05 /gsv/
//    History      : --
// =============================================================================
function TMaskedTableEntry.hasAltBitfields() : boolean;
begin
  result := self.list_alt_bitfields.Count > 0;
end; // function TMaskedTableEntry.hasAltBitfields() : boolean;


// =============================================================================
//    Class        : TMaskedTableEntry
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TMaskedTableEntry.Clone() : TMtxCloneableObject;
begin
  result                             := TMaskedTableEntry.create();
  TMaskedTableEntry(result).copyFrom ( self );
end; // function TMaskedTableEntry.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//                     C L A S S   T M a s k e d T a b l e
// =============================================================================
// =============================================================================

// =============================================================================
//   Class      :
//   Function   : 
//   Parameter  : --
//   Return     : --
//   Exceptions : --
// =============================================================================

// =============================================================================
//   Class      : TMaskedTable
//   Function   : create()
//                Creates an empty translation table.   
//   Parameter  : --
//   Return     : --
//   Exceptions : --
// =============================================================================
constructor TMaskedTable.create();
begin
  // 2013-07-30 /gsv/: Umstellung von TList auf TObjectList.
  // Dadurch wird der Speicherplatz beim Löschen der Einträge automatisch freigegeben.
  self.entries               := TObjectList.Create();
  self.entries.OwnsObjects   := true;
  self.mask.s_alias          := '';
  self.mask.lw_mask          := 0;
  self.FOnAdd                := nil;
  self.FOnClear              := nil;
  self.FOnDelete             := nil;
  self.CapitalizeFirstLetter := false;
end; // constructor TMaskedTable.create();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : copyFrom
//                This function copies the object table in this object.
//                If table is not assigned, nothing will be done!
//                Only the table data (mask and entries) will be copied!
//                Any (event) pointers remain unchanged!
//   Parameter  : table - The object to copy
//   Return     : --
//   Exceptions : --
// =============================================================================
procedure TMaskedTable.copyFrom ( table : TMaskedTable );
var
  i   : integer;
  tbe : TMaskedTableEntry;
begin
  // There is nothing more to do, when table is not assigned!
  if ( not Assigned(table) ) then exit;

  // first clear the items of this object
  self.clear();

  // copy the mask
  self.mask := table.mask;

  // copy the table entries
  for i := 0 to table.entries.Count-1 do
  begin
    tbe := TMaskedTableEntry.create();
    tbe.copyFrom ( TMaskedTableEntry(table.entries.Items[i]) );
    // 2013-07-30 /gsv/: Falls der Eintrag nicht hinzugefügt werden kann,
    // z.B. weil dieser bereits in der Liste enthalten ist, dann die Instanz freigeben,
    // damit keine Memory Leaks entstehen.  
    if ( not self.Add ( tbe ) ) then
    begin
      tbe.Free();
    end;
  end; // for i := 0 to table.entries.Count-1 do

end; // procedure TMaskedTable.copyFrom()


// =============================================================================
//   Class      : TMaskedTable
//   Function   : destroy()
//                Destroys this object and frees the memory,
//                allocated by this object.
//   Parameter  : --
//   Return     : --
//   Exceptions : --
// =============================================================================
destructor TMaskedTable.destroy();
begin
  // 2013-02-25 /gsv/: Sämtliche Objekte freigeben, die Memory Leaks verursachen
  self.entries.Clear();
  self.entries.Free();

  inherited Destroy();
end; // destructor TMaskedTable.destroy();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : clear()
//                Clears the table.   
//   Parameter  : --
//   Return     : --
//   Exceptions : --
// =============================================================================
procedure TMaskedTable.clear();
begin
  // clear the entries
  self.entries.Clear();

  // fire OnClear event
  if ( Assigned ( self.FOnClear ) ) then self.FOnClear ( self );
end; // procedure TMaskedTable.clear();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : add()
//                Adds the given entry to the table.
//                This function does nothing,
//                if the table already contains the given entry!
//   Parameter  : entry - The entry to add.
//   Return     : TRUE  - Eintrag hinzugefügt
//                FALSE - Eintrag nicht hinzugefügt (ggf. bereis vorhanden)
//   Exceptions : --
//   History    : 2013-07-30 /gsv/: Rückgabewert ergänzt
// =============================================================================
function TMaskedTable.add ( entry : TMaskedTableEntry ) : boolean;
begin
  result := false;
  
  // if the entry is not assigned, then there is nothing more to do here!
  if ( not Assigned ( entry ) ) then exit;

  // The entry will be added to the table, only if the table does not yet contain it!
  if ( not self.contains(entry) ) then
  begin
    // Add the entry to the table
    self.entries.Add ( entry );

    result := true;

    // fire OnAdd event
    if ( Assigned ( FOnAdd ) ) then FOnAdd ( self, entry );
  end; // if ( not self.contains(entry) ) then
  
end; // function TMaskedTable.add ( entry : TMaskedTableEntry ) : boolean;


// =============================================================================
//   Class      : TMaskedTable
//   Function   : add()
//                Adds the given entry to the table.
//                This function does nothing,
//                if the table already contains the given entry!
//   Parameter  : alias    - The alias of the entry to add
//                bitfield - The bitfield of the entry to add
//                text     - The text of the entry to add
//   Return     : TRUE  - Eintrag hinzugefügt
//                FALSE - Eintrag nicht hinzugefügt (ggf. bereis vorhanden)
//   Exceptions : --
//   History    : 2007-11-30 /gsv/: There is a new extended function "add()",
//                which is called here.
//                2013-07-30 /gsv/: Rückgabewert ergänzt
// =============================================================================
function TMaskedTable.add ( alias : string; bitfield : longword; text : string ) : boolean;
begin
  result := self.add ( alias, bitfield, text, C_TEXT_ID_DEFAULT_VALUE, nil );
end; // function TMaskedTable.add ( alias : string; bitfield : longword; text : string ) : boolean;


// =============================================================================
//   Class      : TMaskedTable
//   Function   : add()
//                Adds the given entry to the table.
//                This function does nothing,
//                if the table already contains the given entry!
//   Parameter  : alias    - The alias of the entry to add
//                bitfield - The bitfield of the entry to add
//                text     - The text of the entry to add
//                userData - User defined data (may also be nil!)
//   Return     : TRUE  - Eintrag hinzugefügt
//                FALSE - Eintrag nicht hinzugefügt (ggf. bereis vorhanden)
//   Exceptions : --
//   History    : 2013-07-30 /gsv/: Rückgabewert ergänzt
// =============================================================================
function TMaskedTable.add ( alias : string; bitfield : longword; text : string; userData : TMtxCloneableObject ) : boolean;
begin
  result := self.add ( alias, bitfield, text, C_TEXT_ID_DEFAULT_VALUE, userData );
end; // function TMaskedTable.add ( alias : string; bitfield : longword; text : string; userData : TMtxCloneableObject ) : boolean;

function TMaskedTable.add ( alias : string; bitfield : longword; text : string; userData : TObject ) : boolean;
begin
  result := self.add ( alias, bitfield, text, C_TEXT_ID_DEFAULT_VALUE, userData );
end;

// =============================================================================
//   Class      : TMaskedTable
//   Function   : add()
//                Adds the given entry to the table.
//                This function does nothing,
//                if the table already contains the given entry!
//   Parameter  : alias    - The alias of the entry to add
//                bitfield - The bitfield of the entry to add
//                text     - The text of the entry to add
//                text_id  - The text ID (placeholder) for multilanguage texts
//
//   Return     : TRUE  - Eintrag hinzugefügt
//                FALSE - Eintrag nicht hinzugefügt (ggf. bereis vorhanden)
//   Exceptions : --
//   History    : 2015-12-02 /gsv/
// =============================================================================
function TMaskedTable.add ( alias : string; bitfield : longword; text : string; text_id : integer ) : boolean;
begin
  result := self.add ( alias, bitfield, text, text_id, nil );
end; // function TMaskedTable.add ( ... )


// =============================================================================
//   Class      : TMaskedTable
//   Function   : add()
//                Adds the given entry to the table.
//                This function does nothing,
//                if the table already contains the given entry!
//   Parameter  : alias    - The alias of the entry to add
//                bitfield - The bitfield of the entry to add
//                text     - The text of the entry to add
//                text_id  - The text ID (placeholder) for multilanguage texts
//                userData - User defined data (may also be nil!)
//   Return     : TRUE  - Eintrag hinzugefügt
//                FALSE - Eintrag nicht hinzugefügt (ggf. bereis vorhanden)
//   Exceptions : --
//   History    : 2015-12-02 /gsv/
// =============================================================================
function TMaskedTable.add ( alias : string; bitfield : longword; text : string; text_id : integer; userData : TObject ) : boolean;
var
  tbe : TMaskedTableEntry;
begin
  // 2016-09-28 /gsv/: Ggf. den ersten Buchstaben in Großbuchstaben umwandeln
  if ( self.CapitalizeFirstLetter ) then
  begin
    if ( Length(text) >= 1 ) then
    begin
      text[1] := UpCase ( text[1] );
    end;
  end;

  tbe          := TMaskedTableEntry.create();
  tbe.Alias    := alias;
  tbe.Bitfield := bitfield;
  tbe.Text     := text;
  tbe.TextID   := text_id;
  tbe.UserData := userData;
  result       := self.add ( tbe );

  // 2013-07-30 /gsv/: Memory Leaks beseitigen
  // Falls das neue Objekt der Liste nicht hinzugefügt werden konnte, z.B. weil es bereits enthalten ist,
  // dann muss die lokale Variable wieder freigegeben werden, da sonst Memory Leaks entstehen.
  if ( not result ) then
  begin
    tbe.Free();
  end;
end; // function TMaskedTable.add ( ... )


// =============================================================================
//   Class      : TMaskedTable
//   Function   : delete()
//                Removes the given entry from the table.
//                This function does nothing,
//                if the table does not contain the given entry!
//   Parameter  : entry - The entry to be removed.
//   Return     : The table entry with the given alias.
//   Exceptions : EObjectNotExist, if there is no entry with the given alias.
// =============================================================================
procedure TMaskedTable.delete ( entry : TMaskedTableEntry );
var
  tbe : TMaskedTableEntry;
  i   : integer;
  b_entry_deleted : boolean;
begin

  b_entry_deleted := false;

  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    tbe := TMaskedTableEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( tbe.equals ( entry ) ) then
    begin
      self.entries.Delete ( i );
      b_entry_deleted := true;
      break;
    end; // if ( tbe.equals ( entry ) ) then

  end; // for i := 0 to self.entries.Count-1 do

  // fire OnDelete event, only when the entry was really deleted
  if ( b_entry_deleted and Assigned ( FOnDelete ) ) then FOnDelete ( self, entry );
end; // procedure TMaskedTable.delete();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : getByAlias()
//                Returns the entry with the given alias. 
//   Parameter  : alias - The alias of the entry (bitfield) to search for.
//   Return     : The table entry with the given alias. 
//   Exceptions : EObjectNotExist, if there is no entry with the given alias.
// =============================================================================
function TMaskedTable.getByAlias ( alias : string ) : TMaskedTableEntry;
var
  tbe : TMaskedTableEntry;
  i   : integer;
begin
  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    tbe := TMaskedTableEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( tbe.s_alias = alias ) then
    begin
      result := tbe;
      exit;
    end; // if ( tbe.s_alias = alias ) then
    
  end; // for i := 0 to self.entries.Count-1 do

  // the entry was not found ==> throw new exception
  raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry alias="%s" does not exist!', [alias] ) );
end; // function TMaskedTable.getByAlias();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : getByBitfield()
//                Returns the entry with the given bitfield.
//   Parameter  : bitfield - The bitfield of the entry to search for.
//   Return     : The table entry with the given bitfield.
//   Exceptions : EObjectNotExist, if there is no entry with the given bitfield.
// =============================================================================
function TMaskedTable.getByBitfield ( bitfield : longword ) : TMaskedTableEntry;
var
  i : integer;
begin
  if ( not self.getByBitfield ( bitfield, i, result ) ) then
  begin
    // Eintrag nicht gefunden
    raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry bitfield=0x%x does not exist!', [bitfield] ) );
  end;
end;


function TMaskedTable.getIndexByBitfield ( bitfield : longword ) : integer;
begin
  if ( not getByBitfield ( bitfield, result ) ) then
  begin
    // Eintrag nicht gefunden.
    result := -1;
  end;

end;

// =============================================================================
//    Class        : TMaskedTable
//    Function     : getByBitfield()
//                   Eintrag mit dem angegebenen Bitfeld zurückgeben
//                   Diese Funktion löst keine Exception aus, sondern gibt den
//                   Status (Eintrag gefunden/nicht gefunden) über den Rückgabewert
//                   result zurück. Dadurch ist sie effizienter als das Auslösen
//                   einer Exception.
//    Parameter    : bitfield - Bitfeld des gesuchten Eintrags
//                   index    - Rückgabe des Index des gefundenen Eintrags,
//                              -1, falls der Eintrag nicht gefunden wird.
//                   entry    - Rückgabe des gefundenen Eintrags,
//                              nil, falls der Eintrag nicht gefunden wird.
//    Return       : true  - Eintrag gefunden
//                   false - Eintrag nicht gefunden
//    Exceptions   : --
//    First author : 2016-09-16 /gsv/
//    History      : --
// =============================================================================
function TMaskedTable.getByBitfield ( bitfield : longword; var index : integer ) : boolean;
var
  entry : TMaskedTableEntry;
begin
  result := self.getByBitfield ( bitfield, index, entry );
end;

function TMaskedTable.getByBitfield ( bitfield : longword; var index : integer; var entry : TMaskedTableEntry ) : boolean;
var
  tbe : TMaskedTableEntry;
  i   : integer;
begin
  // Interne Liste nach dem Bitfeld durchsuchen
  for i := 0 to self.entries.Count-1 do
  begin
    tbe := TMaskedTableEntry ( self.entries.Items[i] );

    // Eintrag gefunden?
    // Auch die alternativen Bitfeld-Definitionen berücksichtigen / durchsuchen
    if ( (tbe.lw_bitfield = bitfield) or
         (tbe.hasAltBitfields() and tbe.AltBitfields.contains(bitfield)) ) then
    begin
      index  := i;
      entry  := tbe;
      result := true;
      exit;
    end; // if ( tbe.lw_bitfield = bitfield ) then

  end; // for i := 0 to self.entries.Count-1 do

  // Eintrag nicht gefunden
  index  := -1;
  entry  := nil;
  result := false;
end; // function TMaskedTable.getByBitfield ( ... )


// =============================================================================
//   Class      : TMaskedTable
//   Function   : getByText()
//                Returns the entry with the given text.
//   Parameter  : text - The text of the entry to search for.
//   Return     : The table entry with the given text.
//   Exceptions : EObjectNotExist, if there is no entry with the given text.
// =============================================================================
function TMaskedTable.getByText ( text : string ) : TMaskedTableEntry;
var
  tbe : TMaskedTableEntry;
  i   : integer;
begin
  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    tbe := TMaskedTableEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( tbe.s_text = text ) then
    begin
      result := tbe;
      exit;
    end; // if ( tbe.s_text = text ) then
    
  end; // for i := 0 to self.entries.Count-1 do

  // the entry was not found ==> throw new exception
  raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry text="%s" does not exist!', [text] ) );
end; // function TMaskedTable.getByText() 


// =============================================================================
//   Class      : TMaskedTable
//   Function   : getMask()
//                Returns the mask associated with this table.
//   Parameter  : --
//   Return     : The mask associated with this table.
//   Exceptions : --
// =============================================================================
function TMaskedTable.getMask : TMaskedTableMask;
begin
  result := self.mask;
end; // function TMaskedTable.getMask();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : setMask
//                Sets the mask, associated with this translation table. 
//   Parameter  : new_mask - The new mask to be set.
//   Return     : --
//   Exceptions : --
// =============================================================================
procedure TMaskedTable.setMask ( new_mask : TMaskedTableMask );
begin
  self.mask := new_mask;
end; // procedure TMaskedTable.setMask();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : setMask
//                Sets/changes the mask, associated with this translation table.
//   Parameter  : alias - The alias of the mask
//                value - The mask value
//   Return     : --
//   Exceptions : --
// =============================================================================
procedure TMaskedTable.setMask ( alias : string; value : longword );
begin
  self.mask.s_alias := alias;
  self.mask.lw_mask := value;
end; // procedure TMaskedTable.setMask();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : isEmpty()
//                Tests wether this table is empty or not.
//   Parameter  : --
//   Return     : true  - This table is empty, i.e. it has no entries!
//                false - This table is not empty, i.e. it has at least one entry!
//   Exceptions : --
// =============================================================================
function TMaskedTable.isEmpty() : boolean;
begin
  result := (self.entries.Count < 1);
end; // function TMaskedTable.isEmpty();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : indexOf()
//                Returns the index of the given table entry
//   Parameter  : entry - The entry to get the index of
//   Return     : index of the given table entry (the index is in range [0...Count) ) or
//                -1, if the entry could not be found
//   Exceptions : --
// =============================================================================
function TMaskedTable.indexOf ( entry : TMaskedTableEntry ) : integer;
var
  i : integer;
begin
  result := -1;

  for i := 0 to self.entries.Count-1 do
  begin
    // The entry has been found?
    if ( TMaskedTableEntry(self.entries.Items[i]).equals ( entry ) ) then
    begin
      result := i;
      exit;
    end;
  end; // for i := 0 to self.entries.Count-1 do
  
end; // function TMaskedTable.indexOf();


// =============================================================================
//   Class      : TMaskedTable
//   Function   : contains()
//                Tests wether the table contains the given entry.
//   Parameter  : entry - The entry to be tested
//   Return     : true  - The table contains the given entry.
//                false - The table does not contain the given entry.
//   Exceptions : --
// =============================================================================
function TMaskedTable.contains ( entry : TMaskedTableEntry ) : boolean;
begin
  result := (self.indexOf(entry) >= 0);
end; // function TMaskedTable.contains()


// =============================================================================
//   Class      : TMaskedTable
//   Function   : getCount
//                Returns the number of entries, contained currently in the table. 
//   Parameter  : --
//   Return     : number of table entries
//   Exceptions : --
// =============================================================================
function TMaskedTable.getCount() : integer;
begin
  result := self.entries.Count;
end; // function TMaskedTable.getCount() : integer;


// =============================================================================
//   Class      : TMaskedTable
//   Function   :
//
//   Parameter  : --
//   Return     : --
//   Exceptions : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of table entries
// =============================================================================
function TMaskedTable.getEntry ( inx : integer    ) : TMaskedTableEntry;
begin
  if ( (inx < 0) or (inx >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [inx, self.Count-1] ) );

  result := TMaskedTableEntry ( self.entries.Items[inx] );
end; // function TMaskedTable.getEntry ( inx : integer    ) : TMaskedTableEntry;


// =============================================================================
//    Class        : TMaskedTable
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TMaskedTable.Clone() : TMtxCloneableObject;
begin
  result                      := TMaskedTable.create();
  TMaskedTable(result).copyFrom ( self );
end; // function TMaskedTable.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TDoubleList   
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new object, initialized with default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TDoubleList.create();
begin
  self.list_values := TList.Create();
end; // constructor TDoubleList.create();


// =============================================================================
//    Destructor   : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
// =============================================================================
destructor TDoubleList.destroy();
begin
  self.clear();
  self.list_values.Free();

  inherited Destroy();
end; // destructor TDoubleList.destroy();


// =============================================================================
//    Function     : copyFrom
//                   Copies the contents of list to the calling object.
//    Parameter    : list - The double list to be copied.
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TDoubleList.copyFrom ( list : TDoubleList );
var
  i : integer;
begin
  self.clear(); // Clear the internal list

  // If the list to be copied is not assigned, then do nothing. ==> self = empty!
  if ( not Assigned ( list ) ) then exit;

  // Copy the contents of list to the calling object self
  for i := 0 to list.Count-1 do
    self.add ( list.get(i) );
  
end; // procedure TDoubleList.copyFrom ( list : TDoubleList );


// =============================================================================
//    Class        : TDoubleList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TDoubleList.Clone() : TMtxCloneableObject;
begin
  result                      := TDoubleList.create();
  TDoubleList(result).copyFrom ( self );
end; // function TDoubleList.Clone() : TMtxCloneableObject;


// =============================================================================
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : Number of elements in the list
//    Exceptions   : --
// =============================================================================
function TDoubleList.getCount() : integer;
begin
  result := self.list_values.Count;
end; // function TDoubleList.getCount() : integer;


// =============================================================================
//    Function     : update
//                   Updates the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   value - the new value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TDoubleList.update ( index : integer; value : double );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  PDouble(self.list_values[index])^ := value;

end; // procedure TDoubleList.update ( index : integer; value : double );


// =============================================================================
//    Function     : get
//                   Returns the element (double-value) at the specified index.
//    Parameter    : index - index of the element to be returned
//    Return       : The element (double value) at the specified index.
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
function TDoubleList.get ( index : integer ) : double;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  // The index is valid ==> return the double value at the specified index 
  result := PDouble(self.list_values.Items[index])^;
end; // function TDoubleList.get ( index : integer ) : double;


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TDoubleList.add ( value : double );
var
  ptr_value : PDouble;
begin
  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Add ( ptr_value );
end; // procedure TDoubleList.add ( value : double );


// =============================================================================
//    Function     : add
//                   Adds the value at the specified index in the list.
//                   The list can contain the same value more than once.
//    Parameter    : index - the index in the list, at which the value will be added.
//                   value - the value to be added
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index > number of elements in the list
// =============================================================================
procedure TDoubleList.add ( index : integer; value : double );
var
  ptr_value : PDouble;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index > self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Insert ( index, ptr_value );
end; // procedure TDoubleList.add ( index : integer; value : double );


// =============================================================================
//    Function     : clear
//                   Clears the contents of the list and frees the memory,
//                   allocated by the elements of the list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TDoubleList.clear();
var
  i : integer;
begin
  for i := 0 to self.list_values.Count-1 do
    Dispose ( self.list_values.Items[i] );

  self.list_values.Clear();
end; // procedure TDoubleList.clear();


// =============================================================================
//    Function     : isEmpty
//                   Tests wether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
// =============================================================================
function TDoubleList.isEmpty() : boolean;
begin
  result := self.Count > 0;
end; // function TDoubleList.isEmpty() : boolean;



// =============================================================================
// =============================================================================
//
//                      C L A S S   TAcknowledgedDoubleList
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new empty list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TAcknowledgedDoubleList.create();
begin
  self.list_values := TList.Create();
end; // constructor TAcknowledgedDoubleList.create();


// =============================================================================
//    Destructor   : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
// =============================================================================
destructor TAcknowledgedDoubleList.destroy();
begin
  self.clear();
  self.list_values.Free();

  inherited Destroy();
end; // destructor TAcknowledgedDoubleList.destroy();


// =============================================================================
//    Function     : copyFrom
//                   Copies the contents of list to the calling object.
//    Parameter    : list - The acknowledged double list to be copied.
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TAcknowledgedDoubleList.copyFrom ( list : TAcknowledgedDoubleList );
var
  i : integer;
begin
  self.clear(); // Clear the internal list

  // If the list to be copied is not assigned, then do nothing. ==> self = empty!
  if ( not Assigned ( list ) ) then exit;

  // Copy the contents of list to the calling object self
  for i := 0 to list.Count-1 do
    self.add ( list.get(i) );
  
end; // procedure TAcknowledgedDoubleList.copyFrom ( list : TAcknowledgedDoubleList );


// =============================================================================
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : Number of elements in the list
//    Exceptions   : --
// =============================================================================
function TAcknowledgedDoubleList.getCount() : integer;
begin
  result := self.list_values.Count;
end; // function TAcknowledgedDoubleList.getCount() : integer;


// =============================================================================
//    Function     : get
//                   Returns the entry at the specified index.
//    Parameter    : index - index of the element to be returned
//    Return       : The entry at the specified index.
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
function TAcknowledgedDoubleList.get ( index : integer ) : TAcknowledgedDouble;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  // The index is valid ==> return the entry at the specified index
  result := PAcknowledgedDouble(self.list_values.Items[index])^;
end; // function TAcknowledgedDoubleList.get ( index : integer ) : TAcknowledgedDouble;


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TAcknowledgedDoubleList.add ( value : double; b_valid : boolean );
var
  ack_double : TAcknowledgedDouble;
begin
  ack_double         := TAcknowledgedDouble.create();
  ack_double.Value   := value;
  ack_double.isValid := b_valid;
  self.add ( ack_double );
end; // procedure TAcknowledgedDoubleList.add ( value : TAcknowledgedDouble );


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TAcknowledgedDoubleList.add ( value : TAcknowledgedDouble );
var
  ptr_value : PAcknowledgedDouble;
begin
  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Add ( ptr_value );
end; // procedure TAcknowledgedDoubleList.add ( value : TAcknowledgedDouble );


// =============================================================================
//    Function     : add
//                   Adds the value at the specified index in the list.
//                   The list can contain the same value more than once.
//    Parameter    : index - the index in the list, at which the value will be added.
//                   value - the value to be added
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index > number of elements in the list
// =============================================================================
procedure TAcknowledgedDoubleList.add ( index : integer; value : TAcknowledgedDouble );
var
  ptr_value : PAcknowledgedDouble;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index > self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Insert ( index, ptr_value );
end; // procedure TAcknowledgedDoubleList.add ( index : integer; value : TAcknowledgedDouble );


// =============================================================================
//    Function     : update
//                   Updates the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   entry - the new value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TAcknowledgedDoubleList.update ( index : integer; entry : TAcknowledgedDouble );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  self.list_values[index] := entry;
end; // procedure TAcknowledgedDoubleList.update ( index : integer; entry : TAcknowledgedDouble );


// =============================================================================
//    Function     : update
//                   Updates the double value of the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   value - the new double value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TAcknowledgedDoubleList.update ( index : integer; d_value : double );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  PAcknowledgedDouble(self.list_values[index])^.Value := d_value;
end; // procedure TAcknowledgedDoubleList.update ( index : integer; d_value : double );


// =============================================================================
//    Function     : update
//                   Updates the boolean value of the entry at the specified index
//    Parameter    : index   - the index of the entry to be updated
//                   b_valid - the new boolean value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TAcknowledgedDoubleList.update ( index : integer; b_valid : boolean );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  PAcknowledgedDouble(self.list_values[index])^.isValid := b_valid;
end; // procedure TAcknowledgedDoubleList.update ( index : integer; b_valid : boolean );


// =============================================================================
//    Function     : clear
//                   Clears the contents of the list and frees the memory,
//                   allocated by the elements of the list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TAcknowledgedDoubleList.clear();
var
  i : integer;
begin
  for i := 0 to self.list_values.Count-1 do
    Dispose ( self.list_values.Items[i] );
  
  self.list_values.Clear();
end; // procedure TAcknowledgedDoubleList.clear();


// =============================================================================
//    Function     : isEmpty
//                   Tests whether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
// =============================================================================
function TAcknowledgedDoubleList.isEmpty() : boolean;
begin
  result := self.Count > 0;
end; // function TAcknowledgedDoubleList.isEmpty() : boolean;


// =============================================================================
//    Class        : TAcknowledgedDoubleList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TAcknowledgedDoubleList.Clone() : TMtxCloneableObject;
begin
  result                      := TAcknowledgedDoubleList.create();
  TAcknowledgedDoubleList(result).copyFrom ( self );
end; // function TAcknowledgedDoubleList.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TInt64List
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new object, initialized with default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TInt64List.create();
begin
  self.list_values := TList.Create();
end; // constructor TInt64List.create();


// =============================================================================
//    Destructor   : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
// =============================================================================
destructor TInt64List.destroy();
begin
  self.clear();
  self.list_values.Free();

  inherited Destroy();
end; // destructor TInt64List.destroy();


// =============================================================================
//    Function     : copyFrom
//                   Copies the contents of list to the calling object.
//    Parameter    : list - The int64 list to be copied.
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TInt64List.copyFrom ( list : TInt64List );
var
  i : integer;
begin
  self.clear(); // Clear the internal list

  // If the list to be copied is not assigned, then do nothing. ==> self = empty!
  if ( not Assigned ( list ) ) then exit;

  // Copy the contents of list to the calling object self
  for i := 0 to list.Count-1 do
    self.add ( list.get(i) );

end; // procedure TInt64List.copyFrom ( list : TInt64List );


// =============================================================================
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : Number of elements in the list
//    Exceptions   : --
// =============================================================================
function TInt64List.getCount() : integer;
begin
  result := self.list_values.Count;
end; // function TInt64List.getCount() : integer;


// =============================================================================
//    Function     : get
//                   Returns the element (int64-value) at the specified index.
//    Parameter    : index - index of the element to be returned
//    Return       : The element (int64 value) at the specified index.
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
function TInt64List.get ( index : integer ) : int64;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  // The index is valid ==> return the double value at the specified index 
  result := PInt64(self.list_values.Items[index])^;
end; // function TInt64List.get ( index : integer ) : int64;


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TInt64List.add ( value : int64 );
var
  ptr_value : PInt64;
begin
  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Add ( ptr_value );
end; // procedure TInt64List.add ( value : int64 );


// =============================================================================
//    Function     : add
//                   Adds the value at the specified index in the list.
//                   The list can contain the same value more than once.
//    Parameter    : index - the index in the list, at which the value will be added.
//                   value - the value to be added
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index > number of elements in the list
// =============================================================================
procedure TInt64List.add ( index : integer; value : int64 );
var
  ptr_value : PInt64;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index > self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Insert ( index, ptr_value );
end; // procedure TInt64List.add ( index : integer; value : int64 );


// =============================================================================
//    Function     : update
//                   Updates the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   value - the new value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TInt64List.update ( index : integer; value : int64 );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  PInt64(self.list_values[index])^ := value;
end; // procedure TInt64List.update ( index : integer; value : int64 );


// =============================================================================
//    Function     : clear
//                   Clears the contents of the list and frees the memory,
//                   allocated by the elements of the list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TInt64List.clear();
var
  i : integer;
begin
  for i := 0 to self.list_values.Count-1 do
    Dispose ( self.list_values.Items[i] );

  self.list_values.Clear();
end; // procedure TInt64List.clear();


// =============================================================================
//    Function     : isEmpty
//                   Tests wether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
// =============================================================================
function TInt64List.isEmpty() : boolean;
begin
  result := self.Count > 0;
end; // function TInt64List.isEmpty() : boolean;


// =============================================================================
//    Class        : TInt64List
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TInt64List.Clone() : TMtxCloneableObject;
begin
  result                      := TInt64List.create();
  TInt64List(result).copyFrom ( self );
end; // function TInt64List.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TLongwordList (2009-10-05 /gsv/)
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new object, initialized with default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TLongwordList.create();
begin
  self.list_values     := TList.Create();
  self.AllowDuplicates := true;
end; // constructor TLongwordList.create();


// =============================================================================
//    Destructor   : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
// =============================================================================
destructor TLongwordList.destroy();
begin
  self.clear();
  self.list_values.Free();
  inherited Destroy();
end; // destructor TLongwordList.destroy();


// =============================================================================
//    Function     : copyFrom
//                   Copies the contents of list to the calling object.
//    Parameter    : list - The longword list to be copied.
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TLongwordList.copyFrom ( list : TLongwordList );
var
  i : integer;
begin
  self.clear(); // Clear the internal list

  // If the list to be copied is not assigned, then do nothing. ==> self = empty!
  if ( not Assigned ( list ) ) then exit;

  // Copy the contents of list to the calling object self
  for i := 0 to list.Count-1 do
    self.add ( list.get(i) );

end; // procedure TLongwordList.copyFrom ( list : TLongwordList );


// =============================================================================
//    Class        : TLongwordList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TLongwordList.Clone() : TMtxCloneableObject;
begin
  result                      := TLongwordList.create();
  TLongwordList(result).copyFrom ( self );
end; // function TLongwordList.Clone() : TMtxCloneableObject;


// =============================================================================
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : Number of elements in the list
//    Exceptions   : --
// =============================================================================
function TLongwordList.getCount() : integer;
begin
  result := self.list_values.Count;
end; // function TLongwordList.getCount() : integer;


// =============================================================================
//    Function     : get
//                   Returns the element (longword-value) at the specified index.
//    Parameter    : index - index of the element to be returned
//    Return       : The element (longword value) at the specified index.
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
function TLongwordList.get ( index : integer ) : longword;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  // The index is valid ==> return the double value at the specified index 
  result := PLongword(self.list_values.Items[index])^;
end; // function TLongwordList.get ( index : integer ) : longword;


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : EObjectAlreadyExist, falls AllowDuplicates = false und
//                   value bereits in der Liste enthalten ist.
// =============================================================================
procedure TLongwordList.add ( value : longword );
var
  ptr_value : PLongword;
begin
  // 2009-10-05 /gsv/: Ggf. keine Duplicate zulassen und Exception auslösen
  if ( (not self.AllowDuplicates) and self.contains(value) )  then
  begin
    raise EObjectAlreadyExist.Create ( 'Value 0x' + SysUtils.IntToHex(value,8) + ' has already been contained in the list!' );
  end;

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Add ( ptr_value );
end; // procedure TLongwordList.add ( value : longword );


// =============================================================================
//    Function     : add
//                   Adds the value at the specified index in the list.
//                   The list can contain the same value more than once.
//    Parameter    : index - the index in the list, at which the value will be added.
//                   value - the value to be added
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index > number of elements in the list
// =============================================================================
procedure TLongwordList.add ( index : integer; value : longword );
var
  ptr_value : PLongword;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index > self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Insert ( index, ptr_value );
end; // procedure TLongwordList.add ( index : integer; value : longword );


// =============================================================================
//    Function     : update
//                   Updates the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   value - the new value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TLongwordList.update ( index : integer; value : longword );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  PLongword(self.list_values[index])^ := value;
end; // procedure TLongwordList.update ( index : integer; value : longword );


// =============================================================================
//    Function     : clear
//                   Clears the contents of the list and frees the memory,
//                   allocated by the elements of the list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TLongwordList.clear();
var
  i : integer;
begin
  for i := 0 to self.list_values.Count-1 do
    Dispose ( self.list_values.Items[i] );

  self.list_values.Clear();
end; // procedure TLongwordList.clear();


// =============================================================================
//    Function     : isEmpty
//                   Tests wether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
// =============================================================================
function TLongwordList.isEmpty() : boolean;
begin
  result := self.Count > 0;
end; // function TLongwordList.isEmpty() : boolean;


// =============================================================================
//    Class        : TLongwordList
//    Function     : contains()
//                   Prüft, ob ein Longword-Wert in der Liste bereits enthalten ist.
//    Parameter    : lw - zu prüfender Wert
//    Return       : TRUE  - lw ist in der Liste bereits vorhanden
//                   FALSE - lw ist in der Liste nicht vorhanden
//    Exceptions   : --
//    First author : 2009-10-05 /gsv/
//    History      : --
// =============================================================================
function TLongwordList.contains ( lw : longword ) : boolean;
var
  i : integer;
begin
  // Liste nach dem Wert durchsuchen
  for i := 0 to self.Count-1 do
  begin
    if ( lw = self.get(i) ) then
    begin
      // Wert gefunden ==> Abbruch
      result := true;
      exit;
    end; // if ( lw = self.get(i) ) then
  end; // for i := 0 to self.Count-1 do

  // Wert nicht gefunden 
  result := false;
end; // function TLongwordList.contains ( lw : longword ) : boolean;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TByteList
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new object, initialized with default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TByteList.create();
begin
  self.list_values := TList.Create();
end; // constructor TByteList.create();


// =============================================================================
//    Destructor   : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
// =============================================================================
destructor TByteList.destroy();
begin
  self.clear();
  self.list_values.Free();

  inherited Destroy();
end; // destructor TByteList.destroy();


// =============================================================================
//    Function     : copyFrom
//                   Copies the contents of list to the calling object.
//    Parameter    : list - The byte list to be copied.
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TByteList.copyFrom ( list : TByteList );
var
  i : integer;
begin
  self.clear(); // Clear the internal list

  // If the list to be copied is not assigned, then do nothing. ==> self = empty!
  if ( not Assigned ( list ) ) then exit;

  // Copy the contents of list to the calling object self
  for i := 0 to list.Count-1 do
    self.add ( list.get(i) );

end; // procedure TByteList.copyFrom ( list : TByteList );


// =============================================================================
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : Number of elements in the list
//    Exceptions   : --
// =============================================================================
function TByteList.getCount() : integer;
begin
  result := self.list_values.Count;
end; // function TByteList.getCount() : integer;


// =============================================================================
//    Function     : get
//                   Returns the element (byte-value) at the specified index.
//    Parameter    : index - index of the element to be returned
//    Return       : The element (byte value) at the specified index.
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
function TByteList.get ( index : integer ) : byte;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  // The index is valid ==> return the double value at the specified index 
  result := Pbyte(self.list_values.Items[index])^;
end; // function TByteList.get ( index : integer ) : byte;


// =============================================================================
//    Function     : add
//                   Adds the value at the end of the list.
//                   The list can contain the same value more than once.
//    Parameter    : value - the value to be added
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TByteList.add ( value : byte );
var
  ptr_value : Pbyte;
begin
  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Add ( ptr_value );
end; // procedure TByteList.add ( value : byte );


// =============================================================================
//    Function     : add
//                   Adds the value at the specified index in the list.
//                   The list can contain the same value more than once.
//    Parameter    : index - the index in the list, at which the value will be added.
//                   value - the value to be added
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index > number of elements in the list
// =============================================================================
procedure TByteList.add ( index : integer; value : byte );
var
  ptr_value : Pbyte;
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index > self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  New ( ptr_value );
  ptr_value^ := value;
  self.list_values.Insert ( index, ptr_value );
end; // procedure TByteList.add ( index : integer; value : byte );


// =============================================================================
//    Function     : update
//                   Updates the entry at the specified index
//    Parameter    : index - the index of the entry to be updated
//                   value - the new value of the entry
//    Return       : --
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of elements in the list
// =============================================================================
procedure TByteList.update ( index : integer; value : byte );
begin
  // When the specified index is invalid, a new exception is thrown!
  if ( (index < 0) or (index >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [index, self.Count-1] ) );

  Pbyte(self.list_values[index])^ := value;
end; // procedure TByteList.update ( index : integer; value : byte );


// =============================================================================
//    Function     : clear
//                   Clears the contents of the list and frees the memory,
//                   allocated by the elements of the list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TByteList.clear();
var
  i : integer;
begin
  for i := 0 to self.list_values.Count-1 do
    Dispose ( self.list_values.Items[i] );

  self.list_values.Clear();
end; // procedure TByteList.clear();


// =============================================================================
//    Function     : isEmpty
//                   Tests wether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
// =============================================================================
function TByteList.isEmpty() : boolean;
begin
  result := self.Count > 0;
end; // function TByteList.isEmpty() : boolean;


// =============================================================================
//    Class        : TByteList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TByteList.Clone() : TMtxCloneableObject;
begin
  result                      := TByteList.create();
  TByteList(result).copyFrom ( self );
end; // function TByteList.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TFraction
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Constructor  : create
//                   Creates a new object, initialized with default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
constructor TFraction.create();
begin
  self.i64_numerator := 0;
  self.i64_divisor   := 1;
  self.refreshDecimalFraction();
end; // constructor TFraction.create();


// =============================================================================
//    Function     : setNumerator
//                   Sets the numerator of the fraction and recalculates
//                   the decimal fraction.
//    Parameter    : new_numerator - the new numerator to set
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TFraction.setNumerator ( new_numerator : int64 );
begin
  self.i64_numerator := new_numerator;

  // recalculate the decimal fraction
  self.refreshDecimalFraction();
end; // procedure TFraction.setNumerator ( new_numerator : int64 );


// =============================================================================
//    Function     : setDivisor
//                   Sets the divisor of the fraction and recalculates
//                   the decimal fraction.
//    Parameter    : new_divisor - the new divisor to set
//    Return       : --
//    Exceptions   : EInvalidArgument, if new_divisor equals 0.
// =============================================================================
procedure TFraction.setDivisor ( new_divisor : int64 );
begin
  if ( new_divisor = 0 ) then raise EInvalidArgument.Create ( 'The divisor cannot be 0!' );
  
  self.i64_divisor := new_divisor;

  // recalculate the decimal fraction
  self.refreshDecimalFraction();
end; // procedure TFraction.setDivisor ( new_divisor : int64 );


// =============================================================================
//    Function     : refreshDecimalFraction
//                   Recalculates/refreshes the corresponding decimal fraction
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
// =============================================================================
procedure TFraction.refreshDecimalFraction();
begin
  self.d_decimal_fraction := self.i64_numerator / self.i64_divisor;
end; // procedure TFraction.refreshDecimalFraction();


// =============================================================================
//    Function     : equals
//                   Tests wether this object equals to the object fraction.
//    Parameter    : fraction - the object to compare this object with
//    Return       : true - this object equals fraction
//    Exceptions   : --
// =============================================================================
function TFraction.equals ( fraction : TFraction ) : boolean;
begin
  if ( fraction = nil  ) then begin result := false; exit; end;
  if ( fraction = self ) then begin result := true;  exit; end;

  result := ((self.Numerator = fraction.Numerator) and
             (self.Divisor   = fraction.Divisor  ) ) OR
            (self.DecimalFraction = fraction.DecimalFraction);
end; // function TFraction.equals ( fraction : TFraction );


// =============================================================================
//    Class        : TFraction
//    Function     : getGCD()
//                   Berechnet den größten gemeinsamen Teiler von
//                   Zähler und Nenner und gibt diesen züruck.
//                   Berechnung des GGTs nach dem Euklidischen Algorithmus
//                   mit Modulo-Operation. 
//    Parameter    : --
//    Return       : GGT(numeration,divisor)
//    Exceptions   : --
//    First author : 2009-11-21 /gsv/
//    History      : --
// =============================================================================
function TFraction.getGCD() : int64;
var
  i64_rest, i64_a, i64_b : int64;
begin
  i64_a := Abs(Self.Numerator);
  i64_b := Abs(Self.Divisor);

  // Euklidischer Algorithmus mit Modulo-Operation
  repeat
    i64_rest := i64_a mod i64_b;
    i64_a    := i64_b;
    i64_b    := i64_rest;
  until ( i64_rest = 0 );

  result := i64_a;
end; // function TFraction.getGCD() : int64;


// =============================================================================
//    Class        : TFraction
//    Function     : shorten()
//                   Bruch kürzen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2009-11-21 /gsv/
//    History      : --
// =============================================================================
procedure TFraction.shorten();
var
  i64_gcd : int64;
begin
  // Größten gemeinsamen Teiler von Zähler und Nenner ermitteln
  i64_gcd   := self.getGCD();
  Numerator := Numerator div i64_gcd;
  Divisor   := Divisor   div i64_gcd;
end; // procedure TFraction.shorten();


// =============================================================================
//    Class        : TFraction
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TFraction.Clone() : TMtxCloneableObject;
begin
  result                               := TFraction.create();
  TFraction(result).Numerator          := self.Numerator;
  TFraction(result).Divisor            := self.Divisor;
  TFraction(result).d_decimal_fraction := self.DecimalFraction;
end; // function TFraction.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TBitEntry
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TBitListEntry
//    Function     : create
//                   Creates a new object of this class, initialized with
//                   default values.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
constructor TBitListEntry.create();
begin
  self.create ( '', 0, '', false );
end; // constructor TBitListEntry.create();



// =============================================================================
//    Class        : TBitListEntry
//    Function     : create
//                   Creates a new object of this class.
//    Parameter    : s_alias     - Alias for the symbolic access to this bit,
//                                 e.g. in a BitList (it must be unique!).
//                   bit         - The bit value to be tested,
//                                 e.g. 0x80000000 = Bit31, 0x00000001 = Bit0
//                   s_text      - Textual description of the bit
//                   inv_logic   - The bit has inverted logic?
//                                 (placeholder for the later bit evaluation)
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
constructor TBitListEntry.create ( s_alias : string; bit : longword; s_text : string;
                                   inv_logic : boolean = C_DEF_BIT_INV_LOGIC );
begin
  self.str_alias     := s_alias;
  self.lw_bit        := bit;
  self.str_text      := s_text;
  self.b_inv_logic   := inv_logic;
  self.i64_user_data := 0;
end; // constructor TBitListEntry.create();


// =============================================================================
//    Class        : TBitListEntry
//    Function     : destroy
//                   Frees the memory allocated by this object.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
destructor TBitListEntry.destroy();
begin
  inherited;
end; // destructor TBitListEntry.destroy();


// =============================================================================
//    Class        : TBitListEntry
//    Function     : equals
//                   Tests wether this bit entry equals the bit entry e.
//    Parameter    : e - the entry to test this with
//    Return       : true - self equals e
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
function TBitListEntry.equals ( e : TBitListEntry ) : boolean;
begin
  result := (self.str_alias = e.str_alias) and (self.lw_bit = e.lw_bit);
end; // function TBitListEntry.equals ( e : TBitListEntry ) : boolean;


// =============================================================================
//    Class        : TBitListEntry
//    Function     : copyFrom
//                   Copies the entry in this object.
//                   No changes are made if the entry is not assigned!
//    Parameter    : entry - the bit list entry to copy
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
procedure TBitListEntry.copyFrom ( entry : TBitListEntry );
begin
  // If the entry is not assigned, then there is nothing more to do.
  if ( not Assigned ( entry ) ) then exit;

  // Copy the entry in self
  self.Alias         := entry.Alias;
  self.Bit           := entry.Bit;
  self.Text          := entry.Text;
  self.UserData      := entry.UserData;
  self.InvLogic      := entry.InvLogic;
end; // procedure TBitListEntry.copyFrom ( entry : TBitListEntry );


// =============================================================================
//    Class        : TBitListEntry
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TBitListEntry.Clone() : TMtxCloneableObject;
begin
  result                       := TBitListEntry.create();
  TBitListEntry(result).copyFrom ( self );
end; // function TBitListEntry.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TBitList
//
// =============================================================================
// =============================================================================


// =============================================================================
//    Class        : TBitList
//    Function     : create
//                   Creates an empty bit list
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
constructor TBitList.create();
begin
  inherited;
  self.entries      := TList.Create();
end; // constructor TBitList.create();


// =============================================================================
//    Class        : TBitList
//    Function     : destroy
//                   Frees the memory, allocated by this object.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
destructor TBitList.destroy();
begin
  self.clear();
  self.entries.Free;
end; // destructor TBitList.destroy();


// =============================================================================
//   Class      : TBitList
//   Function   : indexOf()
//                Returns the index of the given bit entry in the list
//   Parameter  : entry - The entry to get the index of
//   Return     : index of the given table entry (the index is in range [0...Count) )
//                -1, if the entry could not be found
//   Exceptions : --
// =============================================================================
function TBitList.indexOf ( entry : TBitListEntry ) : integer;
var
  i : integer;
begin
  result := -1; // For the case that the entry does not exist
  for i := 0 to self.entries.Count-1 do
  begin
    // The entry has been found?
    if ( TBitListEntry(self.entries.Items[i]).equals ( entry ) ) then
    begin
      result := i;
      exit;
    end;
  end; // for i := 0 to self.entries.Count-1 do
end; // function TBitList.indexOf();


// =============================================================================
//    Class        : TBitList
//    Function     : getCount
//                   Returns the number of entries, currently contained in the list.
//    Parameter    : --
//    Return       : see above
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
function TBitList.getCount() : integer;
begin
  result := self.entries.Count;
end; // function TBitList.getCount() : integer;


// =============================================================================
//    Class        : TBitList
//    Function     : isEmpty
//                   Tests wether the list is empty or not.
//    Parameter    : --
//    Return       : true - the list is empty
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
function TBitList.isEmpty() : boolean;
begin
  result := self.getCount() <= 0;
end; // function TBitList.isEmpty() : boolean;


// =============================================================================
//    Class        : TBitList
//    Function     : clear
//                   Clears the list, the contents will be deleted
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
procedure TBitList.clear();
var
  i : integer;
begin
  for i := 0 to self.entries.Count-1 do
  begin
    TBitListEntry(entries[i]).Free();
  end;

  self.entries.Clear();
end; // procedure TBitList.clear();


// =============================================================================
//    Class        : TBitList
//    Function     : add
//                   Adds the given entry to the table.
//                   This function does nothing,
//                   if the table already contains the given entry!
//    Parameter    : entry - The entry to add.
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
procedure TBitList.add ( entry : TBitListEntry );
begin
  // if the entry is not assigned, then there is nothing more to do here!
  if ( not Assigned ( entry ) ) then exit;

  // The entry will be added to the table, only if the table does not yet contain it!
  if ( not self.contains(entry) ) then
  begin
    // Add the entry to the table
    self.entries.Add ( entry );
  end; // if ( not self.contains(entry) ) then
  
end; // procedure TBitList.add();


// =============================================================================
//    Class        : TBitList
//    Function     : add
//                   Adds the given entry to the table.
//                   This function does nothing,
//                   if the table already contains the given entry!
//    Parameter    : alias     - The alias of the entry to add
//                   bit       - The bit of the entry to add
//                   text      - The text of the entry to add
//                   inv_logic - Placeholder: The bit will be evaluated as
//                               "normal" or inverted logic.
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-11 /gsv/
//    History      : --
// =============================================================================
procedure TBitList.add ( alias : string; bit : longword; text : string;
                         inv_logic : boolean = C_DEF_BIT_INV_LOGIC );
var
  entry : TBitListEntry;
begin
  entry := TBitListEntry.create ( alias, bit, text, inv_logic );
  self.add ( entry );
end; // procedure TBitList.add ( alias : string; bit : longword; text : string );


// =============================================================================
//   Class      : TBitList
//   Function   : delete()
//                Removes the given entry from the list.
//                This function does nothing,
//                if the table does not contain the given entry!
//   Parameter  : entry - The entry to be removed.
//   Return     : The table entry with the given alias.
//   Exceptions : EObjectNotExist, if there is no entry with the given alias.
// =============================================================================
procedure TBitList.delete ( entry : TBitListEntry );
var
  tbe : TBitListEntry;
  i   : integer;
begin

  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    tbe := TBitListEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( tbe.equals ( entry ) ) then
    begin
      self.entries.Delete ( i );
      exit;
    end; // if ( tbe.equals ( entry ) ) then
  end; // for i := 0 to self.entries.Count-1 do

  // The searched object does not exist
  raise EObjectNotExist.Create ( Format ( 'Bit %s (0x%s) does not exist!', [entry.Alias, SysUtils.IntToHex(entry.Bit, 8)] ) );
end; // procedure TBitList.delete();


// =============================================================================
//    Class        : TBitList
//    Function     : Returns the entry at index inx.
//
//    Parameter    : inx - the index of the entry to be returned
//    Return       : see above
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of table entries
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TBitList.getEntry ( inx : integer ) : TBitListEntry;
begin
  if ( (inx < 0) or (inx >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [inx, self.Count-1] ) );

  result := TBitListEntry ( self.entries.Items[inx] );
end; // function TBitList.getEntry ( inx : integer ) : TBitListEntry;


// =============================================================================
//    Class        : TBitList
//    Function     : getByAlias()
//                   Returns the entry with the given alias.
//    Parameter    : alias - The alias of the entry to search for.
//    Return       : The bit list entry with the given alias.
//    Exceptions   : EObjectNotExist, if there is no entry with the given alias.
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TBitList.getByAlias ( alias : string ) : TBitListEntry;
var
  entry : TBitListEntry;
  i   : integer;
begin
  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    entry := TBitListEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( entry.Alias = alias ) then
    begin
      result := entry;
      exit;
    end; // if ( entry.Alias = alias ) then
    
  end; // for i := 0 to self.entries.Count-1 do

  // the entry was not found ==> throw new exception
  raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry alias="%s" does not exist!', [alias] ) );
end; // function TBitList.getByAlias();


// =============================================================================
//    Class        : TBitList
//    Function     : getByBit
//                   Returns the entry with the given bit.
//    Parameter    : bit - The bit of the entry to search for.
//    Return       : The list entry with the given bit.
//    Exceptions   : EObjectNotExist, if there is no entry with the given bit.
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TBitList.getByBit ( bit : longword ) : TBitListEntry;
var
  entry : TBitListEntry;
  i     : integer;
begin
  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    entry := TBitListEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( entry.Bit = bit ) then
    begin
      result := entry;
      exit;
    end; // if ( entry.Bit = bit ) then
    
  end; // for i := 0 to self.entries.Count-1 do

  // the entry was not found ==> throw new exception
  raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry bit=0x%x does not exist!', [bit] ) );
  
end; // function TBitList.getByBit()


// =============================================================================
//    Class        : TBitList
//    Function     : getByText
//                   Returns the entry with the given text.
//    Parameter    : text - The text of the entry to search for.
//    Return       : The list entry with the given text.
//    Exceptions   : EObjectNotExist, if there is no entry with the given text.
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TBitList.getByText ( text : string ) : TBitListEntry;
var
  entry : TBitListEntry;
  i     : integer;
begin
  // search for the given entry
  for i := 0 to self.entries.Count-1 do
  begin
    entry := TBitListEntry ( self.entries.Items[i] );

    // the given entry has been found?
    if ( entry.Text = text ) then
    begin
      result := entry;
      exit;
    end; // if ( entry.Text = text ) then
    
  end; // for i := 0 to self.entries.Count-1 do

  // the entry was not found ==> throw new exception
  raise EObjectNotExist.Create ( MtxWideFormat ( 'Entry text="%s" does not exist!', [text] ) );
end; // function TBitList.getByText()


// =============================================================================
//    Class        : TBitList
//    Function     : contains
//                   Tests whether the list contains the given entry.
//    Parameter    : entry - The entry to be tested
//    Return       : true  - The list contains the given entry.
//                   false - The list does not contain the given entry.
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TBitList.contains ( entry : TBitListEntry ) : boolean;
begin
  result := (self.indexOf(entry) >= 0);
end; // function TBitList.contains()


// =============================================================================
//    Class        : TBitList
//    Function     : copyFrom
//                   This function copies the object list in this object.
//                   If list is not assigned, nothing will be done!
//    Parameter    : list - the list to copy
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
procedure TBitList.copyFrom ( list : TBitList );
var
  i     : integer;
  entry : TBitListEntry;
begin
  // There is nothing more to do, when list is not assigned!
  if ( not Assigned(list) ) then exit;

  // first clear the items of this object
  self.clear();

  // copy the list entries
  for i := 0 to list.entries.Count-1 do
  begin
    entry := TBitListEntry.create();
    entry.copyFrom ( TBitListEntry(list.entries.Items[i]) );
    self.Add ( entry );
  end; // for i := 0 to list.entries.Count-1 do

end; // procedure TBitList.copyFrom()


// =============================================================================
//    Class        : TBitList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TBitList.Clone() : TMtxCloneableObject;
begin
  result                  := TBitList.create();
  TBitList(result).copyFrom ( self );
end; // function TBitList.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      C L A S S   TMaskedTableList
//
// =============================================================================
// =============================================================================


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : create
//                   Creates an empty list
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
constructor TMaskedTableList.create();
begin
  // 2013-07-30 /gsv/: Umstellung von TList auf TObjectList.
  // Dadurch wird der Speicherplatz beim Löschen der Einträge automatisch freigegeben.
  self.entries             := TObjectList.Create();
  self.entries.OwnsObjects := true;
end; // constructor TMaskedTableList.create();


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : destroy
//                   Frees the memory, allocated by this object.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
destructor TMaskedTableList.destroy();
begin
  self.entries.Free;
end; // destructor TMaskedTableList.destroy();


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : clear
//                   Clears the contents of this list.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
procedure TMaskedTableList.clear();
begin
  self.entries.Clear();
end; // procedure TMaskedTableList.clear();


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : add
//                   Adds the table to the list
//    Parameter    : table - the table to be add
//    Return       : --
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
procedure TMaskedTableList.add ( table : TMaskedTable );
begin
  self.entries.Add ( table );
end; // procedure TMaskedTableList.add ( table : TMaskedTable );


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : getCount
//                   Returns the number of elements in the list.
//    Parameter    : --
//    Return       : number of parameters currently contained in the list
//    Exceptions   : --
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TMaskedTableList.getCount() : integer;
begin
  result := self.entries.Count;
end; // function TMaskedTableList.getCount() : integer;


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : getTable
//                   Returns the table at index inx
//    Parameter    : inx - index of the table to return
//    Return       : the table at index inx
//    Exceptions   : EIndexOutOfBounds, if the specified index is invalid, i.e.
//                   index < 0 or index >= number of table entries
//    First author : 2007-07-13 /gsv/
//    History      : --
// =============================================================================
function TMaskedTableList.getTable ( inx : integer ) : TMaskedTable;
begin
  if ( (inx < 0) or (inx >= self.Count) ) then
    raise EIndexOutOfBounds.create ( format ( 'The index %d is not in range [0...%d]!', [inx, self.Count-1] ) );

  result := TMaskedTable ( self.entries.Items[inx] );
end; // function TMaskedTableList.getTable ( inx : integer ) : TMaskedTable;


// =============================================================================
//    Class        : TMaskedTableList
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function TMaskedTableList.Clone() : TMtxCloneableObject;
var
  i : integer;
begin
  result := TMaskedTableList.create();

  for i := 0 to self.Count-1 do
  begin
    TMaskedTableList(result).add ( TMaskedTable ( self.getTable(i).Clone() ) );
  end;

end; // function TMaskedTableList.Clone() : TMtxCloneableObject;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TOszCluster
//
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : TOszCluster
//    Function     : create()
//                   Konstruktor: Neues Objekt mit Default-Werten erzeugen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2008-11-06 /she/
//    History      : --
// =============================================================================
constructor TOszCluster.Create();
begin
  self.iChannel      := 0;
  self.iCluster      := 0;
  self.str_answer    := '';
  StringOfChar( '0', 16 * 8 );  // 16 * 8-stellige Null
end; // constructor TOszCluster.Create();


// =============================================================================
//    Class        : TOszCluster
//    Function     : copyFrom
//                   Kopierfunktion: Kopiert osz in self
//    Parameter    : osz - zu kopierendes Objekt in self
//    Return       : --
//    Exceptions   : --
//    First author : 2008-11-13 /she/
//    History      : --
// =============================================================================
procedure TOszCluster.copyFrom ( const osz : TOszCluster );
begin
  self.iChannel      := osz.iChannel;
  self.iCluster      := osz.iCluster;
  self.str_answer    := osz.str_answer;
end; // procedure TOszCluster.copyFrom ( const osz : TOszCluster );


// =============================================================================
//    Class        : TOszCluster
//    Function     : getlong()
//                   Datentwort aus Cluster-Struktur auslesen
//    Parameter    : i   : Index des Datenwortes (0...15)
//    Return       : Datenlongwort
//    Exceptions   : ERangeError, falls ein Fehler bei der Konvertierung auftritt.
//    First author : 2008-11-06 /she/
//    History      : --
// =============================================================================
function ToszCluster.getlong ( i : integer ) : longword;
var
  code: integer;
begin
  Val( '$' + Copy( self.Answer, 1 + 8*i, 8 ), Result, code );
  if ( code <> 0 ) then
  begin
    raise ERangeError.Create ( 'fatal error ToszCluster.getlong' );
    Result := 0;
  end;

end; // function ToszCluster.getlong ( i : integer ) : longword;


// =============================================================================
//    Class        : ToszCluster
//    Function     : Clone()
//                   Erstellt eine neue Kopie des Objektes
//                   Hinweis: Der Aufrufer muss sich um die Freigabe des
//                            Objektes kümmern! 
//    Parameter    : --
//    Return       : neue Kopie von self
//    Exceptions   : --
//    First author : 2014-08-21 /gsv/
//    History      : --
// =============================================================================
function ToszCluster.Clone() : TMtxCloneableObject;
begin
  result                  := ToszCluster.create();
  ToszCluster(result).copyFrom ( self );
end; // function ToszCluster.Clone() : TMtxCloneableObject;


constructor TMtxGUICODataBase.Create();
begin
  inherited Create();
  self.i64Value           := 0;
  self.i64ValueBackup     := 0;
  self.i64ValueOI         := 0;
  self.bHasOIValue        := false;
  self.bReadCyclic        := false;
  self.bReadORValue       := true;
  self.bReadOIValue       := false;
  self.bRestoreOldValue   := true;
end;


procedure TMtxGUICODataBase.copyFrom ( obj : TMtxGUICODataBase );
begin
  if ( not Assigned ( obj ) ) then exit;

  self.Value           := obj.Value;
  self.ValueBackup     := obj.Value;
  self.ValueOI         := obj.Value;
  self.HasOIValue      := obj.HasOIValue;
  self.ReadCyclic      := obj.ReadCyclic;
  self.ReadORValue     := obj.ReadORValue;
  self.ReadOIValue     := obj.ReadOIValue;
  self.RestoreOldValue := obj.RestoreOldValue;
end;


constructor TMtxGUICODataSingleCO.Create();
begin
  inherited Create();

  self.sName := '';
end;

procedure TMtxGUICODataSingleCO.copyFrom ( obj : TMtxGUICODataBase );
begin
  if ( not Assigned ( obj ) ) then exit;

  inherited copyFrom ( obj );

  if ( obj is TMtxGUICODataSingleCO ) then
  begin
    self.Name := TMtxGUICODataSingleCO(obj).Name;
  end;
end;




constructor TMtxGUICODataInt64.Create();
begin
  inherited Create();

  self.sNameHigh := '';
  self.sNameLow  := '';
end;

procedure TMtxGUICODataInt64.copyFrom ( obj : TMtxGUICODataBase );
begin
  if ( not Assigned ( obj ) ) then exit;

  inherited copyFrom ( obj );

  if ( obj is TMtxGUICODataInt64 ) then
  begin
    self.NameHigh := TMtxGUICODataInt64(obj).NameHigh;
    self.NameLow  := TMtxGUICODataInt64(obj).NameLow;
  end;
end;



constructor TMtxGUICODataSimpleMultiCO.Create();
begin
  self.FPointerCOName       := '';
  self.FPointerCOMin        := 0;
  self.FPointerCOMax        := 0;
  self.FPointerCOVal        := 0;
  self.FDataCOInfo          := TMtxGUICODataSingleCO.Create();
  self.FDataCOValues        := TInt64List.create();
  self.FDataCOValuesBackup  := TInt64List.create();

  inherited;
end;


procedure TMtxGUICODataSimpleMultiCO.copyFrom ( obj : TMtxGUICODataSimpleMultiCO );
begin
  if ( not Assigned ( obj ) ) then exit;


  self.PointerCOName      := obj.PointerCOName;
  self.PointerCOVal       := obj.PointerCOVal;
  self.PointerCOMin       := obj.PointerCOMin;
  self.PointerCOMax       := obj.PointerCOMax;

  self.DataCOInfo.copyFrom         ( obj.DataCOInfo );
  self.DataCOValues.copyFrom       ( obj.DataCOValues       );
  self.DataCOValuesBackup.copyFrom ( obj.DataCOValuesBackup );
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentBase
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentBase.Create ( AOwner : TComponent );
begin
  FOnReset              := nil;
  FOnCalcValueFromServo := nil;
  FEnabled              := true;
  FWriteAccessRunning   := false;
  FAssociatedComponents := TMtxExtCOAssociatedComponentList.Create(); // Liste mit den Komponenten, die mit diesem KO verlinkt sind.
  inherited Create ( AOwner );
end;

destructor TMtxCOComponentBase.Destroy();
begin
  self.FAssociatedComponents.Free();

  inherited Destroy();
end;

// Berechnung Servo --> GUI (==> COValue anzeigen)
procedure TMtxCOComponentBase.calcValueFromServo();
begin
  if ( Assigned ( FOnCalcValueFromServo ) ) then
  begin
    FOnCalcValueFromServo ( self );
  end;
end;

// Komponente zurücksetzen (bei win_reset()).
procedure TMtxCOComponentBase.reset();
begin
  if ( Assigned ( FOnReset ) ) then
  begin
    FOnReset ( self );
  end;
end;

// Getter / Setter für die Enabled-Eigenschaft
function TMtxCOComponentBase.GetEnabled() : boolean;
begin
  result := FEnabled;
end;
procedure TMtxCOComponentBase.SetEnabled ( b_on : boolean );
begin
  FEnabled := b_on;
end;


// Getter/Setter für die Property WriteAccessRunning
function TMtxCOComponentBase.getWriteAccessRunning() : boolean;
begin
  result := FWriteAccessRunning;
end;
procedure TMtxCOComponentBase.setWriteAccessRunning ( b : boolean );
begin
  FWriteAccessRunning := b;
end;

// Getter/Setter für die Property AssociatedComponents
function TMtxCOComponentBase.getAssociatedComponents() : TMtxExtCOAssociatedComponentList;
begin
  result := self.FAssociatedComponents;
end;

// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentSingleCOBase
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentSingleCOBase.Create ( AOwner : TComponent );
begin
  // KO-Struktur intialisieren + weitere Initialisierungen vor dem inherited Konstruktor,
  self.FCOData := TMtxGUICODataSingleCO.Create();
  inherited Create ( AOwner );
end;

destructor TMtxCOComponentSingleCOBase.Destroy();
begin
  FCOData.Free();
  inherited Destroy();
end;


// Getter/Setter für die Property COData
function TMtxCOComponentSingleCOBase.getCOData() : TMtxGUICODataSingleCO;
begin
  result := FCOData;
end;
procedure TMtxCOComponentSingleCOBase.setCOData ( obj : TMtxGUICODataSingleCO );
begin
  self.FCOData.copyFrom ( obj );
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentSingleCOReadWrite
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentSingleCOReadOnly.Create ( AOwner : TComponent );
begin
  inherited Create ( AOwner );
  self.FCOData.RestoreOldValue := false;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentSingleCOReadWrite
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentSingleCOReadWrite.Create ( AOwner : TComponent );
begin
  self.b_is_changed        := false;
  self.FOnCalcValueToServo := nil;
  inherited Create ( AOwner );
end;

// Getter / Setter der Property is_changed
function TMtxCOComponentSingleCOReadWrite.getIsChanged() : boolean;
begin
  result := self.b_is_changed;
end;
procedure TMtxCOComponentSingleCOReadWrite.setIsChanged ( b : boolean );
begin
  self.b_is_changed := b;
end;

// Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
procedure TMtxCOComponentSingleCOReadWrite.calcValueToServo();
begin
  if ( Assigned ( FOnCalcValueToServo ) ) then
  begin
    FOnCalcValueToServo ( self );
  end;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentControlCOBase
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentControlCOBase.Create ( AOwner : TComponent );
begin
  // KO-Struktur intialisieren + weitere Initialisierungen vor dem inherited Konstruktor,
  self.FCOData := TMtxGUICODataSingleCO.Create();
  inherited Create ( AOwner );
end;

destructor TMtxCOComponentControlCOBase.Destroy();
begin
  FCOData.Free();
  inherited Destroy();
end;


// Getter/Setter für die Property COData
function TMtxCOComponentControlCOBase.getCOData() : TMtxGUICODataSingleCO;
begin
  result := FCOData;
end;
procedure TMtxCOComponentControlCOBase.setCOData ( obj : TMtxGUICODataSingleCO );
begin
  self.FCOData.copyFrom ( obj );
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentControlCOReadOnly
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentControlCOReadOnly.Create ( AOwner : TComponent );
begin
  inherited Create ( AOwner );
  self.FCOData.RestoreOldValue := false;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentControlCOReadWrite
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentControlCOReadWrite.Create ( AOwner : TComponent );
begin
  self.b_is_changed        := false;
  self.FOnCalcValueToServo := nil;
  inherited Create ( AOwner );
end;

// Getter / Setter der Property is_changed
function TMtxCOComponentControlCOReadWrite.getIsChanged() : boolean;
begin
  result := self.b_is_changed;
end;
procedure TMtxCOComponentControlCOReadWrite.setIsChanged ( b : boolean );
begin
  self.b_is_changed := b;
end;

// Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
procedure TMtxCOComponentControlCOReadWrite.calcValueToServo();
begin
  if ( Assigned ( FOnCalcValueToServo ) ) then
  begin
    FOnCalcValueToServo ( self );
  end;
  
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentInt64COBase
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentInt64COBase.Create ( AOwner : TComponent );
begin
  // KO-Struktur intialisieren + weitere Initialisierungen vor dem inherited Konstruktor,
  self.FCOData := TMtxGUICODataInt64.Create();
  inherited Create ( AOwner );
end;

destructor TMtxCOComponentInt64COBase.Destroy();
begin
  FCOData.Free();
  inherited Destroy();
end;


// Getter/Setter für die Property COData
function TMtxCOComponentInt64COBase.getCOData() : TMtxGUICODataInt64;
begin
  result := FCOData;
end;
procedure TMtxCOComponentInt64COBase.setCOData ( obj : TMtxGUICODataInt64 );
begin
  self.FCOData.copyFrom ( obj );
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentInt64COReadOnly
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentInt64COReadOnly.Create ( AOwner : TComponent );
begin
  inherited Create ( AOwner );
  self.FCOData.RestoreOldValue := false;
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentInt64COReadWrite
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentInt64COReadWrite.Create ( AOwner : TComponent );
begin
  self.b_is_changed        := false;
  self.FOnCalcValueToServo := nil;
  inherited Create ( AOwner );
end;

// Getter / Setter der Property is_changed
function TMtxCOComponentInt64COReadWrite.getIsChanged() : boolean;
begin
  result := self.b_is_changed;
end;
procedure TMtxCOComponentInt64COReadWrite.setIsChanged ( b : boolean );
begin
  self.b_is_changed := b;
end;

// Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
procedure TMtxCOComponentInt64COReadWrite.calcValueToServo();
begin
  if ( Assigned ( FOnCalcValueToServo ) ) then
  begin
    FOnCalcValueToServo ( self );
  end;
  
end;



// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentSimpleMultiCOBase
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentSimpleMultiCOBase.Create ( AOwner : TComponent );
begin
  // KO-Struktur intialisieren + weitere Initialisierungen vor dem inherited Konstruktor,
  self.FCOData := TMtxGUICODataSimpleMultiCO.Create();
  inherited Create ( AOwner );
end;

destructor TMtxCOComponentSimpleMultiCOBase.Destroy();
begin
  FCOData.Free();
  inherited Destroy();
end;


// Getter/Setter für die Property COData
function TMtxCOComponentSimpleMultiCOBase.getCOData() : TMtxGUICODataSimpleMultiCO;
begin
  result := FCOData;
end;
procedure TMtxCOComponentSimpleMultiCOBase.setCOData ( obj : TMtxGUICODataSimpleMultiCO );
begin
  self.FCOData.copyFrom ( obj );
end;


// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxCOComponentSimpleMultiCOReadWrite
//
// =============================================================================
// =============================================================================
constructor TMtxCOComponentSimpleMultiCOReadWrite.Create ( AOwner : TComponent );
begin
  self.b_is_changed        := false;
  self.FOnCalcValueToServo := nil;
  inherited Create ( AOwner );
end;

// Getter / Setter der Property is_changed
function TMtxCOComponentSimpleMultiCOReadWrite.getIsChanged() : boolean;
begin
  result := self.b_is_changed;
end;
procedure TMtxCOComponentSimpleMultiCOReadWrite.setIsChanged ( b : boolean );
begin
  self.b_is_changed := b;
end;

// Berechnung GUI --> Servo (==> COValue berechnen/aktualisieren)
procedure TMtxCOComponentSimpleMultiCOReadWrite.calcValueToServo();
begin
  if ( Assigned ( FOnCalcValueToServo ) ) then
  begin
    FOnCalcValueToServo ( self );
  end;
  
end;


constructor TMtxGUICheckedInfo.Create();
begin
  inherited Create();
  self.FMask              := 0;
  self.FBitfieldChecked   := 0;
  self.FBitfieldUnchecked := 0;
end;



// =============================================================================
//    Class        : TMtxStringList
//    Function     : findString()
//                   Suchfunktion: Nach einem beliebigen String suchen
//    Parameter    : str   - String, nach dem gesucht wird
//                   i_inx - Rückgabe: Index, an dem der gesuchte String gefunden wurde.
//    Return       : true  - String gefunden
//    Exceptions   : --
//    First author : 2016-01-25 /gsv/
//    History      : --
// =============================================================================
function TMtxStringList.findString ( str : string; var i_inx : integer ) : boolean;
var
  i : integer;
begin
  for i:= 0 to self.Count-1 do
  begin
    if ( StringCompareICase ( self.Strings[i], str ) ) then
    begin
      // Eintrag gefunden ==> Funktion beenden
      i_inx  := i;
      result := true;
      exit;
    end;
  end;

  // Eintrag nicht gefunden
  result := false;
end; // function TMtxStringList.findString ( ... )

function TMtxStringList.getNextLine ( var i_inx : integer; var s_line : string ) : boolean;
var
  i : integer;
  s : string;
begin
  for i := i_inx+1 to self.Count-1 do
  begin
    s := self.Strings[i];
    if ( not (self.isEmptyString ( s ) or
              self.isCommentLine ( s )) ) then
    begin
      i_inx  := i;
      s_line := s;
      result := true;
      exit;
    end;
  end;

  // Liste zu Ende
  result := false;
end; // function TMtxStringList.getNextLine ( var i_inx : integer ) : boolean;


function TMtxStringList.getNextLineUntilSectionBegin ( var i_inx : integer; var s_line : string ) : boolean;
var
  i : integer;
  s : string;
begin
  i      := i_inx;
  result := getNextLine ( i, s );
  if ( result ) then
  begin
    result := not isSectionBegin ( s );
    if ( result ) then
    begin
      i_inx  := i;
      s_line := s;
    end;
  end;
end; // function TMtxStringList.getNextLineUntilSectionBegin ( var i_inx : integer; var s_line : string ) : boolean;


function TMtxStringList.isEmptyString ( str : string ) : boolean;
begin
  result := Trim(str) = '';
end; // function TMtxStringList.isEmptyString ( str : string ) : boolean;

function TMtxStringList.isCommentLine ( str : string ) : boolean;
begin
  if ( self.isEmptyString ( str ) ) then
  begin
    result := false;
  end
  else
  begin
    result := str[1] = C_CHAR_COMMENT_LINE;
  end;
end; // function TMtxStringList.isCommentLine ( str : string ) : boolean;


function TMtxStringList.isSectionBegin ( str : string ) : boolean;
begin
  if ( self.isEmptyString ( str ) ) then
  begin
    result := false;
  end
  else
  begin
    result := str[1] = '[';
  end;
end; // function TMtxStringList.isSectionBegin ( str : string ) : boolean;

procedure TMtxStringList.addNewLine();
begin
  self.Add ( '' );
end;

{$IFDEF DELPHI_XE_UP}
constructor TMtxThread.Create();
begin
  inherited Create();
end;
{$ENDIF}

constructor TMtxThread.Create ( CreateSuspended: Boolean );
begin
  FSuspendEvent       := TEvent.Create();
  FCreateSuspendedMtx := CreateSuspended;

  if ( FCreateSuspendedMtx ) then FSuspendEvent.ResetEvent()
  else                            FSuspendEvent.SetEvent();

  inherited Create ( CreateSuspended );
end;


destructor TMtxThread.Destroy();
begin
  inherited;
  FSuspendEvent.Free();
end;


{$IFDEF DELPHI_XE_UP}
procedure TMtxThread.TerminatedSet();
begin
  inherited;
  self.FSuspendEvent.SetEvent();
end;
{$ENDIF}

procedure TMtxThread.SuspendWork();
begin
  self.FSuspendEvent.ResetEvent();
end;

procedure TMtxThread.ResumeWork();
begin
  //if ( not isSuspended() ) then exit;

  self.FSuspendEvent.SetEvent();

  // Falls der Thread als Suspenden erstellt wurde, dann diesen über die
  // Start-Funktion initial anstarten.
  if ( FCreateSuspendedMtx ) then
  begin
    FCreateSuspendedMtx := false;
  {$IFDEF DELPHI_XE_UP}
    self.Start();
  {$ELSE}
    while ( self.Suspended ) do self.Resume();
  {$ENDIF}
  end;

end;

procedure TMtxThread.CheckHandleSuspended();
begin
{
  if ( WaitForSingleObject( FSuspendEvent, INFINITE ) = WAIT_OBJECT_0 ) then
  begin
    FSuspendEvent.ResetEvent();
  end;
}
  FSuspendEvent.WaitFor ( INFINITE );
  //FSuspendEvent.ResetEvent();
end;

function TMtxThread.isSuspended() : boolean;
begin
  result := self.Suspended;
end;





// =============================================================================
// =============================================================================
//
//                      K L A S S E   TMtxWindowPositionEntry
//
// =============================================================================
// =============================================================================
constructor TMtxWindowPositionEntry.Create();
begin
  self.FWinName := '';
  self.FLeft    := C_MTX_DEF_WIN_POS_LEFT;
  self.FTOP     := C_MTX_DEF_WIN_POS_TOP;
end;

function TMtxWindowPositionEntryComparer.Compare ( const Left, Right: TMtxWindowPositionEntry ): Integer;
begin
  result := CompareText ( Left.WinName, Right.WinName );
end;


constructor TMtxWindowPositionList.Create();
begin
  inherited Create ( TMtxWindowPositionEntryComparer.Create(), true );
end;


function TMtxWindowPositionList.getByName ( AWinName : string; var AEntry : TMtxWindowPositionEntry ) : boolean;
var
  i     : integer;
  entry : TMtxWindowPositionEntry;
begin
  for i := 0 to self.Count-1 do
  begin
    entry := self.Items[i];

    if ( StringCompareICase ( entry.FWinName, AWinName ) ) then
    begin
      // Eintrag gefunden
      AEntry := entry;
      result := true;
      exit;
    end;
  end; // for i := 0 to self.Count-1 do

  // Eintrag nicht gefunden
  result := false;
end; // function TMtxWindowPositionList.getByName ( AWinName : string; var AEntry : TMtxWindowPositionEntry ) : boolean;


function TMtxWindowPositionList.getWinPosByName ( AWinName : string; var ALeft, ATop : integer ) : boolean;
var
  entry : TMtxWindowPositionEntry;
begin
  result := getByName ( AWinName, entry );

  if ( result ) then
  begin
    // Eintrag gefunden
    ALeft  := entry.Left;
    ATop   := entry.Top;
  end
  else
  begin
    // Eintrag nicht gefunden ==> Default Window-Position zurückgeben
    ALeft  := C_MTX_DEF_WIN_POS_LEFT;
    ATop   := C_MTX_DEF_WIN_POS_TOP;
  end;
end; // function TMtxWindowPositionList.getWinPosByName ( AWinName : string; var ALeft, ATop : integer ) : boolean;


procedure TMtxWindowPositionList.updateWinPos ( AWinName : string; ALeft, ATop : integer );
var
  entry   : TMtxWindowPositionEntry;
  b_found : boolean;
begin
  b_found := getByName ( AWinName, entry );

  if ( b_found ) then
  begin
    // Entrag existiert bereits
    entry.Left    := ALeft;
    entry.Top     := ATop;
  end
  else
  begin
    // Eintrag existiert nicht ==> Neu anlegen
    entry         := TMtxWindowPositionEntry.Create();
    entry.WinName := AWinName;
    entry.Left    := ALeft;
    entry.Top     := ATop;
    self.Add ( entry );
  end;
end; // procedure TMtxWindowPositionList.updateWinPos ( AWinName : string; ALeft, ATop : integer );


procedure TMtxWindowPositionList.Add ( AWinName : string; ALeft, ATop : integer );
var
  entry : TMtxWindowPositionEntry;
begin
  // Falls der Eintrag bereits existiert, dann dieses nicht nochmal hinzufügen.
  if ( self.getByName ( AWinName, entry ) ) then
  begin
    exit;
  end;

  entry         := TMtxWindowPositionEntry.Create();
  entry.WinName := AWinName;
  entry.Left    := ALeft;
  entry.Top     := ATop;
  self.Add ( entry );
end;


function TMtxExtCOAssociatedComponentComparer.Compare ( const Left, Right: TWinControl ): Integer;
begin
  result := CompareText ( Left.Name, Right.Name );
end;

constructor TMtxExtCOAssociatedComponentList.Create();
begin
  // Diese Liste enthält nur Referenzen auf GUI-Komponenten,
  // die anderweitig freigegeben werden.
  inherited Create ( TMtxExtCOAssociatedComponentComparer.Create(), false );
end;

end.


