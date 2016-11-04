unit MtxUtils;

// =============================================================================
// =============================================================================
//
//     Module name       : $RCSfile: MtxUtils.pas,v $
//     Short description : diverse Hilfsfunktionen
//     Project           : --
//     Compiler          : Delphi 7
//     First author      : 2007-06-19 /gsv/
//     Actual version    : $Revision: 1.37 $
//     Copyright         : (c) Metronix GmbH 2007
// -----------------------------------------------------------------------------
//     History           : 2009-04-22 /aha/ Delphi7 Compiler flag for use of
//                                    this unit in "prüfplätze"
// -----------------------------------------------------------------------------
//     Descriptions
// =============================================================================

{$INCLUDE MtxCompilers.inc}

interface
{$IFDEF VER150}
// das bedeutet "Delphi7"
uses classes, TnTWideStrings, MtxTypes, Controls, DSIntf, Forms, TntForms,
     SysUtils, Windows, DateUtils, TypInfo, MtxGuiWrapper;
{$ELSE}
  // Delphi 2007 und höher
  {$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
    uses classes, WideStrings, MtxTypes, Controls, DSIntf, Forms, SysUtils,
         Windows, DateUtils, TypInfo, Graphics, MtxGuiWrapper, Dialogs,
         AnsiStrings, System.Diagnostics;
  {$ELSE}
    uses classes, WideStrings, MtxTypes, Controls, DSIntf, Forms, TntForms,
         TntSysUtils, SysUtils, Windows, DateUtils, TypInfo, Graphics,
         MtxGuiWrapper, Dialogs, TntDialogs;
  {$ENDIF}
{$ENDIF}

const
  C_IP_ADDR_SEPARATOR = '.'; // Trennzeichen für IPv4-Adressen (z.B. 192.168.0.1)

  // 2013-08-16 /gsv/: Konstanten für die Funktion generateCRC16_CCITT()
  C_CRC16_CCITT_INITIAL_VALUE = $FFFF; // Initialwert des Parameters crc_old
                                       // Der erste Aufruf der Funktion muss mit diesem Wert stattfinden!
  C_CRC16_CCITT_NOT_LAST_DATA = false; // s. Parameter b_last_data_part
  C_CRC16_CCITT_LAST_DATA     = true;  // s. Parameter b_last_data_part

  // 2013-08-21 /gsv/: Domain-Name der Metronix-Benutzer
  // Über den Domain-Namen können bestimmte Funktionen ein-/ausgeblendet werden.
  // Dadurch ist es sichergestellt, dass diese nur bei Metronix sichtbar sind!
  C_DOMAIN_NAME_METRONIX = 'ATG';

  // Konvertierung String ==> Double
  function stringToDouble ( value : string; decimalSeparator : char ) : double;

  // String in Substrings durch ein Trennzeichen unterteilen
  function  extract_strings ( c_separator : char; s_in : string; var tsl_result : TStringList ) : integer; overload;
  function  extract_strings ( c_separator : char; s_in : string; var tsl_result : TWideStringList ) : integer; overload; deprecated;

  // Fügt Nullen am Anfang eines Strings ein, bis die Länge len erreicht wurde.
  function fillWithLeadingZeros   ( const ws : string; len : integer ) : string;

  // Fügt Leerzeichen am Ende eines Strings ein, bis die Länge len erreicht wurde.
  function fillWithTrailingSpaces ( const ws : string; len : integer ) : string;

  // Fügt Leerzeichen am Anfang eines Strings ein, bis die Länge len erreicht wurde.
  function fillWithLeadingSpaces  ( const ws : string; len : integer ) : string;

  // Konvertierung einer Zeit in String, Format HH:MM:SS:LL
  // HH = Stunden, MM = Minuten, SS = Sekunden, LL = Millisekunden
  function timeToStrMs ( const aTime : TDateTime ) : string;

{$IFNDEF VER150}
  // Erstellt eine Liste mit allen Dateien im Pfad path,
  // die dem Dateinamen entsprechen.
  function  GetFileList   ( path : string; filename : string ) : TStringList;

  // 2013-04-29 /gsv/: Liste mit den Unterverzeichnissen erstellen
  procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList ); overload;
  procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList; i_subdir_depth : integer ); overload;
{$ENDIF}

  // Gibt die Länge des Datentyps in Bits zurück
  function getBitSize  ( dataType : TDataType ) : byte;
  // Gibt die Länge des Datentyps in Bytes zurück
  function getByteSize ( dataType : TDataType ) : byte;
  // Leitet den Datentyp von der Bitlänge ab
  function getDataTypeFromBitSize ( bitSize : byte  ) : TDataType;
  // untere Grenze des Datentyps ermitteln
  function getMinValue ( dataType : TDataType ) : int64;
  // obere Grenze des Datentyps ermitteln
  function getMaxValue ( dataType : TDataType ) : int64;


  // Bytereihenfolge eines Words vertauschen: 0x1122 ==> 0x2211
  function swap ( w  : word     ) : word;     overload;
  // Bytereihenfolge eines Longwords vertauschen: 0x11223344 ==> 0x44332211
  function swap ( lw : longword ) : longword; overload;

  // Konvertiert den INT64-Wert entsprechend dem Datentyp dataType
  function convertDataType ( dataIn : int64; dataType : TDataType ) : int64;

  // Konvertiert den Datentyp in die entsprechende Stringdarstellung
  function convertDataTypeToString ( dataType : TDataType ) : string;

  // Gibt den Text der GUI-Komponente zurück
  function  getComponentText ( comp : TWinControl ) : string;
  // Setzt den Text der GUI-Komponente
  procedure setComponentText ( comp : TWinControl; ws : string );

  // Konvertierung: String in UINT32 (ASCII-Darstellung des Strings)
  function  MtxStrToAscii    ( str : string;     fill_char : char = '0' ) : longword; {$IFDEF DELPHI_XE_UP} overload; {$ENDIF}
{$IFDEF DELPHI_XE_UP}
  function  MtxStrToAscii    ( str : AnsiString; fill_char : char = '0' ) : longword; overload;
{$ENDIF}

  // Konvertierung: String in UINT32 (ASCII-Darstellung des Strings)
  function  convertStrToUint32 ( str : string; fill_char : char = '0' ) : longword;
  // Konvertierung: UINT32 in String (ASCII-Darstellung)
  function  convertUint32ToStr ( lw : longword ) : string;


  // Checksumme von sData bilden (einfache Aufsummierung der ASCII-Codes)
  function generateCRCSimple ( const sData : string     ): longword; {$IFDEF DELPHI_XE_UP} overload; {$ENDIF}
{$IFDEF DELPHI_XE_UP}
  function generateCRCSimple ( const sData : AnsiString ): longword; overload;
{$ENDIF}  

  // String auf Umgebungsvariablen untersuchen und diese durch den jeweiligen
  // Wert ersetzen.
  function ExpandEnvStr ( const str: string ): string;

  // 2010-06-18 /gsv/: Vergleich zweier Strings ohne Unterscheidung
  // zwischen Groß-/Kleinschreibung!
  function StringCompareICase ( s1, s2 : string     ) : boolean; {$IFDEF DELPHI_XE_UP} overload; {$ENDIF}
{$IFDEF DELPHI_XE_UP}
  function StringCompareICase ( s1, s2 : AnsiString ) : boolean; overload;
{$ENDIF}


  // 2012-09-21 /gsv/: Verweis auf Systemfunktion ergänzt (s. auch Delphi-Hilfe)
  // The CheckNameLegalDOS8Dot3 function determines whether or not a specified name can be used to create a file on a FAT file system.
  // Parameters
  //   lpName:               [in ] A pointer to a null-terminated string that specifies a file name, in 8.3 format.
  //   lpOemName:            [out] A pointer to a buffer that receives the OEM string that corresponds to Name. This parameter can be NULL.
  //   OemNameSize:          [in ] The size of the lpOemName buffer, in characters. If lpOemName is NULL, this parameter must be 0 (zero).
  //   pbNameContainsSpaces: [out] Indicates whether or not a name contains spaces. This parameter can be NULL.
  //                               If the name is not a valid 8.3 FAT file system name, this parameter is undefined.
  //   pbNameLegal:          [out] If the function succeeds, this parameter indicates whether or not a file name is a valid 8.3 FAT file name
  //                               when the current OEM code page is applied to the file name.
  // Return Value
  //   If the function succeeds, the return value is nonzero.
  //   If the function fails, the return value is 0 (zero). To get extended error information, call GetLastError.
  //
  // Remarks
  //   This function can be used to determine whether or not a file name can be passed to a 16-bit Windows-based application or an MS-DOS-based application.
  //   Client: Requires Windows Vista or Windows XP SP1. 
  //   Server: Requires Windows Server "Longhorn" or Windows Server 2003.
  function CheckNameLegalDOS8Dot3 ( Name : PChar; OemName: PChar; OemNameSize: LongWord;
                                    NameContainsSpaces, NameLegal: PLongBool ): LongBool; stdcall;


  // Konvertierung integer --> Stringdarstellung IPv4 (z.B. $C0A80001 ==> '192.168.0.1')
  function IntToStrIPv4 ( i64_ip : int64 ) : string;

  // 2013-08-16 /gsv/: Konvertierung UINT16 --> Byte-Array (byte_array[0] := MSB)
  procedure convUint16ToByteArray (  w :     word; var byte_array : array of byte );
  // 2013-08-16 /gsv/: Konvertierung UINT32 --> Byte-Array (byte_array[0] := MSB)
  procedure convUint32ToByteArray ( lw : longword; var byte_array : array of byte );

  // 2013-08-16 /gsv/: Einfache Checksumme über ein UINT32 bilden
  function generateCRC         ( lw : longword ) : word;
  // 2013-08-16 /gsv/: CRC16-CCITT über einen Datenbereich unter Berücksichtigung einer vorherigen Teil-CRC bilden
  function generateCRC16_CCITT ( crc_old : word; const data : array of byte; cnt : integer; b_last_data_part : boolean ) : word; overload;
  // 2013-08-16 /gsv/: CRC16-CCITT über einen UINT16 unter Berücksichtigung einer vorherigen Teil-CRC bilden
  function generateCRC16_CCITT ( crc_old : word; data : word;                         b_last_data_part : boolean ) : word; overload;
  // 2013-08-16 /gsv/: CRC16-CCITT über einen UINT32 unter Berücksichtigung einer vorherigen Teil-CRC bilden
  function generateCRC16_CCITT ( crc_old : word; data : longword;                     b_last_data_part : boolean ) : word; overload;


  // 2014-08-14 /gsv/: Allgemeine Funktionen zur Berechnung einer CRC16 mit beliebigem Polynom
  function generateCRC16       ( crc_old : word; const data : array of byte;
                                 start_inx : integer; cnt : integer; polynom : word ) : word; overload;
  function generateCRC16       ( crc_old : word; data : AnsiString; polynom : word ) : word; overload;

  // 2013-08-21 /gsv/: Domain des eingeloggten Benutzers ermitteln
  function getUserDomainName() : string;

  // 2013-08-21 /gsv/: Diese Funktion prüft anhand des Domain-Name,
  // ob die Para-SW in der Metronix-Umgebung oder irgendwo beim Kunden läuft.
  function isRunningInMetronixEnvironment() : boolean;

  // 2013-11-29 /gsv/: Größe des Client-Bereichs eines MDI-Hauptformulars ermitteln
  function GetMDIClientAreaBoundsRect ( MDIForm : TForm ): TRect;

  
  function getCurrentSystemFont() : TFont;

  // Fenster innerhalb des darunterliegenden / aktuell aktiven Fensters zentrieren
  procedure centerFormOnActiveForm ( dlg : TMtxForm );

  // 2015-10-28 /gsv/: Prüfen, ob Wert im Wertebereich
  function isInRange ( lw_val, lw_min, lw_max : longword ) : boolean;

  // 2015-11-10 /gsv/: Prüfen, ob Unicode unterstützt
  function isUnicodeSupported() : boolean;


  // 2015-12-01 /gsv/: Testen, ob das Bitfeld innerhalb der Maske gesetzt ist.
  function testValueMasked ( i64_value, i64_mask, i64_bitfield : int64 ) : boolean;
  // 2015-12-01 /gsv/: Bitfeld innerhalb der Maske setzen
  function setValueMasked  ( i64_value, i64_mask, i64_bitfield : int64 ) : int64;


  // 2016-04-07 /gsv/: Umstellung auf Delphi XE7
  // Wrapper-Funktionen erstellt, damit im Quellcode möglichst wenig Compilerschalter verwendet werden.
  function  MtxWideFormat ( const Format: string; const Args: array of const ) : string;
  function  MtxGetDecimalSeparator() : char;
  function  MtxWideTextPos ( const SubStr, S: string ) : Integer;

  { IntToHex converts the given value to a hexadecimal string representation
    with the minimum number of digits specified. }
  function IntToHexAnsi ( Value: Integer;  Digits: Integer ) : AnsiString; overload;
  function IntToHexAnsi ( Value: Longword; Digits: Integer ) : AnsiString; overload;
  function IntToHexAnsi ( Value: Int64;    Digits: Integer ) : AnsiString; overload;
  function IntToHexAnsi ( Value: UInt64;   Digits: Integer ) : AnsiString; overload;
  function AnsiStrToInt ( const S: AnsiString ): Integer;
  function IntToStrAnsi ( Value: Integer ): AnsiString; overload;
  function IntToStrAnsi ( Value: Int64   ): AnsiString; overload;

{$IFNDEF DELPHI_XE_UP}
  function CharInSet ( C: AnsiChar; const CharSet: TSysCharSet ) : Boolean; overload;
  function CharInSet ( C: WideChar; const CharSet: TSysCharSet ) : Boolean; overload;
{$ENDIF}

  procedure MtxOutputDebugString ( s : string );


{$IFDEF DELPHI_XE_UP}
  procedure MtxOutputEllapsedTime ( s : string; stop_watch : TStopWatch );
{$ENDIF}


  function checkWinControlVisible ( AWinControl : TWinControl ) : boolean;

implementation


procedure MtxOutputDebugString ( s : string );
begin
  OutputDebugString ( PChar(timeToStrMs(Now()) + ': ' + s) );
end;

{$IFDEF DELPHI_XE_UP}
procedure MtxOutputEllapsedTime ( s : string; stop_watch : TStopWatch );
begin
  MtxOutputDebugString ( Format ( '%s = %d ms', [s, stop_watch.ElapsedMilliseconds] ) );
end;
{$ENDIF}


// =============================================================================
//    Class        : --
//    Function     : stringToDouble
//                   Konvertiert einen String in den entsprechenden Double-Wert.
//                   ACHTUNG: Der dezimale Separator wird durch den Parameter
//                   decimalSeparator bestimmt und kann vom aktuell eingestellten
//                   Trennzeichen unter "Ländereinstellungen" abweichen.
//                   Diese ist notwendig, z.B. wenn Werte aus einer INI-Datei
//                   eingelesen werden und in einem fest definierten Double-Format
//                   abgespeichert sind.
//    Parameter    : value            - der zu konvertierende String
//                   decimalSeparator - Das für die Konvertierung zu verwendende
//                                      Trennzeichen
//    Return       : Den Double-Wert des Strings valus
//    Exceptions   : EConvertError, falls bei der Konvertierung ein Fehler auftritt.
//    First author : 2007-06-19 /gsv/
//    History      : --
// =============================================================================
function stringToDouble ( value : string; decimalSeparator : char ) : double;
var
  FormatSettings : TFormatSettings;
begin
{$IFDEF DELPHI_XE_UP}
  // FormatSettings initialisieren
  {$WARN SYMBOL_PLATFORM OFF}
  FormatSettings := TFormatSettings.Create ( LOCALE_USER_DEFAULT);
  {$WARN SYMBOL_PLATFORM ON}
  FormatSettings.DecimalSeparator := decimalSeparator;
{$ELSE}
  // FormatSettings initialisieren
  GetLocaleFormatSettings ( LOCALE_USER_DEFAULT, FormatSettings );
  FormatSettings.DecimalSeparator := decimalSeparator;
{$ENDIF}

  // String formatieren
  try
    if ( Trim ( value ) = '' ) then result := 0
    else result := StrToFloat ( value, FormatSettings );

    except on E:Exception do
      raise EConvertError.Create ( E.Message );
  end; // try
end; // function stringToDouble ( value : string; decimalSeparator : char ) : double;


// =============================================================================
//    Class        : --
//    Function     : extract_strings
//                   Füllt eine Liste mit Teil-Strings, die aus einem String extrahiert werden
//
//                   HINWEIS: Leere Strings werden auch in die Liste hinzugefügt
//
//    Parameter    : c_separator : enthält das Trennzeichen, mit dem die Teil-Strings voneinander getrennt sind.
//                   s_in        : der String, der in Teil-Strings zerlegt werden soll
//                   tsl_result  : die String-Liste, der alle aus Content extrahierten
//                     Teil-Strings hinzugefügt werden. Diese Liste wird von dieser Funktion
//                     nicht gelöscht, d.h. alle in ihr enthaltenen Strings bleiben
//                     beim Aufruf von extract_strings erhalten.
//    Return       : gibt die Anzahl der String zurück, die ungleich '' sind.
//    Exceptions   : --
//    History      : --
// =============================================================================
function extract_strings ( c_separator : char; s_in : string; var tsl_result : TStringList ) : integer;
var s_tmp : string;
    i_pos : integer;
begin
  result := 0;
  // Falls die Stringliste tsl_result noch nicht erzeugt ist, wird diese hier initialisiert.
  if ( not Assigned(tsl_result) ) then tsl_result := TStringList.create();
  tsl_result.Clear();

  i_pos := Pos ( c_separator, s_in );
  while ( i_pos <> 0 ) do
  begin
    s_tmp := '';
    // Text bis zum ersten Separator kopieren
    s_tmp := Copy ( s_in, 1, i_pos - 1 );
    tsl_result.Add ( s_tmp );
    // Wenn der Wert existiert, die Anzahl der Werte <> '' inkrementieren
    if ( s_tmp <> '' ) then inc ( result );
    Delete ( s_in, 1, i_pos );
    i_pos := Pos ( c_separator, s_in );
  end;

  // Beim letzten Durchlauf wird der Separator nicht gefunden, da er jedes Mal
  // aus dem String entfernt wird. D.h., dass der letzte String in die Liste
  // nicht hinzugefügt wird. Deshalb wird dies anschließend hier gemacht.
  if ( s_in <> '' ) then
  begin
    tsl_result.Add ( s_in );
    inc ( result );
  end;
end; // function extract_strings ( ... ) : integer;


function  extract_strings ( c_separator : char; s_in : string; var tsl_result : TWideStringList ) : integer;
var s_tmp : string;
    i_pos : integer;
begin
  result := 0;
  // Falls die Stringliste tsl_result noch nicht erzeugt ist, wird diese hier initialisiert.
  if ( not Assigned(tsl_result) ) then tsl_result := TWideStringList.create();
  tsl_result.Clear();

  i_pos := Pos ( c_separator, s_in );
  while ( i_pos <> 0 ) do
  begin
    s_tmp := '';
    // Text bis zum ersten Separator kopieren
    s_tmp := Copy ( s_in, 1, i_pos - 1 );
    tsl_result.Add ( s_tmp );
    // Wenn der Wert existiert, die Anzahl der Werte <> '' inkrementieren
    if ( s_tmp <> '' ) then inc ( result );
    Delete ( s_in, 1, i_pos );
    i_pos := Pos ( c_separator, s_in );
  end;

  // Beim letzten Durchlauf wird der Separator nicht gefunden, da er jedes Mal
  // aus dem String entfernt wird. D.h., dass der letzte String in die Liste
  // nicht hinzugefügt wird. Deshalb wird dies anschließend hier gemacht.
  if ( s_in <> '' ) then
  begin
    tsl_result.Add ( s_in );
    inc ( result );
  end;
end;



// =============================================================================
//    Class        : --
//    Function     : fillWithLeadingZeros()
//                   Fügt Nullen am Anfang eines Strings ein,
//                   bis die gewünschte Länge len erreicht wurde.
//    Parameter    : ws  - zu erweiternder String
//                   len - gewünschte Länge
//    Return       : ws mit aufgefüllt mit führenden Nullen
//    Exceptions   : --
//    First author : 2008-10-31 /gsv/
//    History      : --
// =============================================================================
function fillWithLeadingZeros ( const ws : string; len : integer ) : string;
var
  li_zeroes : longint;
  li_inx    : longint;
  ws_tmp    : string;
begin
  li_zeroes := len - Length ( ws );
  if ( li_zeroes < 1 ) then
  begin
    result := ws;
  end
  else
  begin
    ws_tmp := '';
    for li_inx := 0 to li_zeroes-1 do ws_tmp := '0' + ws_tmp;
    result := ws_tmp + ws;
  end;
end; // function fillWithLeadingZeros ( const ws : string; len : integer ) : string;

// =============================================================================
//    Class        : --
//    Function     : fillWithTrailingSpaces()
//                   Fügt Leerzeichen am Ende eines Strings ein,
//                   bis die Länge len erreicht wurde.
//    Parameter    : ws  - zu erweiternder String
//                   len - gewünschte Länge
//    Return       : ws aufgefüllt mit Leerzeichen
//    Exceptions   : --
//    First author : 2013-08-20 /gsv/
//    History      : --
// =============================================================================
function fillWithTrailingSpaces ( const ws : string; len : integer ) : string;
var
  li_spaces : longint;
  li_inx    : longint;
  ws_tmp    : string;
begin
  li_spaces := len - Length ( ws );
  if ( li_spaces < 1 ) then
  begin
    result := ws;
  end
  else
  begin
    ws_tmp := '';
    for li_inx := 0 to li_spaces-1 do ws_tmp := ws_tmp + ' ';
    result := ws + ws_tmp;
  end;

end; // function fillWithTrailingSpaces ( const ws : string; len : integer ) : string;


// =============================================================================
//    Class        : --
//    Function     : fillWithTrailingSpaces()
//                   Fügt Leerzeichen am Anfang eines Strings ein
//                   bis die Länge len erreicht wurde.
//    Parameter    : ws  - zu erweiternder String
//                   len - gewünschte Länge
//    Return       : ws aufgefüllt mit Leerzeichen
//    Exceptions   : --
//    First author : 2013-08-20 /gsv/
//    History      : --
// =============================================================================
function fillWithLeadingSpaces ( const ws : string; len : integer ) : string;
var
  li_spaces : longint;
  li_inx    : longint;
  ws_tmp    : string;
begin
  li_spaces := len - Length ( ws );
  if ( li_spaces < 1 ) then
  begin
    result := ws;
  end
  else
  begin
    ws_tmp := '';
    for li_inx := 0 to li_spaces-1 do ws_tmp := ws_tmp + ' ';
    result := ws_tmp + ws;
  end;

end; // function fillWithLeadingSpaces ( const ws : string; len : integer ) : string;


// =============================================================================
//    Class        : --
//    Function     : timeToStrMs()
//                   Konvertierung einer Zeit in String, Format HH:MM:SS:LL
//                   HH = Stunden, MM = Minuten, SS = Sekunden, LL = Millisekunden
//    Parameter    : aTime - zu konvertierende Zeit
//    Return       : Zeit als String, s.o.
//    Exceptions   : --
//    First author : 2008-10-31 /gsv/
//    History      : --
// =============================================================================
function timeToStrMs ( const aTime : TDateTime ) : string;
begin
  result := fillWithLeadingZeros ( IntToStr(HourOf(aTime)),   2 ) + ':' +
            fillWithLeadingZeros ( IntToStr(MinuteOf(aTime)), 2 ) + ':' +
            fillWithLeadingZeros ( IntToStr(SecondOf(aTime)), 2 ) + ':' +
            fillWithLeadingZeros ( IntToStr(MillisecondOf(aTime)), 3 );
end; // function timeToStrMs ( const aTime : TDateTime ) : string;

{$IFNDEF VER150}
// =============================================================================
//    Class        : --
//    Function     : GetFileList()
//                   Erstellt eine Liste mit den existierenden Dateien im Pfad path,
//                   die dem Dateinamen filename entsprechen.
//                   ACHTUNG: Die Unterverzeichnisse in path werden nicht durchsucht!
//    Parameter    : path     - Suchpfad
//                   filename - Dateiname, kann auch Wildcards enthalten, z.B.:
//                              *.*, *.txt, etc.
//    Return       : Liste mit den gefundenen Dateien (komplette Pfadangabe)
//    Exceptions   : --
//    First author : 2009-03-20 /gsv/
//    History      : 2013-04-29 /gsv/: Jetzt werden auch die Unterverzeichnisse
//                                     berücksichtigt.
// =============================================================================
function GetFileList ( path : string; filename : string ) : TStringList;
var
  searchRec   : TSearchRec;
  tsl_subdirs : TStringList;
  i           : integer;
begin
  // Leere Liste erstellen
  result      := TStringList.Create();

  // 2013-04-29 /gsv/: Unterverzeichnisse
  tsl_subdirs := TStringList.Create();

  // Prüfen, ob path mit "\" endet bzw. "\" anhängen.
  path        := IncludeTrailingPathDelimiter ( path );

  // Hauptverzeichnis der Liste hinzufügen, damit auch die Dateien auf der
  // Hauptebene ermittelt werden.
  tsl_subdirs.Add ( path );

  // 2013-04-29 /gsv/: Unterverzeichnisse ermitteln
  GetSubDirList ( path, tsl_subdirs );

  for i := 0 to tsl_subdirs.Count-1 do
  begin

    path := IncludeTrailingPathDelimiter ( tsl_subdirs[i] );

    // Suche starten
    if ( FindFirst ( path + filename, faAnyFile, searchRec ) = 0 ) then
    begin
      // Mindestens eine Datei wurde gefunden!
      try
        repeat
          // Verzeichnisse an dieser Stelle ignorieren
          if ( (searchRec.Attr and faDirectory) = 0 ) then
          begin
            // Gefundene Datei in die Liste eintragen
            result.Add ( path + searchRec.Name );
          end;

          // und nächste Datei suchen
        until ( FindNext ( searchRec ) <> 0 );
      // Anschließend den durch FindFirst belegten Speicherplatz
      // wieder freigeben.
      finally SysUtils.FindClose ( searchRec );
      end;
    end; // if ( FindFirst ( path + filename, faAnyFile, searchRec ) = 0 ) then
  end;

end; // function GetFileList ( path : string; filename : string ) : TStringList;


// =============================================================================
//    Class        : --
//    Function     : GetSubDirList()
//                   Erstellt eine Liste den Unterverzeichnissen im Pfad path
//                   Die Funktion wird für jedes Unterverzeichnis rekursiv aufgerufen,
//                   also werden alle Unterverzeichnisse mit Unterverzeichnissen
//                   ermittelt.
//    Parameter    : path         - Hauptverzeichnis
//                   tsl_dir_list - Rückgabewert (Liste mit den Unterverzeichnissen
//                                  (vollständige Pfadangabe) )
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-04-29 /gsv/
//    History      : --
// =============================================================================
procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList );
var
  searchRec : TSearchRec;
begin
  // Prüfen, ob path mit "\" endet bzw. "\" anhängen.
  path   := IncludeTrailingPathDelimiter ( path );

  // Suche starten
  if ( FindFirst ( path + '*.*', faDirectory, searchRec ) = 0 ) then
  begin
    // Mindestens ein Unterverzeichnis gefunden!
    try
      repeat
        if ( ((searchRec.Attr and faDirectory) <> 0) and
             (searchRec.Name <> '.') and (searchRec.Name <> '..') ) then
        begin
          // Gefundenes Unterverzeichnis in die Liste eintragen
          tsl_dir_list.Add ( path + searchRec.Name );

          // Die Unterverzeichisse vom aktuellen Unterverzeichnis ermitteln
          GetSubDirList ( path + searchRec.Name, tsl_dir_list );
        end;
        // und nächstes Unterverzeichnis suchen
      until ( FindNext ( searchRec ) <> 0 );

    // Anschließend den durch FindFirst belegten Speicherplatz
    // wieder freigeben.
    finally SysUtils.FindClose ( searchRec );
    end;
  end; // if ( FindFirst ( path + filename, faAnyFile, searchRec ) = 0 ) then
end; // procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList );

// =============================================================================
//    Class        : --
//    Function     : GetSubDirList_internal()
//                   Erstellt eine Liste den Unterverzeichnissen im Pfad path
//                   Es werden alle Unterverzeichnisse bis zur angegebenen
//                   Tiefe i_subdir_depth ermittelt.
//                   Funktion nur für den internen Gebrauch innerhalb dieses Moduls!
//    Parameter    : path           - Hauptverzeichnis
//                   tsl_dir_list   - Rückgabewert (Liste mit den Unterverzeichnissen
//                                    (vollständige Pfadangabe) )
//                   i_subdir_depth - Ordnertiefe / Anzahl Unterordnerebenen, die ermittelt werden.
//                   i_curr_depth   - Aktuelle Ordnertiefe/Rekursionstiefe (interner Parameter)
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-05-24 /gsv/
//    History      : --
// =============================================================================
procedure GetSubDirList_internal ( path : string; var tsl_dir_list : TStringList;
                                   i_subdir_depth : integer; i_curr_depth : integer );
var
  searchRec : TSearchRec;
begin
  // Prüfen, ob path mit "\" endet bzw. "\" anhängen.
  path   := IncludeTrailingPathDelimiter ( path );
  inc ( i_curr_depth );

  // Suche starten
  if ( FindFirst ( path + '*.*', faDirectory, searchRec ) = 0 ) then
  begin
    // Mindestens ein Unterverzeichnis gefunden!
    try
      repeat
        if ( ((searchRec.Attr and faDirectory) <> 0) and
             (searchRec.Name <> '.') and (searchRec.Name <> '..') ) then
        begin
          // Gefundenes Unterverzeichnis in die Liste eintragen
          tsl_dir_list.Add ( path + searchRec.Name );

          if ( i_subdir_depth < 1 ) then
          begin
            // Die Unterverzeichisse vom aktuellen Unterverzeichnis ermitteln
            GetSubDirList_internal ( path + searchRec.Name, tsl_dir_list, i_subdir_depth, i_curr_depth );
          end
          else
          begin
            if ( i_subdir_depth > i_curr_depth ) then
            begin
              GetSubDirList_internal ( path + searchRec.Name, tsl_dir_list, i_subdir_depth, i_curr_depth );
            end;
          end;
        end;
        // und nächstes Unterverzeichnis suchen
      until ( FindNext ( searchRec ) <> 0 );

    // Anschließend den durch FindFirst belegten Speicherplatz
    // wieder freigeben.
    finally SysUtils.FindClose ( searchRec );
    end;
  end; // if ( FindFirst ( path + filename, faAnyFile, searchRec ) = 0 ) then
end; // procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList );

// =============================================================================
//    Class        : --
//    Function     : GetSubDirList()
//                   Erstellt eine Liste den Unterverzeichnissen im Pfad path
//                   Es werden alle Unterverzeichnisse bis zur angegebenen
//                   Tiefe i_subdir_depth ermittelt.
//    Parameter    : path           - Hauptverzeichnis
//                   tsl_dir_list   - Rückgabewert (Liste mit den Unterverzeichnissen
//                                    (vollständige Pfadangabe) )
//                   i_subdir_depth - Ordnertiefe / Anzahl Unterordnerebenen, die ermittelt werden.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-05-24 /gsv/
//    History      : --
// =============================================================================
procedure GetSubDirList ( path : string; var tsl_dir_list : TStringList;
                          i_subdir_depth : integer ); overload;
begin
  GetSubDirList_internal ( path, tsl_dir_list, i_subdir_depth, 0 );
end;                          

{$ENDIF}


// =============================================================================
//    Class        : --
//    Function     : getBitSize()
//                   Gibt die Bitlänge des Datentyps zurück
//    Parameter    : --
//    Return       : Länge des Datentyps in Bits
//    Exceptions   : EInvalidState, falls DataType unbekannt ist.
//    First author : 2009-03-24 /gsv/
//    History      : --
// =============================================================================
function getBitSize ( dataType : TDataType ) : byte;
begin
  case ( dataType ) of
    dtINT8,  dtUINT8  : result := C_BIT_SIZE_INT8;
    dtINT16, dtUINT16 : result := C_BIT_SIZE_INT16;
    dtINT32, dtUINT32 : result := C_BIT_SIZE_INT32;
    else raise EInvalidState.Create ( 'unknow data type ' + GetEnumName(TypeInfo(TDataType), integer(dataType) ) );
  end; // case ( self.DataType ) of
  
end; // function getBitSize ( dataType : TDataType  ) : byte;


// =============================================================================
//    Class        : --
//    Function     : getByteSize()
//                   Gibt die Bytelänge des Datentyps zurück
//    Parameter    : --
//    Return       : Länge des Datentyps in Bytes
//    Exceptions   : EInvalidState, falls DataType unbekannt ist.
//    First author : 2009-03-24 /gsv/
//    History      : --
// =============================================================================
function getByteSize ( dataType : TDataType ) : byte;
begin
  result := getBitSize ( dataType ) div C_BITS_PER_BYTE;
end; // function getByteSize ( dataType : TDataType ) : byte;


// =============================================================================
//    Class        : --
//    Function     : getDataTypeFromBitSize()
//                   Leitet den Datentyp von der Bitlänge ab.
//                   Der abgeleitete Datentyp ist immer unsigned,
//                   da keine Unterscheidung zwischen singned/unsigned möglich. 
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : EInvalidState, falls bitSize unbekannt ist.
//    First author : 2009-03-24 /gsv/
//    History      : --
// =============================================================================
function getDataTypeFromBitSize ( bitSize : byte  ) : TDataType;
begin
  case ( bitSize ) of
    C_BIT_SIZE_INT8  : result := dtUINT8;
    C_BIT_SIZE_INT16 : result := dtUINT16;
    C_BIT_SIZE_INT32 : result := dtUINT32;
    else raise EInvalidState.Create ( 'unknow bit size  ' + IntToStr(bitSize) );
  end; // case ( self.DataType ) of

end; // function getDataTypeFromBitSize ( bitSize : byte  ) : TDataType;


// =============================================================================
//    Class        : --
//    Function     : getMinValue()
//                   Ermittelt die untere Grenze des Datentyps dataType
//    Parameter    : dataType - Datentyp dessen untere Grenze ermittelt wird
//    Return       : s.o.
//    Exceptions   : EInvalidState, falls DataType unbekannt ist.
//    First author : 2009-05-12 /gsv/
//    History      : --
// =============================================================================
function getMinValue ( dataType : TDataType ) : int64;
begin
  case ( dataType ) of
    dtINT8   : result := int64(Low(shortint));
    dtUINT8  : result := int64(longword(Low(byte)));
    dtINT16  : result := int64(Low(smallint));
    dtUINT16 : result := int64(longword(Low(word)));
    dtINT32  : result := int64(Low(longint));
    dtUINT32 : result := int64(longword(Low(longword)));
    else raise EInvalidState.Create ( 'unknow data type ' + GetEnumName(TypeInfo(TDataType), integer(dataType) ) );
  end; // case ( self.DataType ) of
end; // function getMinValue ( dataType : TDataType ) : int64;

// =============================================================================
//    Class        : --
//    Function     : getMaxValue()
//                   Ermittelt die obere Grenze des Datentyps dataType
//    Parameter    : dataType - Datentyp dessen obere Grenze ermittelt wird
//    Return       : s.o.
//    Exceptions   : EInvalidState, falls DataType unbekannt ist.
//    First author : 2009-05-12 /gsv/
//    History      : --
// =============================================================================
function getMaxValue ( dataType : TDataType ) : int64;
begin
  case ( dataType ) of
    dtINT8   : result := int64(High(shortint));
    dtUINT8  : result := int64(longword(High(byte)));
    dtINT16  : result := int64(High(smallint));
    dtUINT16 : result := int64(longword(High(word)));
    dtINT32  : result := int64(High(longint));
    dtUINT32 : result := int64(longword(High(longword)));
    else raise EInvalidState.Create ( 'unknow data type ' + GetEnumName(TypeInfo(TDataType), integer(dataType) ) );
  end; // case ( self.DataType ) of
end; // function getMaxValue ( dataType : TDataType ) : int64;

// =============================================================================
//    Class        : --
//    Function     : swapWord
//                   Bytereihenfolge eines Words vertauschen: 0x1122 ==> 0x2211
//    Parameter    : w - Word, dessen Bytes vertauscht werden
//    Return       : w mit vertauschten Bytes (MSB...LSB <-> LSB...MSB)
//    Exceptions   : --
//    First author : 2009-03-25 /gsv/
//    History      : --
// =============================================================================
function swap ( w : word ) : word;
begin
  // 0x1122 ==> 0x2211
  result :=           word((w and $00FF) shl 8);
  result := result or word((w and $FF00) shr 8);
end; // function swap ( w : word ) : word;


// =============================================================================
//    Class        : --
//    Function     : swapWord
//                   Bytereihenfolge eines Longwords vertauschen:
//                   0x11223344 ==> 0x44332211
//    Parameter    : lw - Longword, dessen Bytes vertauscht werden
//    Return       : lw mit vertauschten Bytes (MSB...LSB <-> LSB...MSB)
//    Exceptions   : --
//    First author : 2009-03-25 /gsv/
//    History      : --
// =============================================================================
function swap ( lw : longword ) : longword;
begin
  // 0x11223344 ==> 0x44332211
  result :=           ((lw and $000000FF) shl 24);
  result := result or ((lw and $0000FF00) shl  8);
  result := result or ((lw and $00FF0000) shr  8);
  result := result or ((lw and $FF000000) shr 24);
end; // function swap ( lw : longword ) : longword;


// =============================================================================
//    Class        : --
//    Function     : convertDataType()
//                   Konvertiert den INT64-Wert entsprechend dem Datentyp dataType
//    Parameter    : dataIn   - zu konvertierender INT64
//                   dataType - Zieldatentyp
//    Return       : in dataType konvertierter Datenwert
//    Exceptions   : EInvalidState, falls dataType nicht unterstützt wird.
//    First author : 2009-03-30 /gsv/
//    History      : --
// =============================================================================
function convertDataType ( dataIn : int64; dataType : TDataType ) : int64;
begin
  case ( dataType ) of
    dtINT8   : result := int64 ( shortint(dataIn) );
    dtUINT8  : result := int64 ( byte(dataIn) );
    dtINT16  : result := int64 ( smallint(dataIn) );
    dtUINT16 : result := int64 ( word(dataIn) );
    dtINT32  : result := int64 ( longint(dataIn) );
    dtUINT32 : result := int64 ( longword(dataIn) );
    else       raise EInvalidState.Create ( 'Unsupported data type ' +
                                            GetEnumName(TypeInfo(TDataType), integer(dataType) ) );
  end; // case ( dataType ) of

end; // function convertDataType ( dataIn : int64; dataType : TDataType ) : int64;

// =============================================================================
//    Class        : --
//    Function     : convertDataTypeToString
//                   Konvertiert den Datentyp in die entsprechende Stringdarstellung  
//    Parameter    : dataType - zu konvertierender Datentyp
//    Return       : Stringdarstellung von dataType
//    Exceptions   : --
//    First author : 2009-05-05 /gsv/
//    History      : --
// =============================================================================
function convertDataTypeToString ( dataType : TDataType ) : string;
begin
  case ( dataType ) of
    dtINT8   : result := 'INT8';
    dtUINT8  : result := 'UINT8';
    dtINT16  : result := 'INT16';
    dtUINT16 : result := 'UINT16';
    dtINT32  : result := 'INT32';
    dtUINT32 : result := 'UINT32';
    else       raise EInvalidState.Create ( 'Unsupported data type ' +
                                            GetEnumName(TypeInfo(TDataType), integer(dataType) ) );
  end; // case ( dataType ) of
end; // function convertDataTypeToString ( dataType : TDataType ) : string;


// =============================================================================
//    Class        : --
//    Function     : getComponentText()
//                   Gibt den Text der GUI-Komponente comp zurück
//    Parameter    : s.o.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2009-05-07 /gsv/
//    History      : --
// =============================================================================
function getComponentText ( comp : TWinControl ) : string;
var
  Len: Integer;
begin
  Len := GetWindowTextLengthW ( comp.Handle );
  SetLength(Result, Len);
  if Len > 0 then GetWindowTextW ( comp.Handle, PWideChar(Result), Len+1 );

end; // function getComponentText ( comp : TWinControl ) : string;


// =============================================================================
//    Class        : --
//    Function     : getComponentText()
//                   Setzt den Text der GUI-Komponente
//    Parameter    : comp - GUI-Komponente
//                   ws   - neuer Text
//    Return       : --
//    Exceptions   : --
//    First author : 2009-05-07 /gsv/
//    History      : --
// =============================================================================
procedure setComponentText ( comp : TWinControl; ws : string );
begin
  SetWindowTextW ( comp.Handle, PWideChar(ws) );
end; // procedure setComponentText ( comp : TWinControl; ws : string );


// =============================================================================
//    Class        : --
//    Function     : MtxStrToAscii()
//                   Konvertiert einen String in die entsprechende
//                   ASCII-Darstellung um.
//                   Der String darf maximal 4 Zeichen enthalten,
//                   da als Rückgabewert UINT32 zurückgegeben wird.
//    Parameter    : str       - zu konvertierender String
//                   fill_char - Füllzeichen, falls str weniger als 4 Zeichen hat.
//                               Die Füllzeichen werden am Anfang des Strings
//                               eingetragen.
//    Return       : UINT32 - ASCII-Darstellung von str
//    Exceptions   : EInvalidArgument, falls der String leer ist oder mehr als
//                   4 Zeichen enthält (mit UINT32 nicht darstellbar!)
//    First author : 2010-01-21 /gsv/
//    History      : --
// =============================================================================
function MtxStrToAscii ( str : string; fill_char : char = '0' ) : longword;
const
  C_MAX_ASCII_CHAR_UINT32 = 4; // Max. Anzahl von ASCII-Zeichen, die mit UINT32 darstellbar sind
var
  li_length : longint;
  li        : longint;
  li_shift  : longint;
begin
  li_length := Length ( str );

  // Der String darf min. 1 und max. 4 Zeichen enthalten!
  // (Mit UINT32 sind max. 4 Zeichen darstellbar)
  if ( (li_length < 1) or (li_length > 4) ) then
    raise EInvalidArgument.Create ( 'The length of the string must be in range [1...4]!' );

  // String mit dem angegebenen Zeichen auffüllen 
  for li := li_length+1 to C_MAX_ASCII_CHAR_UINT32 do str := fill_char + str;

  li_shift := 24;
  result   := 0;
  for li := 1 to Length ( str ) do
  begin
    result   := result or ( longword(byte(str[li])) shl li_shift );
    li_shift := li_shift - 8;
  end;

end; // function MtxStrToAscii ( str : string ) : longword;

{$IFDEF DELPHI_XE_UP}
function MtxStrToAscii ( str : AnsiString; fill_char : char = '0' ) : longword;
begin
  result := MtxStrToAscii ( string(str), fill_char );
end;
{$ENDIF}


// Konvertierung: String in UINT32 (ASCII-Darstellung des Strings)
// Hinweis: Im Vergleich zu MtxStrToAscii wird der String rechts aufgefüllt und nicht links!
function convertStrToUint32 ( str : string; fill_char : char = '0' ) : longword;
const
  C_MAX_ASCII_CHAR_UINT32 = 4; // Max. Anzahl von ASCII-Zeichen, die mit UINT32 darstellbar sind
var
  li_length : longint;
  li        : longint;
  li_shift  : longint;
begin
  li_length := Length ( str );

  // Der String darf max. 4 Zeichen enthalten! (Mit UINT32 sind max. 4 Zeichen darstellbar)
  if ( li_length > 4 ) then
  begin
    raise EInvalidArgument.Create ( 'The length of the string must be in range [0...4]!' );
  end;

  // String mit dem angegebenen Zeichen auffüllen 
  for li := li_length+1 to C_MAX_ASCII_CHAR_UINT32 do str := str + fill_char;

  li_shift := 24;
  result   := 0;
  for li := 1 to Length ( str ) do
  begin
    result   := result or ( longword(byte(str[li])) shl li_shift );
    li_shift := li_shift - 8;
  end;
end;

// =============================================================================
//    Class        : --
//    Function     : convertUint32ToStr()
//                   Konvertiert ein Longword (UINT32) in die entsprechende
//                   Stringdarstellung.
//    Parameter    : lw     - zu konvertierender Wert
//    Return       : string - Stringdarstellung von lw
//    Exceptions   : --
//    First author : 2015-12-03 /gsv/
//    History      : --
// =============================================================================
function convertUint32ToStr ( lw : longword ) : string;
var
  c : char;
begin
  result := '';
  c      := char ( (lw and $FF000000) shr 24 );
  if ( c = #0 ) then exit;
  result := result + c;

  c      := char ( (lw and $00FF0000) shr 16 );
  if ( c = #0 ) then exit;
  result := result + c;

  c      := char ( (lw and $0000FF00) shr  8 );
  if ( c = #0 ) then exit;
  result := result + c;

  c      := char (  lw and $000000FF );
  if ( c = #0 ) then exit;
  result := result + c;
end;


// =============================================================================
//    Class        : --
//    Function     : generateCRCSimple()
//                   Generiert die Checksumme für den String sData
//                   (Checksumme = Aufsummierung der einzelnen ASCII-Zeichen/Codes)
//    Parameter    : sData - String/Text, desses Checksumme gebildet wird
//    Return       : die gebildete UINT32-Checksumme von sData
//    Exceptions   : --
//    First author : 2010-01-21 /gsv/
//    History      : --
// =============================================================================
function generateCRCSimple ( const sData : string ): longword;
var
  lw_crc : longword;
  i      : integer;
begin
  lw_crc := 0;
  for i := 1 to length ( sData ) do
  begin
    Inc ( lw_crc, byte(sData[i]) );
  end;

  result := lw_crc;
end; // function generateCRCSimple ( const sData : string ): longword;

{$IFDEF DELPHI_XE_UP}
function generateCRCSimple ( const sData : AnsiString ): longword;
begin
  result := generateCRCSimple ( string(sData) );
end;
{$ENDIF}


// =============================================================================
//    Class        : --
//    Function     : ExpandEnvStr()
//                   Prüft und ggf. ersetzt die Umgebungsvariable im angegebenen
//                   String durch deren Wert und gibt den Ergebnisstring zurück.
//                   Die Umgebungsvariablen werden im Format %Name% angegeben! 
//                   Implementierung aus dem Internet übernommen!
//    Parameter    : str - zu untersuchender String
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2010-01-21 /gsv/
//    History      : --
// =============================================================================
function ExpandEnvStr ( const str: string ): string;
const
  MAXSIZE = 32768; // laut PSDK sind 32k das Maximum
begin
  SetLength ( Result, MAXSIZE );
  SetLength ( Result, ExpandEnvironmentStringsW(pwidechar(str),
              @Result[1],length(Result))-1); //-1 um abschließendes #0 zu verwerfen
end; // function ExpandEnvStr(const str: string): string;


// =============================================================================
//    Class        : --
//    Function     : StringCompareICase()
//                   Vergleich zweier Strings ohne Unterscheidung
//                   zwischen Groß-/Kleinschreibung!
//    Parameter    : s1, s2 - zu vergleichende Strings
//    Return       : TRUE  - s1 und s2 sind gleich (ggf. Unterschiede in der
//                           Groß-/Kleinschreibung).
//                   FALSE - s1 und s2 sind unterschiedlich!
//    Exceptions   : --
//    First author : 2010-06-18 /gsv/
//    History      : --
// =============================================================================
function StringCompareICase ( s1, s2 : string ) : boolean;
begin
  result := StrIComp ( PChar(s1), PChar(s2) ) = 0;
end; // function StringCompareICase ( s1, s2 : string ) : boolean;

{$IFDEF DELPHI_XE_UP}
function StringCompareICase ( s1, s2 : AnsiString ) : boolean;
begin
  result := AnsiStrings.StrIComp ( PAnsiChar(s1), PAnsiChar(s2) ) = 0;
end;
{$ENDIF}


function CheckNameLegalDOS8Dot3; external 'Kernel32.dll' name 'CheckNameLegalDOS8Dot3A';

// Konvertierung integer --> Stringdarstellung IPv4 (z.B. $C0A80001 ==> '192.168.0.1')
function IntToStrIPv4 ( i64_ip : int64 ) : string;
begin
  result := IntToStr ( (i64_ip shr 24) and $00FF ) + '.' +
            IntToStr ( (i64_ip shr 16) and $00FF ) + '.' +
            IntToStr ( (i64_ip shr  8) and $00FF ) + '.' +
            IntToStr ( (i64_ip       ) and $00FF );
end; // function IntToStrIPv4 ( i64_ip : int64 ) : string;



// =============================================================================
//    Class        : --
//    Function     : convUint16ToByteArray()
//                   Konvertierung UINT16 --> Byte-Array (byte_array[0] := MSB)
//    Parameter    : w          - zu konvertierender UINT16-Wert
//    Return       : byte_array - Ergebnis, w als Byte-Array
//                                byte_array[0] = MSB
//                                byte_array[1] = LSB
//    Exceptions   : --
//    First author : 2013-08-16 /gsv/
//    History      : --
// =============================================================================
procedure convUint16ToByteArray ( w : word; var byte_array : array of byte );
var
  i : integer;
begin
  // Konvertierung longword --> array of bytes
  i               := Low(byte_array);
  byte_array[i+0] := byte ( (w and $FF00) shr  8 );
  byte_array[i+1] := byte ( (w and $00FF)        );
end; // procedure convUint16ToByteArray ( lw : longword; var data : array of byte );


// =============================================================================
//    Class        : --
//    Function     : convUint32ToByteArray()
//                   Konvertierung UINT32 --> Byte-Array (byte_array[0] := MSB)
//    Parameter    : lw         - zu konvertierender UINT32-Wert
//    Return       : byte_array - Ergebnis, lw als Byte-Array
//                                byte_array[0] = MSB
//                                ...
//                                byte_array[3] = LSB
//    Exceptions   : --
//    First author : 2013-08-16 /gsv/
//    History      : --
// =============================================================================
procedure convUint32ToByteArray ( lw : longword; var byte_array : array of byte );
var
  i : integer;
begin
  // Konvertierung longword --> array of bytes
  i               := Low(byte_array);
  byte_array[i+0] := byte ( (lw and $FF000000) shr 24 );
  byte_array[i+1] := byte ( (lw and $00FF0000) shr 16 );
  byte_array[i+2] := byte ( (lw and $0000FF00) shr  8 );
  byte_array[i+3] := byte ( (lw and $000000FF)        );
end; // procedure convUint32ToByteArray ( lw : longword; var data : array of byte );


// =============================================================================
//    Class        : --
//    Function     : generateCRC()
//                   Checksumme über ein UINT32 bilden
//                   Es wird eine einfache Prüfsumme durch Aufsummieren der
//                   einzelnen Bytes gebildet, also keine richtige CRC.
//    Parameter    : lw - UINT32, über das die Checksumme gebildet wird.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-07-16 /gsv/
//    History      : --
// =============================================================================
function generateCRC ( lw : longword ) : word;
begin
  result := word ( longword( (lw and $FF000000) shr 24 ) +
                   longword( (lw and $00FF0000) shr 16 ) +
                   longword( (lw and $0000FF00) shr  8 ) +
                   longword( (lw and $000000FF)        ) );
end; // function TLogFileEntry.generateCRC ( lw : longword ) : longword;



// =============================================================================
//    Class        : --
//    Function     : generateCRC16_CCITT()
//                   CRC16 (CRC-CCITT) über einen Datenbereich bilden
//
//                   Polynom = X^16+X^12+X^5+1 (10001000000100001)
//
//                   Die Checksummenberechnung erfolgt wie im SuperH-Manual
//                   vom MOV-Modul angegeben:
//                     uint16_t CRC_CCITT(uint8_t *pData, uint32_t iSize) 
//                     { 
//                       uint32_t  ui32_i; 
//                       uint8_t *pui8_Data; 
//                       uint16_t  ui16_CRC = 0xFFFFu; 
//                       pui8_Data = (uint8_t *)pData; 
//                       for ( ui32_i = 0; ui32_i < iSize; ui32_i++ ) 
//                       { 
//                         ui16_CRC  = (uint16_t)((ui16_CRC >> 8u) | ((uint16_t)((uint32_t)ui16_CRC << 8u)));    ui16_CRC ^= pui8_Data[ui32_i]; 
//                         ui16_CRC ^= (uint16_t)((ui16_CRC & 0xFFu) >> 4u); 
//                         ui16_CRC ^= (uint16_t) ((ui16_CRC << 8u) << 4u); 
//                         ui16_CRC ^= (uint16_t)(((ui16_CRC & 0xFFu) << 4u) << 1u); 
//                       } 
//                       
//                       ui16_CRC = (uint16_t)( 0x0000FFFFul & ((uint32_t)~(uint32_t)ui16_CRC) ); 
//                       return ui16_CRC; 
//                     }
//
//                     Am Ende wird die CRC einmal negiert. Falls ein CRC-Rechner verwendet wird,
//                     muss man die vom CRC-Rechner berechnete CRC negieren!
//    Parameter    : crc_old          - Teil-CRC, also CRC über den bisher berechneten Bereich
//                                      bzw. 0 beim ersten Aufruf.
//                                      Dadurch ist es möglich, die CRC über mehrere Teilbereiche zu bilden.
//                   data             - Datenbereich, über den die CRC gebildet wird.
//                   cnt              - Anzahl der Bytes im Datenfeld  
//                   b_last_data_part - Flag: Letzter Aufruf / Letztes Datenfeld
//                                      Dies ist für die CRC-Berechnung notwendig,
//                                      da nach dem letzten Datenfeld die CRC
//                                      einmalig negiert werden muss.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-08-16 /gsv/
//    History      : --
// =============================================================================
function generateCRC16_CCITT ( crc_old : word; const data : array of byte; cnt : integer; b_last_data_part : boolean ) : word;
var
  i      : integer;
  w_crc  : word;
begin
  if ( cnt > (High(data)-Low(data)) ) then
  begin
    EInvalidArgument.Create ( Format ( 'Too much data (%d) for byte array [%d..%d]!', [cnt, Low(data), High(data)] ) );
  end;  

  // Teil-CRC16 übernehmen
  w_crc := crc_old;

  // CRC16 berechnen
  for  i := Low(data) to (Low(data)+cnt-1) do
  begin
    w_crc := swap ( w_crc );
    w_crc := w_crc xor data[i];
    w_crc := w_crc xor word( (w_crc and $00FF) shr 4);
    w_crc := w_crc xor word( (w_crc shl 8) shl 4);         // << 12
    w_crc := w_crc xor word( ((w_crc and $00FF) shl 4) shl 1);   // << 5
  end;

  // Bei der letzten Berechnung, also beim letzten Datenfeld, muss die CRC16
  // negiert werden.
  if ( b_last_data_part ) then
  begin
    w_crc := word ( $0000FFFF and longword(not longword(w_crc)) );
  end;

  result := w_crc;

end; // function generateCRC16_CCITT ( crc : word; lw : longword ) : word;


// =============================================================================
//    Class        : --
//    Function     : generateCRC16_CCITT()
//                   CRC16 (CRC-CCITT) über ein UINT16 Datenbereich bilden
//                   Für weitere Informationen siehe die Haupt-Funktion
//                   generateCRC16_CCITT() (data : array of byte)
//
//    Parameter    : crc_old          - Teil-CRC, also CRC über den bisher berechneten Bereich
//                                      bzw. 0 beim ersten Aufruf.
//                                      Dadurch ist es möglich, die CRC über mehrere Teilbereiche zu bilden.
//                   data             - Datenbereich, über den die CRC gebildet wird.
//                   b_last_data_part - Flag: Letzter Aufruf / Letztes Datenfeld
//                                      Dies ist für die CRC-Berechnung notwendig,
//                                      da nach dem letzten Datenfeld die CRC
//                                      einmalig negiert werden muss.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-08-16 /gsv/
//    History      : --
// =============================================================================
function generateCRC16_CCITT ( crc_old : word; data : word; b_last_data_part : boolean ) : word;
var
  data_as_byte_array : array [0..1] of byte;
begin
  // Konvertierung longword --> array of bytes
  convUint16ToByteArray ( data, data_as_byte_array );

  // CRC bilden
  result := generateCRC16_CCITT ( crc_old, data_as_byte_array, 2, b_last_data_part );
end; // function generateCRC16_CCITT ( crc_old : word; data : word; b_last_data_part : boolean ) : word;


// =============================================================================
//    Class        : --
//    Function     : generateCRC16_CCITT()
//                   CRC16 (CRC-CCITT) über ein UINT32 Datenbereich bilden
//                   Für weitere Informationen siehe die Haupt-Funktion
//                   generateCRC16_CCITT() (data : array of byte)
//
//    Parameter    : crc_old          - Teil-CRC, also CRC über den bisher berechneten Bereich
//                                      bzw. 0 beim ersten Aufruf.
//                                      Dadurch ist es möglich, die CRC über mehrere Teilbereiche zu bilden.
//                   data             - Datenbereich, über den die CRC gebildet wird.
//                   b_last_data_part - Flag: Letzter Aufruf / Letztes Datenfeld
//                                      Dies ist für die CRC-Berechnung notwendig,
//                                      da nach dem letzten Datenfeld die CRC
//                                      einmalig negiert werden muss.
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-08-16 /gsv/
//    History      : --
// =============================================================================
function generateCRC16_CCITT ( crc_old : word; data : longword; b_last_data_part : boolean ) : word;
var
  data_as_byte_array : array [0..3] of byte;
begin
  // Konvertierung longword --> array of bytes
  convUint32ToByteArray ( data, data_as_byte_array );

  // CRC bilden
  result := generateCRC16_CCITT ( crc_old, data_as_byte_array, 4, b_last_data_part );
end; // function generateCRC16_CCITT ( crc_old : word; lw : longword; b_last_data_part : boolean ) : word;


// =============================================================================
//    Class        : --
//    Function     : generateCRC16()
//                   Berechnung einer CRC16 mit dem angegebenen Polynom
//    Parameter    : crc_old   - Initialwert der CRC bzw. alte Teil-CRC
//                   data      - Die Daten, über die die CRC berechnet wird.
//                   start_inx - Startindex für die CRC-Berechnung
//                               (ggf. Berechnung einer CRC über einen Teilbereich
//                                des Datenpuffers)
//                   cnt       - Datenlänge / Anzahl der Bytes, über die die CRC
//                               berechnet wird.
//                   polynom   - CRC-Polynom
//    Return       : berechnete CRC16
//    Exceptions   : --
//    First author : 2014-08-14 /gsv/
//    History      : --
// =============================================================================
function generateCRC16 ( crc_old : word; const data : array of byte;
                         start_inx : integer; cnt : integer; polynom : word ) : word;
var
  i_inx_byte : integer;
  i_inx_bit  : integer;
  crc        : longword;
begin
  // Anfangswert der CRC bzw. alte Teil-CRC
  crc := crc_old;

  for i_inx_byte := start_inx to ( start_inx + cnt - 1 ) do
  begin
    crc := crc xor ( longword(data[i_inx_byte]) shl 8 );

    for i_inx_bit := 0 to 7 do
    begin
      if ( (crc and $8000) <> 0 ) then crc := (crc shl 1) xor polynom
      else                             crc := (crc shl 1);
    end; // for i_inx_bit ...
  end; // for i_inx_byte ...

  result := crc and $0000FFFF;
end; // function generateCRC16 ( crc_old : word; const data : array of byte; cnt : integer; polynom : word ) : word;




// =============================================================================
//    Class        : --
//    Function     : generateCRC16()
//                   Berechnung einer CRC16 mit dem angegebenen Polynom
//    Parameter    : crc_old   - Initialwert der CRC bzw. alte Teil-CRC
//                   data      - Die Daten, über die die CRC berechnet wird.
//                   polynom   - CRC-Polynom
//    Return       : berechnete CRC16
//    Exceptions   : --
//    First author : 2014-08-14 /gsv/
//    History      : --
// =============================================================================
function generateCRC16 ( crc_old : word; data : AnsiString; polynom : word ) : word;
var
  i_inx_byte : integer;
  i_inx_bit  : integer;
  crc        : longword;
  i_length   : integer;
begin
  // Anfangswert der CRC bzw. alte Teil-CRC
  crc := crc_old;

  // Datenlänge ermitteln
  i_length := Length(data);
  if ( i_length = 0 ) then
  begin
    result := crc and $0000FFFF;
    exit;
  end;
  

  for i_inx_byte := 1 to i_length do
  begin
    crc := crc xor ( longword( byte(data[i_inx_byte]) ) shl 8 );

    for i_inx_bit := 0 to 7 do
    begin
      if ( (crc and $8000) <> 0 ) then crc := (crc shl 1) xor polynom
      else                             crc := (crc shl 1);
    end; // for i_inx_bit ...
  end; // for i_inx_byte ...

  result := crc and $0000FFFF;
end; // function generateCRC16 ( crc_old : word; data : string; polynom : word ) : word;


// =============================================================================
//    Class        : --
//    Function     : getUserDomain()
//                   Ermittelt den Domain-Name des eingeloggten Benutzers
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : --
//    First author : 2013-08-21 /gsv/
//    History      : --
// =============================================================================
function getUserDomainName() : string;
begin
  // Domain-Name über Umgebungsvariable ermitteln
  result := GetEnvironmentVariable ( 'USERDOMAIN' );
end; // function getUserDomainName() : string;


// =============================================================================
//    Class        : --
//    Function     : getUserDomain()
//                   Diese Funktion prüft anhand des Domain-Name,
//                   ob die Para-SW intern bei Metronix oder
//                   irgendwo beim Kunden läuft.
//    Parameter    : --
//    Return       : TRUE  - Ausführung intern bei Metronix
//                   FALSE - Ausführung extern beim Kunden
//    Exceptions   : --
//    First author : 2013-08-21 /gsv/
//    History      : --
// =============================================================================
function isRunningInMetronixEnvironment() : boolean;
begin
  result := StringCompareICase ( getUserDomainName(), C_DOMAIN_NAME_METRONIX );
end; // function isRunningInMetronixEnvironment() : boolean;


// =============================================================================
//    Class        : --
//    Function     : GetMDIClientAreaBoundsRect()
//                   Ermittelt den Client-Bereich eines MDI-Hauptformulars
//    Parameter    : MDIForm - MDI-Hauptfenster, dessen Client-Bereich ermittelt wird
//    Return       : Client-Bereich von MDIForm, also den nutzbaren Bereich für
//                   die MDI-Childs.
//    Exceptions   : --
//    First author : 2013-11-29 /gsv/
//    History      : --
// =============================================================================
function GetMDIClientAreaBoundsRect ( MDIForm : TForm ): TRect;
begin
  if ( MDIForm.FormStyle = fsMDIForm ) then
  begin
    if ( not Windows.GetClientRect ( MDIForm.ClientHandle, Result ) ) then
    begin
      RaiseLastOSError;
    end;
  end
  else
  begin
    raise EInvalidArgument.Create ( 'MDIForm is not an MDI form' );
  end;
end; // function GetMDIClientAreaBoundsRect ( MDIForm : TTntForm ): TRect;



// =============================================================================
//    Class        : --
//    Function     : getCurrentSystemFont()
//                   Ermittelt die aktuelle System-Schriftart
//    Parameter    : --
//    Return       : s.o.
//    Exceptions   : EInvalidOperation, falls die System-Schriftart nicht
//                   ermittelt werden konnte.
//    First author : 2014-09-02 /gsv/
//    History      : --
// =============================================================================
function getCurrentSystemFont() : TFont;
var
  font    : TFont;
  metrics : NONCLIENTMETRICS;
begin

  metrics.cbSize := SizeOf(NONCLIENTMETRICS);

  if ( SystemParametersInfo ( SPI_GETNONCLIENTMETRICS, SizeOf(NONCLIENTMETRICS),
                              @metrics, 0 ) ) then
  begin
    // Funktion erfolgreich
    font        := TFont.Create();
    font.Handle := CreateFontIndirect ( metrics.lfMessageFont );
    result      := font;
  end
  else
  begin
    // Fehler aufgetreten
    raise EInvalidOperation.Create ( 'Could not retrieve system font!' );
  end;
end; // function getCurrentSystemFont() : TFont;


// Fenster innerhalb des darunterliegenden / aktuell aktiven Fensters zentrieren
// Diese Funktion nach MtxUtils auslagern!!!
// =============================================================================
//    Class        : --
//    Function     : centerFormOnActiveForm()
//                   Diese Funktion zentriert das Fenster dlg innerhalb des
//                   darunterliegenden / aktuell aktiven Fensters.
//    Parameter    : dlg - zu zentrierendes Fenster
//    Return       : --
//    Exceptions   : --
//    First author : 2015-08-26 /gsv/
//    History      : --
// =============================================================================
procedure centerFormOnActiveForm ( dlg : TMtxForm );
var
  R : TRect;
begin
  // Fenster innerhalb des aufrufenden Fensters zentrieren
  GetWindowRect ( Screen.ActiveForm.Handle, R );
  dlg.Left := R.Left + ((R.Right  - R.Left) div 2) - (dlg.Width  div 2);
  dlg.Top  := R.Top  + ((R.Bottom - R.Top ) div 2) - (dlg.Height div 2);
end; // procedure centerFormOnActiveForm ( dlg : TMtxForm );


// =============================================================================
//    Class        : --
//    Function     : isInRange()
//                   Prüfen, ob der Wert im angegebenen Wertebereich liegt.
//    Parameter    : lw_val - zu testender Wert
//                   lw_min - min. zulässiger Wert für lw_val
//                   lw_max - max. zulässiger Wert für lw_val
//    Return       : true - lw_val im Wertebeich
//    Exceptions   : --
//    First author : 2015-10-28 /gsv/
//    History      : --
// =============================================================================
function isInRange ( lw_val, lw_min, lw_max : longword ) : boolean;
begin
  result := (lw_val >= lw_min) and (lw_val <= lw_max);
end; // function isInRange ( lw_val, lw_min, lw_max : longword ) : boolean;


// =============================================================================
//    Class        : --
//    Function     : isUnicodeSupported()
//                   Prüfen, ob Unicode unterstützt
//    Parameter    : --
//    Return       : true  - Unicode unterstützt
//    Exceptions   : --
//    First author : 2015-11-10 /gsv/
//    History      : --
// =============================================================================
function isUnicodeSupported() : boolean;
begin
  // Unicode-Unterstützung ab der NT-Plattform
  result := (Win32Platform = VER_PLATFORM_WIN32_NT);
end;


// =============================================================================
//    Class        : --
//    Function     : testValueMasked()
//                   Testen, ob das Bitfeld innerhalb der Maske gesetzt ist.
//                   (value & mask) == bitfield
//    Parameter    : i64_value    - Zu testender Wert
//                   i64_mask     - Maske
//                   i64_bitfield - Erwartetes Bitfeld
//    Return       : true  - Bitfeld innerhalb der Maske gesetzt
//    Exceptions   : --
//    First author : 2015-12-01 /gsv/
//    History      : --
// =============================================================================
function testValueMasked ( i64_value, i64_mask, i64_bitfield : int64  ) : boolean;
begin
  result := ( ( i64_value AND i64_mask ) = i64_bitfield );
end;

// =============================================================================
//    Class        : --
//    Function     : setValueMasked()
//                   Bitfeld innerhalb der Maske setzen
//                   result := (value & ~mask) | (bitfield & mask)
//    Parameter    : i64_value    - Zu testender Wert
//                   i64_mask     - Maske
//                   i64_bitfield - Zu setzendes Bitfeld
//    Return       : i64_value mit modifizierter Maske (Bitfeld aktiviert)
//    Exceptions   : --
//    First author : 2015-12-01 /gsv/
//    History      : --
// =============================================================================
function setValueMasked ( i64_value, i64_mask, i64_bitfield : int64 ) : int64;
begin
  result := (i64_value and (not i64_mask)) or (i64_mask and i64_bitfield);
end;


function MtxWideFormat ( const Format: string; const Args: array of const ) : string;
begin
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}

  result := SysUtils.Format ( Format, Args );
{$ELSE}
  result := Tnt_WideFormat  ( Format, Args );
{$ENDIF}
end;

function MtxGetDecimalSeparator() : char;
begin
{$IFDEF DELPHI_XE_UP}
  result := FormatSettings.DecimalSeparator;
{$ELSE}
  result := DecimalSeparator;
{$ENDIF}
end;


function  MtxWideTextPos ( const SubStr, S: string ) : Integer;
begin
{$IFDEF USE_STD_LIB_INSTEAD_OF_TNT_LIB}
  result :=         Pos ( SubStr, S );
{$ELSE}
  result := WideTextPos ( SubStr, S );
{$ENDIF}
end;



{$IFNDEF DELPHI_XE_UP}
function CharInSet ( C: AnsiChar; const CharSet: TSysCharSet ) : Boolean;
begin
  Result := C in CharSet;
end;

function CharInSet ( C: WideChar; const CharSet: TSysCharSet ) : Boolean;
begin
{$WARN WIDECHAR_REDUCED OFF}
  Result := C in CharSet;
{$WARN WIDECHAR_REDUCED ON}
end;
{$ENDIF}

{ IntToHex converts the given value to a hexadecimal string representation
  with the minimum number of digits specified. }
function IntToHexAnsi ( Value: Integer; Digits: Integer ) : AnsiString;
begin
  result := AnsiString ( SysUtils.IntToHex ( Value, Digits ) );
end;

function IntToHexAnsi ( Value: Longword; Digits: Integer ) : AnsiString;
begin
  result := AnsiString ( SysUtils.IntToHex ( Value, Digits ) );
end;

function IntToHexAnsi ( Value: Int64;   Digits: Integer ) : AnsiString;
begin
  result := AnsiString ( SysUtils.IntToHex ( Value, Digits ) );
end;

function IntToHexAnsi ( Value: UInt64;  Digits: Integer ) : AnsiString;
begin
  result := AnsiString ( SysUtils.IntToHex ( Value, Digits ) );
end;

function AnsiStrToInt ( const S: AnsiString ): Integer;
begin
  result := StrToInt ( string(S) );
end;

function IntToStrAnsi ( Value: Integer ): AnsiString;
begin
  result := AnsiString( SysUtils.IntToStr ( Value ) );
end;
function IntToStrAnsi ( Value: Int64   ): AnsiString;
begin
  result := AnsiString( SysUtils.IntToStr ( Value ) );
end;


// =============================================================================
//    Class        : --
//    Function     : checkWinControlVisible()
//                   Diese Funktion prüft, ob die GUI-Komponente AWinControl
//                   sichtbar ist. Dies ist der Fall, wenn die GUI-Komponente
//                   und alle ihre Parents sichtbar sind.
//                   Auf die Properties Visible und Showing ist leider kein Verlass,
//                   da diese auch bei einer unsichtbaren Komponente ggf. true
//                   zurückmelden.
//                   Auszug aus der Online-Hilfe:
//                    Showing - "Wenn eine der übergeordneten Komponenten, die das Steuerelement enthalten,
//                               den Wert false in der Eigenschaft Visible aufweist, kann Showing true oder false sein."
//
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : 2016-10-06 /gsv/
//    History      : --
// =============================================================================
function checkWinControlVisible ( AWinControl : TWinControl ) : boolean;
var
  AParent   : TWinControl;
begin
  result  := true;
  AParent := AWinControl;

  // Visible und Showing der Komponente selber und aller ihrer Parents prüfen
  while ( Assigned ( AParent ) ) do
  begin
    result := result and AParent.Showing and AParent.Visible;
    if ( not result ) then break;
    AParent := AParent.Parent;
  end;
end; // function checkWinControlVisible ( AWinControl : TWinControl ) : boolean;

end.
