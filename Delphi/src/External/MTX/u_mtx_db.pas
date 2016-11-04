// =============================================================================
// =============================================================================
//
//     Module name       : $RCSfile: u_mtx_db.pas,v $
//     Short description : Klasse für Variantenverwaltung für Prüfplätze
//                           + USB
//     Compiler          : Delphi 7
//     First author      : 2009-04-08 /aha/
//     Actual version    : $Revision 1.0$
//     Copyright         : (c) Metronix GmbH 2010
// -----------------------------------------------------------------------------
//     History           : /aha/ 2014-09-17: Funktionspointer
//                         /aha/ 2016-08-23: Änderung Konstruktor / Destruktor
//                                           t_mtx_db
// -----------------------------------------------------------------------------
//     Descriptions
// =============================================================================

{$M+}
unit u_mtx_db;

interface

uses
  StrUtils, SysUtils, DB, ADODB, Dialogs, Controls, OleCtrls, Classes,
  StdCtrls, Grids;

type

  // ===========================================================================
  // Exceptions
  // ===========================================================================
  T_Display_Procedure = procedure ( s: string );

  // ===========================================================================
  // Exceptions
  // ===========================================================================

  // Allgemeiner Datenbankfehler
  E_Database_General_Error = class(Exception);

  // Wenn Verbindung zu Datenbank nicht aufgebaut oder wenn eine
  // Operation auf nicht verbundener Datenbank versucht wird
  E_Database_Connection_Error = class(Exception);

  // Wenn SQL-Befehl nicht ausgeführt werden kann (z.B. ungültige SQL-Syntax)
  E_Database_SQL_Error = class(Exception);

  // Wenn Transaktion bereits aktiv ist oder aus anderen Gründen
  // keine Transaktion gestartet werden kann
  E_Database_Transaction_Error = class(Exception);

  // Wenn kein Rollback in der Transaktionssteuerung vorgenommen werden kann
  E_Database_Rollback_Error = class(Exception);

  // Wenn versucht wird, auf eine verbundene Datenbank sich ein zweites Mal
  // zu verbinden
  E_Database_Already_Connected = class(Exception);

  // Div. Excel-Exportfehler
  E_Database_Excelexport = class(Exception);

  // Div. StringGrid-Exportfehler
  E_Database_Stringgridexport = class(Exception);

  // Zugriff auf nicht durchgeführten Query
  EQueryNotPerformed = class(Exception);


  // ===========================================================================
  // Klasse: t_db_query
  //         Ergebnismenge eines Querys
  //         Das Ergebnis können mehrere Datensätze sein.
  //         Für jeden Datensatz und jeden angefragten Parameter muss ein
  //         Speicherplatz exisitieren.
  // ===========================================================================
  t_db_query = class
    public

      constructor create();                        // Konstruktor
      constructor create_cmd ( s_cmd : string;       // erweiterter Konstruktor
                               b_include : boolean );
      destructor  Destroy(); override;     // Destruktor
      procedure   clear();                 // Alle Daten löschen

      procedure   set_performed_flag();    // Merker: Query durchgeführt

      function    get_value ( inx : integer;
                              s_descriptor : string ) : string;
      function    get_count() : integer;

      function    add_dataset( tsl_data : TStringList ) : integer;
      procedure   delete_inx ( i_inx : integer );

      procedure   get_coulumn_data ( i_inx, i_col : integer; var s_column, s_data : string );
      procedure   export_to_excel      ();

      procedure   export_to_StringGrid ( sg_dst : TStringGrid;
                                         ts_export : TStringList );

    private
      b_query_performed              : boolean;             // Flag: Query performed
      i_max_datasets                 : integer;             // max. Anzahl Zeilen excel
      tl_intern                      : TList;
      s_query_int                    : string;              // abzusetzender Query-String
      tsl_params_int                 : TStringList;         // abzufragende Parameter
      b_intern_include_all           : boolean;             // alle Daten berücksichtigen
      v_excel                        : variant;
      function  get_number_columns() : integer;
      procedure set_include_all_data  ( b_include : boolean );
      function  get_max_len           ( i_col : integer ) : integer;
      procedure set_excel_columnwidth ( i_col_nu, i_width : integer );
      function  get_max_excel_datasets(): integer;

    published
      property s_query    : string      read s_query_int    write s_query_int;
      property tsl_params : TStringList read tsl_params_int write tsl_params_int;
      property number_coloumns : integer read get_number_columns;
      property b_include_all : boolean   read b_intern_include_all write set_include_all_data;
      property max_excel_datasets : integer read get_max_excel_datasets;
  end;


  // ===========================================================================
  // Klasse: t_mtx_db
  // MySQL-Datenbank
  // ===========================================================================
  t_mtx_db = class
    public
      constructor create_plain      ();
      constructor create            ( s_config : string );
      destructor  Destroy           (); override;
      procedure   Login             ( s_user, s_password : string );
      procedure   Logout            ();

      procedure set_conn_string     ( s_conn       : string );
      procedure perform_query       ( my_db_query  : t_db_query );
      procedure perform_command     ( s_command    : string );

      procedure transaction_begin    ();
      procedure transaction_commit   ();
      procedure transaction_rollback ();

      procedure set_debug           ( mem_deb   : TMemo ); overload;
      procedure set_debug           ( f_display : T_Display_Procedure ); overload;
      procedure debug               ( s_deb     : string );

    private
      s_connection_intern       : string;       // Speicher für Connection String
      ADOConnection1            : TADOConnection;
      ADOCommand1               : TADOCommand;
      ADOQuery1                 : TADOQuery;


      bShowDB                   : boolean;     // Flag: Detailanzeige
      b_transaction_active      : boolean;     // Flag: Transaktion aktiv?

      f_debug_display           : T_Display_Procedure;
      mem_deb_internal          : TMemo;
  end;

// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
implementation
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

uses
  Forms, wideStrings, MtxTypes, ComObj;

const
  C_DEFAULT_MAX_EXCEL_DATASETS = 16384;   // müsste eigentlich jedes Excel können....

  CS_QUERY_NOT_PERFORMED =
    'Unerlaubter Zugriff auf Query, der noch nicht durchgeführt wurde';

// =============================================================================
//    Class        : t_db_query
//    Function     : create
//                   Konstruktor: Neues Objekt mit Default-Werten erzeugen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : --
// =============================================================================
constructor t_db_query.Create();
begin
  i_max_datasets  := C_DEFAULT_MAX_EXCEL_DATASETS;  // max. Anzahl Zeilen excel
  tsl_params      := TStringList.Create();
  tl_intern       := TList.Create();
  clear();
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : Create_cmd
//                   Konstruktor: Kommando erzeugen und alle Daten berücksichtigen
//    Parameter    : s_cmd       : Query-Kommando
//                   b_include   : Flag : alle Daten automatisch berücksichtigen?
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-10-05
//    History      : --
// =============================================================================
constructor t_db_query.Create_cmd ( s_cmd : string; b_include : boolean );
begin
  self.Create();
  self.clear();
  self.s_query       := s_cmd;
  self.b_include_all := b_include;
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : destroy
//                   Destruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : /aha/ 2011-03-03 : tsl_paramams_int Destruktor ergänzt
// =============================================================================
destructor  t_db_query.Destroy();              // Destruktorvar
var
  i           : integer;
  tmp_strlist : TStringList;

begin
  if ( assigned (tl_intern )) then
  begin
    // alle Stringlisten freigeben
    for i := 0 to tl_intern.Count-1 do
    begin
      // einzelne Stringliste freigeben
      tmp_strlist := TSTringList ( tl_intern.Items[i] );
      tmp_strlist.Free();
    end;
    tl_intern.Free();
  end;

  // /aha/ Destruktor für tsl_params_int
  if ( assigned ( tsl_params_int )) then
  begin
    tsl_params_int.Free();
  end;

  inherited;   // Interne Freigabe
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : clear
//                   alle Listen und sonstigen Daten löschen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : --
// =============================================================================
procedure   t_db_query.clear();
var
  i           : integer;
  tmp_strlist : TStringList;

begin

  b_query_performed := FALSE;    // Merker: noch kein Query durchgeführt

  // Spaltenparameter-Stringliste löschen
  tsl_params.Clear();

  // alle Stringlisten freigeben
  for i := 0 to tl_intern.Count-1 do
  begin
    // einzelne Stringliste freigeben
    tmp_strlist := TSTringList ( tl_intern.Items[i] );
    tmp_strlist.Free();
  end;

  tl_intern.Clear();
  self.s_query          := '';
  b_intern_include_all  := FALSE;
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : set_performed_flag
//                   Merker setzen, dass Query durchgeführt wurde.
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-06-02
//    History      : --
// =============================================================================
procedure t_db_query.set_performed_flag();
begin
  b_query_performed := TRUE;    // Merker: Query durchgeführt
end;


// =============================================================================
//    Class        : t_db_query
//    Function     : add_dataset
//                   Einen neuen Datansatz des Queries in die interne Struktur
//                   aufnehmen
//                   Dabei werden nur die Werte der Spalten aufgenommen,
//                   die in tsl_params aufgeführt sind
//    Parameter    : tsl_data [0]  : Spaltenname 1. Spalte
//                   tsl_data [1]  : Spaltenwert 1. Spalte
//                   tsl_data [2]  : Spaltenname 2. Spalte
//                   tsl_data [3]  : Spaltenwert 2. Spalte
//                   ....
//    Return       : Index des zugefügten Datensatzes
//    Exceptions   : EInvalidArgument : tsl_data enthält keine gerade Anzahl
//                                      von Argumenten
//                                      tsl_data enthält nicht die gewünschten
//                                      Spaltenwerte
//    First author : /aha/ 2010-11-11
//    History      : --
// =============================================================================
function t_db_query.add_dataset ( tsl_data : TStringList ) : integer;
var
  i, i2       : integer;
  new_strlist : TStringlist;
  s_row_name  : string;              // Spaltenname
  b_found     : boolean;             // Flag: gefunden?

begin

  // Prüfung auf gerade Anzahl Argumente
  if (( tsl_data.Count mod 2 ) > 0 ) then
  begin
    // keine gerade Anzahl Argumente ==> Exception
    Raise ( EInvalidArgument.Create( format (
           't_db_query.add_dataset: Argumentenstringliste hat %d (ungerade!!) Argumente!',
           [tsl_data.Count])));
  end;

  // temporäre Stringliste erzeugen (für späteres Einfügen in tl_intern)
  new_strlist := TStringlist.Create();      // Neue Stringliste
  new_strlist.Clear;                        // erstmal leer

  // /aha/ 2011-05-30: wenn b_intern_include_all
  // und tsl_params_int_count noch nicht gefüllt,
  // dann die interne Parameterliste auf alle Parameter setzen!
  // also: tsl_data [0], tsl_data [2], tsl_data [4]...
  if (( tl_intern.Count = 0 ) and self.b_intern_include_all ) then
  begin
    for i := 0 to ((tsl_data.Count div 2)-1) do
    begin
      tsl_params_int.Add( tsl_data.Strings[2*i]);  // Spaltennamen (gerade Indizees) übernehmen!
    end;
  end;

  // Alle Argumente aus der Liste tsl_params heraussuchen und in die
  // neue Liste eintragen...
  for i := 0 to ( self.tsl_params_int.Count  -1 )do
  begin
    // Spaltennamen einer gesuchten Spalte
    s_row_name := self.tsl_params_int.Strings [i];

    // tsl_data auf diesen Spaltennamen durchsuchen
    i2      := 0;
    repeat
      b_found := tsl_data.Strings [i2] = s_row_name;
      if ( b_found ) then
      begin
        // den dazugehörigen Wert in die temporäre Liste eintragen
        new_strlist.Add ( tsl_data.Strings [i2+1]);
      end
      else
      begin
        // nicht gefunden: i2 um 2 inkrementieren (nächste Spaltenüberschrift)
        i2 := i2+2;
      end;
    until (b_found or ( i2 > tsl_data.Count-1 ));  // bis gefunden oder Listenende

    if ( not b_found ) then
    begin
      // darf nicht sein: die gesuchte Spalte ist nicht im Datensatz vorhanden
      Raise ( EInvalidArgument.Create( format (
             't_db_query.add_dataset: Im Argument/Datensatz fehlt die Spalte "%s"!',
             [s_row_name])));
    end;
  end;  // << Alle Argumente heraussuchen und eintragen

  // Schließlich den Zeiger auf die neue Liste in tl_intern eintragen
  tl_intern.Add( pointer ( new_strlist) );  // Eintrag in Hauptliste
  result := tl_intern.Count-1;

  //ShowMessage ( 'Add_index: result = ' + IntToStr ( result ));

end;

// =============================================================================
//    Class        : t_db_query
//    Function     : delete_inx
//                   Datensatz am Index "i_inx" löschen
//    Parameter    : i_inx         : Nr. des zu löschenden Datensatzes
//    Return       : --
//    Exceptions   : EIndexOutOfBounds   : falscher Index-Wert
//    First author : /aha/ 2012.06-19
//    History      : --
// =============================================================================
procedure t_db_query.delete_inx ( i_inx : integer );
var
  tmp_strlist : TStringList;

begin
  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.delete_inx: ' + CS_QUERY_NOT_PERFORMED );
  end;

  // Index auf Gültigkeit überprüfen
  if (( i_inx < 0 ) or ( i_inx > tl_intern.Count-1 )) then
  begin
    raise EIndexOutOfBounds.Create( format (
          't_db_query.delete_inx: ungültiger Index %d', [i_inx]));
  end
  else
  begin
    tmp_strlist := TStringList ( tl_intern.Items[i_inx] );   // gewünschte Stringliste
    tmp_strlist.Free();            // Die verwiesene Liste löschen
    tl_intern.Delete ( i_inx );    // Eintrag in Liste löschen
  end;
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : get_value
//                   Query-Ergebnis lesen:
//                   Aus Ergebnisrecord "inx"   ( 0 = 1. Record)
//                   den Parameter "s_descriptor"
//    Parameter    : inx           : Nr. Ergebnisrecord
//                   s_descriptor  : Spaltenname
//    Return       : Parameterwert, '' bei Fehlern
//    Exceptions   : EIndexOutOfBounds   : falscher Index-Wert
//                   EObjectNotExist     : Spaltenname existiert nicht
//    First author : /aha/ 2010-11-11
//    History      : /aha/ 2016-06-02: Exception bei Zugriff auf
//                                     nicht durchgeführten Query
// =============================================================================
function t_db_query.get_value ( inx : integer;
                        s_descriptor : string ) : string;
const
  C_NOT_IN_LIST = -1;

var
  tmp_strlist : TStringList;
  inx_target  : integer;

begin

  result := '';

  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.get_value: ' + CS_QUERY_NOT_PERFORMED );
  end;

  // Index auf Gültigkeit überprüfen
  if (( inx < 0 ) or ( inx > tl_intern.Count-1 )) then
  begin
    raise EIndexOutOfBounds.Create( format (
          't_db_query.get_value: ungültiger Index %d', [inx]));
  end
  else tmp_strlist := TStringList ( tl_intern.Items[inx] );   // gewünschte Stringliste

  // Suchparameter auf Gültigkeit überprüfen
  inx_target := self.tsl_params.IndexOf ( s_descriptor );
  if ( inx_target = C_NOT_IN_LIST )  then
  begin
    raise EObjectNotExist.Create( format (
          't_db_query.get_value: ungültiger Suchparameter %s', [s_descriptor]));
  end;

  // inx_target enthält an dieser Stelle den Zeiger auf den gesuchten Parameter
  result := tmp_strlist.Strings[inx_target];

end;

// =============================================================================
//    Class        : t_db_query
//    Function     : get_count
//                   Anzahl der Ergebnisrecords ausgeben
//    Parameter    : --
//    Return       : Anzahl Ergebnisrecords
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : /aha/ 2016-06-02: Exception bei Zugriff auf
//                                     nicht durchgeführten Query
// =============================================================================
function  t_db_query.get_count() : integer;
begin

  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.get_count: ' + CS_QUERY_NOT_PERFORMED );
  end;

  result := tl_intern.Count;
end;


// =============================================================================
//    Class        : t_db_query
//    Function     : get_number_columns
//                   Anzahl der Spalten ausgeben
//    Parameter    : --
//    Return       : Anzahl Spalten
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : --
// =============================================================================
function t_db_query.get_number_columns() : integer;
begin
  result := tsl_params_int.Count;
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : get_coulumn_data
//                   Für die Zeile i_inx den Wert der Spalte i_col auslesen
//    Parameter    : i_inx    : Zeile
//                   i_col    : Spaltennummer
//                   s_column : Name der Spalte
//                   s_data   : Wert der Zelle
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : /aha/ 2016-06-02: Exception bei Zugriff auf
//                                     nicht durchgeführten Query
// =============================================================================
procedure t_db_query.get_coulumn_data ( i_inx, i_col : integer; var s_column, s_data : string );
begin
  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.get_value: ' + CS_QUERY_NOT_PERFORMED );
  end;

  s_column    := tsl_params_int.Strings[i_col];
  s_data      := get_value ( i_inx, s_column );
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : set_include_all_data
//                   Alle Daten des Query aufnehmen
//    Parameter    : TRUE     : Alle Daten aufnehmen
//                   FALSE    : nur die in den Spalten bezeichneten Daten aufnehmen
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-11-11
//    History      : --
// =============================================================================
procedure t_db_query.set_include_all_data ( b_include : boolean );
begin
  self.b_intern_include_all := b_include;
  tsl_params_int.Clear;
end;


// =============================================================================
//    Class        : t_db_query
//    Function     : set_excel_columnwidth
//                   Spaltenbreite setzen
//    Parameter    : i_col_nu   : Spaltennummer    0 = Spalte A
//                   i_width    : Weite
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-07-27
//    History      : /aha/ 2011-10-11: Zu große Spaltenbreiten werden nicht
//                                     umgesetzt
//                   /aha/ 2013-07-24: Spalten AA .... ZZ unterstützen
// =============================================================================
procedure t_db_query.set_excel_columnwidth ( i_col_nu, i_width : integer );
const
  C_MAX_COL_WIDTH_EXCEL = 100;
var
  s_col : string;
  c     : char;
  i_z   : integer;

begin

  i_z := 1 + integer ( 'Z' ) - integer ('A');

  if  ( i_width > C_MAX_COL_WIDTH_EXCEL ) then
  begin
    exit;
  end;

  s_col := '';

  if ( i_z <= i_col_nu ) then
  begin
    // Hier beginnt Excel mit den Spaltennamen 'AA' etc.
    s_col    := char ( integer ('A') + ( i_col_nu div i_z ) - 1 );
    i_col_nu := i_col_nu mod i_z;
  end;

  c     := char ( i_col_nu + integer ('A'));
  s_col := s_col + c + '1';

  try
    v_excel.Range[ s_col, s_col].Columns.ColumnWidth:= i_width;
  Except
    ShowMessage ( format (
      'Hinweis: Die Spalte "%s" kann nicht auf die Weite %d gesetzt werden!',
      [s_col, i_width]));
  end;
end;

// =============================================================================
//    Class        : t_db_query
//    Function     : get_max_len
//                   maximale Stringlänge der Zeile i_col
//    Parameter    : i_col: Spaltennummer     0 = Spalte A
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-07-27
//    History      : /aha/ 2011-08-04 : Indexkorrektur 1->0
// =============================================================================
function t_db_query.get_max_len ( i_col : integer ) : integer;
var
  s_tmp, s_val : string;
  i_row        : integer;

begin

  // Initialwert (Länge der Spaltenüberschriften)
  get_coulumn_data ( 0, i_col, s_tmp, s_val );  // /aha/ 2011-08-04
  result := length ( s_tmp );

  for i_row := 0 to get_count()-1 do
  begin
    get_coulumn_data ( i_row, i_col, s_tmp, s_val );

    // max. Zeichenlänge ggf. nach oben korrigieren
    if ( length ( s_val ) > result ) then
    begin
      result := length ( s_val );
    end;
  end;

end;


// =============================================================================
//    Class        : t_db_query
//    Function     : export_to_excel
//                   Exportiert die Ergebnismenge in Excel hinein
//    Parameter    : --
//    Return       : --
//    Exceptions   : diverse, bei Problemen;
//                   Der Aufruf sollte mit "try" abgefangen werden
//    First author : /aha/ 2011-07-27
//    History      : /aha/ 2011-10-11 : Export von mehr als 5000 Datensätzen
//                                      wird nicht unterstützt!
//                   /aha/ 2016-06-02: Exception bei Zugriff auf
//                                     nicht durchgeführten Query
// =============================================================================
procedure t_db_query.export_to_excel ();
const
  C_FRIEMELFAKTOR_Z   = 3;     // Friemelfaktor Zähler
  C_FRIEMELFAKTOR_N   = 2;     // Friemelfaktor Nenner

var
  i_row, i_col  : integer;    // Zeilen und Spalten
  s_tmp, s_val  : string;     // Universal für Strings
  i_max_len     : integer;

begin

  // ====================================
  // Plausibilitätsprüfungen
  // ====================================

  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.export_to_excel: ' + CS_QUERY_NOT_PERFORMED );
  end;

  // Anzahl Ergebnisdatensätze
  if ( get_count() = 0 ) then
  begin
    Raise ( E_Database_Excelexport.Create ('Ergebnismenge ist leer! => Kein Excelexport'));
  end;

  // Anzahl Spalten
  if ( get_number_columns() = 0 ) then
  begin
    Raise ( E_Database_Excelexport.Create ('Das Ergebnis hat keine Spalten! => Kein Excelexport'));
  end;

  // Lässt sich Excel starten?
  try
    v_excel := CreateOleObject ('Excel.Application');
    v_excel.workbooks.add;
    v_excel.visible := true;
    v_excel.Range['A1','A1'].Columns.ColumnWidth:= 200;

    i_max_datasets := v_excel.Rows.count;  // max. Anzazhl Datensätze von Excel

  except
    Raise ( E_Database_Excelexport.Create ('Excel kann nicht gestartet werden!'));
  end;

  if ( get_count() > get_max_excel_datasets() ) then
  begin
    Raise ( E_Database_Excelexport.Create ( format (
          'Ergebnismenge hat %d Datensätze! Maximal erlaubt: %d Datensätze!', [get_count(),
                              get_max_excel_datasets()] )));
  end;




  // ====================================
  // Füllen der Exceltabelle
  // ====================================

  // Zeilenbreiten auf die max. Länge anpassen
  for i_col := 0 to get_number_columns()-1 do
  begin
    i_max_len := get_max_len ( i_col );
    set_excel_columnwidth ( i_col, ( i_max_len * C_FRIEMELFAKTOR_Z ) div C_FRIEMELFAKTOR_N  );
  end;

  // Daten einfüllen
  for i_col := 0 to get_number_columns()-1 do
  begin
    // Spaltenüberschriften
    s_tmp   := tsl_params_int.Strings[i_col];
    v_excel.cells  [ 1, i_col+1 ] := s_tmp;

    // Zeilenüberschriften
    for i_row := 0 to get_count()-1 do
    begin
      get_coulumn_data ( i_row, i_col, s_tmp, s_val );
      v_excel.cells  [ i_row+2, i_col+1 ] := s_val;
    end;
  end;


end;


// =============================================================================
//    Class        : t_db_query
//    Function     : export_to_StringGrid
//                   Exportiert die Ergebnismenge in ein StringGrid
//                   Es werden die Daten exportiert, die in der StringListe
//                   ts_export angegeben sind
//    Parameter    : sg_dst    : Stringgrid
//                   ts_export : Liste der zu exportierenden Daten
//    Return       : --
//    Exceptions   : diverse, bei Problemen;
//                   Der Aufruf sollte mit "try" abgefangen werden
//    First author : /aha/ 2014-08-25
//    History      : /aha/ 2014-08-25 : Fehlende Ergebnisdatensätze lösen keinen Fehler mehr aus!
//                   /aha/ 2016-06-02 : Exception bei Zugriff auf
//                                      nicht durchgeführten Query
// =============================================================================
procedure t_db_query.export_to_StringGrid  ( sg_dst : TStringGrid;
                                             ts_export : TStringList );

const
  C_FRIEMELFAKTOR_Z   = 3;     // Friemelfaktor Zähler
  C_FRIEMELFAKTOR_N   = 2;     // Friemelfaktor Nenner

var
  i_row, i_col  : integer;    // Zeilen und Spalten
  s_tmp, s_val  : string;     // Universal für Strings
  i             : integer;
  b_is_empty    : boolean;    // Flag: leere Ergebnismenge

begin

  // ====================================
  // Plausibilitätsprüfungen
  // ====================================
  // /aha/ 2016-06-02: Exception bei nicht durchgeführten Query
  if ( not b_query_performed )  then
  begin
    raise EQueryNotPerformed.Create(
          't_db_query.export_to_StringGrid: ' + CS_QUERY_NOT_PERFORMED );
  end;


  // ====================================
  // Plausibilitätsprüfungen
  // ====================================
  b_is_empty   := ( get_count() = 0 ) or
                  ( get_number_columns() = 0 ) or
                  ( ts_export.Count = 0 );

  // altes StringGrid löschen
  for i := 0 to sg_dst.RowCount do sg_dst.Rows[i].Clear();

  if ( b_is_empty ) then exit;     // leeres StringGrid nicht weiter behandeln


  // Größe des Stringgrid anpassen
  sg_dst.ColCount := ts_export.Count;
  sg_dst.RowCount := get_count()+1;

  // Folgende Exception dürfte eigentlich nicht auftauchen!
  if ( sg_dst.ColCount <> ts_export.Count ) then
  begin
    Raise ( E_Database_Stringgridexport.Create ('Spaltenproblem! => Kein Stringgrid'));
  end;

  // Folgende Exception dürfte eigentlich nicht auftauchen!
  if ( sg_dst.RowCount <> get_count()+1) then
  begin
    Raise ( E_Database_Stringgridexport.Create ('Zeilenproblem (zuviel?)! => Kein Stringgrid'));
  end;


  // ====================================
  // Füllen des StringGrids
  // ====================================
  for i_col := 0 to ts_export.Count-1 do
  begin
    // Spaltenüberschriften in Zeile 0
    s_tmp                       := ts_export.Strings[i_col];
    sg_dst.cells  [ i_col, 0 ]  := s_tmp;

    // Zeileninhalte
    for i_row := 0 to get_count()-1 do
    begin
      s_tmp                            := ts_export.Strings[i_col];
      s_val                            := get_value(i_row, s_tmp );
      sg_dst.cells  [ i_col, i_row+1 ] := s_val;
    end;
  end;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : get_max_excel_datasets
//                   Info: Anzahl der von Excel verkraftbaren Datensätze
//    Parameter    : --
//    Return       : Anzahl der von Excel verkraftbaren Datensätze
//    Exceptions   : --
//    First author : /aha/ 2012-06-18
//    History      : /aha/ 2012-07-03: umgestellt auf interne Variable
// =============================================================================
function t_db_query.get_max_excel_datasets(): integer;
begin
  result := i_max_datasets;
end;


// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================


// =============================================================================
//    Class        : t_mtx_db
//    Function     : create_plain
//                   Konstruktor ohne connection String.
//                   Diese müssen nachträglich über set_conn_string() gesetzt werden
//                   - oder gleich den Konstruktor create nutzen!!
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-11-30
//    History      :
// =============================================================================
constructor t_mtx_db.create_plain ();
begin
  create ( '' );
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : create
//                   Konstruktor: Neues Objekt mit Default-Werten erzeugen
//    Parameter    : s_config   : Connection String (ohne User und Login)
//    Return       : --
//    Exceptions   : --
//    First author : /she/
//    History      : /aha/ 2010-10-21
//                   /aha/ Änderung: ADOConnection ADOCommand und ADOQuery
//                         werden jetzt über NIL erzeugt
//                         ==> Destruktor wird auch geändert
// =============================================================================
constructor t_mtx_db.create ( s_config : string );
begin

  self.s_connection_intern           := s_config;

  ADOConnection1                     := TADOConnection.Create ( nil );
  ADOCommand1                        := TADOCommand.Create    ( nil );
  ADOQuery1                          := TADOQuery.Create      ( nil );

  AdoConnection1.CommandTimeout      := 30;
  AdoConnection1.Connected           := FALSE;
  AdoConnection1.ConnectionTimeout   := 15;
  AdoConnection1.ConnectOptions      := coConnectUnspecified;
  AdoConnection1.CursorLocation      := clUseServer;
  AdoConnection1.IsolationLevel      := ilCursorStability;
  AdoConnection1.KeepConnection      := TRUE;
  AdoConnection1.LoginPrompt         := FALSE;
  AdoConnection1.Mode                := cmUnknown;
  AdoConnection1.Provider            := 'MSDASQL.1';
  AdoConnection1.Tag                 := 0;

  ADOQuery1.Active                   := FALSE;
  ADOQuery1.AutoCalcFields           := TRUE;
  ADOQuery1.CacheSize                := 1;
  ADOQuery1.CommandTimeout           := 10;
  ADOQuery1.ConnectionString         := '';
  ADOQuery1.CursorLocation           := clUseClient;
  ADOQuery1.CursorType               := ctKeyset;
  ADOQuery1.enableBCD                := TRUE;
  ADOQuery1.ExecuteOptions           := [];
  ADOQuery1.Filter                   := '';
  ADOQuery1.Filtered                 := FALSE;
  ADOQuery1.LockType                 := ltOptimistic;
  ADOQuery1.MarshalOptions           := moMarshalAll;
  ADOQuery1.MaxRecords               := 0;
  ADOQuery1.ParamCheck               := TRUE;
  ADOQuery1.Prepared                 := FALSE;
  ADOQuery1.tag                      := 0;
  ADOQuery1.Connection               := AdoConnection1;
  ADOQuery1.ObjectView               := FALSE;

  ADOCommand1.Connection             := AdoConnection1;

  bShowDB                            := FALSE;
  b_transaction_active               := FALSE;    // keine Transaktion aktiv

  // Initialisierungen für Debug
  mem_deb_internal                   := nil;
  f_debug_display                    := nil;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : destroy
//                   Destruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-12-08
//    History      : /aha/ 2011-11-30: Destruktoren deaktiviert
//                   /aha/ 2016-08-23: Destruktoren wieder aktiviert weil
//                                     Konstruktor geändert
// =============================================================================
destructor t_mtx_db.Destroy();
begin
  ADOConnection1.Free();
  ADOCommand1.Free();
  ADOQuery1.Free();
  inherited;
end;


// =============================================================================
//    Class        : t_mtx_db
//    Function     : set_conn_string
//                   Connection String setzen (muss benutzt werden, wenn als
//                   Konstruktor create_plain benutzt wird!
//    Parameter    : s_conn  : Connection String (ohne User und Login)
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-11-30
//    History      : --
// =============================================================================
procedure t_mtx_db.set_conn_string   ( s_conn : string );
begin
  self.s_connection_intern           := s_conn;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : Login
//                   Versuch, die Datenbank mit User+Passwort zu öffnen
//    Parameter    : s_user      : Username
//                   s_password  : Passwort
//    Return       : --
//    Exceptions   : E_Database_connection_error
//    First author : /she/
//    History      : /aha/ 2010-10-21 allgemeine Umbaumaßnahmen
//                   /aha/ 2011-01-18 function in procedure umgewandelt
//                   /aha/ 2011-03-02 already connected erzeugt jetzt eine Exception!
// =============================================================================
procedure t_mtx_db.Login ( s_user, s_password : string );
begin

  if ADOConnection1.Connected then
  begin
    // bereits verbunden
    raise E_Database_already_connected.Create('t_mtx_db.Login(): already connected!');
  end
  else
  begin
    // noch nicht verbunden ==> Verbindung aufbauen
    if bShowDB then ShowMessage( 'Anmeldung DB !' );

    try
      ADOConnection1.ConnectionTimeout := 5;  // [s]
      ADOConnection1.Connected := FALSE;	    // ADOConnection1.Close()
      ADOConnection1.ConnectionString := self.s_connection_intern
                         + ';Password=' + s_password
                         + ';User ID='  + s_user;
      ADOConnection1.Connected := TRUE;
    except
      raise E_Database_connection_error.Create(
            't_mtx_db.Login: keine Verbindung zur Datenbank!' );
    end;
  end;

end;


// =============================================================================
//    Class        : t_mtx_db
//    Function     : Logout : Verbindung zur DB kappen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /she/
//    History      : /aha/ 2010-10-21 allgemeine Umbaumaßnahmen
// =============================================================================
procedure t_mtx_db.Logout();
begin
  ADOConnection1.Connected := FALSE;
end;


// =============================================================================
//    Class        : t_mtx_db
//    Function     : perform_query
//                   Abfrage (Query) durchführen
//    Parameter    : my_query   : Spezifikation der Abfrage
//                   my_query.s_query    : Query-String, z.B. "SELECT * FROM ADRESS"
//                   my_query.tsl_params : Abzufragende Parameter als Stringlist
//    Return       : --
//    Exceptions   : E_Database_connection_error
//    First author : /she/
//    History      : /aha/ 2010-10-21 : allgemeine Umbaumaßnahmen
//                   /aha/ 2011-01-25 : Exception E_Database_SQL_error
//                   /aha/ 2011-03-03 : Bugfix my_tsl Konstruktor verschoben
//                                      (memory leak problem)
//                   /aha/ 2014-09-17 : Debug Query-String
// =============================================================================
procedure t_mtx_db.perform_query ( my_db_query  : t_db_query );
var
  s_tmp    : string;
  i        : integer;
  my_tsl   : TStringList;

begin

  debug ( my_db_query.s_query );            // Debug Query-String

  my_tsl := nil;

  // ---------------------------------------
  // Abfangen: nicht verbundene Datenbank
  // ---------------------------------------
  if ( not ADOConnection1.Connected ) then
  begin
    raise E_Database_connection_error.Create('t_mtx_db.perform_query: Database not connected');
  end;
  // ---------------------------------------

  // ---------------------------------------
  // Durchführung der Suchanfrage
  // ---------------------------------------
  try
    ADOQuery1.SQL.Clear();
    ADOQuery1.SQL.Add( my_db_query.s_query );
    ADOQuery1.open();
    my_db_query.set_performed_flag();
  except on E:Exception do
  begin
    raise E_Database_SQL_error.Create(
      format ( 't_mtx_db.perform_query: (%s) Fehler bei SQL-Kommando: %s',
               [my_db_query.s_query, E.Message]));
  end;
  end;

  // ---------------------------------------
  // Alle Ergebnisrecords eintragen!
  // ---------------------------------------
  while ( not ADOQuery1.EOF ) do
  begin
    if ( not assigned ( my_tsl )) then
    begin
      my_tsl := TStringList.Create();
    end;

    // -------------------------------------------------
    // einen einzelnen Ergebnisrecord eintragen...
    // -------------------------------------------------

    // Stringliste erzeugen
    my_tsl.Clear;

    for i := 0 to adoquery1.FieldCount-1 do
    begin
      s_tmp := ADOQuery1.Fields[i].FieldName;   // Spaltenname
      my_tsl.Add ( s_tmp );                     // Spaltenname in Stringliste

      s_tmp := ADOQuery1.FieldByname( s_tmp ).AsString;   // Spaltenwert
      my_tsl.Add ( s_tmp );                               // Spaltenwert in Stringliste
    end;

    my_db_query.add_dataset ( my_tsl );         // Ergebnis an Query übergeben
    ADOQuery1.Next();                           // nächster Datensatz
  end;  // << while ( not ADOQuery1.EOF ) do

  ADOQuery1.Close();

  if ( assigned ( my_tsl )) then
  begin
    my_tsl.free();
  end;

end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : perform_command    : Kommando ausführen
//    Parameter    : s_command  : SQL-Kommando
//    Return       : --
//    Exceptions   : --
//    First author : /she/
//    History      : /aha/ 2010-10-21 : kopiert und modifiziert
//                   /aha/ 2011-01-25 : Exception E_Database_SQL_error
//                   /aha/ 2014-09-17 : Debug Command-String
// =============================================================================
procedure t_mtx_db.perform_command ( s_command: string );
begin

  debug ( s_command );       // Debug-Info

  // ---------------------------------------
  // Abfangen: nicht verbundene Datenbank
  // ---------------------------------------
  if ( not ADOConnection1.Connected ) then
  begin
    raise E_Database_connection_error.Create('t_mtx_db.perform_command: Database not connected');
  end;
  // ---------------------------------------

  ADOCommand1.CommandText := s_command;
  try
    ADOCommand1.Execute();        // Befehl ausfuehren
  except ON E:Exception do
  begin
    raise E_Database_SQL_error.Create(
      format ( 't_mtx_db.perform_query: (%s) Fehler bei SQL-Kommando: %s',
              [s_command, E.Message ]));
  end;
  end;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : transaction_begin
//                   Start einer Transaktion.
//                   Im Fehlerfall wird eine Exception ausgelöst
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-10-28 (Steve besucht...)
//    History      : --
// =============================================================================
procedure t_mtx_db.transaction_begin ();
begin

  debug ('transaction_begin');

  // ---------------------------------------
  // Abfangen: nicht verbundene Datenbank
  // ---------------------------------------
  if ( not ADOConnection1.Connected ) then
  begin
    raise E_Database_connection_error.Create('t_mtx_db.transaction_begin: Database not connected');
  end;
  // ---------------------------------------


  // ---------------------------------------
  // Abfangen: bereits aktive Transaktion
  // ---------------------------------------
  if ( self.b_transaction_active ) then
  begin
    raise E_Database_Transaction_Error.Create('t_mtx_db.transaction_begin: Transaction already open!');
  end;

  try
    ADOConnection1.BeginTrans();
  except
    // ---------------------------------------
    // Abfangen: Transaktion kann nicht geöffnet werden
    // ---------------------------------------
    raise E_Database_Transaction_Error.Create('t_mtx_db.transaction_begin: Cannot open transaction!');
  end;

  // Wenn wir hier vorbeikommen, ist die Transaktion geöffnet worden!
  b_transaction_active := TRUE;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : transaction_commit
//                   Fertigstellen einer Transaktion.
//                   Im Fehlerfall wird eine Exception ausgelöst
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-10-28 (Steve besucht...)
//    History      : --
// =============================================================================
procedure t_mtx_db.transaction_commit ();
begin

  debug ('transaction_commit');

  // ---------------------------------------
  // Abfangen: nicht verbundene Datenbank
  // ---------------------------------------
  if ( not ADOConnection1.Connected ) then
  begin
    raise E_Database_connection_error.Create('t_mtx_db.transaction_commit: Database not connected');
  end;
  // ---------------------------------------

  // ---------------------------------------
  // Abfangen: keine offene Transaktion
  // ---------------------------------------
  if ( not self.b_transaction_active ) then
  begin
    raise E_Database_Transaction_Error.Create('t_mtx_db.transaction_commit: no active transaction!');
  end;

  try
    ADOConnection1.CommitTrans(); // Transaktion festschreiben
  except
    // ---------------------------------------
    // Abfangen: Transaktion kann nicht commited werden
    // ---------------------------------------
    raise E_Database_Transaction_Error.Create('t_mtx_db.transaction_commit: Cannot commit transaction!');
  end;

  // Wenn wir hier vorbeikommen, ist die Transaktion fertig!
  b_transaction_active := FALSE;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : transaction_rollback
//                   Reset einer Transaktion.
//                   Im Fehlerfall wird eine Exception ausgelöst
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2010-10-28 (Steve besucht...)
//    History      : --
// =============================================================================
procedure t_mtx_db.transaction_rollback ();
begin
  debug ('transaction_rollback');

  // ---------------------------------------
  // Abfangen: nicht verbundene Datenbank
  // ---------------------------------------
  if ( not ADOConnection1.Connected ) then
  begin
    raise E_Database_connection_error.Create('t_mtx_db.transaction_rollback: database not connected');
  end;
  // ---------------------------------------


  // ---------------------------------------
  // Abfangen: keine offene Transaktion
  // ---------------------------------------
  if ( not self.b_transaction_active ) then
  begin
    raise E_Database_Transaction_Error.Create('t_mtx_db.transaction_rollback: no active transaction!');
  end;

  try
    ADOConnection1.RollbackTrans(); // Transaktion Reset
  except
    // ---------------------------------------
    // Abfangen: Kein Rollback möglich
    // ---------------------------------------
    raise E_Database_Rollback_Error.Create('t_mtx_db.transaction_rollback: cannot rollback transaction!');
  end;


  // Wenn wir hier vorbeikommen, ist die Transaktion resettet!
  b_transaction_active := FALSE;
end;


// =============================================================================
//    Class        : t_mtx_db
//    Function     : set_debug: Debugfenster definieren
//    Parameter    : mem_deb : Debugfenster
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2013-03-13
//    History      :
// =============================================================================
procedure t_mtx_db.set_debug ( mem_deb : TMemo );
begin
  mem_deb_internal := mem_deb;
  mem_deb_internal.Clear();
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : set_debug: Debugfunktion festlegen
//                   Diese sorgt "irgendwie" für die Darstellung der Debug-Info
//    Parameter    : f_display : Funktion zur Darstellung der Debuginfo
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-09-17
//    History      :
// =============================================================================
procedure t_mtx_db.set_debug ( f_display : T_Display_Procedure );
begin
  f_debug_display := f_display;
end;

// =============================================================================
//    Class        : t_mtx_db
//    Function     : debug: Debugfenster definieren
//    Parameter    : mem_deb : Debugfenster
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2013-03-13
//    History      :
// =============================================================================
procedure t_mtx_db.debug ( s_deb : string );
begin

  // Darstellung über explizites Memo-Element
  if ( mem_deb_internal <> nil ) then
  begin
    mem_deb_internal.Lines.Add ( s_deb );
  end;

  // Darstellung über Funktion
  if ( assigned ( f_debug_display ) )  then
  begin
    f_debug_display ( s_deb );
  end;

end;

// #############################################################################
// #############################################################################
// #############################################################################
// #############################################################################
// #############################################################################

end.

