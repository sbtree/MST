//==============================================================================
// Projekt     : Prüfplatz
// Dateiname   : Datenbank.pas
// Inhalt      : Diese Unit enthält alle Funktionen und Prozeduren, die notwendig sind,
//               um mit der hinter der PSW liegenden Delphi-Datenbank zu kommunizieren.
//               Auch die datensätze für die PDE werden hier in die Datei geschrieben.
//
// Copyright : (c) Metronix / Cooper Tools 2003
//
// letzte Aenderung : Juni/2006 : Anbindung an SQL-Datenbank per ADO
//                    /aha/ 2010-07-21: Code ersetzt / auskommentiert, um es
//                          unter DATENBANK_ADO lauffähig zu bekommen!
//                    /aha/ Neuer Anlauf: Aktivierung unter PDE
//
//     2012-02-22 /aha/ Lock-Mechanismus gegen Überschreiben von DB-Fehlern in NIO-Loops,
//                      wenn vorher schon Fehler vorhanden waren: implementiert
//     2012-03-20 /aha/ Erweiterung b_blocked (temporäre Blockierung)
//     2013-01-25 /aha/ neue Klasse t_db_comment (für ADD_DB_COMMENT)
//     2015-01-07 /aha/ Erweiterungen für Mantis-ID 778
//     2015-08-06 /aha/ b_blocked: raus (Mantis 1148)
//     2015-12-03 /aha/ 2015-12-03 : Implementierung Mantis 1079
//     2016-07-18 /aha/ Umbau auf streng abgeteiltes Interface
//==============================================================================

{$M+}

unit Datenbank;


interface

uses
  u_mtx_db, StdCtrls, Classes, MtxUtils, MtxTypes;


type

  // ======================================================================
  // Exceptions
  // ======================================================================
  E_DBCONN_InvalidArg = class ( MtxException )
  end; // Ungültige Argumente

  E_SQL_GENERAL  = class ( MtxException )
  end; // allgemeiner SQL-Fehler

  E_DB_GENERAL  = class ( MtxException )
  end; // allgemeiner Datenbankfehler

  E_DATA_INCONSISTENT  = class ( MtxException )
  end; // Dateninkonsistenz

  E_PROG_STRUCTURE = class ( MtxException )
  end; // Programmstrukturfehler

  // ======================================================================
  // Klassen
  // ======================================================================
  t_db_comment    = class;  // Definition später benutzt
  t_db_vers_info  = class;  // Definition später benutzt
  t_db_mess_info  = class;  // Definition später benutzt
  t_db_connection = class;  // Definition später benutzt
  t_db_infoblock  = class;  // Definition später benutzt

  // ---------------------------------------------------------------------------
  // Record: Messergebnis
  // ---------------------------------------------------------------------------
  t_mess_entry = record
    s_ps_nr           : string;       // Prüfschrittnummer
    s_ps_value        : string;       // Wert der Messung
  end;

  // ---------------------------------------------------------------------------
  // Record: Fehlerinformation
  // ---------------------------------------------------------------------------
  t_error_info = record
    f_pruefschritt    : double;       // PS-Nummer bei Fehler
    s_function        : string;       // Funktionsbezeichnung
    s_value_actual    : string;       // aktuell gemessener Wert (String)
    s_value_ref       : string;       // Sollwert (String)
  end;

  // ---------------------------------------------------------------------------
  // Record: User-Versionsinformationen
  // ---------------------------------------------------------------------------
  t_versinfo_user = record
    s_vers_exe        : string;                  // Version PP_ATT_UNI.exe
    s_vers_ini        : string;                  // Version (Razor) der INI-Datei
    s_vers_psl        : string;                  // Version (Razor) der Prüfschrittliste
    s_vers_bl1        : string;                  // Version des Bootloaders     1
    s_vers_fw1        : string;                  // Version der FW              1
    s_vers_hw1        : string;                  // Version der HW              1
    s_vers_bl2        : string;                  // Version des Bootloaders     2
    s_vers_fw2        : string;                  // Version der FW              2
    s_vers_hw2        : string;                  // Version der HW              2
  end;

  // =====================================================================
  // Aufzaehlungstyp: Testart (Funktionstest, Dauertest)
  // es sind momentan folgende Eingabezeilentypen erlaubt:
  // =====================================================================
  t_en_db_testmode = (
    en_dbt_functiontest,       // Funktionstest
    en_dbt_endurancetest       // Dauertest
  );

  // =====================================================================
  // Funktionspointer
  // =====================================================================
  tfptr_debug      = procedure ( s_debug : string );

  // =====================================================================
  // sonstige Zeigertypen
  // =====================================================================
  p_mess_entry = ^t_mess_entry;


  // ===========================================================================
  // Klasse: t_db_infoblock: Ergebnisse Prüflauf
  // ===========================================================================
  t_db_infoblock = class
      // Konstruktor und Destruktor
      constructor Create ( );
      destructor  Destroy; override;
      public
        fptr_debug        : tfptr_debug;             // Debug-Prozedurpointer
        my_error_info     : t_error_info;            // Fehlerinformationen
        my_versinfo       : t_versinfo_user;         // sämtliche Versionsinformationen
        s_starttime       : string;                  // Testbeginn YYYY-MM-DD HH:MM:SS
        s_endtime         : string;                  // Testende   YYYY-MM-DD HH:MM:SS
        s_boardnumber     : string;                  // Platinennummer   (Funktionstest)
        s_devicenumber    : string;                  // Endgerätenummer  (Funktionstest und Dauertest)
        s_errcode         : string;                  // Fehlercode       (Dauertest)
        s_comment         : string;                  // Benutzerkommentar zur Prüfung
        b_result          : boolean;                 // Prüfung bestanden?

        procedure   clear                       ( );
        procedure   add_mess_value              ( s_step, s_value  : string );
        procedure   add_comment                 ( i_comment        : integer         );
        function    check_consistency_variant   ( my_db            : t_db_connection ) : boolean;
        function    check_consistency_fa        ( my_db            : t_db_connection ) : boolean;
        procedure   write_test_result           ( my_db            : t_db_connection );

      private
        en_testmode_internal      : t_en_db_testmode;  // Prüfmodus

        i_prodnr_functiontest     : integer;           // Automat.Produktart_Pr.Produktart_Nr
        i_prodnr_endurancetest    : integer;           // Konfig.Geraete_Typ.Typ_Nr

        s_pruefplatz_nr           : string;            // Prüfplatz-Nr als String

        s_sap_main                : string;            // SAP-ID des Hauptproduktes
        s_id_pruefplatz           : string;            // Bezeichnung des Prüfplatzes
        s_fa_nr                   : string;            // Fertigungsauftragsnummer
        s_ff_internal             : string;            // Fremdfertiger-Kürzel

        b_fa_nr_valid             : boolean;           // Flag: gültige FA-Nummer?
        my_pde_comment            : t_db_comment;      // Kommentare für Prüfdaten
        my_vers_info_internal     : t_db_vers_info;    // Versionsinformationen für Prüfdaten
        my_mess_info              : t_db_mess_info;    // Messinformationen für Prüfdaten

        procedure   debug2                 ( s_debug      : string );                 // Debugausgabe
        function    check_fa_number        ( s_fa_number  : string;
                                             i_min, i_max    : integer ) : boolean;   // FA-Nummer prüfen
        function    check_devicenumber     ( var s_device    : string  ) : boolean;   // Endgerätenummer prüfen
        function    exists_dataset_fa_sap  ( db_handle       : t_mtx_db;
                                             s_sap1, s_fa_nr : string;
                                             var s_other_sap : string )  : boolean;

        function    exists_combination     ( db_handle       : t_mtx_db;
                                             lw_prodid       : longword;
                                             s_fa_nr         : string ) : boolean;

        function    check_type_lowlevel    ( db_handle       : t_mtx_db;
                                             lw_prodid       : longword;
                                             s_fa_nr,
                                             s_sap_nr        : string )    : boolean;
        function    check_main_product     ( db_handle       : t_mtx_db;
                                             s_sap : string )              : boolean;
        procedure   gen_lowlevel_error     ( db_handle       : t_mtx_db;
                                             lw_product      : longword;
                                             s_sap_id        : string );
        procedure   gen_lowlevel_error_fa  ( db_handle       : t_mtx_db;
                                             lw_product      : longword;
                                             s_sap_id,
                                             s_fa            : string );
        function    gen_sap_ids            ( s_sap_src : string;
                                             var s_sap_dst1, s_sap_dst2 : string ) : boolean;

        function    check_via_product_ids  ( db_handle       : t_mtx_db;
                                             tsl_prodtypes : TStringList) : boolean;

        procedure   perform_version_info   ( my_db : t_db_connection;
                                             vers_info : t_db_vers_info );
        procedure   write_result_function_test
                                           ( my_db                     : t_db_connection;
                                             var lw_id                 : longword;
                                             b_additional              : boolean );
        procedure   write_result_endurance_test
                                           ( my_db : t_db_connection;
                                             var lw_id                 : longword );
        function    gen_db_add_command_function_test
                                           ( lw_number        : longword;
                                             s_user           : string;
                                             b_auto_remark    : boolean  ) : string;
        function    gen_db_add_command_endurance_test
                                           ( s_user           : string   ) : string;
        function    gen_remark         ( b_additional : boolean          ) : string;
        function    entferne_zeichen   ( weg: char; s: string            ) : string;
        function    max_str_len        ( s_in : string; lw_max : integer ) : string;

        function    recent_data_exists     ( my_db       : t_db_connection;
                                            var lw_id,
                                            lw_anzahl : longword ) :  boolean;
        function    gen_db_update_command ( lw_id, lw_number : longword;
                                            b_additional     : boolean  ) : string;
        function    get_time              ( d_before         : double   ) : string;
        procedure   perform_mess_info     ( my_db       : t_db_connection;
                                            mess_info   : t_db_mess_info;
                                            lw_vers_inx : longword );
        procedure   get_actual_record     ( my_db       : t_db_connection;
                                            var lw_inx : longword );
        function    ps_as_string() : string;

        procedure   set_sap_main         ( s : string );
        procedure   set_id_pruefplatz    ( s : string );
        procedure   set_fa_nr            ( s : string );
        procedure   set_en_testmode      ( e : t_en_db_testmode );
        procedure   set_mess_info        ( b : boolean );
        function    get_mess_info        ( ) : boolean;

        procedure   reset_pruefplatz_produktart    ();
        procedure   reset_fa_nr_valid              ();
        function    check_consistency_dt_variant  ( my_db : t_db_connection ) : boolean;
        function    check_consistency_dt_fa       ( my_db : t_db_connection ) : boolean;

      published
        property en_testmode : t_en_db_testmode read en_testmode_internal write set_en_testmode;
        property sap_main          : string  read s_sap_main              write set_sap_main;
        property id_pruefplatz     : string  read s_id_pruefplatz         write set_id_pruefplatz;
        property fa_nr             : string  read s_fa_nr                 write set_fa_nr;
        property mess_info         : boolean read get_mess_info           write set_mess_info;

  end;

  // ---------------------------------------------------------------------------
  // Klasse DB-Connection: Anbindung an Datenbank
  // ---------------------------------------------------------------------------
  t_db_connection = class
    public
      fptr_debug                : tfptr_debug;       // Debug-Prozedurpointer

      // Konstruktor und Destruktor
      constructor Create ();
      destructor  Destroy; override;

      // Weitere öffentliche Funktionen
      function    login               ( s_user, s_password : string ) : boolean;
      procedure   logout              ( );
      function    check_boardnumber   ( s_board   : string ) : boolean;
      function    perform_excel_query ( s_query   : string ) : boolean;
      procedure   do_sql_cmd          ( s_cmd     : string );

      // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      // Für Testinterface Modultest
      // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      procedure   temp_perform_query ( s_query : string;
                                       var i_num_data : integer );

      function    temp_get_field     ( inx : integer;
                                       s_field : string ) : string;

      // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

    private
      s_version_internal        : string;      // Versionsstring DB-Interface
      s_user_name_internal      : string;      // angemeldeter Benutzer
      my_pde_db                 : t_mtx_db;    // Die Datenbank
      b_connect_internal        : boolean;     // internes Flag: Anbindung aktiv...?
      s_conn_internal           : string;      // Connect-String

      i_min_fa_len,                            // minimale Länge FA
      i_max_fa_len              : integer;     // maximale Länge FA
      temp_query                : t_db_query;  // allgemeiner Query für Modultest

      function    get_db_handle()       : t_mtx_db;
      procedure   do_query   ( my_query : t_db_query; s_sql   : string );
      function    get_connect_internal() : boolean;    // Ist Datenbank verbunden?
      function    get_user_level  ( )     : integer;
      function    check_ff_id     ( s_id  : string ) : boolean;

      // für Debugfunktion
      procedure   debug1 ( s_debug : string );       // Debugausgabe


    published
      property ConnectString : string  read  s_conn_internal        write s_conn_internal;
      property connected     : boolean read  get_connect_internal;
      property version       : string  read  s_version_internal;
      property user_name     : string  read  s_user_name_internal;
      property fa_min_len    : integer read  i_min_fa_len           write i_min_fa_len;
      property fa_max_len    : integer read  i_max_fa_len           write i_max_fa_len;
      property user_level    : integer read  get_user_level;
  end;

  // ---------------------------------------------------------------------------
  // Kommentare für Automat.Pruef in PDE
  // ---------------------------------------------------------------------------
  t_db_comment = class
    public
      constructor Create ( );
      destructor  Destroy; override;

      procedure   add_comment    ( i_comment      : integer );
      procedure   clear();
      function    get_add_commentstring (
                      lw_inx_record,
                      lw_inx_comment : longword;
                      var s_command  : string ) : boolean;

    private
      tsl_comments       : TStringList;
      //procedure show_error       ( s_text : string          );
      function    comment_exists ( i_comment : integer ) : boolean;

  end;

  // ---------------------------------------------------------------------------
  // Aufzaehlungstyp : Vesionsinformation
  // Beschreibung    : welche Versionsinformationen können hier gesetzt werden?
  // ---------------------------------------------------------------------------
  t_vers_enum = ( vinfo_exe,          // Info EXE-Datei
                  vinfo_ini,          // Info INI-Datei
                  vinfo_psl,          // Info PS-Liste
                  vinfo_bl1,          // Info Bootloader 1
                  vinfo_fw1,          // Info Firmware   1
                  vinfo_hw1,          // Info Hardware   1
                  vinfo_bl2,          // Info Bootloader 2
                  vinfo_fw2,          // Info Firmware   2
                  vinfo_hw2,          // Info Hardware   2

                  // Sonder Enums für Simple-Information
                  // Infos werden nicht direkt in DB übernommen, müssen über
                  // speziellen Funktionsaufruf aktiviert werden!!
                  vinfo_bl_simple,    // simple BL-Info (Funktionsaufruf dbinfo_bl_version_simple)
                  vinfo_fw_simple,    // simple WW-Info (Funktionsaufruf dbinfo_fw_version_simple)
                  vinfo_hw_simple     // simple HW-Info (Funktionsaufruf dbinfo_hw_version_simple)
                 );

  // ---------------------------------------------------------------------------
  // Record: Versionsinformation
  // ---------------------------------------------------------------------------
  t_vers_info_record = record

      // -------- variantenunabhängig / Basisinformationen ------------
      s_version_exe,                // Version der EXE
      s_version_ini,                // Version der INI-Datei

      // -------- variantenabhängige Versionsinformationen ------------
      s_version_psl,                // Version Prüfschrittliste
      s_version_bl1,                // Version Bootloader 1
      s_version_fw1,                // Version Firmware   1
      s_version_hw1,                // Version Hardware   1
      s_version_bl2,                // Version Bootloader 2
      s_version_fw2,                // Version Firmware   2
      s_version_hw2,                // Version Hardware   2

      // -------- variantenabhängige Versionsinformationen (simple) ---
      s_version_bl_simple,          // Version Bootloader  (simple)
      s_version_fw_simple,          // Version Firmware    (simple)
      s_version_hw_simple : string; // Version Hardware    (simple)

  end;

  // ===========================================================================
  // Versionsinformationen fuer Automat.Pruef, siehe auch Tabelle
  // Automat.Pruef_Version
  // ===========================================================================
  t_db_vers_info = class
    public
      b_active : boolean;                                 // TRUE: Infos schreiben, sonst FALSE
      constructor Create ( my_block : t_db_infoblock );   // Konstruktor
      destructor  Destroy; override;                 // Destruktor
      procedure init_variant_versions();             // variantenabhängige Versionsinformationen löschen
      function  gen_sql_query_string() : string;     // SQL-Kommando zum Abfragen der Versionsinfo erzeugen
      function  gen_sql_insert_cmd()   : string ;    // SQL-Kommando zum Speichern der Versionsinfo erzeugen

      procedure set_vers_info (                      // Versionsinformation setzen
                   my_info_type : t_vers_enum;
                   s_vers : string );

      procedure simple_vers_info (                   // Versionsinformation aus simple übernehmen
                   my_info_type : t_vers_enum );

      procedure set_vers_inx   ( l_inx : longint );
      function  get_vers_inx_s () : string;

    private
      my_infoblock_internal : t_db_infoblock;        // Verweis auf Infoblock
      s_vers_inx_internal   : string;                // interner Versionsindex
      versions_internal     : t_vers_info_record;    // Versionsinformationen
      procedure init_all();                          // ALLE Versionsinformationen löschen
      procedure check_length ( var s_test  : string; // Längeninformation überwachen
                               w_maxlen    : word;
                               s_fieldname : string );
  end;

  // ===========================================================================
  // Messdateninformationen fuer Automat.Pruef, siehe auch Tabelle
  // Automat.Pruef_Values
  // ===========================================================================
  t_db_mess_info = class
    public
      b_active : boolean;               // TRUE: Infos schreiben, sonst FALSE
      constructor Create ( );
      destructor  Destroy; override;
      procedure   init();  // interne Datenliste löschen
      procedure   set_pr_nr ( lw_inx : longword );  // Automat.Pr_Nr der Klasse bekanntmachen
      function    store_first_data_packet_str ( var s_cmd : string ) : boolean;
      function    store_next_data_packet_str  ( var s_cmd : string ) : boolean;
      procedure   add_mess_value ( s_step, s_mess : string );

    private
      tl_intern      : TList;        // interne Messwertliste
      lw_int_pr_nr   : longword;     // Automat.Pruef.Pr_Nr
      lw_packet_size : longword;     // Anzahl Datensätze pro INSERT-Kommando
      lw_out_inx     : longword;     // Ausgabesatz-Index
      procedure replace_decimal_separator ( var s_in : string; c_sep : char );
  end;


// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
implementation
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
// IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
uses
  Dialogs, SysUtils, Controls, u_login, SqlTimSt, math, regexpr;

const
  C_ERRCODE_OLD_LEN           = 2;             // Spaltenbreite Konfig.Dauertest.Fehlercode
  C_DEVICENUM_LEN             = 10;            // Endgerätenummmern: 10 Zeichen
  C_ID_UNDEFINED              = -1;            // undefinierte  Produktart
  CS_ID_UNDEFINED             = 'UNDEFINED';   // undefinierter Prüfplatz
  C_STR_VERSION_DB_INTERFACE  = '0.0.0.0.1';   // Versionsinfo
  C_STR_FA_SERVICE            = 'SERVICE';     // Service-FA
  C_STR_FA_REPARATUR          = 'REPARATUR';   // Reparatur-FA

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ++++++++++++++ Klasse t_t_db_infoblock ++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : Create: Konstruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
constructor t_db_infoblock.Create ( );
begin
  my_pde_comment        := t_db_comment.Create   ( );
  my_vers_info_internal := t_db_vers_info.Create ( self );
  my_mess_info          := t_db_mess_info.Create ( );
  clear  ( );
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : Destruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
destructor  t_db_infoblock.Destroy;
begin
  my_pde_comment.Free();
  my_vers_info_internal.Free();
  my_mess_info.Free();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : clear: Setzt sämtliche Daten zurück
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
procedure   t_db_infoblock.clear     ( );
begin
  en_testmode       := en_dbt_functiontest;
  s_id_pruefplatz   := '';
  s_starttime       := '';
  s_endtime         := '';
  s_sap_main        := '';
  s_boardnumber     := '';
  s_devicenumber    := '';
  s_errcode         := '';
  s_comment         := '';
  b_result          := FALSE;
  mess_info         := FALSE;
  s_fa_nr           := '';
  s_ff_internal     := '';

  with my_versinfo do
  begin
    s_vers_exe        := '';
    s_vers_ini        := '';
    s_vers_psl        := '';
    s_vers_bl1        := '';
    s_vers_fw1        := '';
    s_vers_hw1        := '';
    s_vers_bl2        := '';
    s_vers_fw2        := '';
    s_vers_hw2        := '';
  end;

  my_pde_comment.clear();         // Kommentare zurücksetzen
  reset_pruefplatz_produktart();  // Prüfplatz und Produktart zurücksetzen
  reset_fa_nr_valid();            // FA-Nummer Gültigkeitsflag zurücksetzen

  // Fehlerinformation zurücksetzen
  with my_error_info do
  begin
    f_pruefschritt    := 0;
    s_function        := '';
    s_value_actual    := '';
    s_value_ref       := '';
  end;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : reset_pruefplatz_produktart:
//                   Setzt Prüfplatz und Produktart-ID zurück
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_infoblock.reset_pruefplatz_produktart();
begin
  s_pruefplatz_nr         := CS_ID_UNDEFINED;
  i_prodnr_functiontest   := C_ID_UNDEFINED;
  i_prodnr_endurancetest  := C_ID_UNDEFINED;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : reset_fa_nr_valid:
//                   Gültigkeitsflag fa_nr zurücksetzen
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_infoblock.reset_fa_nr_valid();
begin
  b_fa_nr_valid     := FALSE;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : add_mess_value : fügt ein Tupel
//                   (Prüfschritt, resultierendes Messergebnis) in die interne
//                   Liste ein
//    Parameter    : s_step   : Prüfschritt
//                   s_mess   : Messergebnis
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
procedure t_db_infoblock.add_mess_value     ( s_step, s_value  : string );
begin
  if ( mess_info ) then
  begin
    self.my_mess_info.add_mess_value ( s_step, s_value );
  end;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : add_comment: Nimmt den Kommentar in die interne Liste
//                   für das spätere Speichern auf. Wenn die Nummer jedoch schon
//                   vorhanden ist, wird dies nicht gemacht. Es gibt aber keine
//                   Fehlermeldung
//    Parameter    : i_comment : aufzunehmende Kommentarnummer
//    Return       : -
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
procedure t_db_infoblock.add_comment ( i_comment        : integer);
begin
  self.my_pde_comment.add_comment ( i_comment );
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_fa_number
//                   prüft die FA-Nummer
//    Parameter    : FA-Nummer (SERVICE und REPARATUR) wurden bereits vorher
//                   ausgeschlossen
//    Return       : TRUE  : FA-Nummer gültig
//    Exceptions   : FALSE : FA-Nummer ungültig
//    First author : /aha/ 2016-07-21
//    History      : --
// =============================================================================
function t_db_infoblock.check_fa_number ( s_fa_number  : string;
                                          i_min, i_max : integer ) : boolean;
const
  C_STR_FA_ERR =         // FA-Nr hat feste Länge
    'FA-Nummer: erwartet: "SERVICE" oder "REPARATUR" oder %d-stellige Zahl. Erhalten: <%s>';

  C_STR_FA_ERR2 =        // FA_Nr hat Länge von...bis
    'FA-Nummer: erwartet: "SERVICE" oder "REPARATUR" oder (%d...%d)-stellige Zahl. Erhalten: <%s>';

  C_STR_FA_ERR_DIGITS =
    'FA-Nummer: erwartet: reine Ziffernfolge. Erhalten: <%s>';

var
  s_err        : string;
  i            : integer;

begin

  result := FALSE;  // Pessimisitische Initialisierung

  // =======================================================================
  // Prüfung der FA-Nummer:
  //   Länge aus INI-Datei
  // =======================================================================
  if ( ( s_fa_nr <> trim ( s_fa_nr )) OR
       ( length ( s_fa_nr ) < i_min ) OR
       ( length ( s_fa_nr ) > i_max )
       ) then
  begin

    // Fehler: FA-Nummer falsch aufgebaut
    if ( i_min = i_max )  then
    begin
      // Fertigungsauftragsnummer hat feste Länge (z.B: min=max=8)
      s_err := format ( C_STR_FA_ERR,
                        [
                          i_min,
                          s_fa_nr
                        ]);
    end
    else
    begin
      // Fertigungsauftragsnummer variable feste Länge (z.B: 8..9 Zeichen)
      s_err := format ( C_STR_FA_ERR2,
                        [
                          i_min,
                          i_max,
                          s_fa_nr
                        ]);
    end;
    debug2 ( s_err );
    exit;
  end
  else
  begin
    // Bis hierher war die Zeichenkette in Ordnung.
    // Hier nochmals explizit auf Ziffern prüfen!
    for i := 1 to length ( s_fa_nr ) do
    begin
      if ( ( integer ( s_fa_nr[i] ) < integer ('0')) OR
           ( integer ( s_fa_nr[i] ) > integer ('9'))) then
      begin
        // hier gibt es doch einen Fehler
        s_err := format ( C_STR_FA_ERR_DIGITS,
                          [
                            s_fa_nr
                          ]);
        debug2 ( s_err );
        exit;
      end;  // if Fehler...
    end;  // For-Schleife

    // Wenn das Programm hier vorbeikommt, ist alles OK
    result := TRUE;    // schon mal optimistisch sein...
  end;

end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_boardnumber
//                   Prüft die Platinennummer und stellt den Fremdfertiger fest.
//    Parameter    : s_board   : Platinennummer
//    Return       : TRUE      : Platinennummer OK
//                   FALSE     : Fehler
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
function t_db_infoblock.check_devicenumber ( var s_device    : string  ) : boolean;
var
  s_err        : string;
  i            : integer;

begin

  result := FALSE;  // Pessimisitische Initialisierung

  if ( s_device = '') then
  begin
    // Endgerätenummer leer
    s_err := 'Fehler: Endgerätenummer leer!';
    debug2 ( s_err );
    exit;
  end;

  if ( length ( s_device ) > C_DEVICENUM_LEN ) then
  begin
    // Endgerätenummer zu lang
    s_err := 'Fehler: Endgerätenummer zu lang!';
    debug2 ( s_err );
    exit;
  end;

  if ( s_device <> trim ( s_device )) then
  begin
    // Endgerätenummer: verborgene Leerzeichen
    s_err := format ( 'Endgerätenummer: verborgene Leerzeichen! <%s>', [s_device]);
    debug2 ( s_err );
    exit;
  end
  else
  begin
    // Bis hierher war die Zeichenkette in Ordnung.
    // Hier nochmals explizit auf Ziffern prüfen!
    for i := 1 to length ( s_device ) do
    begin
      if ( ( integer ( s_device[i] ) < integer ('0')) OR
           ( integer ( s_device[i] ) > integer ('9'))) then
      begin
        // hier gibt es doch einen Fehler
        s_err := 'Fehler: Endgerätenummer besteht nicht nur aus Ziffern! ' + s_device;
        debug2 ( s_err );
        exit;
      end;  // if Fehler...
    end;  // For-Schleife
    // Wenn das Programm hier vorbeikommt, ist alles OK
    result := TRUE;
  end;

  // Bei Bedarf mit führenden Nullen auffüllen.
  while ( length ( s_device ) < C_DEVICENUM_LEN ) do
  begin
    s_device := '0' + s_device;
  end;
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : check_consistency_dt_variant
//                   DAUERTEST
//                   allgemeine Konsistenzprüfung (bei Auswahl einer Variante)
//    Parameter    : Datenbankverbindung
//    Return       : TRUE       : Konsistenzcheck erfolgreich
//                   FALSE      : Fehler
//    Exceptions   : --
//    First author : /aha/ 2016-08-08
//    History      : --
// =============================================================================
function t_db_infoblock.check_consistency_dt_variant  ( my_db : t_db_connection ) : boolean;
const
  CSTR_SQL_PRUEFPLATZ =
    'SELECT Pruefplatz_Nr '                      +
    'FROM Automat.Pruefplatz '                   +
    'WHERE Pruefplatz= ''%s'';';

  CSTR_SQL_PRODUKTART =
    'SELECT Typ_Nr '                             +
    'FROM Konfig.article_definition '                +
    'WHERE article_id=''%s'';';

  CS_ERROR_PRODUKTART =
    'Dauertest: Das Produkt %s ist ' +
    'in der Tabelle Konfig.article_definition unbekannt!';

var
  my_query        : t_db_query;
  my_db_handle    : t_mtx_db;

begin
  result := FALSE;

  my_query := t_db_query.create();

  // *************************************************************************
  // Die Produktbezeichnung muss in Tabelle "Konfig.article_definition"
  // existieren.
  // Die aufgelöste KONFIG-Produktart wird im Erfolgsfall eingetragen.
  // *************************************************************************
  my_query.clear();
  my_query.b_include_all := TRUE;
  my_query.s_query       := format ( CSTR_SQL_PRODUKTART,
                                     [ s_sap_main ]);

  // Abfrage durchführen
  my_db_handle := my_db.get_db_handle();
  my_db_handle.perform_query ( my_query );
  if ( my_query.get_count = 0 ) then   // Prüfung auf Fehler
  begin
    // Fehler: Die Produktart ist unbekannt
    debug2 ( format ( CS_ERROR_PRODUKTART, [ sap_main ]));
    my_query.Free();
    exit;
  end
  else
  begin
    // Produktart gibt es
    i_prodnr_endurancetest := StrToInt ( my_query.get_value(0, 'Typ_Nr'));
  end;

  // Ergebnis OK
  result := TRUE;
  my_query.Free();

end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : check_consistency_variant
//                   allgemeine Konsistenzprüfung (bei Auswahl einer Variante)
//
//                   Bei fehlender DB-Verbindung ist dieser immer OK!
//    Parameter    : Datenbankverbindung
//    Return       : TRUE       : Konsistenzcheck erfolgreich
//                   FALSE      : Fehler
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
function t_db_infoblock.check_consistency_variant  ( my_db : t_db_connection ) : boolean;
const
  CS_STR_ERROR_KOMBI =
    'Fehler: Die Kombination (sap_main=%s; Pruefplatz_Nr=%s) ' +
    'ist in Automat.Produktart_Pr unbekannt!';

  CS_STR_ERROR_KOMBI2 =
    'Fehler: Die Kombination (sap_main=%s; Pruefplatz_Nr=%s) ' +
    'ist in Automat.Produktart_Pr mehrfach vorhanden!!!!';

  CSTR_SQL_PRUEFPLATZ =
    'SELECT Pruefplatz_Nr '                      +
    'FROM Automat.Pruefplatz '                   +
    'WHERE Pruefplatz= ''%s'';';

  CSTR_SQL_PRODUKTART =
    'SELECT Produktart_Nr '                      +
    'FROM Automat.Produktart_Pr '                +
    'WHERE sap_main=''%s'' '                     +
    'AND Pruefplatz_Nr=''%s'';';

  CS_ERROR_PRUEFPLATZ =
    'Der Prüfplatz <%s> ist in der Tabelle Automat.Pruefplatz unbekannt!';

  CS_ERROR_PRODUKTART =
    'Das Produkt %s ist - zumindest für den Prüfplatz %s (Nr. %d) - ' +
    'in der Tabelle Automat.Produtkart_Pr unbekannt!';

var
  my_query        : t_db_query;
  my_db_handle    : t_mtx_db;

begin
  result := TRUE;
  if ( not my_db.connected ) then exit;

  // Prüfung braucht nicht durchgeführt werden, wenn
  // Prüfplatz und Produktart schon bekannt sind.
  if ( ( s_pruefplatz_nr       <> CS_ID_UNDEFINED ) AND
       ( i_prodnr_functiontest <> C_ID_UNDEFINED  )) then exit;

  my_db_handle := my_db.get_db_handle();

  // Grundabfragen:
  // a) SAP_MAIN muss existieren
  // b) Prüfplatz-ID muss existieren (--> dann auch auflösen)
  // c) Kombi (SAP_MAIN + Produkt-ID)   muss existieren (--> Produkt-ID auflösen)
  result := FALSE;

  // ===========================================================================
  // Main-SAP-ID
  // ===========================================================================
  if ( s_sap_main = '') then
  begin
    debug2 ('t_db_infoblock.check_consistency_variant: Keine Angabe zur Main-SAP-ID!');
    exit;
  end;


  // ===========================================================================
  // Prüfplatz-ID
  // ===========================================================================
  if ( s_id_pruefplatz = '') then
  begin
    debug2 ('t_db_infoblock.check_consistency_variant: Keine Angabe zum Prüfplatz!');
    exit;
  end;

  // **************************************
  // Die Prüfplatzbezeichnung muss in Tabelle "Automat.Pruefplatz" existieren
  // Der aufgelöste Prüfplatz wird im Erfolgsfall eingetragen
  // **************************************
  my_query := t_db_query.create();
  my_query.clear();
  my_query.b_include_all := TRUE;
  my_query.s_query       := format ( CSTR_SQL_PRUEFPLATZ,
                                     [ s_id_pruefplatz ]);
  // Abfrage durchführen
  my_db_handle.perform_query ( my_query );
  if ( my_query.get_count = 0 ) then   // Prüfung auf Fehler
  begin
    // Fehler: Der Prüfplatz ist unbekannt
    debug2 ( format ( CS_ERROR_PRUEFPLATZ, [ s_id_pruefplatz ]));
    my_query.Free();
    exit;
  end
  else
  begin
    // Prüfplatz gibt es
    s_pruefplatz_nr := my_query.get_value(0, 'Pruefplatz_Nr');
  end;

  // ===========================================================================
  // Kombi SAP_MAIN-ID + Prüfplatz-ID
  // ===========================================================================
  // **************************************
  // Die o.g. Kombi muss in Tabelle "Automat.Produktart_Pr" existieren
  // Die aufgelöste Produktart wird im Erfolgsfall eingetragen
  // **************************************
  my_query.clear();
  my_query.b_include_all := TRUE;
  my_query.s_query       := format ( CSTR_SQL_PRODUKTART,
                                     [ s_sap_main,
                                       s_pruefplatz_nr ] );
  // Abfrage durchführen
  my_db_handle.perform_query ( my_query );


  case ( my_query.get_count() ) of
    0 :
    begin
      // Fehler: Die Kombi ist unbekannt
      debug2 ( format ( CS_STR_ERROR_KOMBI, [ s_sap_main, s_pruefplatz_nr ]));
      my_query.Free();
      exit;
    end;
    1 :
    begin
      // Genau eine Kombi: Prima! ==> Produktart-ID bestimmen
      i_prodnr_functiontest := StrToInt ( my_query.get_value ( 0, 'Produktart_Nr'));
    end
    else
    begin
      // Fehler: mehrere Produktarten für eine Kombi
      debug2 ( format ( CS_STR_ERROR_KOMBI2, [ s_sap_main, s_pruefplatz_nr ]));
      my_query.Free();
      exit;
    end;
  end; // case

  // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  // Ggf. Verzweigung: Spezialbehandlung Dauertest
  // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  if ( self.en_testmode = en_dbt_endurancetest ) then
  begin
    // -----------------------------------------------------
    // Spezialfall Dauertest
    // -----------------------------------------------------
    result := check_consistency_dt_variant  ( my_db );
    my_query.Free();
    exit;
  end;
  // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  result := TRUE;
  my_query.Free();

end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : exists_dataset_fa_sap
//                   Prüfung ob es mindestens einen Datensatz gibt,
//                   der eine abweichende 9xxx-SAP besitzt
//                   In diesem Fall wird eine abweichende sap_nr zurückgeliefert
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   s_sap1      : aktuelle SAP-Nummer
//                   s_fa_nr     : aktueller Fertigungsauftrag
//                   s_other_sap : abweichende 9xxx-Nr (falls vorhanden)
//    Return       : TRUE        : Datensatz unter gleicher FA mit abweichender SAP-ID
//                   FALSE       : keine Konsistenzprobleme entdeckt
//    Exceptions   : --
//    First author : /aha/ 2015-12-03
//    History      :
// =============================================================================
function t_db_infoblock.exists_dataset_fa_sap (
                  db_handle : t_mtx_db;
                  s_sap1,
                  s_fa_nr : string;
              var s_other_sap : string ) : boolean;
const
  C_STR_SQL =
    'SELECT sap_main FROM Automat.Pruef ' +
    'where ' +
    'FA_Nr=''%s'' AND ' +
    'sap_main LIKE ''9%%'' AND ' +
    'sap_main<>''%s'';';

  C_STR_DEB2 =
    't_db_connection.exists_dataset_fa_sap: Konflikt 9000-sap-Nummern <%s>, <%s>';
var
  my_query : t_db_query;
  s_query  : string;
  s_err    : string;    // Fehlerstring
begin

  result := TRUE;     // Initialisierung

  // ----------------------------------------------
  // Datenbankanfrage starten und auswerten...
  // ----------------------------------------------
  try
    s_query := format ( C_STR_SQL, [s_fa_nr, s_sap1]);

    my_query := t_db_query.create_cmd( s_query, TRUE );
    debug2 ( s_query );
    db_handle.perform_query( my_query );
    result := (my_query.get_count() <> 0);

    if ( result ) then
    begin
      // Konsistenzproblem
      s_other_sap := my_query.get_value ( 0, 'sap_main' );

      // Debugausgabe
      debug2 ( format ( C_STR_DEB2, [s_sap1, s_other_sap]));
    end
    else
    begin
      s_other_sap := '';   // Kein Konsistenzproblem => keine andere SAP-ID
    end;

  except On E:Exception do
  begin
    // Wenn es eine unerwartete Exception gibt, diese benennen!
    if ( s_err = '' ) then
    begin
      s_err := format ('Unerwartete Exception (%s) aufgetreten bei Query (%s)', [E.ClassName, s_query]);
    end;
    debug2 ( s_err );
    exit;
  end;  // begin (try)
  end;  // except

  my_query.Free();

end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : exists_combination
//                   prüft, ob die Kombination Produkttyp / Fertigungsauftrag
//                   schon einmal in der Datenbank eingetragen wurde
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   lw_prodid   : Automat.Pruef.Produktart_Nr
//                   s_fa_nr     : Automat.Pruef.FA_Nr
//    Return       : TRUE        : Die Kombination gab es bereits
//                   FALSE       : Es ist eine ganz neue Kombination
//    Exceptions   : --
//    First author : /aha/ 2015-12-03
//    History      :
// =============================================================================
function t_db_infoblock.exists_combination    (
              db_handle : t_mtx_db;
              lw_prodid  : longword;
              s_fa_nr    : string ): boolean;

const
  C_STR_SQL =
    'SELECT * FROM Automat.Pruef where ' +
    'FA_Nr=''%s'' and Produktart_Nr=%d;';

  C_STR_DEB1 =
     't_db_connection.exists_combination: Kombination %s %d bereits vorhanden!';


var
  my_query : t_db_query;
  s_query  : string;
  s_err    : string;    // Fehlerstring
begin

  result := FALSE;     // Initialisierung


  // ----------------------------------------------
  // Datenbankanfrage starten und auswerten...
  // ----------------------------------------------
  try
    s_query := format ( C_STR_SQL, [s_fa_nr, lw_prodid]);

    my_query := t_db_query.create_cmd( s_query, TRUE );
    debug2 ( s_query );
    db_handle.perform_query( my_query );
    result := (my_query.get_count() > 0);

    // Optionale Debugausgabe
    if ( result ) then  debug2 ( format ( C_STR_DEB1, [s_fa_nr, lw_prodid] ));

  except On E:Exception do
  begin
    // Wenn es eine unerwartete Exception gibt, diese benennen!
    if ( s_err = '' ) then
    begin
      s_err := format ('Unerwartete Exception (%s) aufgetreten bei Query (%s)', [E.ClassName, s_query]);
    end;
    debug2 ( s_err );
    exit;
  end;  // begin (try)
  end;  // except

  my_query.Free();

end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_main_product
//                   Prüft, ob die übergebene SAP-ID ein Hauptprodukt mit
//                   einer oder mehreren Unterplatinen definiert.
//                   Dies ist der Fall, wenn die SAP-ID in der Tabelle
//                   Konfig.sap_table.sap_main existiert.
//                   (In Zusammenhang mit Mantis 1555)
//
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   s_sap       : zu testende SAP
//    Return       : TRUE        : Hauptprodukt
//                   FALSE       : kein Hauptprodukt
//    Exceptions   : --
//    First author : /aha/ 2016-06-02
//    History      : --
// =============================================================================
function t_db_infoblock.check_main_product    (
                  db_handle : t_mtx_db;
                  s_sap : string ) : boolean;
const
  CS_SQL_EXIST_QUERY =
   'SELECT * FROM Konfig.sap_table WHERE sap_main=''%s'';';

  CS_RESULT_TRUE =
   '%s ist ein Hauptprodukt,weil es in Konfig.sap_table.sap_main existiert!';

  CS_RESULT_FALSE =
   '%s ist KEIN Hauptprodukt,weil es in Konfig.sap_table.sap_main NICHT existiert!';

var
  my_query              : t_db_query;

begin

  // Initialisierung
  my_query                := t_db_query.create;

  // Für jede Produkt-ID die sap_sub's feststellen (ggf. mehrere)
  my_query.clear;
  my_query.s_query            := format ( CS_SQL_EXIST_QUERY, [s_sap]);
  my_query.b_include_all      := TRUE;

  db_handle.perform_query( my_query );
  result := (my_query.get_count() > 0);

  if ( result ) then debug2 ( format ( CS_RESULT_TRUE,  [s_sap]))
  else               debug2 ( format ( CS_RESULT_FALSE, [s_sap]));

  my_query.Free();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_lowlevel_error
//                   Fehlermeldung erzeugen:
//                   "Das Produkt X passt nicht zur SAP-ID Y"
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   lw_product  : Automat.Produktart_Pr.Produktart_Nr
//                   s_sap_id    : Automat.Pruef.sap_main
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2015-12-10
//    History      :
// =============================================================================
procedure t_db_infoblock.gen_lowlevel_error (
                  db_handle  : t_mtx_db;
                  lw_product : longword;
                  s_sap_id   : string );
const
  C_STR_ERROR =
    'Fehler Typprüfung: Das Produkt <%s> (Nr. %d) ' + char (13) +
    'passt generell nicht zur SAP-ID %s (%s)!';

  C_STR_SQL_FIND_ENDTYPE =
    'SELECT article_text FROM Konfig.article_definition where article_id=''%s'';';


  C_STR_SQL_FIND_TYPE =
    'SELECT Produktart FROM Automat.Produktart_Pr ' +
    'WHERE Produktart_Nr=%d;';

var
  s_err,
  s_endproduct,                         // Endproduktname
  s_productname   : string;             // Teilproduktname
  my_query        : t_db_query;

begin

  // Initialisierung
  s_productname := 'nicht in PDE enthalten';
  s_endproduct  := 'nicht in PDE enthalten';

  // -------------------------------------------------------------
  // Teilproduktnamen bestimmen...
  // -------------------------------------------------------------
  my_query        := t_db_query.create_cmd (
                        format ( C_STR_SQL_FIND_TYPE, [lw_product]),
                        TRUE);

  debug2 ( my_query.s_query );
  db_handle.perform_query( my_query );

  if ( my_query.get_count > 0 ) then
  begin
    s_productname := my_query.get_value(0, 'Produktart');
  end;

  // -------------------------------------------------------------
  // Endproduktnamen bestimmen...
  // -------------------------------------------------------------
  my_query.clear;
  my_query.s_query       := format ( C_STR_SQL_FIND_ENDTYPE, [s_sap_id]);
  my_query.b_include_all := TRUE;

  debug2 ( my_query.s_query );
  db_handle.perform_query( my_query );

  if ( my_query.get_count > 0 ) then
  begin
    s_endproduct := my_query.get_value(0, 'article_text');
  end;

  // -------------------------------------------------------------
  // Jetzt die Fehlermeldung generieren
  // -------------------------------------------------------------
  s_err := format ( C_STR_ERROR, [s_productname, lw_product, s_sap_id, s_endproduct]);
  debug2 ( s_err );

  // Speicherplatz wieder freigeben.
  my_query.Free();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_lowlevel_error
//                   Fehlermeldung erzeugen:
//                   "Das Produkt X passt nicht zur SAP-ID Y"
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   lw_product  : Automat.Produktart_Pr.Produktart_Nr
//                   s_sap_id    : Automat.Pruef.sap_main
//                   s_fa        : betroffener Fertigungsauftrag
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2015-12-10
//    History      :
// =============================================================================
procedure t_db_infoblock.gen_lowlevel_error_fa (
                  db_handle  : t_mtx_db;
                  lw_product : longword;
                  s_sap_id,
                  s_fa       : string );
const
  C_STR_ERROR =
    'Fehler Altdaten zu FA-Nr. %s. Typprüfung: Das Produkt <%s> (Nr. %d) ' + char (13) +
    'passt generell nicht zur SAP-ID %s (%s)!';

  C_STR_SQL_FIND_ENDTYPE =
    'SELECT article_text FROM Konfig.article_definition where article_id=''%s'';';


  C_STR_SQL_FIND_TYPE =
    'SELECT Produktart FROM Automat.Produktart_Pr ' +
    'WHERE Produktart_Nr=%d;';

var
  s_err,
  s_endproduct,                         // Endproduktname
  s_productname   : string;             // Teilproduktname
  my_query        : t_db_query;

begin

  // Initialisierung
  s_productname := 'nicht in PDE enthalten';
  s_endproduct  := 'nicht in PDE enthalten';

  // -------------------------------------------------------------
  // Teilproduktnamen bestimmen...
  // -------------------------------------------------------------
  my_query        := t_db_query.create_cmd (
                        format ( C_STR_SQL_FIND_TYPE, [lw_product]),
                        TRUE);

  debug2 ( my_query.s_query );
  db_handle.perform_query( my_query );

  if ( my_query.get_count > 0 ) then
  begin
    s_productname := my_query.get_value(0, 'Produktart');
  end;

  // -------------------------------------------------------------
  // Endproduktnamen bestimmen...
  // -------------------------------------------------------------
  my_query.clear;
  my_query.s_query       := format ( C_STR_SQL_FIND_ENDTYPE, [s_sap_id]);
  my_query.b_include_all := TRUE;

  debug2 ( my_query.s_query );
  db_handle.perform_query( my_query );

  if ( my_query.get_count > 0 ) then
  begin
    s_endproduct := my_query.get_value(0, 'article_text');
  end;

  // -------------------------------------------------------------
  // Jetzt die Fehlermeldung generieren
  // -------------------------------------------------------------
  s_err := format ( C_STR_ERROR, [s_fa, s_productname, lw_product, s_sap_id, s_endproduct]);
  debug2 ( s_err );

  // Speicherplatz wieder freigeben.
  my_query.Free();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_sap_ids
//                   Macht aus einer SAP-ID zwei Unter-IDs, die für eine
//                   SQL-Abfrage benötigt werden.
//                   Beispiel:    SAP_ID = '8019-1234-56'
//                                DST1   = '8019-1234-xx'
//                                DST2   = '8819-1234-xx'
//    Parameter    : s_sap_src   : einkommende ID
//                   s_sap_dst1  : 1. ausgehende ID
//                   s_sap_dst2  : 2. ausgehende ID
//    Return       : TRUE        : Zerlegung erfolgreich
//                   FALSE       : SAP-ID fehlerhaft
//    Exceptions   : --
//    First author : /aha/ 2016-01-05
//    History      :
// =============================================================================
function t_db_infoblock.gen_sap_ids  ( s_sap_src : string;
                                         var s_sap_dst1, s_sap_dst2 : string ) : boolean;
const
  C_STR_ERR1 =
    'Fehler t_db_connection.gen_sap_ids: Die SAP-ID entspricht nicht der Vorgabe ' +
    'XXXX-XXXX-XX. Erhaltene ID = <%s>';
var
    my_regexpr : TRegExpr;

begin
  result     := FALSE;
  my_regexpr := TRegExpr.Create();
  my_regexpr.Expression:= '(.*)-(.*)-(.*)';

  if my_regexpr.Exec ( s_sap_src ) then
  begin
    // hinteren Teil der SAP-ID durch 'xx' ersetzen!
    s_sap_dst1 := my_regexpr.Match[1] + '-' +
                  my_regexpr.Match[2] + '-xx';
    s_sap_dst2 := s_sap_dst1;

    // zwei unterschiedliche SAP-IDs erzeugen: 80xx-xxxx-xx und 88xx-xxxx-xx
    s_sap_dst1[2] := '0';
    s_sap_dst2[2] := '8';
    result        := TRUE;
  end
  else
  begin
    // SAP-ID entspricht nicht den Vorgaben
    debug2 ( format ( C_STR_ERR1, [s_sap_src]));
  end;

  // Speicher freigeben
  my_regexpr.Free();

end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_via_product_ids
//                   Prüft, ob die über die Stringliste übergebenen Produkttypen
//                   mindestens einen gemeinsamen Gerätetyp haben, der alle diese
//                   verschiedenen Platinen vereint.
//                   Im Fehlerfall wird eine Fehlermeldung erzeugt.
//
// Realisierung in einem SQL-Befehl.
// Er besteht aus so vielen Sub-Queries, wie es übergebene Produkttypen gibt.
// In jedem Sub-Query werden die sap-IDs ermittelt, die zum einzelnen Produkttyp
// gehören.
// In der Kombination werden nur die sap_main's übrig gelassen, die in allen
// verwendeten Produkttypen auftauchen
//
//
//    Parameter    : db_handle     : Pointer auf Datenbank
//                   tsl_prodtypes : Liste mit Produkt-IDs
//                                  (Konfig.Produktart_Pr.Produktart_Nr)
//
//    Return       : TRUE          : Gemeinsamer Gerätetyp vorhanden
//                   FALSE         : nicht konsistent
//    Exceptions   : --
//    First author : /aha/ 2015-12-03
//    History      : /aha/ 2016-06-02: Mantis 1554 Bugfix
// =============================================================================
function t_db_infoblock.check_via_product_ids (
                  db_handle     : t_mtx_db;
                  tsl_prodtypes : TStringList) : boolean;
const
  C_STR_SELECT_CMD =
   'SELECT DISTINCT sap_main FROM Konfig.sap_table where sap_sub IN ' +
   '(SELECT sap_sub FROM Automat.Produktart_Pr WHERE Produktart_Nr=%s)';

var
  s_debug,                             // Debugausgabe
  s_mega_query          : string;
  i                     : integer;
  my_mega_query         : t_db_query;

begin

  // Initialisierung
  my_mega_query           := t_db_query.create;
  s_mega_query            := '';

  // Schöne Debugausgabe bauen
  s_debug := 'Prüfe die Typkonfiguration ( ';
  for i := 0 to tsl_prodtypes.Count-1 do
  begin
    s_debug    := s_debug + tsl_prodtypes.Strings[i] + ' ';
  end;
  debug2 ( s_debug + ')');

  // -------------------------------------------------------------------
  // ab hier den Query zusammenbasteln
  // -------------------------------------------------------------------
  for i := 0 to tsl_prodtypes.Count-1 do
  begin

    if ( i > 0 )  then s_mega_query := s_mega_query + 'AND sap_main IN ( ';

    s_mega_query   := s_mega_query +
                      format ( C_STR_SELECT_CMD, [tsl_prodtypes.Strings[i]]);

    if ( i > 0 )  then s_mega_query := s_mega_query + ')';
  end;

  s_mega_query := s_mega_query + ';';
  debug2 ( s_mega_query );

  // Für jede Produkt-ID die sap_sub's feststellen (ggf. mehrere)
  my_mega_query.clear;
  my_mega_query.s_query       := s_mega_query;
  my_mega_query.b_include_all := TRUE;

  db_handle.perform_query( my_mega_query );
  result := (my_mega_query.get_count() > 0);

  my_mega_query.Free();   // Mantis 1554

end;      // Ende For-Schleife




// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_type_lowlevel
//
//                   Schritt1:
//                   prüft, die Platine mit der Produktart lw_prodid
//                   für die SAP-Nr s_sap_nr geeignet ist
//
//                   Schritt2:
//                   prüft, ob alle weiteren Platinentypen, die auf diesen Auftrag
//                   gebucht wurden, ebenfalls für die SAP-Nr s_sap_nr geeinget
//                   sind.
//
//                   Siehe auch Tabelle Konfig.sap_table, verbunden mit
//                                      Konfig.article_definition und
//                                      Automat.Produktart_Pr
//
//    Parameter    : db_handle   : Pointer auf Datenbank
//                   lw_prodid   : Automat.Pruef.Produktart_Nr
//                   s_fa_nr     : Fertigungsauftragsnummer
//                   s_sap_nr    : Automat.Pruef.sap_main
//    Return       : TRUE        : Die Kombination ist zulässig
//                   FALSE       : nicht zulässig
//    Exceptions   : --
//    First author : /aha/ 2015-12-03
//    History      : /aha/ 2016-06-02: Mantis Bugfix 1555
// =============================================================================
function t_db_infoblock.check_type_lowlevel (
                    db_handle       : t_mtx_db;
                    lw_prodid       : longword;
                    s_fa_nr,
                    s_sap_nr        : string ) : boolean;
const

  C_STR_SQL_MATCH =
    'SELECT sap_main FROM Konfig.sap_table '                                +
    'WHERE (Konfig.sap_table.sap_sub IN '                                   +
    '(SELECT sap_sub FROM Automat.Produktart_Pr WHERE Produktart_Nr=%d)) '  +
    'AND Konfig.sap_table.sap_main=''%s'';';

  C_STR_SQL_PRODMATCH =
    'SELECT * FROM Automat.Produktart_Pr WHERE '                            +
    '(sap_sub=''%s'' OR '                                                   +
    'sap_sub=''%s'') AND '                                                  +
    'Produktart_Nr=%d;';

  C_STR_SQL =
    'Select '                                                      +
    'Automat.Produktart_Pr.Produktart_Nr AS Produktart_Nr '        +
    'FROM '                                                        +
    'Automat.Produktart_Pr INNER JOIN Konfig.sap_table '           +
    'ON (Konfig.sap_table.sap_sub=Automat.Produktart_Pr.sap_sub) ' +
    'WHERE Konfig.sap_table.sap_main=''%s'' AND '                  +
    'Automat.Produktart_Pr.Produktart_Nr=%d;';

  // Alle anderen Produktarten finden, inklusive der übergebenen Produktart,
  // die im Parameter übergeben wurde
  C_STR_LIST_SQL =
    'SELECT DISTINCT Produktart_Nr '                               +
    'FROM Automat.Pruef where FA_Nr=''%s'';';

  C_STR_ERR_TYPE_UNKNOWN =
   't_db_connection.check_type_lowlevel: unerwarteter Fehler: ' + char (13) +
   'Gerätetyp %d in Automat.Produktart_Pr unbekannt!';

  C_STR_SQL_FIND_TYPE =
    'SELECT Produktart FROM Automat.Produktart_Pr ' +
    'WHERE Produktart_Nr=%s;';



var
  b_main_product  : boolean;   // TRUE: SAP-Nummer bezeichnet Hauptprodukt mit Unterplatinen
  my_list_query,
  my_query        : t_db_query;
  s_sap_nr1,
  s_sap_nr2,
  s_list_query,
  s_query         : string;
  s_err           : string;    // Fehlerstring
  lw_prod_tmp     : longword;
  i               : integer;
  tsl_prodid      : TStringList;

begin

  // ----------------------------------------------
  // Initialisierungen
  // ----------------------------------------------
  result           := TRUE;
  tsl_prodid       := TStringList.Create;
  my_list_query    := t_db_query.Create();
  my_query         := t_db_query.Create();

  try
    b_main_product := check_main_product ( db_handle,
                                           s_sap_nr );

    // if ( s_sap_nr[1] = '9') then    /aha/ 2016-06-02: Mantis 1555
    if ( b_main_product ) then
    begin
      // =======================================================================
      // Wenn es sich um ein Hauptprodukt handelt,
      // muss geprüft werden, ob der Produkttyp dazu passt.
      // Nutzung von Konfig.sap_table und Automat.Produktart_Pr
      // =======================================================================
      my_query.clear;
      my_query.s_query            := format ( C_STR_SQL_MATCH, [lw_prodid,s_sap_nr]);
      my_query.b_include_all := TRUE;
      debug2 ( my_query.s_query );
      db_handle.perform_query ( my_query );
      if ( my_query.get_count() = 0 )  then
      begin
        // diese Produkt-ID passt nicht zur SAP-ID
        gen_lowlevel_error ( db_handle, lw_prodid, s_sap_nr );

        my_query.Free();         // Speicherplatz freigeben
        my_list_query.Free();    // Speicherplatz freigeben
        tsl_prodid.Free();       // Speicherplatz freigeben

        result  := FALSE;
        exit;
      end;
    end
    else
    begin
      // =======================================================================
      // Wenn es sich um kein Hauptprodukt handelt,
      // muss geprüft werden, in Automat.Produktart_Pr ein passender Eintrag
      // vorhanden ist.
      // =======================================================================
      my_query.clear;
      if ( not gen_sap_ids ( s_sap_nr, s_sap_nr1, s_sap_nr2 )) then
      begin
        // übegebene SAP-ID fehlerhaft!
        my_query.Free();         // Speicherplatz freigeben
        my_list_query.Free();    // Speicherplatz freigeben
        tsl_prodid.Free();       // Speicherplatz freigeben

        result  := FALSE;
        exit;
      end;

      // Hier wird Automat.Produktart_Pr geprüft!
      my_query.s_query            := format ( C_STR_SQL_PRODMATCH,
                                     [
                                       s_sap_nr1,
                                       s_sap_nr2,
                                       lw_prodid
                                       ]);
      my_query.b_include_all := TRUE;
      debug2 ( my_query.s_query );
      db_handle.perform_query ( my_query );
      if ( my_query.get_count() = 0 )  then
      begin
        // diese Produkt-ID passt generell nicht zur SAP-ID
        gen_lowlevel_error ( db_handle, lw_prodid, s_sap_nr );

        my_query.Free();         // Speicherplatz freigeben
        my_list_query.Free();    // Speicherplatz freigeben
        tsl_prodid.Free();       // Speicherplatz freigeben

        result  := FALSE;
        exit;
      end;

      // Wenn wir hier vorbeikommen, passt die SAP-ID zur Produktart-ID!

    end;


    // =======================================================================
    // Nächste Prüfung
    // sämtliche Produkttypen dieses Fertigungsauftrags feststellen
    // Ergebnis in my_list_query
    // =======================================================================
    s_list_query                := format ( C_STR_LIST_SQL, [s_fa_nr, lw_prodid]);
    my_list_query.clear;
    my_list_query.s_query       := s_list_query;
    my_list_query.b_include_all := TRUE;
    debug2 ( s_list_query );
    db_handle.perform_query ( my_list_query );

    // ===========================================================================
    //  Prüfen: ist die übergebene SAP-Nr ein Hauptprodukt?
    //          Wenn ja, kann überprüft werden ob es eine Verbindung
    //          Automat.Produktart_Pr.Produktart_Nr ==>
    //          Automat.Produktart_Pr.sap_sub       ==>
    //          Konfig.sap_table.sap_sub            ==>
    //          Konfig.sap_table.sap_main           gibt.
    // ===========================================================================
    if ( b_main_product ) then   // Mantis 1555
    begin

      // -------------------------------------------------------------------------
      // Die zu prüfende SAP-NR ist ein Hauptprodukt
      // -------------------------------------------------------------------------
      debug2 ( 't_db_connection.check_type_lowlevel: Hauptprodukt!');

      // Prüfen, ob alle Unterprodukte im Hauptprodukt auftauchen
      for i := 0 to my_list_query.get_count()-1 do
      begin

        // Für jeden einzelnen festgestellten Produkttypen folgendes machen:
        // Produktartnummer feststellen
        lw_prod_tmp := StrToInt ( my_list_query.get_value( i, 'Produktart_Nr'));

        my_query.clear;
        my_query.s_query        := format ( C_STR_SQL_MATCH,
                                   [
                                     lw_prod_tmp,
                                     s_sap_nr
                                   ]);
        my_query.b_include_all  := TRUE;
        db_handle.perform_query( my_query );
        result := (my_query.get_count() > 0);

        if ( not result ) then
        begin
          // Fehler bei Schritt 2:
          // Fehlermeldung generieren!
          debug2 ( 't_db_connection.check_type_lowlevel: Fehler in Schritt 2');
          gen_lowlevel_error_fa ( db_handle, lw_prod_tmp, s_sap_nr, s_fa_nr );
          break; // nur den ersten Fehler melden
        end;    // Ende Fehlerfall
      end;     // Ende For-Schleife
    end       // Ende Hauptprodukt
    else
    begin
      // -------------------------------------------------------------------------
      //  Die übergebene SAP-ID beschreibt kein Hauptprodukt
      //  Wir führen an dieser Stelle eine Basisprüfung aus:
      //  Basierend auf den Produkttypen-IDs die Prüfung durchführen
      // -------------------------------------------------------------------------
      debug2 ( 't_db_connection.check_type_lowlevel: kein Hauptprodukt! => Produkttypen-IDs Direktprüfung!');

      // Parameter vorbereiten
      tsl_prodid.Clear();
      tsl_prodid.Add ( IntToStr ( lw_prodid ));  // erster Eintrag ist die eigene Produktart-ID

      for i := 0 to my_list_query.get_count()-1 do
      begin
        // Produktartnummer in Stringliste packen
        tsl_prodid.Add ( ( my_list_query.get_value( i, 'Produktart_Nr')));
      end;      // Ende For-Schleife

      // Aufruf der Check-Funktion
      result := check_via_product_ids ( db_handle, tsl_prodid );
      if ( not result ) then
      begin

        // =====================================================================
        // Fehlermeldung bei Typ-Inkompabilität ausgeben
        // =====================================================================
        s_err := format ( 'Typkonflikt! Inkompatible Produktarten für FA %s:',
                          [s_fa_nr]);


        for i := 0 to tsl_prodid.Count-1 do
        begin

          my_query.clear;
          my_query.s_query        := format ( C_STR_SQL_FIND_TYPE,
                                     [
                                       tsl_prodid.Strings[i]
                                     ]);
          my_query.b_include_all  := TRUE;
          db_handle.perform_query( my_query );
          if ( (my_query.get_count() > 0)) then
          begin
            // Produktartnamen zur Nummer gefunden: Namen ausgeben
            s_err := s_err + char (13) +
                     my_query.get_value (0, 'Produktart');
          end
          else
          begin
            // Produktartnummer unbekannt: Nummer ausgeben.
            s_err := s_err + char (13) +
                     'unbekannte Produktart-Nr ' + tsl_prodid.Strings[i];
          end;
        end;
        debug2 ( s_err );

        // =====================================================================
        // Ende Fehlermeldung
        // =====================================================================
      end;

    end;

  except On E:Exception do
  begin
    // Wenn es eine unerwartete Exception gibt, diese benennen!
    if ( s_err = '' ) then
    begin
      s_err := format ('Unerwartete Exception (%s) aufgetreten bei Query (%s)', [E.ClassName, s_query]);
    end;
    debug2 ( s_err + ' Die Aktion wird abgebrochen!');
    result := FALSE;
    exit;
  end;  // begin (try)
  end;  // except

  // Speicherplatz freigeben
  my_query.Free();
  my_list_query.Free();
  tsl_prodid.Free();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_consistency_dt_fa
//                   DAUERTEST
//                   Konsistenzprüfung, ob der Fertigungsauftrag
//                   zum Gerätetyp passt
//                   ( wurde vorher auf den gleichen Fertigungsauftrag ein
//                     komplett anderes Gerät geprüft?
//                     ==> das darf nicht sein!)
//    Parameter    : Datenbankverbindung
//    Return       : TRUE  : konsistent
//                   FALSE : inkonsistent
//    Exceptions   : --
//    First author : /aha/ 2016-08-08
//    History      : --
// =============================================================================
function t_db_infoblock.check_consistency_dt_fa ( my_db : t_db_connection ) : boolean;
const
  CSTR_SQL_OTHER_SAPID =
    'SELECT sap_id FROM Konfig.Dauertest '                       +
    'WHERE '                                                     +
    'FA_Nr=''%s'' AND '                                          +
    'sap_id<>'''' AND '                                          +
    'sap_id<>''%s'';';

  C_STR_ERR_SAP9xxx  =
    'Für den Fertigungsauftrag %s gibt es ' + char (13) +
    'mindestens eine abweichende SAP-Nummer, nämlich %s!' + char (13) +
    '==> Typkonflikt!';


var
  my_query                       : t_db_query;
  my_db_handle                   : t_mtx_db;

begin

  // Die Konsistenzprüfung gilt als nicht bestanden,
  // wenn es für diesen Fertigungsauftrag
  // mindestens eine andere, nicht leere SAP-Nummer gibt.
  my_query := t_db_query.create_cmd (
                 format ( CSTR_SQL_OTHER_SAPID,
                   [ self.s_fa_nr,
                     self.sap_main]),
                 TRUE
                                    );

  my_db_handle := my_db.get_db_handle();
  my_db_handle.perform_query ( my_query );

  if ( my_query.get_count() > 0 ) then
  begin
    debug2 ( 't_db_infoblock.check_consistency_dt_fa: abweichende SAP-Nr!' + char (13) +
             format ( C_STR_ERR_SAP9xxx,
                  [ s_fa_nr,
                    my_query.get_value(0, 'sap_id')
                  ]));
    result := FALSE;
  end
  else
  begin
    result := TRUE;
  end;

  my_query.Free();

end;



// =============================================================================
//    Class        : t_db_connection
//    Function     : check_consistency_fa
//                   Konsistenzprüfung, ob der Fertigungsauftrag
//                   zum Gerätetyp passt
//                   ( wurde vorher auf den gleichen Fertigungsauftrag ein
//                     komplett anderes Gerät geprüft?
//                     ==> das darf nicht sein!)
//    Parameter    : Datenbankverbindung
//    Return       : TRUE  : konsistent
//                   FALSE : inkonsistent
//    Exceptions   : --
//    First author : /aha/ 2016-07-21
//    History      : --
// =============================================================================
function t_db_infoblock.check_consistency_fa  ( my_db : t_db_connection ) : boolean;
const
  CSTR_SQL_SAP_TABLE =
    'SELECT * FROM Konfig.sap_table '            +
    'WHERE '                                     +
    'sap_main=''%s'' AND '                       +
    'sap_sub=''%s'';';

  CS_ERROR =
    'Die Kombination (%s)/(%s) ist in Konfig.sap_table unbekannt!';

  CSTR_SQL_PRUEFPLATZ =
    'SELECT Pruefplatz_Nr '                      +
    'FROM Automat.Pruefplatz '                   +
    'WHERE Pruefplatz= ''%s'';';

  CS_ERROR_PRUEFPLATZ =
    'Der Prüfplatz <%s> ist in der Tabelle Automat.Pruefplatz unbekannt!';

  C_STR_ERR_SAP9xxx  =
    'Für den Fertigungsauftrag %s gibt es ' + char (13) +
    'mindestens eine abweichende 9xxx-SAP-Nummer, nämlich %s!' + char (13) +
    '==> Typkonflikt!';


var
  my_db_handle                   : t_mtx_db;
  s_other_sap_9xxx_number        : string;        // abweichende SAP-Nummer

begin

  result := TRUE;
  if ( not my_db.connected  ) then exit;     // nicht verbundene Datenbank
  if ( b_fa_nr_valid        ) then exit;     // FA-Nummer wurde schon geprüft


  // Variantencheck ausführen (sehr kurz, wenn bereits durchgeführt...)
  if ( not check_consistency_variant ( my_db )) then
  begin
    debug2 ('t_db_infoblock.check_consistency_fa: Schon Variantencheck fehlerhaft!');
    result := FALSE;
    exit;
  end;

  // ===========================================================================
  // Prüfung Seriennummer Endgerät (für Funktionstest und Dauertest)
  // ===========================================================================
  if ( not check_devicenumber ( s_devicenumber )) then
  begin
    result := FALSE;   // Seriennummer ungültig
    exit;
  end;

  // ===========================================================================
  // Prüfung des Fertigungsauftrags
  // ===========================================================================

  // Auf Service und Reparatur dürfen sämtliche Geräte gebucht werden
  // ==> immer TRUE zurückliefern
  if (( s_fa_nr = C_STR_FA_SERVICE   )  OR
      ( s_fa_nr = C_STR_FA_REPARATUR )) then
  begin
    b_fa_nr_valid := TRUE;
    exit;
  end;

  // An dieser Stelle sollte geprüft werden, ob der Fertigungs-
  // auftrag nur aus Ziffern besteht und die richtige Länge hat.
  if ( not check_fa_number ( s_fa_nr,
                             my_db.fa_min_len,
                             my_db.fa_max_len )) then
  begin
    //debug ('t_db_infoblock.check_consistency_fa: FA-Nummer ungültig: ' + s_fa_nr );
    result := FALSE;
    exit;
  end;

  my_db_handle := my_db.get_db_handle();

  // ============================================================
  // Ggf. Verzweigung: Spezialbehandlung bei Dauertest
  // ============================================================
  if (self.en_testmode = en_dbt_endurancetest ) then
  begin
    // Spezialbehandlung: Dauertest
    result        := check_consistency_dt_fa  ( my_db );
    b_fa_nr_valid := result;
    exit;
  end;

  // Falls es für diesen Fertigungsauftrag eine 9xxx-Nr gibt:
  // Exisitieren bereits andere Datensätze, die eine abweichende 9xxx-SAP-Nr haben?
  // Das darf nicht vorkommen! In diesem Fall hätten wir einen Typ-Konflikt
  if ( ( self.s_sap_main [1] = '9' ) AND
       ( exists_dataset_fa_sap ( my_db_handle,
                                 s_sap_main,
                                 s_fa_nr,
                                 s_other_sap_9xxx_number ))) then
  begin
    debug2 ( format ( C_STR_ERR_SAP9xxx,
                  [ s_fa_nr,
                    s_other_sap_9xxx_number
                  ]));
    result := FALSE;
    exit;
  end;

  // Wenn es die Kombination Platinentypnummer / Fertigungsauftrag schon gibt,
  // können wir davon ausgehen, dass es vorher schon geprüft wurde und ein OK geben
  if ( exists_combination ( my_db_handle,
                            i_prodnr_functiontest,
                            s_fa_nr )) then
  begin
    // Die Kombination gab es schon
    // ==> alles OK
    b_fa_nr_valid := TRUE;
    debug2 ( 't_db_infoblock.check_type_highlevel: Kombi schon bekannt => OK');
    exit;
  end;

  // Wenn wir an dieser Stelle sind
  // liegt für diesen Fertigungsauftrag eine abweichende Platinentypnummer vor.
  // Es muss geprüft werden, ob für diese SAP-ID diese Platinetypnummer erlaubt ist.
  // Eventuelle Fehlermeldungen werden direkt in der Routine generiert.
  result := check_type_lowlevel ( my_db_handle,
                                  i_prodnr_functiontest,
                                  s_fa_nr,
                                  s_sap_main );

  b_fa_nr_valid := result;
end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : Debugausgabe (in Memo, sofern zugewiesen )
//    Parameter    : Auszugebender String
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-12-01
//    History      : --
// =============================================================================
procedure t_db_infoblock.debug2 ( s_debug : string );
begin
  if ( assigned ( fptr_debug )) then
        fptr_debug ( s_debug );
end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : perform_version_info:
//                   Sorgt für das Abspeichern der Versionsinformation, falls gewünscht
//    Parameter    : my_db : Zeiger auf Datenbankklasse
//                   vers_info : aktuelle Versionsinformationen
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-097-14
//    History      : --
// =============================================================================
procedure t_db_infoblock.perform_version_info (
                 my_db     : t_db_connection;
                 vers_info : t_db_vers_info );
var
  my_vers_query  : t_db_query;
  s_vers_store_cmd,                 // SQL-Kommando zum Speichern der Versions-Info
  s_vers_query   : string;          // Query-Kommanbo für die aktuelle Version
  my_handle      : t_mtx_db;

begin

  vers_info.set_vers_inx ( 0 );

  // ------------------------------------------------------------------
  // Behandlung der Versionsinformation
  // ------------------------------------------------------------------
  if ( not my_db.connected    ) then exit;
  if ( not vers_info.b_active )  then exit;

  // Prüfen, ob genau diese Versionsinformation bereits in der DB vorhanden ist
  s_vers_query := vers_info.gen_sql_query_string();
  my_vers_query := t_db_query.create_cmd( s_vers_query, TRUE );
  debug2 ( s_vers_query );

  my_handle := my_db.get_db_handle();
  my_handle.perform_query( my_vers_query );

  if ( my_vers_query.get_count() > 0 )  then
  begin
    // genau diese Versionsinformation wurde bereits einmal benutzt
    // ==> Index feststellen
    vers_info.set_vers_inx (
        StrToInt ( my_vers_query.get_value(0, 'Vers_Inx')));
  end
  else
  begin
    // diese Versionsinformation gibt es noch nicht in Automat.Pruef_Version
    // a) Datensatz speichern
    // b) Index feststellen des unter a) gespeicherten Datensatzes feststellen

    // a) Datensatz abspeichern
    s_vers_store_cmd := vers_info.gen_sql_insert_cmd();
    my_db.do_sql_cmd  ( s_vers_store_cmd  );       // Kommando ausführen

    // b) Index feststellen: jetzt müsste die Information vorhanden sein
    my_handle.perform_query( my_vers_query );
    if ( my_vers_query.get_count() > 0 )  then
    begin
      vers_info.set_vers_inx (
          StrToInt ( my_vers_query.get_value(0, 'Vers_Inx')));
    end
    else
    begin
      // das dürfte nicht auftauchen: Es wurde eine Versionsinfo geschrieben,
      // und gerade diesen Datensatz können wir jetzt nicht lesen...!
      debug2 ('Interner Programmierfehler 5563: gerade geschriebene ' +
      'Versionsinformation kann nicht aus DB gelesen werden! => Programmierer benachrichtigen!');
    end;
  end;
  my_vers_query.Free();

end;

// =============================================================================
//    Class        : t_db_infoblock
//    Funktion     : entferne_zeichen
//                   entfernt aus dem string "s" das Zeichen "weg"
//    Parameter    : -
//    Return       : -
//    Return       : Bemerkungsfeld
//    Exceptions   : --
//    First author : /aha/ 2011-12-07 (Hl. Nikolaus++)
//    History      :
// =============================================================================
function t_db_infoblock.entferne_zeichen ( weg: char; s: string ) : string;
var
  i: integer;
begin
  result := '';
  for i := 1 to length (s) do
    if ( s[i] <> weg ) then result := result + s[i];
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : max_str_len : kürzt einen String auf eine max. Länge,
//                   falls zu lang
//    Parameter    : s_in : Eingabestring
//                   lw_max : max. Länge
//    Return       : gekürzter String oder sonst Originalstring
//    Exceptions   : --
//    First author : /aha/ 2012-01-26
//    History      : ---
// =============================================================================
function t_db_infoblock.max_str_len ( s_in : string; lw_max : integer ) : string;
begin
  if ( length ( s_in ) > lw_max ) then
  begin
    result := copy ( s_in, 1, lw_max );   // String zu lang, kürzen
  end
  else
  begin
    result := s_in;     // Der String war nicht zu lang
  end;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : ps_as_string
//                   Prüfschritt als String zurückgeben
//    Parameter    : Prüfschritt
//    Return       : String
//    Exceptions   : --
//    First author : /aha/ 2016-0725
//    History      : --
// =============================================================================
function t_db_infoblock.ps_as_string(): string;
begin
  result := Format ('%.4f',     [my_error_info.f_pruefschritt]);
end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_remark : Bemerkungsfeld für fehlerhafte Prüfungen
//                   erzeugen
//    Parameter    : b_additional  : TRUE  : Info, dass Datensatz zusätzlich aus
//                                           anderem Test automatisch erzeugt wurde
//                                   FALSE : Keine Zusatzinfo
//    Return       : Bemerkungsfeld
//    Exceptions   : --
//    First author : /aha/ 2011-12-07 (Hl. Nikolaus++)
//    History      : /aha/ 2011-12-08 : String-Nachbearbeitung eingefügt
//                   /aha/ 2014-09-10 : Anpassungen an PDE_PRUEFREP
//                   /aha/ 2016-09-22 : neuer Parameter "b_additional"
// =============================================================================
function t_db_infoblock.gen_remark ( b_additional : boolean ) : string;
const
  C_MAX_REMARK_LEN = 255;  // Maximale Textlänge
  CS_ADDITIONAL =
    ' (Daten aus anderem Test automatisch übernommen)';

var
  s_result   : string;

begin
  s_result := ps_as_string()                 + '   ' +    // Prüfschritt
              my_error_info.s_function       + '   ' +    // Bezeichnung
              my_error_info.s_value_actual   + '   ' +    // aktuell
              my_error_info.s_value_ref;                  // Sollwert

  if ( b_additional ) then
  begin
    s_result := s_result + CS_ADDITIONAL;
  end;

  // ======================================================
  // String-Nachbearbeitungen:
  // ======================================================
  // <'> und <"> sind nicht erlaubt
  s_result := entferne_zeichen ( '''', s_result );
  s_result := entferne_zeichen ( '"', s_result );

  // ----------------------------------------------------
  // hier wird nur noch einmal zur sicherheit abgefragt,
  // ob der String die maximale Länge überschreitet...
  // ----------------------------------------------------
  if ( length ( s_result ) > C_MAX_REMARK_LEN ) then
  begin
    debug2 ('Gesamttext (Bemerkung) für DB zu lang, Kürzung, bitte SW-Programmierer informieren!' + s_result );
    s_result := max_str_len ( s_result, C_MAX_REMARK_LEN );
  end;

  // Übernahme ins Ergebnis
  result := s_result;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_db_add_command_function_test:
//                   Erzeugt SQL-Kommando zum Einschreiben
//                   in die Prüf-Datenbank!
//    Parameter    : lw_number      : Wert für DB-Feld "Automat.Pruef.Anzahl"
//                   s_user         : Benutzerkürzel
//                   b_auto_remark  : Hinweis, dass Kommentar automatisch erzeugt wurde
//    Return       : SQL-String für Prüfdatenbank
//    Exceptions   : --
//    First author : /aha/ 2011-12-06 (Nikolaus)
//    History      : /aha/ 2012-02-02: Prüfschritt nur Vorkommaanteil, kein Nachkomma!
//                   /aha/ 2012-02-26: Bugfix: "Automat.Pruef" (Korrekte Groß- und
//                                     Kleinschreibung für PDE-Server
//                   /aha/ 2012-10-23: Parameter eingeführt: lw_number
//                                     Prüfschrittnummer mit 2 Nachkommastellen
//                   /aha/ 2012-10-24: ID_FF als neue Spalte eingefügt
//                   /aha/ 2013-02-06: HW_Rev in DB mit eintragen
//                   /aha/ 2015-12-09: sap_main mit in DB eintragen (sofern vorhanden)
//                   /aha/ 2016-09-22: neuer Parameter "gen_auto_remark"
// =============================================================================
function t_db_infoblock.gen_db_add_command_function_test
                              ( lw_number     : longword;
                                s_user        : string;
                                b_auto_remark : boolean ) : string;
var
  s_tmp,
  s_sql            : string;
  s_remark         : string;     // Bemerkung
  s_result,                      // Ergebnis als String
  s_pruefschritt   : string;     // Prüfschritt als String

begin

  if ( b_result ) then
  begin
    // Prüfung wurde bestanden
    s_result       := 'OK';          // OK
    s_remark       := '';            // Keine Bemerkung
    s_pruefschritt := '';
  end
  else
  begin
    // Fehler in Prüfung
    s_result       := 'Fehler';      // Fehler
    s_remark       := gen_remark ( b_auto_remark );
    s_pruefschritt := ps_as_string();
  end;

  // Führende Nullen beim Prüfplatz entfernen.
  s_tmp := IntToStr ( StrToInt ( s_pruefplatz_nr ));

  s_sql  := 'INSERT INTO Automat.Pruef '                                             +
            '(Datum, Pruefplatz, Platinennummer, Seriennummer, '                     +
            'ID_FF, Produktart_Nr, Bearbeiter, '                                     +
            'Pruefschritt, Status, Anzahl, Bemerkung, FA_Nr, HW_Rev_Device, '        +
            ' Vers_Inx, sap_main ) VALUES ('                                         +
            '''' + self.s_starttime             + ''','          +  // Datum
            '''' + s_tmp                        + ''','          +  // Prüflatz-ID
            '''' + self.s_boardnumber           + ''','          +  // Platinennummer
            '''' + self.s_devicenumber          + ''','          +  // Seriennummer Endgerät
            '''' + s_ff_internal                + ''','          +  // Fremdfertiger
            '''' + IntToStr ( i_prodnr_functiontest ) + ''','    +  // Produktart
            '''' + s_user                       + ''','          +  // Bearbeiter
            '''' + s_pruefschritt               + ''','          +  // Prüfschritt
            '''' + s_result                     + ''','          +  // Status
            '''' + IntToStr ( lw_number)        + ''','          +  // Anzahl
            '''' + s_remark                     + ''','          +  // Bemerkung
            '''' + self.s_fa_nr                 + ''','          +  // Auftrags-Nummer
            '''' + self.my_versinfo.s_vers_hw1  + ''','          +  // HW-Rev
            '''' + self.my_vers_info_internal.get_vers_inx_s()   + ''','  +  // Versionsindex
            '''' + self.s_sap_main                               +  // SAP-Nr.
            ''');';

  result := s_sql;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_db_add_command_endurance_test:
//                   Erzeugt SQL-Kommando zum Einschreiben
//                   in den Dauertest
//    Parameter    : s_user  : "Eingelegt" und "Entnommen"
//    Return       : SQL-String für Prüfdatenbank
//    Exceptions   : --
//    First author : /aha/ 2016-08-08  übernommen aus gen_db_add_command_function_test
//    History      : --
// =============================================================================
function t_db_infoblock.gen_db_add_command_endurance_test
                        ( s_user           : string   ) : string;
var
  s_result,                      // Ergebnis als String
  s_sql            : string;
  s_err_short      : string;

begin

  // Komprimierten Fehlercode erstellen (falls notwendig)
  if ( length ( s_errcode ) > C_ERRCODE_OLD_LEN )  then
       s_err_short := copy ( s_errcode, 1, C_ERRCODE_OLD_LEN )
  else s_err_short := s_errcode;


  if ( b_result ) then  s_result       := 'OK'           // OK
  else                  s_result       := 'Fehler';      // Fehler

  s_sql  := 'INSERT INTO Konfig.Dauertest '                                          +
            '(Startzeit, FA_Nr, Endzeit, '                                           +
            'Typ_Nr, Endgeraetenummer, Fehlercode, Eingelegt, Entnommen, '           +
            'Bemerkung, Status, Fehlercode2, sap_id ) VALUES ('                      +
            '''' + self.s_starttime             + ''','          +  // Startzeit
            '''' + self.s_fa_nr                 + ''','          +  // Auftrags-Nummer
            '''' + self.s_endtime               + ''','          +  // Endzeit
            '''' + IntToStr ( i_prodnr_endurancetest ) + ''','   +  // Produktart
            '''' + s_devicenumber               + ''','          +  // Endgerätenummer
            '''' + s_err_short                  + ''','          +  // Fehlercode (kurz)
            '''' + s_user                       + ''','          +  // Eingelegt
            '''' + s_user                       + ''','          +  // Entnommen
            '''' + s_comment                    + ''','          +  // Bemerkung
            '''' + s_result                     + ''','          +  // Status
            '''' + s_errcode                    + ''','          +  // Fehlercode
            '''' + sap_main                     +                   // SAP-ID Hauptprodukt
            ''');';

  result := s_sql;
end;




// =============================================================================
//    Class        : t_db_infoblock
//    Function     : recent_data_exists:
//                   Prüft, ob für dieses Board, diesen Prüfplatz, diesen Prüfschritt
//                   und einen Zeitpunkt nicht mehr als 5 Minuten vor jetzt
//                   schon mal etwas eingetragen wurde
//    Parameter    : lw_id     : ID dieses Datensatzes, falls vorhanden
//                               sonst 0
//                   lw_anzahl : Wert des DB-Feldes "Anzahl", falls vorhanden
//                               sonst 0
//    Return       : TRUE      : Es existiert ein ähnlicher Datensatz
//                   FALSE     : kein Datensatz vorhanden
//    Exceptions   : --
//    First author : /aha/ 2012-10-23
//    History      : /aha/ 2012-10-24 Nicht Prüfplatz, sondern Produktart
//                                    ist für den Datensatz relevant
// =============================================================================
function t_db_infoblock.recent_data_exists (
                         my_db     : t_db_connection;
                         var lw_id,
                         lw_anzahl : longword ) :  boolean;
const
  CS_SQL_RECENT =
    'SELECT Pr_Nr, Anzahl FROM Automat.Pruef WHERE Platinennummer=''%s'' ' +
    'AND Produktart_Nr=%d AND Pruefschritt=''%s'' AND Status=''Fehler'' '     +
    'AND Datum>=''%s'';';

  C_SLIGHTLY_BEFORE =
    ( 5.0 / ( 24.0 * 60.0 ));  // 5 Minuten vor Jetzzeit

var
  my_query   : t_db_query;
  s_ps_nr    : string;

begin
  // Initialisierungen
  my_query := t_db_query.create();

  s_ps_nr  := ps_as_string();

  my_db.do_query ( my_query, format ( CS_SQL_RECENT,
             [
             self.s_boardnumber,                      // Platinennummer
             self.i_prodnr_functiontest,              // Produktart-ID
             s_ps_nr,                                 // Prüfschrittnummer
             get_time ( C_SLIGHTLY_BEFORE )           // vor 5 Minuten
             ] ));

  if ( my_query.get_count() > 0 ) then
  begin
    // Ein ähnlicher Datensatz wurde vor nicht sehr langer Zeit eingetragen!
    lw_id     := StrToInt ( my_query.get_value(0, 'Pr_Nr'));
    lw_anzahl := StrToInt ( my_query.get_value(0, 'Anzahl'));
    result    := TRUE;
  end
  else
  begin
    // keine Datensätze dieser Eigenschaft vorhanden
    lw_id     := 0;
    lw_anzahl := 0;
    result    := FALSE;
  end;

  my_query.Free();    // Klasse wieder freigeben
end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : gen_db_update_command: Erzeugt SQL-Kommando zum Updaten
//                   eines Datensatzes
//                   - aktuelles Datum/Uhrzeit
//                   - aktuelle Bemerkung
//                   - "Anzahl" aus Parameter
//    Parameter    : lw_id        : Schlüsselwert (Automat.Pruef.Pr_Nr)
//                   lw_number    : Wert für DB-Feld "Automat.Pruef.Anzahl"
//                   b_addtional  : Hinweis, dass es nur Zusatz-Datensatz ist
//    Return       : SQL-String für Prüfdatenbank
//    Exceptions   : --
//    First author : /aha/ 2012-10-24 (in Memoriam Sr. Faustina)
//    History      : /aha/ 2016-09-22: Neuer Parameter b_additional
// =============================================================================
function t_db_infoblock.gen_db_update_command ( lw_id, lw_number : longword;
                                                b_additional : boolean ) : string;
const
  CS_SQL_UPDATE1 =
    'UPDATE Automat.Pruef SET Datum=''%s'', '+
    'Anzahl=%d, Bemerkung=''%s'' WHERE Pr_Nr=%d;';
begin
  result := format ( CS_SQL_UPDATE1,
            [
            get_time( 0 ),                            // Datum
            lw_number,                                // Anzahl
            gen_remark ( b_additional ),              // Bemerkung
            lw_id                                     // Pr_Nr (Schlüssel)
            ]);

end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : get_time(): Zeit in DB-Format
//                   d_before = 0 : Jetztzeit
//                   d_before = 1 : 24 Stunden vor Jetztzeit
//    Parameter    : d_before
//    Return       : Zeitangabe als SQL-geeigneter String
//    Exceptions   : --
//    First author : /aha/ 2011-12-06 (Hl. Nikolaus)
//    History      : /aha/ 2012-10-23: Neuer Parameter "d_before"
// =============================================================================
function t_db_infoblock.get_time ( d_before : double ) : string;
var
  t_stamp      : TSQLTimeStamp;  // Konfigurationszeit
  d_date_time  : double;
begin

  d_date_time  := double ( now()) - d_before;
  t_stamp              := DateTimeToSQLTimeStamp ( TDateTime ( d_date_time ));
  result               := format ('%.4d-%.2d-%.2d %.2d:%.2d:%.2d',
                              [ t_stamp.Year,
                                t_stamp.Month,
                                t_stamp.Day,
                                t_stamp.Hour,
                                t_stamp.Minute,
                                t_stamp.Second ] );
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : get_actual_record:
//                   Erfragt den Index des aktuell geschriebenen Prüfdatensatzes
//    Parameter    : my_db    : Zeiger auf Datenbankklasse
//                   lw_inx   : Index des aktuellen Prüfdatensatzes
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2013-01-29
//    History      :
// =============================================================================
procedure t_db_infoblock.get_actual_record (
                             my_db       : t_db_connection;
                             var lw_inx  : longword );
const
  CS_SQL_RECENT =
    'SELECT Pr_Nr FROM Automat.Pruef WHERE Platinennummer=''%s'' ' +
    'AND Produktart_Nr=%d ORDER BY Datum DESC;';

var
  my_query   : t_db_query;

begin
  // Initialisierungen
  my_query := t_db_query.create();

  my_db.do_query ( my_query, format ( CS_SQL_RECENT,
             [
               self.s_boardnumber,                    // Platinennummer
               self.i_prodnr_functiontest             // Produktart-ID
             ] ));

  if ( my_query.get_count() = 0 ) then
  begin
    // Kann eigentlich nicht sein:
    // Ein Datensatz müsste kurz vorher eingetragen worden sein.
    debug2 ('t_db_connection.get_actual_record: SW-Strukturfehler! ' +
       'Kann eben geschriebnen Datensatz nicht wiederfinden!');
    lw_inx := 0;
  end
  else
  begin
    // Es können einer oder mehrere Datensätze geschrieben worden sein.
    // Es ist nur der neueste Datensatz interessant
    lw_inx := StrToInt ( my_query.get_value(0, 'Pr_Nr'));
  end;

  my_query.Free();    // Klasse wieder freigeben
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : write_test_result_function_test
//                   Test als Platinentest interpertieren und wegspeichern
//    Parameter    : my_db          : Zeiger auf Datenbankklassse
//                   lw_id          : Schlüsselwert bei Automat.Pruef
//                   b_additional   : FALSE : Eintrag nur bei Funktionstest
//                                    TRUE  : Zusatzeintrag von anderem Test (z.B. Dauertest)
//    Return       : --
//    Exceptions   : werden durchgereicht
//    First author : /aha/ 2016-07-25
//    History      : /aha/ 2016-09-22 Neuer Parameter "b_additional"
// =============================================================================
procedure t_db_infoblock.write_result_function_test (
                                      my_db         : t_db_connection;
                                      var lw_id     : longword;
                                      b_additional  : boolean );
var
  s_command         : string;       // Kommando
  lw_anzahl         : longword;     // "Anzahl"-Feld in Automat.Pruef

begin
  // ------------------------------------------------------------
  // Versuche das Ergebnis in die Datenbank einzutragen
  // ------------------------------------------------------------
  if ( self.b_result ) then
  begin
    // Prüfung wurde bestanden
    // ==> Datenbankkommando wie gewohnt schreiben
    s_command := gen_db_add_command_function_test ( 1, my_db.user_name, b_additional );  // Kommando zum Schreiben erzeugen
    my_db.do_sql_cmd  ( s_command    );       // Kommando ausführen
    get_actual_record ( my_db, lw_id );       // ID des aktuellen Datensatzes
  end
  else
  begin
    // ---------------------------
    // Fehler in Prüfung
    // ---------------------------
    if ( recent_data_exists( my_db,
                             lw_id,
                             lw_anzahl )) then
    begin
      // Kürzlich geschriebener Datensatz existiert!
      // Dieser Prüfplatz, dieses Ergebnis, dieser Prüfschritt, diese Platine
      // für diesen Datensatz:
      // => Anzahl inkrementieren
      // => Neue Bemerkung eintragen
      // => neue Uhrzeit eintragen
      s_command := gen_db_update_command ( lw_id, lw_anzahl+1, b_additional );
      my_db.do_sql_cmd  ( s_command  );       // Kommando ausführen

      // ID des Datensatzes exisitiert bereits, musss nicht noch mal
      // mit "get_actual_record" abgefragt werden!
    end
    else
    begin
      // Das Ergebnis der Prüfung ist zwar NOK, aber
      // es gibt keinen alten Datensatz, der hierzu passend wäre
      // Daher den Datensatz "normal" addieren
      s_command := gen_db_add_command_function_test ( 1,
                                                      my_db.user_name,
                                                      b_additional );  // Kommando zum Schreiben erzeugen
      my_db.do_sql_cmd        ( s_command     ); // Kommando ausführen
      get_actual_record       ( my_db, lw_id  ); // ID des aktuellen Datensatzes
    end;
  end;
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : write_test_result_endurance_test
//                   Test als Dauertest interpertieren und wegspeichern
//    Parameter    : my_db  : Zeiger auf Datenbankklassse
//    Return       : --
//    Exceptions   : werden durchgereicht
//    First author : /aha/ 2016-07-25
//    History      : --
// =============================================================================
procedure t_db_infoblock.write_result_endurance_test   ( my_db      : t_db_connection;
                                                      var lw_id  : longword );
var
  s_command         : string;       // Kommando

begin
  s_command := gen_db_add_command_endurance_test ( my_db.user_name );  // Kommando zum Schreiben erzeugen
  my_db.do_sql_cmd  ( s_command    );       // Kommando ausführen
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : perform_mess_info:
//                   Sorgt für das Abspeichern der Messinformation, falls gewünscht
//    Parameter    : my_db       : Zeiger auf Datenbankklasse
//                   mess_info   : aktuelle Messinformationen
//                   lw_vers_inx : Index der gerade geschriebenen Prüfdaten
//                                 (Automat.Pruef.Pr_Nr)
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
procedure t_db_infoblock.perform_mess_info ( my_db       : t_db_connection;
                                             mess_info   : t_db_mess_info;
                                             lw_vers_inx : longword );
var
  b_data_available : boolean;
  s_store_cmd      : string;

begin

  // ------------------------------------------------------------------
  // Behandlung der Messinformation
  // ------------------------------------------------------------------
  mess_info.set_pr_nr ( lw_vers_inx );  // Automat.Pr_Nr der Klasse bekanntmachen

  // erstes Datenpaket speichern (falls vorhanden)
  b_data_available := mess_info.store_first_data_packet_str ( s_store_cmd );
  if ( b_data_available ) then
  begin
    // Daten zum Abspeichern verfügbar!
    repeat
      my_db.do_sql_cmd ( s_store_cmd  );       // Speicherkommando ausführen

      // ggf. noch weitere Pakete abspeichern
      b_data_available := mess_info.store_next_data_packet_str ( s_store_cmd );
    until (not b_data_available);
  end;  // wenn Daten zum Abspeichern verfügbar

  mess_info.init();  // Anschließend alle Daten aus interner Liste löschen!
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : write_test_result: Schreibt das Ergebnis in die Datenbank.
//                   Nicht verbundene DB werden hier gleich abgefangen!
//    Parameter    : my_db  : Zeiger auf Datenbankklassse
//    Return       : --
//    Exceptions   : werden durchgereicht
//    First author : /aha/ 2016-07-19
//    History      : /aha/ 2016-09-22 Spezialbehandlung für NOK im
//                                    Dauertest implementiert
// =============================================================================
procedure t_db_infoblock.write_test_result ( my_db : t_db_connection );
const
  CS_SQL_FF_QUERY =
    'SELECT Name_Short FROM Konfig.fremdfertiger WHERE ID=''%s'';';
var
  s_command      : string;
  s_msg, s_error : string;
  lw_inx_comment : longword;     // den i.-ten Kommentar zu diesem Datensatz
  b_comment      : boolean;      // Flag: Kommentar vorhanden?
  lw_data_id1    : longword;     // Schlüsselwert des Datensatzes
  my_ff_query    : t_db_query;   // Für Fremdfertigerbestimmung
  my_db_handle   : t_mtx_db;     // Datenbank-Handle

begin

  if ( not my_db.connected ) then exit;     // nicht in eine nicht verbundene DB schreiben!

  // Prüfe, ob die Daten konsistent sind.
  if ( not check_consistency_fa ( my_db )) then
  begin
    // Daten für den Schreibvorgang sind nicht konsistent.
    s_error := 'Daten inkonsistent für Schreibvorgang';
    raise E_DATA_INCONSISTENT.Create ( s_error );
  end;

  if (( en_testmode = en_dbt_functiontest ) OR
      ( ( en_testmode = en_dbt_endurancetest) AND (not b_result))
     ) then
  begin
    // ----------------------------------------------------------------
    // Nur notwendig, wenn Funktionstestdaten eingetragen werden.
    // Platinennummer prüfen und Fremdfertiger bestimmen
    // ----------------------------------------------------------------
    if ( not my_db.check_boardnumber ( s_boardnumber )) then
    begin
      // Daten für den Schreibvorgang sind nicht konsistent.
      s_error := 'Platinennummer inkonsistent für Schreibvorgang';
      raise E_DATA_INCONSISTENT.Create ( s_error );
    end;

    my_ff_query := t_db_query.create_cmd (
                      format ( CS_SQL_FF_QUERY,
                               [copy ( s_boardnumber, 5,2 )]),
                      TRUE);
    my_db_handle := my_db.get_db_handle();
    my_db_handle.perform_query ( my_ff_query );

    s_ff_internal := my_ff_query.get_value(0, 'Name_Short');
    my_ff_query.Free();

    // ----------------------------------------------------------------
    // Wenn das Programm hier vorbeikommt, sind die Daten konsistent
    // ----------------------------------------------------------------
    perform_version_info ( my_db,
                           my_vers_info_internal ); // Behandlung der Versionsinformation
  end;

  // -----------------------------------------------------------------------
  // /aha/ 2016-09-21: auf Anforderung MRI: immer Endgerätenummer prüfen
  // -----------------------------------------------------------------------
  if ( not check_devicenumber ( s_devicenumber )) then
  begin
    // Daten für den Schreibvorgang sind nicht konsistent.
    s_error := 'Endgerätenummer inkonsistent für Schreibvorgang';
    raise E_DATA_INCONSISTENT.Create ( s_error );
  end;


  if ( en_testmode = en_dbt_endurancetest ) then
  begin
    // -----------------------------------------------------
    // Nur für Dauertest...!
    // Im Fehlerfall muss ein Fehlercode angegeben sein
    // -----------------------------------------------------
    if ( not self.b_result ) and ( self.s_errcode = '' ) then
    begin
      // Fehlercode bei NOK-Ereignis erforderlich
      s_error := 'Fehlercode-Angabe bei NOK-Ergebnissen im Dauertest erforderlich!';
      raise E_DATA_INCONSISTENT.Create ( s_error );
    end;
  end;

  // -----------------------------------------------------------------
  // Wenn das Programm hier vorbeikommt, sind die Daten konsistent
  // -----------------------------------------------------------------

  try
    // Eigentlicher Schreibvorgang des Ergebnisses
    case ( en_testmode ) of
      en_dbt_functiontest  : write_result_function_test  ( my_db, lw_data_id1, FALSE );
      en_dbt_endurancetest :
        begin
          write_result_endurance_test ( my_db, lw_data_id1 );

          // Hat der Dauertest ein NOK-Ergebnis, soll auch gleich im
          // Funktionstest für diese Platine ein NOK eingetragen werden
          if ( not b_result ) then
          begin
            write_result_function_test  ( my_db, lw_data_id1, TRUE );
          end;
        end
      else
      begin
        s_error := 'Programmfehler 123488';
        raise E_PROG_STRUCTURE.Create ( s_error );
      end;
    end;

    // ===================================================================
    // Hier die Messinformationen schreiben (falls vorhanden+gewünscht)
    // ===================================================================
    if ( mess_info ) then
    begin
      perform_mess_info ( my_db,
                          my_mess_info,
                          lw_data_id1 );
    end;

    // -------------------------------------------------------------
    // Folgendes nur für den Funktionstest machen...
    // -------------------------------------------------------------
    if ( en_testmode = en_dbt_functiontest ) then
    begin
      // ===================================================================
      // Kommentare (sofern vorhanden) in die Datenbank mit eintragen
      // ===================================================================
      lw_inx_comment := 0;
      repeat
        b_comment := my_pde_comment.get_add_commentstring (
                        lw_data_id1,
                        lw_inx_comment,
                        s_command );
        if ( b_comment ) then
        begin
          my_db.do_sql_cmd   ( s_command );  // Kommentar vorhanden => Eintragen in DB
        end;
        inc ( lw_inx_comment );
      until ( not b_comment  );
    end;

  except on E:Exception do
  begin
    s_msg :='Beim Schreiben der Ergebnisdaten in die Datenbank ist ein Fehler aufgetreten!' + char(13) +
            'Die Daten wurden nicht in die Datenbank übernommen.' + char (13) +
            'Fehler beim Einschreiben in PDE: ' + E.Message;
    debug2 ( s_msg );
  end;
  end;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : set_en_testmode
//                   setzt den Testmodus (Funktionstest, Dauertest)
//                   Zurücksetzen Prüfplatz- und Produktart-ID
//                   Zurücksetzen Gültigkeitsflag FA-Nummer
//    Parameter    : e : siehe "Function"
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-08-08
//    History      : --
// =============================================================================
procedure t_db_infoblock.set_en_testmode ( e : t_en_db_testmode );
begin
  en_testmode_internal    := e;
  reset_pruefplatz_produktart();
  reset_fa_nr_valid          ();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : set_mess_info
//                   aktiviert bzw. deaktiviert das Abspeichern von Messwerten
//    Parameter    : TRUE  : Messwerte speichern
//                   FALSE : keine Messwerte speichern
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-09-01
//    History      : --
// =============================================================================
procedure t_db_infoblock.set_mess_info ( b : boolean );
begin
  my_mess_info.b_active := b;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : get_mess_info
//                   Info: Ist Messwertspeicherung aktiv?
//    Parameter    : --
//    Return       : TRUE  : Messwerte speichern
//                   FALSE : keine Messwerte speichern
//    Exceptions   : --
//    First author : /aha/ 2016-09-01
//    History      : --
// =============================================================================
function t_db_infoblock.get_mess_info  ( ) : boolean;
begin
  result :=  my_mess_info.b_active;
end;


// =============================================================================
//    Class        : t_db_infoblock
//    Function     : set_sap_main
//                   setzt main-sap
//                   Zurücksetzen Prüfplatz- und Produktart-ID
//                   Zurücksetzen Gültigkeitsflag FA-Nummer
//    Parameter    : s : siehe "Function"
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_infoblock.set_sap_main         ( s : string );
begin
  s_sap_main := s;
  reset_pruefplatz_produktart();
  reset_fa_nr_valid          ();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : set_id_pruefplatz
//                   setzt Prüfplatz-String
//                   Zurücksetzen Prüfplatz- und Produktart-ID
//                   Zurücksetzen Gültigkeitsflag FA-Nummer
//    Parameter    : s : siehe "Function"
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_infoblock.set_id_pruefplatz    ( s : string );
begin
  s_id_pruefplatz := s;
  reset_pruefplatz_produktart();
  reset_fa_nr_valid          ();
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : set_fa_nr
//                   setzt FA-Nummer
//                   Zurücksetzen Gültigkeitsflag FA-Nummer
//    Parameter    : s : siehe "Function"
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_infoblock.set_fa_nr ( s : string );
begin
  s_fa_nr := s;
  reset_fa_nr_valid          ();
end;



// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// ============= Klasse t_db_connection ========================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================
// =============================================================================

// =============================================================================
//    Class        : t_db_connection
//    Function     : create: Konstruktor
//                   Datenbank erstmal als "inaktiv" markieren
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-11-29
//    History      : /aha/ 2016-07-18  : Anpassung auf neues Interface
//                   /aha/ 2016-08-17  : Create ohne Parameter
// =============================================================================
constructor t_db_connection.create ( );
const
  C_DEFAULTLEN_FA = 8;    // Länge Fertigungsauftrag

begin

  // -----------------------------------------------------
  // Initialwerte herstellen
  // -----------------------------------------------------
  s_version_internal    := C_STR_VERSION_DB_INTERFACE; // Versionsinfo
  s_user_name_internal  := '';                         // angemeldeter Benutzer
  fptr_debug            := nil;                        // Keine Debug-Routine

  s_conn_internal       := '';                         // Verbindungsstring leer

  my_pde_db             := t_mtx_db.create_plain();    // Datenbank-Handle
  b_connect_internal    := FALSE;                      // DB nicht verbunden

  i_min_fa_len          := C_DEFAULTLEN_FA;            // minimale Länge FA
  i_max_fa_len          := C_DEFAULTLEN_FA;            // maximale Länge FA

  temp_query            := t_db_query.create();        // allgeiner Query erzeugen
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : destroy: Destruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2011-11-29
//    History      : --
// =============================================================================
destructor t_db_connection.Destroy;
begin
  temp_query.Free();
  my_pde_db.Free();
  inherited;
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : check_ff_id: Prüfung, ob der Fremdfertiger existiert
//    Parameter    : ID (String mit 2 char)
//    Return       : TRUE  : so einen Fremdfertiger gibt es
//                   FALSE : Fremdfertiger unbekannt
//    Exceptions   : --
//    First author : /aha/ 2016-07-19
//    History      : --
// =============================================================================
function t_db_connection.check_ff_id  ( s_id  : string ) : boolean;
const
  CS_SQL_FF_QUERY =
    'SELECT * FROM Konfig.fremdfertiger where ID=''%s'';';
var
  my_query : t_db_query;

begin
   result := TRUE;
   if ( not connected ) then exit;

   my_query := t_db_query.create_cmd( format ( CS_SQL_FF_QUERY, [s_id]),
                                      TRUE );
   try
     my_pde_db.perform_query( my_query );
     result := my_query.get_count > 0;
   except
     raise E_SQL_GENERAL.Create ( 'SQL-Fehler bei: ' + my_query.s_query );
   end;

   my_query.Free();
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : get_db_handle
//                   Datenbankhandle zurückliefern, falls verbunden
//    Parameter    : --
//    Return       : Datenbankhandle
//    Exceptions   : bei nicht verbundener Datenbank
//    First author : /aha/ 2016-07-21
//    History      : --
// =============================================================================
function t_db_connection.get_db_handle() : t_mtx_db;
const
  CS_ERR = 't_db_connection.get_db_handle(): nicht verbundene Datenbank!';
var
  s_error : string;
begin
  if ( not connected ) then
  begin
    s_error := CS_ERR;
    debug1 ( s_error );
    raise E_DB_GENERAL.Create ( s_error );
  end
  else result := my_pde_db;
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : do_query   : Query allgemein durchführen
//                                Debugausgabe durchführen
//    Parameter    : my_query   : Anfrageverwaltung
//                   s_sql      : SQL-Syntax
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2012-05-10
//    History      : --
// =============================================================================
procedure t_db_connection.do_query ( my_query : t_db_query; s_sql   : string );
begin
  my_query.clear();
  my_query.b_include_all := TRUE;
  my_query.s_query       := s_sql;

  debug1 ( s_sql );
  my_pde_db.perform_query( my_query );
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : do_sql_cmd   : SQL-Kommando ausführen
//    Parameter    : s_cmd      : Kommando
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2012-05-10
//    History      : --
// =============================================================================
procedure t_db_connection.do_sql_cmd ( s_cmd : string );
begin
  debug1 ( s_cmd );
  my_pde_db.perform_command ( s_cmd );
end;


// =============================================================================
//    Class        : t_db_connection
//    Function     : get_connect_internal
//                   mit Datenbank verbunden?
//    Parameter    : --
//    Return       : TRUE   : verbunden
//                   FALSE  : nicht verbunden
//    Exceptions   : --
//    First author : /aha/ 2011-11-30
//    History      : --
// =============================================================================
function t_db_connection.get_connect_internal() : boolean;
begin
  result := b_connect_internal;
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : get_user_level()
//                   gibt den Benutzer level raus
//    Parameter    :
//    Return       : user level
//    Exceptions   : --
//    First author : /lzh/ 2013-04-09
//    History      : /aha/lzh/ Defaultlevel eingeführt.
//                   /aha/ 2016-07-27 Benutzerlevel letzte Dezimalstelle von
//                                    Konfig.user_level
// =============================================================================
function t_db_connection.get_user_level() : integer;
var
  my_query : t_db_query;
  s_query  : string;
  s_err    : string;    // Fehlerstring
  s_level  : string;
begin

  // ----------------------------------------------
  // Initialisierung
  // ----------------------------------------------
  result := 0;
  s_err := '';

  if ( not connected ) then exit;  // nicht verbunden

  // ----------------------------------------------
  // Datenbankanfrage starten und auswerten...
  // ----------------------------------------------
  try
    s_query := format ( 'SELECT * FROM Konfig.user_level where ' +
                        'user="%s";', [s_user_name_internal]);

    my_query := t_db_query.create_cmd( s_query, TRUE );
    debug1 ( s_query );
    my_pde_db.perform_query( my_query );

    case ( my_query.get_count()) of
      0 :
        begin
          // Benutzer ist unbekannt
          // ==> user_level= 1 (minimale Stufe) 2013-07-02 /lzh/aha/
          result := 1;
        end;
      1 :
        begin
          // -----------------------------------------
          // genau 1 Eingtrag
          // -----------------------------------------
          s_query := format ( 'SELECT level FROM Konfig. user_level where ' +
                              'user="%s";', [s_user_name_internal]);

          my_query.clear();
          my_query.s_query := s_query;
          my_query.b_include_all := TRUE;
          debug1 ( s_query );
          my_pde_db.perform_query( my_query );

          // Bearbeitung der Benutzerlevel
          s_level := my_query.get_value(0, 'level');
          result  := (StrToInt ( s_level)  MOD 10);
          if (result=0) then result:= 1;
        end
      else
      begin
        // merkwürdiger Fehler (sollte nicht vorkommen):
        // Prüfplatzbezeichnung mehrfach vorhanden!
        s_err := format ( 'Interner Fehler: Benutzername <%s> sind mehrfach in Datenbank vorhanden(?)',
                 [s_user_name_internal]);
        raise ETooManyObjects.Create( s_err );
      end;
    end;

  except On E:Exception do
  begin
    // Wenn es eine unerwartete Exception gibt, diese benennen!
    if ( s_err = '' ) then
    begin
      s_err := format ('Unerwartete Exception (%s) aufgetreten bei Query (%s)', [E.ClassName, s_query]);
    end;
    debug1 ( s_err );
    exit;
  end;  // begin (try)
  end;  // except

  my_query.Free();

end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : perform_excel_query : Führt ein Query aus
//                   Ergebnis in Excel-Tabelle, sofern vorhanden!
//    Parameter    : s_query   : Query in SQL-Syntax
//    Return       : TRUE      : Aufruf erfolgreich und Daten vorhanden
//                   FALSE     : Aufruf nicht erfolgreich ODER
//                               keine Daten
//    Exceptions   : bei nicht vorhandener Datenbank
//    First author : /aha/ 2011-12-01
//    History      : --
// =============================================================================
function t_db_connection.perform_excel_query ( s_query : string ) : boolean;
var
  my_admin_query : t_db_query;
  s_error        : string;

begin

  result := FALSE;     // pessimistischer Ansatz

  try

    my_admin_query := t_db_query.create_cmd( s_query, TRUE );
    debug1 ( s_query );
    my_pde_db.perform_query( my_admin_query );

    if ( my_admin_query.get_count() <> 0 ) then
    begin
      my_admin_query.export_to_excel();
      result := TRUE;
    end
    else
    begin
      debug1 ('t_db_connection.perform_excel_query: leeres Ergebnis');
    end;
  except On E:Exception do
  begin
    s_error := 'Fehler in Excel-Export: ' + E.Message;
    debug1 ( s_error );
    exit;
  end;  // begin (try)
  end;  // except

  debug1 ( format ( 'Kommando (%s) bearbeitet', [s_query]));
  my_admin_query.Free();
end;


// =============================================================================
//    Class        : t_db_connection
//    Function     : Debugausgabe (sofern zugewiesen )
//    Parameter    : Auszugebender String
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-08-17
//    History      : --
// =============================================================================
procedure t_db_connection.debug1 ( s_debug : string );
begin
  if ( assigned ( fptr_debug ))  then fptr_debug ( s_debug );
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : temp_peform_query
//                   Ausführung eines temporären Queries, die Ergebnisse
//                   werden nicht gelöscht.
//                   Man kann später mit "temp_get_field" darauf zugreifen.
//    Parameter    : s_query    : Query
//                   i_num_data : Anzahl der gefundenden Daten
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
procedure t_db_connection.temp_perform_query ( s_query : string;
                                            var i_num_data : integer );
var
  s_error  : string;

begin
  if ( not connected ) then
  begin
    s_error := 'temp_perform_query: nicht verbundene DB';
    raise E_SQL_GENERAL.Create ( s_error );
  end;

  debug1 ( s_query );
  temp_query.clear();
  temp_query.b_include_all := TRUE;
  temp_query.s_query       := s_query;

  // Abfrage durchführen
  self.my_pde_db.perform_query ( temp_query );

  i_num_data := temp_query.get_count();
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : temp_get_field
//                   Feldabfrage eines zuvor ausgeführten Queries
//                   (temp_perform_query)
//    Parameter    : inx        : Index des gewünschten Datensatzes
//                   s_field    : Feldname
//    Return       : Feldwert
//    Exceptions   : bei nicht verbundener DB oder
//                   ungültigem Index oder
//                   ungültigem Feldnamen
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
function t_db_connection.temp_get_field
                            ( inx : integer;
                              s_field : string ) : string;
var
  s_error  : string;

begin
  if ( not connected ) then
  begin
    s_error := 'temp_get_field: nicht verbundene DB';
    raise E_SQL_GENERAL.Create ( s_error );
  end;

  if ( ( inx < 0 ) OR
       ( inx > temp_query.get_count()-1 ))  then
  begin
    s_error := 'temp_get_field: Indexproblem';
    raise E_DBCONN_InvalidArg.Create ( s_error );
  end;

  try
    result := temp_query.get_value ( inx, s_field );
  except
    s_error := 'temp_get_field: unbekannter Feldname ' + s_field;
    raise E_DBCONN_InvalidArg.Create ( s_error );
  end;

end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : login
//                   Anmeldeversuch
//    Parameter    : Benutzername
//                   Passwort
//    Return       : TRUE   : Anmeldung erfolgreich
//                   FALSE  : Fehler bei Anmeldung
//    Exceptions   : Exceptions werden hier abgefangen
//    First author : /aha/ 2016-08-23
//    History      : --
// =============================================================================
function t_db_connection.login ( s_user, s_password : string ) : boolean;
var
  s_error : string;
begin

  result := FALSE;

  // Wenn bereits Verbindung besteht, diese abbauen.
  if ( connected ) then
  begin
    // Verbindung abbauen
    my_pde_db.Logout();
    b_connect_internal := FALSE;
  end;

  //
  //  Wenn das Programm hier vorbeikommt, gibt es keine Verbindung!
  //

  // ===========================================
  // Verbindung aufbauen
  // ===========================================
  if (( s_conn_internal = '')  or
      ( s_conn_internal = '*')) then
  begin
    // Verbindungsstring ungültig
    s_error := format ('t_db_connection.set_connect: ungültiger DB-Connector:<%s>',
               [s_conn_internal]);
    debug1 ( s_error );
    raise E_DBCONN_InvalidArg.Create ( s_error );
  end;

  my_pde_db.set_conn_string ( s_conn_internal ); // Connect-String übergeben

  try

  my_pde_db.login ( s_user, s_password );   // Intern den Login mit User und Passwort versuchen

  b_connect_internal   := TRUE;
  s_user_name_internal := s_user;
  result               := TRUE;

  except On E:Exception do
  begin
    // ---------------------------------------------
    // Verbindung zur Datenbank konnte mit diesen Daten
    // nicht aufgebaut werden
    // ---------------------------------------------
    s_user_name_internal   := '';
    b_connect_internal     := FALSE;
    s_error := 'Das Programm kann mit diesen Daten keine Verbindung zur Datenbank herstellen!';
    debug1 ( s_error );
  end;
  end; // << begin/except
end;

// =============================================================================
//    Class        : t_db_connection
//    Function     : logout
//                   bei vorhandener Verbindung diese trennen
//    Parameter    : ---
//    Return       : ---
//    First author : /aha/ 2016-08-25
//    History      : --
// =============================================================================
procedure t_db_connection.logout ( );
begin

  // Wenn bereits Verbindung besteht, diese abbauen.
  if ( connected ) then
  begin
    // Verbindung abbauen
    my_pde_db.Logout();
    b_connect_internal   := FALSE;
    s_user_name_internal := '';
  end;
end;

// =============================================================================
//    Class        : t_db_infoblock
//    Function     : check_boardnumber
//                   Prüft die Platinennummer
//                   und stellt den Fremdfertiger fest (sofern DB-Verbindung besteht)
//    Parameter    : s_board   : Platinennummer
//    Return       : TRUE      : Platinennummer OK
//                   FALSE     : Fehler
//    Exceptions   : --
//    First author : /aha/ 2016-07-27
//    History      : --
// =============================================================================
function t_db_connection.check_boardnumber ( s_board : string ) : boolean;
const
  C_BOARDNUM_LEN = 10;    // Platinennummern sind immer 10 Zeichen lang!

var
  s_tmp,
  s_err        : string;
  i_tmp,
  i            : integer;

begin

  result := FALSE;  // Pessimisitische Initialisierung

  if ( ( s_board <> trim ( s_board )) OR
       ( length ( s_board ) <> C_BOARDNUM_LEN )) then
  begin
    // Falsche Länge Platinennummer
    s_err := 'Platinennummer: falsche Länge / falscher Aufbau! ' + s_board;
    debug1 ( s_err );
    exit;
  end;

  // Bis hierher war die Zeichenkette in Ordnung.
  // Hier nochmals explizit auf Ziffern prüfen!
  for i := 1 to length ( s_board ) do
  begin
    if ( ( integer ( s_board[i] ) < integer ('0')) OR
         ( integer ( s_board[i] ) > integer ('9'))) then
    begin
      // hier gibt es doch einen Fehler
      s_err := 'Fehler: Platinennummer besteht nicht nur aus Ziffern! ' + s_board;
      debug1 ( s_err );
      exit;
    end;  // if Fehler...
  end;  // For-Schleife
  // Die Platinennummer besteht aus genau 10 Zeichen!

  // Bis hierher war die Zeichenkette in Ordnung.
  // Jetzt Kalenderwoche (01..53) prüfen
  i_tmp := StrToInt ( copy ( s_board, 3, 2 ));
  if ( ( i_tmp < 1) OR ( i_tmp > 53 )) then
  begin
    // Kalenderwoche fehlerhaft
    s_err := format ( 'Fehler Platinennummer (%s): Kalenderwoche nicht in 00..53',
                      [s_board]);
    debug1 ( s_err );
    exit;
  end;  // if Fehler...
  // Die Kalenderwoche ist OK

  // Bis hierher war die Zeichenkette in Ordnung.
  // Jetzt Fremdfertiger prüfen, falls PDE-Verbindung
  if  ( connected ) then
  begin
    // Datenbankverbindung besteht: prüfen, ob die FF-ID bekannt ist
    s_tmp  := copy ( s_board, 5, 2 );
    if ( not  check_ff_id ( s_tmp )) then
    begin
      // FF-ID unbekannt
      s_err := format ('Fehler: Fremdfertiger ID (%s) unbekannt!', [s_tmp]);
      debug1 ( s_err );
      exit;
    end;
  end;

  // Wenn das Programm hier vorbeikommt, ist alles OK
  result := TRUE;
end;

// TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
// TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
// TTTTTTTTTTTTTTTT t_db_comment TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
// TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
// TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

// =============================================================================
//    Class        : t_db_comment
//    Function     : Konstuktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2013-01-25
//    History      : /aha/ 2016-07-19 ab jetzt ohne interne DB
// =============================================================================
constructor t_db_comment.Create ( );
begin
  tsl_comments   := TStringList.Create();
  clear();
end;

// =============================================================================
//    Class        : t_db_comment
//    Function     : Destruktor
//    Parameter    : --
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2013-01-25
//    History      : --
// =============================================================================
destructor  t_db_comment.Destroy;
begin
  Clear();
  tsl_comments.Free();
  inherited;
end;

// =============================================================================
//    Class        : t_db_comment
//    Function     : add_comment: Nimmt den Kommentar in die interne Liste
//                   für das spätere Speichern auf. Wenn die Nummer jedoch schon
//                   vorhanden ist, wird dies nicht gemacht. Es gibt aber keine
//                   Fehlermeldung
//    Parameter    : i_comment : aufzunehmende Kommentarnummer
//    Return       : -
//    Exceptions   : --
//    First author : /aha/ 2013-01-25
//    History      : --
// =============================================================================
procedure   t_db_comment.add_comment ( i_comment : integer );
begin
  if ( not comment_exists ( i_comment )) then
  begin
    tsl_comments.Add(IntToStr(i_comment));
  end;
end;

// =============================================================================
//    Class        : t_db_comment
//    Function     : clear: Löscht die interne Liste der Kommentare
//    Parameter    : --
//    Return       : -
//    Exceptions   : --
//    First author : /aha/ 2013-01-25
//    History      : --
// =============================================================================
procedure t_db_comment.clear();
begin
  tsl_comments.Clear();
end;

// =============================================================================
//    Class        : t_db_comment
//    Function     : comment_exists : prüft, ob der Kommentar schon
//                   in der internen Liste aufgenommen wurde
//    Parameter    : i_comment : zu prüfender Kommentar
//    Return       : TRUE      : bereits in Liste
//                   FALSE     : noch nicht in Liste
//    Exceptions   : --
//    First author : /aha/ 2013-01-25
//    History      : --
// =============================================================================
function t_db_comment.comment_exists ( i_comment : integer ) : boolean;
var
  i  : integer;

begin
  result := FALSE;
  for i := 0 to tsl_comments.Count-1 do
  begin
    if ( IntToStr ( i_comment ) = tsl_comments.Strings[i] ) then
    begin
      result := TRUE;   // Übereinstimmung gefunden
      exit;
    end;
  end;
end;

// =============================================================================
//    Class        : t_db_comment
//    Function     : get_add_commentstring :
//                   für einen bestimmten Datensatz einen
//                   bestimmten Kommentar einfügen:
//                   hierzu den SQL-Befehl erzeugen und zurückliefern!
//    Parameter    : w_inx_record  : Pr_Nr in PDE.Automat.Pruef (Schlüssel Prüf-Datensatz)
//                   w_inx_comment : Kommentarindex in Liste "tsl_comments"
//                   s_command     : resultierendes SQL-Kommando, leer, wenn
//                                   Listenindex nicht gefüllt ist
//    Return       : TRUE          : Kommentar vorhanden => s_command ist gefüllt
//                   FALSE         : für diesen w_inx_comment kein Kommentar
//                                   => s_command ist leer
//    Exceptions   : --
//    First author : /aha/ 2013-01-29
//    History      : --
// =============================================================================
function t_db_comment.get_add_commentstring (
            lw_inx_record,
            lw_inx_comment : longword;
            var s_command  : string ) : boolean;
const
  CS_SQL_ADDCOMMENT =
    'INSERT INTO Automat.Pruefcomment (Pr_Nr, Pruefcomment_ID) ' +
    ' VALUES ( %d, %s);';
begin

  if ( lw_inx_comment >= longword ( tsl_comments.Count )) then
  begin
    // für diesen Index ist kein Kommentar eingetragen
    s_command     := '';
    result        := FALSE;
  end
  else
  begin
    // Kommentar vorhanden => String zurechtbasteln
    result        := TRUE;
    s_command     := format ( CS_SQL_ADDCOMMENT, [
                              lw_inx_record,
                              tsl_comments.Strings [lw_inx_comment]
                              ]);
  end;
end;


// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : Create (Konstruktor)
//    Parameter    : my_block : Verweis auf t_db_infoblock (für Debug-Ausgaben)
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : /aha/ 2016-08-30: Argument in Konstruktor
// =============================================================================
constructor t_db_vers_info.Create ( my_block : t_db_infoblock );
begin
  my_infoblock_internal := my_block;
  b_active              := FALSE;    // Defaultwert: Info inaktiv
  s_vers_inx_internal   := '0';
  init_all();
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : init_variant_versions
//                   variantenabhängige Versionsinformationen löschen
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
procedure t_db_vers_info.init_variant_versions();
begin
  versions_internal.s_version_psl       := '';

  versions_internal.s_version_bl1       := '';
  versions_internal.s_version_fw1       := '';
  versions_internal.s_version_hw1       := '';

  versions_internal.s_version_bl2       := '';
  versions_internal.s_version_fw2       := '';
  versions_internal.s_version_hw2       := '';

  versions_internal.s_version_bl_simple := '';
  versions_internal.s_version_fw_simple := '';
  versions_internal.s_version_hw_simple := '';
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : init_all
//                   ALLE Versionsinformationen löschen
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
procedure t_db_vers_info.init_all();
begin
  // Basisinformatoin zurücksetzen
  versions_internal.s_version_exe := '';
  versions_internal.s_version_ini := '';

  init_variant_versions();
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : gen_sql_query_string
//                   SQL-Kommando zum Abfragen der Versionsinfo erzeugen
//    Parameter    : ---
//    Return       : SQL-Kommando zum Abfragen der Versionsinfo
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
function  t_db_vers_info.gen_sql_query_string() : string;
begin

  result := format (
  'SELECT Vers_Inx  FROM Automat.Pruef_Version WHERE ' +
  'Version_EXE=''%s'' AND ' +
  'Version_INI=''%s'' AND ' +
  'Version_PSL=''%s'' AND ' +
  'Version_BL1=''%s'' AND ' +
  'Version_FW1=''%s'' AND ' +
  'Version_HW1=''%s'' AND ' +
  'Version_BL2=''%s'' AND ' +
  'Version_FW2=''%s'' AND ' +
  'Version_HW2=''%s'' ;',
  [
    self.versions_internal.s_version_exe,
    self.versions_internal.s_version_ini,
    self.versions_internal.s_version_psl,
    self.versions_internal.s_version_bl1,
    self.versions_internal.s_version_fw1,
    self.versions_internal.s_version_hw1,
    self.versions_internal.s_version_bl2,
    self.versions_internal.s_version_fw2,
    self.versions_internal.s_version_hw2
  ]);
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : init_all
//                   SQL-Kommando zum Speichern der Versionsinfo erzeugen
//    Parameter    : ---
//    Return       : SQL-Kommando zum Speichern der Versionsinfo
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
function t_db_vers_info.gen_sql_insert_cmd() : string;
begin
  result := format (
  'INSERT INTO Automat.Pruef_Version ( ' +
  'Version_EXE , ' +
  'Version_INI , ' +
  'Version_PSL , ' +
  'Version_BL1 , ' +
  'Version_FW1 , ' +
  'Version_HW1 , ' +
  'Version_BL2 , ' +
  'Version_FW2 , ' +
  'Version_HW2 ) VALUES ( ' +
  '''%s'' ,  ' +     // EXE
  '''%s'' ,  ' +     // INI
  '''%s'' ,  ' +     // PSL
  '''%s'' ,  ' +     // BL1
  '''%s'' ,  ' +     // FW1
  '''%s'' ,  ' +     // HW1
  '''%s'' ,  ' +     // BL2
  '''%s'' ,  ' +     // FW2
  '''%s'' ); ',      // HW2
  [
    self.versions_internal.s_version_exe,
    self.versions_internal.s_version_ini,
    self.versions_internal.s_version_psl,
    self.versions_internal.s_version_bl1,
    self.versions_internal.s_version_fw1,
    self.versions_internal.s_version_hw1,
    self.versions_internal.s_version_bl2,
    self.versions_internal.s_version_fw2,
    self.versions_internal.s_version_hw2
  ]);
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : set_vers_info
//                   Versionsinformation setzen
//    Parameter    : my_info_type : welche Versionsinformation soll gesetzt werden?
//                   s_vers       : Revisisionsstring der Version
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
procedure t_db_vers_info.check_length (
                         var s_test : string;
                         w_maxlen : word;
                         s_fieldname : string );
var
  s_tmp, s_msg : string;

begin
  if ( length ( s_test ) > w_maxlen ) then
  begin
    // Die Länge des Testfelds überschreitet die Ziellänge
    // ==> Feld kürzen
    // ==> Benutzer informieren
    s_tmp  := s_test;
    s_test := copy ( s_tmp, 1, w_maxlen );
    s_msg  := 'Die Versionsinformation für das Feld %s überschreitet die maximal ' +
              'erlaubte Länge von %d Zeichen für das DB-Feld %s.' + #13 +
              'Die Versionsinformation wird daher von <%s> ' + #13 +
              'auf <%s> gekürzt!' + #13 +
              'Informieren Sie den Programmierer über dieses Problem!';
    my_infoblock_internal.debug2 (
       format ( s_msg, [w_maxlen, s_fieldname, s_tmp, s_test]));
  end;
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : set_vers_info
//                   Versionsinformation setzen
//    Parameter    : my_info_type : welche Versionsinformation soll gesetzt werden?
//                   s_vers       : Revisisionsstring der Version
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : 2015-01-30 /bsu/ Bugfix: Stringlänge für vinfo_ini
// =============================================================================
procedure t_db_vers_info.set_vers_info (
             my_info_type : t_vers_enum;
             s_vers : string );
const
  C_LEN_EXE    = 25;
  C_LEN_INI    = 20;
  C_LEN_PSL    = 20;
  C_LEN_BL     = 25;
  C_LEN_FW     = 25;
  C_LEN_HW     = 25;

begin
  case ( my_info_type ) of
    vinfo_exe :
      begin
        versions_internal.s_version_exe := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_EXE,
                       'Automat.Pruef_Version.Version_EXE');
      end;
    vinfo_ini :
      begin
        versions_internal.s_version_ini := s_vers;
        check_length ( versions_internal.s_version_exe,
                       {*C_LEN_EXE*}C_LEN_INI,
                       'Automat.Pruef_Version.Version_INI');
      end;
    vinfo_psl :
      begin
        versions_internal.s_version_psl := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_PSL,
                       'Automat.Pruef_Version.Version_PSL');
      end;
    vinfo_bl1 :
      begin
        versions_internal.s_version_bl1 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_BL,
                       'Automat.Pruef_Version.Version_BL1');
      end;
    vinfo_fw1 :
      begin
        versions_internal.s_version_fw1 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_FW,
                       'Automat.Pruef_Version.Version_FW1');
      end;
    vinfo_hw1 :
      begin
        versions_internal.s_version_hw1 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_HW,
                       'Automat.Pruef_Version.Version_HW1');
      end;
    vinfo_bl2 :
      begin
        versions_internal.s_version_bl2 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_BL,
                       'Automat.Pruef_Version.Version_BL2');
      end;
    vinfo_fw2 :
      begin
        versions_internal.s_version_fw2 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_FW,
                       'Automat.Pruef_Version.Version_FW2');
      end;
    vinfo_hw2 :
      begin
        versions_internal.s_version_hw2 := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_HW,
                       'Automat.Pruef_Version.Version_HW2');
      end;

    // Simpel - Informationen
    vinfo_bl_simple :
      begin
        versions_internal.s_version_bl_simple := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_BL,
                       'Automat.Pruef_Version.Version_BLx');
      end;
    vinfo_fw_simple :
      begin
        versions_internal.s_version_fw_simple := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_FW,
                       'Automat.Pruef_Version.Version_FWx');
      end;
    vinfo_hw_simple :
      begin
        versions_internal.s_version_hw_simple := s_vers;
        check_length ( versions_internal.s_version_exe,
                       C_LEN_HW,
                       'Automat.Pruef_Version.Version_HWx');
      end;

  else
    my_infoblock_internal.debug2 (
       format ('Interner Programmierfehler t_db_vers_info.set_vers_info (%s): ' +
       'nicht unterstützte Versionsinfo!', [s_vers]));
  end;

end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : simple_vers_info
//                   Versionsinformation aus simple übernehmnen
//    Parameter    : my_info_type : welche Versionsinformation soll übernommen werden?
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-10-22
//    History      : /aha/ Bugfix: bl2, fw2, hw2 gingen auf bl1, fw1, hw1!
// =============================================================================
procedure t_db_vers_info.simple_vers_info (
             my_info_type : t_vers_enum );
begin
  case ( my_info_type ) of
    vinfo_bl1 : versions_internal.s_version_bl1 := versions_internal.s_version_bl_simple;
    vinfo_fw1 : versions_internal.s_version_fw1 := versions_internal.s_version_fw_simple;
    vinfo_hw1 : versions_internal.s_version_hw1 := versions_internal.s_version_hw_simple;

    vinfo_bl2 : versions_internal.s_version_bl2 := versions_internal.s_version_bl_simple;
    vinfo_fw2 : versions_internal.s_version_fw2 := versions_internal.s_version_fw_simple;
    vinfo_hw2 : versions_internal.s_version_hw2 := versions_internal.s_version_hw_simple;
  else
    my_infoblock_internal.debug2 (
      't_db_vers_info.simple:vers_info: nicht unterstützte Versionsinfo!' );
  end;
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : set_vers_inx
//                   Versionsindex (Schlüssel Tabelle Automat.Pruef_Version setzen
//    Parameter    : Index
//    Return       : --
//    Exceptions   : --
//    First author : /aha/ 2014-07-14
//    History      : --
// =============================================================================
procedure t_db_vers_info.set_vers_inx   ( l_inx : longint );
begin
  s_vers_inx_internal := IntToStr ( l_inx );
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : get_vers_inx_s
//                   Versionsindex (Schlüssel Tabelle Automat.Pruef_Version ermitteln
//                   0 im inaktiven Zustand
//    Parameter    : --
//    Return       : Index
//    Exceptions   : --
//    First author : /aha/ 2014-07-14
//    History      : --
// =============================================================================
function  t_db_vers_info.get_vers_inx_s () : string;
begin
  if ( b_active )  then result := s_vers_inx_internal
  else                  result := '0';
end;

// =============================================================================
//    Class        : t_db_vers_info
//    Function     : Destroy (Destruktor)
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
destructor  t_db_vers_info.Destroy;
begin
  inherited;
end;

// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
// VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV

// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : Create (Konstruktor)
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : /aha/ 2014-12-03: Packet-Size 3 ==> 16 (wie im FD gefordert)
// =============================================================================
constructor t_db_mess_info.Create ( );
const
  C_PACKET_SIZE = 16;   // Anzahl Datensätze pro INSERT-Kommando

begin
  b_active        := FALSE;          // Defaultwert: Info inaktiv
  tl_intern       := TList.Create;   // interne Datenliste anlegen
  init();                            // interne Datenliste löschen
  lw_int_pr_nr    := 0;
  lw_packet_size  := C_PACKET_SIZE;  // wie viele Daten werden auf einmal exportiert?
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : Destroy (Destruktor)
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-07-10
//    History      : --
// =============================================================================
destructor  t_db_mess_info.Destroy;
begin
  init();            // Speicher für die Listeneinträge wieder freigeben
  tl_intern.Free();  // Liste selber löschen
  inherited;
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : init: interne Datenliste löschen
//                   Speicher für Einträge wieder freigeben
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
procedure t_db_mess_info.init();
var
  i  : integer;
  p1 : p_mess_entry;

begin
  for i := 0 to tl_intern.Count-1 do
  begin
    p1 := p_mess_entry  ( tl_intern.Items[i] );
    dispose ( p1 );                              // Speicher freigeben
  end;

  tl_intern.clear();
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : set_pr_nr:
//                   Automat.Pr_Nr der Klasse bekanntmachen
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
procedure t_db_mess_info.set_pr_nr ( lw_inx : longword );
begin
  lw_int_pr_nr := lw_inx;
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : store_first_data_packet_str:
//                   Generierung eines Strings zur Abspeicherung der ersten X
//                   Messwerte
//                   X = lw_packet_size (wird im Konstruktor festgelegt )
//    Parameter    : s_cmd  : SQL-Kommandostring (wenn Daten vorhanden)
//                   <leer>, wenn keine Daten
//    Return       : TRUE   : es gab Daten, die zu exportieren waren
//                   FALSE  : keine Daten
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
function t_db_mess_info.store_first_data_packet_str ( var s_cmd : string ) : boolean;
begin
  lw_out_inx := 0;    // Ausgabeindex zurücksetzen (wg. "...first...")
  result := store_next_data_packet_str ( s_cmd );
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : store_next_data_packet_str:
//                   Generierung eines Strings zur Abspeicherung der nächsten X
//                   Messwerte
//                   X = lw_packet_size (wird im Konstruktor festgelegt )
//    Parameter    : s_cmd  : SQL-Kommandostring (wenn Daten vorhanden)
//                   <leer>, wenn keine Daten
//    Return       : TRUE   : es gab Daten, die zu exportieren waren
//                   FALSE  : keine Daten
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
function t_db_mess_info.store_next_data_packet_str ( var s_cmd : string ) : boolean;
const
  CS_SQL =
    'INSERT INTO Automat.Pruef_Values ' +
    '(Pr_Nr, PS_Nr, PS_Value ) VALUES ';

var
  lw_packet_ctr : longword;       // temporärer Paketzähler
  b_last_data   : boolean;        // Flag: letzer Datensatz
  p1            : p_mess_entry;   // Messeintrag

begin

  // Initialisierung
  s_cmd := '';
  result := FALSE;

  if ( lw_out_inx >= longword (tl_intern.Count) ) then exit;   // keine zu exportierenden Daten

  // =============================================
  // Es gibt Daten zu exportieren!
  // =============================================
  result            := TRUE;
  s_cmd             := CS_SQL;
  lw_packet_ctr     := 1;

  // solange die Schleife durchlaufen, bis
  // a) die Maximalzahl der erlaubten Pakete erreicht ist, ODER
  // b) keine Pakete sich mehr in der internen Liste befinden
  repeat
    b_last_data := ((lw_packet_ctr > lw_packet_size-1 ) or
                    (lw_out_inx >= longword ( tl_intern.count-1 )));

    // --------------------------------------------
    // Ausgabestring weiterbauen
    // --------------------------------------------
    p1    := p_mess_entry (tl_intern.Items[lw_out_inx]); // aktueller Messeintrag aus Liste
    s_cmd := s_cmd +
             format ( '(%d, ''%s'', ''%s'')',
                      [ lw_int_pr_nr,             // Verweis: Automat.Pruef.Pr_Nr
                        p1^.s_ps_nr,              // Prüfschrittnummer
                        p1^.s_ps_value]);         // Messwert

    if ( b_last_data ) then s_cmd := s_cmd + ';'  // letzter Datensatz endet auf ";"
    else                    s_cmd := s_cmd + ','; // sonst immer auf ","

    inc ( lw_out_inx );     // Ausgabeindex erhöhen
    inc ( lw_packet_ctr );  // Paketzähler  erhöhen
  until (b_last_data );

end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : add_mess_value : fügt ein Tupel
//                   (Prüfschritt, resultierendes Messergebnis) in die interne
//                   Liste ein
//    Parameter    : s_step   : Prüfschritt
//                   s_mess   : Messergebnis
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
procedure t_db_mess_info.add_mess_value ( s_step, s_mess : string );
const
  C_MAX_STRLEN_STEP = 15;      // Maximale Länge für Prüfschrittnummer
  C_MAX_STRLEN_MESS = 25;      // Maximale Länge für Messergebnis

var
  p_new : p_mess_entry;

begin
  // =================================
  // Argumentprüfungen
  // =================================

  // Prüfschritt
  if ( length ( s_step) > C_MAX_STRLEN_STEP) then
  begin
    s_step := copy ( s_step, 1, C_MAX_STRLEN_STEP );
  end;

  // Messwert
  if ( length ( s_mess) > C_MAX_STRLEN_MESS) then
  begin
    s_mess := copy ( s_mess, 1, C_MAX_STRLEN_MESS );
  end;

  // ======================================
  // Einheitlicher Dezimalseparator (".")
  // ======================================
  replace_decimal_separator ( s_step, '.' );
  replace_decimal_separator ( s_mess, '.' );

  // ======================================
  // neues Listenelement erstellen und Werte zuweisen
  // ======================================
  new ( p_new );
  p_new^.s_ps_nr     := s_step;
  p_new^.s_ps_value  := s_mess;

  tl_intern.Add(pointer (p_new));    // in Liste übernehmen
end;

// =============================================================================
//    Class        : t_db_mess_info
//    Function     : replace_decimal_separator
//                   ersetzt jeden anderen Dezimalseparator durch c_sep
//    Parameter    : ---
//    Return       : ---
//    Exceptions   : --
//    First author : /aha/ 2014-08-13
//    History      : --
// =============================================================================
procedure t_db_mess_info.replace_decimal_separator ( var s_in : string; c_sep : char );
var
  i : integer; ch_decsep: char;
begin
{$IF CompilerVersion >= 12.0}
  ch_decsep := FormatSettings.DecimalSeparator;
{$ELSE}
  ch_decsep := DecimalSeparator;
{$ENDIF}
  if ch_decsep = c_sep then exit;   // schon der geünschte Separator

  for i := 1 to length ( s_in ) do
  begin
    if ( s_in[i] = ch_decsep ) then   // anderer Separator
    begin
      s_in[i] := c_sep;    // durch Wunschwert ersetzen
    end;
  end;
end;

// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM



end.


