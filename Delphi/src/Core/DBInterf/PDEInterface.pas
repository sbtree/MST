unit PDEInterface;

interface
uses Classes, Datenbank{, u_types_att};

type
  IPdeConnection = interface
    function Login(const user, passwd: string): boolean;
    function IsConnected(): boolean;
    function GetVersion(): string;
    function GetUserName(): string;
    function GetUserLevel(): integer;
    procedure SetMessageHandle(phandle: tfptr_debug);
    procedure SetConnectStr(connstr: string);
    procedure Logout();

    property ConnectStr: string write SetConnectStr;
    property Connected: boolean read IsConnected;
    property UnitVersion: string read GetVersion;
    property UserName: string read GetUserName;
    property UserLevel: integer read GetUserLevel;
  end;

  TPdeConnection = class(TInterfacedObject, IPdeConnection)
  protected
    t_conn: t_db_connection;
  public
    constructor Create(const conn: t_db_connection); overload;
    destructor  Destroy; override;

    function Login(const user, passwd: string): boolean;
    function IsConnected(): boolean;
    function GetVersion(): string;
    function GetUserName(): string;
    function GetUserLevel(): integer;
    procedure SetMessageHandle(phandle: tfptr_debug);
    procedure SetConnectStr(connstr: string);
    procedure Logout();

    property ConnectStr: string write SetConnectStr;
    property Connected: boolean read IsConnected;
    property UnitVersion: string read GetVersion;
    property UserName: string read GetUserName;
    property UserLevel: integer read GetUserLevel;
  end;

  IPdeTestData = interface
    procedure SetProductID(const prodid: string);
    procedure SetTestStationID(const tsid: string);
    procedure SetContractNr(const cntrnr: string);
    procedure SetContractNrLen(const lmin, lmax: integer);
    procedure SetVersion(const evers: t_vers_enum; const verstr: string);
    procedure SetBoardNr(const boardnr: string);
    procedure SetSubBoardNr(const boardnr: string);
    procedure SetDeviceNr(const devnr: string);
    procedure SetTestBegin(startdt: TDateTime);
    procedure SetTestEnd(enddt: TDateTime);
    procedure SetTestMode(const tm: t_en_db_testmode);
    procedure SetResultOK();
    procedure SetResultError(const stepnr: double; const func, actval, refval: string);
    procedure SetErrorCode(const errcode: string);
    procedure SetComment(const cmnt: string);
    procedure AddMeasValue(const stepnr, sval: string);
    procedure ClearAll();
    procedure ClearResult();
    procedure ClearComments();
    procedure ClearMeasValues();
    procedure ClearVersionInfo();

    property ProductID: string write SetProductID;
    property TestStationID: string write SetTestStationID;
    property ContractNr: string write SetContractNr;
    property BoardNumber: string write SetBoardNr;
    property SubBoardNumber: string write SetSubBoardNr;
    property DeviceNumber: string write SetDeviceNr;
    property TestBegin: TDateTime write SetTestBegin;
    property TestEnd: TDateTime write SetTestEnd;
    property TestMode: t_en_db_testmode write SetTestMode;
  end;

  TPdeTestData = class(TInterfacedObject, IPdeTestData)
  protected
    t_conn: t_db_connection;
    t_tdata:t_db_infoblock;
  protected
    procedure ClearResultError();
  public
    constructor Create(const tdata: t_db_infoblock; const conn: t_db_connection); overload;
    destructor  Destroy; override;

    procedure SetProductID(const prodid: string);
    procedure SetTestStationID(const tsid: string);
    procedure SetContractNr(const cntrnr: string);
    procedure SetContractNrLen(const lmin, lmax: integer);
    procedure SetVersion(const evers: t_vers_enum; const verstr: string);
    procedure SetBoardNr(const boardnr: string);
    procedure SetSubBoardNr(const boardnr: string);
    procedure SetDeviceNr(const devnr: string);
    procedure SetTestBegin(startdt: TDateTime);
    procedure SetTestEnd(enddt: TDateTime);
    procedure SetTestMode(const tm: t_en_db_testmode);
    procedure SetResultOK();
    procedure SetResultError(const stepnr: double; const func, actval, refval: string);
    procedure SetErrorCode(const errcode: string);
    procedure SetComment(const cmnt: string);
    procedure AddMeasValue(const stepnr, sval: string);
    procedure ClearAll();
    procedure ClearResult();
    procedure ClearComments();
    procedure ClearMeasValues();
    procedure ClearVersionInfo();

    property ProductID: string write SetProductID;
    property TestStationID: string write SetTestStationID;
    property ContractNr: string write SetContractNr;
    property BoardNumber: string write SetBoardNr;
    property SubBoardNumber: string write SetSubBoardNr;
    property DeviceNumber: string write SetDeviceNr;
    property TestBegin: TDateTime write SetTestBegin;
    property TestEnd: TDateTime write SetTestEnd;
    property TestMode: t_en_db_testmode write SetTestMode;
  end;

  IPdeActor = interface
    function CheckContractNr(): boolean;
    function CheckConsistency(): boolean;
    function CheckBoardNr(): boolean;
    function AddComment(idx: integer): boolean;
    procedure WriteResult();
    procedure SetMeasValueActive(const active: boolean);
    procedure SetVersInfoActive(const active: boolean);
  end;

  TPdeAdapter = class(TInterfacedObject, IPdeActor, IPdeConnection, IPdeTestData)
  protected
    t_infoblock:t_db_infoblock;
    t_dbconn:   t_db_connection;
    t_connimpl: TPdeConnection;
    t_dataimpl: TPdeTestData;
    b_vers: boolean;
    b_meas: boolean;
  protected
    function IsConnected(): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    property ConnectionService: TPdeConnection read t_connimpl implements IPdeConnection;
    property TestDataService: TPdeTestData read t_dataimpl implements IPdeTestData;

    function CheckContractNr(): boolean;
    function CheckConsistency(): boolean;
    function CheckBoardNr(): boolean;
    function AddComment(idx: integer): boolean;
    procedure WriteResult();
    procedure SetMeasValueActive(const active: boolean);
    procedure SetVersInfoActive(const active: boolean);

    property VersInfoActived: boolean read b_vers write SetVersInfoActive;
    property MeasValueActived: boolean read b_meas write SetMeasValueActive;
  End;

implementation
uses SysUtils;

constructor TPdeConnection.Create(const conn: t_db_connection);
begin
  inherited Create();
  t_conn := conn;
end;

destructor  TPdeConnection.Destroy;
begin
  inherited Destroy();
end;

function TPdeConnection.Login(const user, passwd: string): boolean;
begin
  if assigned(t_conn) then result := t_conn.login(user, passwd)
  else result := false;
end;

function TPdeConnection.IsConnected(): boolean;
begin
  if assigned(t_conn) then result := t_conn.connected
  else result := false;
end;

function TPdeConnection.GetVersion(): string;
begin
  if assigned(t_conn) then result := t_conn.version
  else result := '';
end;

function TPdeConnection.GetUserName(): string;
begin
  if assigned(t_conn) then result := t_conn.user_name
  else result := '';
end;

function TPdeConnection.GetUserLevel(): integer;
begin
  if assigned(t_conn) then result := t_conn.user_level
  else result := -1;
end;

procedure TPdeConnection.SetMessageHandle(phandle: tfptr_debug);
begin
  if assigned(t_conn) then t_conn.fptr_debug := phandle;
end;

procedure TPdeConnection.SetConnectStr(connstr: string);
begin
  if assigned(t_conn) then t_conn.ConnectString := connstr;
end;

procedure TPdeConnection.Logout();
begin
  if assigned(t_conn) then t_conn.logout();
end;


function TPdeAdapter.IsConnected(): boolean;
begin
  result := t_dbconn.connected;
end;

procedure TPdeTestData.ClearResultError();
begin
  if assigned(t_tdata) then begin
    t_tdata.my_error_info.f_pruefschritt := 0.0;
    t_tdata.my_error_info.s_function := '';
    t_tdata.my_error_info.s_value_actual := '';
    t_tdata.my_error_info.s_value_ref := '';
  end;
end;

constructor TPdeTestData.Create(const tdata: t_db_infoblock; const conn: t_db_connection);
begin
  inherited Create();
  t_tdata := tdata;
  t_conn := conn;
end;

destructor  TPdeTestData.Destroy;
begin
  inherited Destroy();
end;

procedure TPdeTestData.SetProductID(const prodid: string);
begin
  if assigned(t_tdata) then t_tdata.sap_main := prodid;
end;

procedure TPdeTestData.SetTestStationID(const tsid: string);
begin
  if assigned(t_tdata) then t_tdata.id_pruefplatz := tsid;
end;

procedure TPdeTestData.SetContractNr(const cntrnr: string);
begin
  if assigned(t_tdata) then t_tdata.fa_nr := cntrnr;
end;

procedure TPdeTestData.SetContractNrLen(const lmin, lmax: integer);
begin
  if assigned(t_conn) then begin
    t_conn.fa_min_len := lmin;
    t_conn.fa_max_len := lmax;
  end;
end;

procedure TPdeTestData.SetVersion(const evers: t_vers_enum; const verstr: string);
begin
  if assigned(t_tdata) then begin
    case evers of
      vinfo_exe: t_tdata.my_versinfo.s_vers_exe := verstr;
      vinfo_ini: t_tdata.my_versinfo.s_vers_ini := verstr;
      vinfo_psl: t_tdata.my_versinfo.s_vers_psl := verstr;
      vinfo_bl1: t_tdata.my_versinfo.s_vers_bl1 := verstr;
      vinfo_fw1: t_tdata.my_versinfo.s_vers_fw1 := verstr;
      vinfo_hw1: t_tdata.my_versinfo.s_vers_hw1 := verstr;
      vinfo_bl2: t_tdata.my_versinfo.s_vers_bl2 := verstr;
      vinfo_fw2: t_tdata.my_versinfo.s_vers_fw2 := verstr;
      vinfo_hw2: t_tdata.my_versinfo.s_vers_hw2 := verstr;
      vinfo_bl_simple: ;
      vinfo_fw_simple: ;
      vinfo_hw_simple: ;
    end;
  end;
end;

procedure TPdeTestData.SetBoardNr(const boardnr: string);
begin
  if assigned(t_tdata) then t_tdata.s_boardnumber := boardnr;
end;

procedure TPdeTestData.SetSubBoardNr(const boardnr: string);
begin
  //todo:;
end;

procedure TPdeTestData.SetDeviceNr(const devnr: string);
begin
  if assigned(t_tdata) then t_tdata.s_devicenumber := devnr;
end;

procedure TPdeTestData.SetTestBegin(startdt: TDateTime);
begin
  if assigned(t_tdata) then t_tdata.s_starttime := FormatDateTime('yyyy-mm-dd hh:nn:ss', startdt);
end;

procedure TPdeTestData.SetTestEnd(enddt: TDateTime);
begin
  if assigned(t_tdata) then t_tdata.s_endtime := FormatDateTime('yyyy-mm-dd hh:nn:ss', enddt);
end;

procedure TPdeTestData.SetTestMode(const tm: t_en_db_testmode);
begin
  if assigned(t_tdata) then t_tdata.en_testmode := tm;
end;

procedure TPdeTestData.SetResultOK();
begin
  if assigned(t_tdata) then begin
    t_tdata.b_result := true;
    ClearResultError();
  end;
end;

procedure TPdeTestData.SetResultError(const stepnr: double; const func, actval, refval: string);
begin
  if assigned(t_tdata) then begin
    t_tdata.b_result := false;
    t_tdata.my_error_info.f_pruefschritt := stepnr;
    t_tdata.my_error_info.s_function := func;
    t_tdata.my_error_info.s_value_actual := actval;
    t_tdata.my_error_info.s_value_ref := refval;
  end;
end;

procedure TPdeTestData.SetErrorCode(const errcode: string);
begin
  if assigned(t_tdata) then t_tdata.s_errcode := errcode;
end;

procedure TPdeTestData.SetComment(const cmnt: string);
begin
  if assigned(t_tdata) then t_tdata.s_comment := cmnt;
end;

procedure TPdeTestData.AddMeasValue(const stepnr, sval: string);
begin
  if assigned(t_tdata) then t_tdata.add_mess_value(stepnr, sval);
end;

procedure TPdeTestData.ClearAll();
begin
  if assigned(t_tdata) then t_tdata.clear();
end;

procedure TPdeTestData.ClearResult();
begin
  ClearResultError();
  if assigned(t_tdata) then begin
    t_tdata.s_starttime := '';
    t_tdata.s_endtime := '';
    t_tdata.s_boardnumber := '';
    t_tdata.s_devicenumber := '';
    t_tdata.s_errcode := '';
    t_tdata.s_comment := '';
    t_tdata.b_result := FALSE;
  end;
end;

procedure TPdeTestData.ClearComments();
begin
  //todo: discuss about DBInterface with aha
end;

procedure TPdeTestData.ClearMeasValues();
begin
  //todo: discuss about DBInterface with aha
end;

procedure TPdeTestData.ClearVersionInfo();
begin
  if assigned(t_tdata) then begin
    t_tdata.my_versinfo.s_vers_exe := '';
    t_tdata.my_versinfo.s_vers_ini := '';
    t_tdata.my_versinfo.s_vers_psl := '';
    t_tdata.my_versinfo.s_vers_bl1 := '';
    t_tdata.my_versinfo.s_vers_fw1 := '';
    t_tdata.my_versinfo.s_vers_hw1 := '';
    t_tdata.my_versinfo.s_vers_bl2 := '';
    t_tdata.my_versinfo.s_vers_fw2 := '';
    t_tdata.my_versinfo.s_vers_hw2 := '';
  end;
end;

constructor TPdeAdapter.Create();
begin
  inherited Create();
  t_infoblock := t_db_infoblock.Create();
  t_dbconn := t_db_connection.Create();
  t_connimpl := TPdeConnection.Create(t_dbconn);
  t_dataimpl := TPdeTestData.Create(t_infoblock, t_dbconn);
end;

destructor TPdeAdapter.Destroy();
begin
  t_dataimpl.Free();
  t_connimpl.Free();
  t_dbconn.Free();
  t_infoblock.Free();
  inherited Destroy();
end;

function TPdeAdapter.CheckContractNr(): boolean;
begin
  result := t_infoblock.check_consistency_fa(t_dbconn)
end;

function TPdeAdapter.CheckConsistency(): boolean;
begin
  result := t_infoblock.check_consistency_variant(t_dbconn);
end;

function TPdeAdapter.CheckBoardNr(): boolean;
begin
  result := t_dbconn.check_boardnumber(t_infoblock.s_boardnumber);
end;

function TPdeAdapter.AddComment(idx: integer): boolean;
begin
  try
    t_infoblock.add_comment(idx);
    result := true;
  except
    result := false;
  end;
end;

procedure TPdeAdapter.WriteResult();
begin
  t_infoblock.write_test_result(t_dbconn);
end;

procedure TPdeAdapter.SetMeasValueActive(const active: boolean);
begin
  b_meas := active;
end;

procedure TPdeAdapter.SetVersInfoActive(const active: boolean);
begin
  b_vers := active;
end;

end.
