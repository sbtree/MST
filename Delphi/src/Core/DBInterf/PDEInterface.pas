unit PDEInterface;

interface
uses Classes, Datenbank, u_types_att;

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
    procedure SetResultError(const stepnr, func, actval, refval: string);
    procedure SetErrorCode(const errcode: string);
    procedure SetComment(const cmnt: string);
    procedure Clear();

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

  IPdeMeasValues = interface
    procedure AddMeasValue(const stepnr, sval: string);
    procedure Clear();
  end;

  IPdeActor = interface
    function CheckContractNr(): boolean;
    function CheckConsistency(): boolean;
    function CheckBoardNr(): boolean;
    function WriteResult(): boolean;
    function AddComment(idx: integer): boolean;
    procedure SetToStoreMeas(const active: boolean);
    procedure SetToStoreVers(const active: boolean);
    procedure Clear();
  end;

  TPdeAdapter = class
  protected
    t_infoblock:t_db_infoblock;
    t_dbconn:   t_db_connection;
    //t_comment:  t_db_comment;
    t_verinfo:  t_db_vers_info;
    //t_meainfo:  t_db_mess_info;
    b_vers: boolean;
    b_meas: boolean;
  protected
    function IsConnected(): boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    function AddComment(const idx: integer): boolean;
    function CheckConsistency(): boolean;
    procedure SetProductID(const prodid: string);
    procedure WriteResult();
    procedure ClearComments();

    property ProductID: string write SetProductID;
    property InfoBlock: t_db_infoblock read t_infoblock;
    property Connection: t_db_connection read t_dbconn;
    property VersionInfo: t_db_vers_info read t_verinfo;
    property VersInfoActived: boolean read b_vers write b_vers;
    property MeasValueActived: boolean read b_meas write b_meas;
    //property MeasureValue: t_db_mess_info read t_meainfo;
    //property Comment: t_db_comment read t_comment;
  End;

implementation

function TPdeAdapter.IsConnected(): boolean;
begin
  result := t_dbconn.connected;
end;

constructor TPdeAdapter.Create();
begin
  inherited Create();
  t_infoblock := t_db_infoblock.Create();
  t_dbconn := t_db_connection.Create();
  //t_comment := t_db_comment.Create();
  t_verinfo := t_db_vers_info.Create(t_infoblock);
  //t_meainfo := t_db_mess_info.Create();
end;

destructor TPdeAdapter.Destroy();
begin
  //t_meainfo.Free();
  t_verinfo.Free();
  //t_comment.Free();
  t_dbconn.Free();
  t_infoblock.Free();
  inherited Destroy();
end;

function TPdeAdapter.AddComment(const idx: integer): boolean;
begin
  try
    t_infoblock.add_comment(idx);
    result := true;
  except
    result := false;
  end;
end;

function TPdeAdapter.CheckConsistency(): boolean;
begin
  result := t_infoblock.check_consistency_variant(t_dbconn);
end;

procedure TPdeAdapter.SetProductID(const prodid: string);
begin
  t_infoblock.sap_main := prodid;
end;

procedure TPdeAdapter.WriteResult();
begin
  t_infoblock.write_test_result(t_dbconn);
end;

procedure TPdeAdapter.ClearComments();
begin
  //2016-11-07 bsu: todo
end;

end.
