unit MtxProduct;

interface
uses Classes, IniFiles, ConnBase, ProtocolBase, DeviceBase;
type
  EStartMode = (
                PS_ORIGIN,    //started with original BL/FW of manufacturer
                PS_MTX_UPD,   //started with Metronix bl-updater
                PS_MTX_SRV,   //started in mode of Metronix Service
                PS_MTX_APP   //started in mode of Metronix Application
                );
                  
  EProductEvent = (
                    PE_RESET,
                    PE_SWITCH,
                    PE_SEND,
                    PE_RECV,
                    PE_UPDATE,
                    PE_PARA
                    );

  TProductBase = class(TDeviceBase)
  protected
    e_mode: EStartMode;
    s_startmsg: string;

    t_conns: array[LOW(EConnectionType)..HIGH(EConnectionType)] of TConnBase; //array of all possible connections
    e_actconn: EConnectionType; //type of currently active connection

  strict private
    function GetModeString(): string;

  protected
    function RecvExpectedStr(const str: string; const timeout: cardinal): boolean; virtual;
    function ConfigConnections(const ini: TMemIniFile): integer; virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property StartMode: EStartMode read e_mode;
    property StartModeString: string read GetModeString;
    property StartMessage: string read s_startmsg;
    function ConfigDevice(const ini: TMemIniFile): boolean; override;

    function PowerOn(): boolean; virtual; abstract;
    function PowerOff(): boolean; virtual; abstract;
    function ResetHW(): boolean; virtual; abstract;
    function ResetSW(): boolean; virtual; abstract;
    function SwitchTo(const mode: EStartMode): boolean; virtual; abstract;
    function UpdateBootloader(): boolean; virtual; abstract;
    function UpdateFirmware(): boolean; virtual; abstract;
    function UpdateParameter(): boolean; virtual; abstract;
  end;
  PProductBase = ^TProductBase;

implementation
uses Windows, SysUtils, GenUtils, RS232;

const
  CSTR_START_MODE : array[LOW(EStartMode)..HIGH(EStartMode)] of string = (
                      'origin',
                      'mtx-bl-updater',
                      'mtx-service',
                      'mtx-application'
                      );

  CSTR_PROD_SEC  : string = 'ProductGeneral';

function TProductBase.GetModeString(): string;
begin
  result := CSTR_START_MODE[e_mode];
end;

function TProductBase.RecvExpectedStr(const str: string; const timeout: cardinal): boolean;
var c_time: cardinal; s_recv: string; i_len: integer;
begin
  result := false;
  if (e_state in C_DEV_STATES[DE_DISCONNECT]) then begin
    c_time := GetTickCount() + timeout;
    t_rbuf.Clear;

    while ((GetTickCount() < c_time) and (not result)) do begin
      i_len := t_conn.RecvData(t_rbuf, c_timeout);
      s_recv := s_recv + t_rbuf.ReadStr();
      result := (Pos(str, s_recv) > 0);
      if (i_len <= 0) then Delay(C_DELAY_MSEC * 5); //wait a moment if no data is arrived
    end;
  end;
end;

function TProductBase.ConfigConnections(const ini: TMemIniFile): integer;
var i: EConnectionType; s_inivalue: string;
begin
  result := 0;
  for i := LOW(EConnectionType) to HIGH(EConnectionType) do begin
    if ini.ValueExists(CSTR_PROD_SEC, CSTR_CONN_KEYS[i]) then begin
      s_inivalue := trim(ini.ReadString(CSTR_PROD_SEC, CSTR_CONN_KEYS[CT_RS232], ''));
      if not assigned(t_conns[i]) then begin
        case i of
          CT_RS232: begin
            t_conns[i] := TConnRS232.Create(self);
            if (t_conns[i].Config(s_inivalue)) then inc(result);
          end;
          CT_JTAG, CT_GPIB, CT_USB, CT_ETHERNET, CT_CAN, CT_PROFIL: ; //todo
        end;
      end;
    end;
  end;
end;

constructor TProductBase.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_actconn := CT_RS232;
  t_conns[CT_RS232] := TConnRS232.Create(self);
end;

destructor TProductBase.Destroy;
var i: EConnectionType;
begin
  for i := LOW(EConnectionType) to LOW(EConnectionType) do begin
    if assigned(t_conns[i]) then FreeAndNil(t_conns[i]);
  end;

  inherited Destroy();
end;

// =============================================================================
// Class        : TProductBase
// Function     : ConfigDevice
//                generally config product using settings from INI like:
//                [ProductGeneral]
//                DESCRIPTION=A universal programmer for writing flash
//                PRODUCER=SMH Technologies
//                TYPE=FR01LAN
//                CONN_RS232=PORT:8|Baudrate:115200
//                TIMEOUT=30000
// Parameter    : ini, a instance of TMemIniFile, in which section [FlashRunner]
//                     is saved
// Return       : true, if the device is configured successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TProductBase.ConfigDevice(const ini: TMemIniFile): boolean;
//var s_inivalue: string;
begin
  result := false;
  if (e_state in C_DEV_STATES[DE_CONFIG]) then
  begin
    if (ini.SectionExists(CSTR_PROD_SEC)) then
    begin
      c_timeout := ini.ReadInteger(CSTR_PROD_SEC, CSTR_DEV_TIMEOUT, C_TIMEOUT_MSEC);
      result := (ConfigConnections(ini) > 0);
    end;
    PostEvent(DE_CONFIG, result);;
  end;
end;

end.
