unit MtxProduct;

interface
uses Classes, IniFiles, ConnBase, ProtocolBase;
type
  EProductState = (
                    PS_UNKNOWN,   //no communication
                    PS_STARTING,  //the product is starting (through power on or reset)
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

  TProductBase = class(TComponent)
  protected
    e_state: EProductState;
    s_message: string;
    i_timeout: cardinal;    //timeout in millisecond
    i_lasterr: integer;     //last error number
    b_comhex : boolean;     //hexadizcimal data in string to transfer if it is true

    t_conns: array[LOW(EConnectionType)..HIGH(EConnectionType)] of TConnBase; //array of all possible connections
    t_actconn: EConnectionType; //type of currently active connection
    t_prot: TProtBase;      //protocol of communication
  strict private
    function GetStateString(): string;

  protected
    procedure PostEvent(const event: EProductEvent; const ok: boolean);
    //function Sync(): boolean; virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property State: EProductState read e_state;
    property StartingMessage: string read s_message;
    property StateString : string read GetStateString;
    property HexComm : boolean read b_comhex write b_comhex;

    function SetTimeout(const msec: integer): integer;
    function GetLastError(var msg: string): Integer; virtual; abstract;

    function Config(const ini: TMemIniFile): Boolean; virtual;abstract;
    function Connect(): Boolean; virtual;
    function Disconnect: boolean; virtual;
    function SendStr(const sdata: string; const bans: boolean = true): Integer; virtual;abstract;
    function RecvStr(var sdata: string): Integer; virtual;abstract;

    function PowerOn(): boolean; virtual; abstract;
    function PowerOff(): boolean; virtual; abstract;
    function ResetHW(): boolean; virtual;
    function ResetSW(): boolean; virtual; abstract;
    function SwitchTo(const state: EProductState): boolean; virtual; abstract;
    function UpdateBootloader(): boolean; virtual; abstract;
    function UpdateFirmware(): boolean; virtual; abstract;
    function UpdateParameter(): boolean; virtual; abstract;
  end;

implementation

const
  CSTR_PROD_STATES : array[LOW(EProductState)..HIGH(EProductState)] of string = (
                  'unknow',
                  'starting',
                  'origin',
                  'mtx-bl-updater',
                  'mtx-service',
                  'mtx-application'
                  );

function TProductBase.GetStateString(): string;
begin
  result := CSTR_PROD_STATES[e_state];
end;

constructor TProductBase.Create(owner: TComponent);
begin

end;

destructor TProductBase.Destroy;
begin

end;

procedure TProductBase.PostEvent(const event: EProductEvent; const ok: boolean);
begin

end;
end.
