// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit DeviceBase;

interface
uses Classes, SysUtils, IniFiles, ConnBase, ProtocolBase;

type
// =============================================================================
// Enumeration  : EDeviceState
// Description  : This enumeration describes current state of the device.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceState = (
                  DS_NONE, //none state, in which the device object is just created and yet configured
                  DS_CONFIGURED, //configured state, in which the device is configured
                  DS_CONNECTED, //connected state, in which the device is connected
                  DS_READY, //ready state, in which the device is ready to communicate
                  DS_WAITING, //waiting state, in which the device is waiting for answer
                  DS_COMERROR //communication error state, in which an error exists in the last communication
                  );

// =============================================================================
// Enumeration  : EDeviceEvent
// Description  : This enumeration describes events, which function of IDeviceInterf is called.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceEvent = (
                  DE_CONFIG, //event, which configs device, e.g. ConfigDevice is called
                  DE_CONNECT, //event, which connects device, e.g. Connect is called
                  DE_SYNC, //event, which tries to communicate with device, e.g. Sync is called
                  DE_SEND, //event, which sends data to device, e.g. SendData is called
                  DE_RECV, //event, which receives data from device, e.g. RecvData is called
                  DE_DISCONNECT, //event, which disconnects from device, e.g. Disconnect is called
                  DE_FREE //event, which releases device, e.g. FreeDevice is called
                  );
  DeviceStateSet = set of EDeviceState;

  // =============================================================================
  // Class        : TDeviceBase
  // Description  : Definition of class TDeviceBase, which is a base class of measurement device
  //                deriving from TComponent. In this class the general functions and
  //                interfaces are defined or implemented. All concrete device schall
  //                derive from this class
  // First author : 2015-08-14 /bsu/
  // History      :
  // =============================================================================
  TDeviceBase=class(TComponent)
  protected
  var
    e_state: EDeviceState;  //device state
    i_timeout: cardinal;    //timeout in millisecond
    i_lasterr: integer;     //last error number
    s_lastmsg: string;      //last message
    s_devname: string;      //name of the device
    b_comhexa: boolean;     //hexadizcimal data in string to transfer if it is true

    t_conn: TConnBase;      //connection
    t_prot: TProtBase;      //protocol of communication

  strict private
    function GetDeviceStateString : string;

  protected
    procedure PostEvent(const event: EDeviceEvent; const ok: boolean);
    function Sync(): boolean; virtual;
    function TryToReady: boolean; virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property DeviceState : EDeviceState read e_state;
    property DeviceStateString : string read GetDeviceStateString;
    function SetTimeout(const msec: integer): integer;
    function SetComHexa(const bhex: boolean = true): boolean;
    function GetLastError(var msg: string): Integer; virtual; abstract;

    function ConfigDevice(const ini: TMemIniFile): Boolean; virtual;abstract;
    function FreeDevice(): Boolean; virtual; abstract;
    function Connect(): Boolean; virtual;abstract;
    function Disconnect: boolean; virtual; abstract;
    function SendStr(const sdata: string; const bans: boolean = true): Integer; virtual;abstract;
    function RecvStr(var sdata: string): Integer; virtual;abstract;
  end;
  PDeviceBase = ^TDeviceBase;

const
  C_ERR_NOERROR = $0000; // no error
  C_ERR_UNKNOWN = $8000; // base error number as unknown

  C_TIMEOUT_MSEC: Cardinal = 30000;  //default timeout 30000 milli seconds for communication
  C_DELAY_MSEC: Cardinal = 20;  //delay 20 milli seconds for communication error
  C_STR_STATE : array[LOW(EDeviceState)..HIGH(EDeviceState)] of string = ('unusable','configured','connected','communicable','waiting','device error');

  //define sets of device states, which are allowed for each event
  C_DEV_STATES: array[LOW(EDeviceEvent)..HIGH(EDeviceEvent)] of DeviceStateSet = (
                [DS_NONE, DS_CONFIGURED], //allowed states for config
                [DS_CONFIGURED, DS_CONNECTED], //allowed states for connect
                [DS_CONNECTED, DS_COMERROR], //allowed states for sync
                [DS_READY], //allowed states for send
                [DS_WAITING], //allowed states for recv
                [DS_CONFIGURED, DS_CONNECTED, DS_READY, DS_WAITING, DS_COMERROR], //allowed states for disconnect
                [DS_NONE, DS_CONFIGURED, DS_CONNECTED, DS_READY, DS_WAITING, DS_COMERROR]  //allowed states for free
                );
implementation

// =============================================================================
// Class        : TDeviceBase
// Function     : Create
//                constructor of the class, which initializes the instance with
//                default values
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
constructor TDeviceBase.Create(owner: TComponent);
begin
	inherited Create(owner);
  e_state := DS_NONE;
  i_timeout := C_TIMEOUT_MSEC;
  i_lasterr := 0;
  b_comhexa := false;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : Destroy
//                destructor of the class, which finilizes the instance
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
destructor TDeviceBase.Destroy;
begin
	inherited Destroy;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : SetTimeout
//                set timeout in milli seconds for communication
// Parameter    : msec, milli seconds to be set
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.SetTimeout(const msec: integer): integer;
begin
  if (msec >= 0) then i_timeout := msec;
  result := i_timeout;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : SetComHexa
//                set
// Parameter    : bhex, transfering string will be converted into hexadicimal if it is true
// Return       : return current value of b_comhex
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.SetComHexa(const bhex: boolean = true): boolean;
begin
  b_comhexa := bhex;
  result := b_comhexa;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : GetLastError
//                return last error number
// Parameter    : msg, output string
// Return       : integer, last error number and set msg with the last message
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
{function TDeviceBase.GetLastError(var msg: string): Integer;
begin
  case i_lasterr of
  C_ERR_NOERROR: msg := 'No error exist.';
  else msg := 'This error is not specified.';
  end;
end;
}

// =============================================================================
// Class        : TDeviceBase
// Function     : GetDeviceStateString
//                return a string, which descipts current state of the device
// Parameter    : --
// Return       : string from C_STR_STATE
// Exceptions   : --
// First author : 2015-09-03 /bsu/
// History      :
// =============================================================================
function TDeviceBase.GetDeviceStateString: string;
begin
  result := C_STR_STATE[e_state];
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : PostEvent
//                change device state, state machine
// Parameter    : event, in which this function is called
//                ok, if the event is successfully executed
//                NOTE: this function has to be called at end of the following
//                functions, if they are overrided in sub class :
//                ConfigDevice, FreeDevice, Connect, Disconnect, Sync, SendStr, RecvStr
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure TDeviceBase.PostEvent(const event: EDeviceEvent; const ok: boolean);
begin
  if (e_state in C_DEV_STATES[event]) then begin
    case event of
      DE_CONFIG: if ok then e_state := DS_CONFIGURED;
      DE_CONNECT: if ok then e_state := DS_CONNECTED;
      DE_SYNC: if ok then e_state := DS_READY;
      DE_SEND: if ok then e_state := DS_WAITING;
      DE_RECV: begin
        if ok then e_state := DS_READY
        else e_state := DS_COMERROR;
      end;
      DE_DISCONNECT: if ok then e_state := DS_CONFIGURED;
      DE_FREE: if ok then e_state := DS_NONE;
    end;
  end;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : Sync
//                test communication with device and post event SYNC
// Parameter    : --
// Return       : boolean, true if the communication is ok. false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.Sync(): boolean;
var ds_set : set of EDeviceState;
begin
  ds_set := [DS_CONNECTED, DS_COMERROR];
  result := (e_state in ds_set);
  PostEvent(DE_SYNC, result);
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : TryToReady
//                try to change the device state into ready state
// Parameter    : --
// Return       : boolean, true if ready state is reached. false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.TryToReady: boolean;
begin
  case e_state of
    DS_NONE: ; //not possible. ConfigDevice has firstly to be called with ini outside of this class
    DS_CONFIGURED: if Connect() then Sync();
    DS_CONNECTED, DS_COMERROR: Sync();
    DS_READY: ; //do nothing
    DS_WAITING: ;//not possible because the answer is expected.
  end;
  result := (e_state = DS_READY);
end;

end.
