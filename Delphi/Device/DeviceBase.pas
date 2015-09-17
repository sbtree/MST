// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit DeviceBase;

interface
uses Classes, SysUtils, IniFiles, ConnBase, DataBuffer, ProtocolBase;

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
    c_timeout: cardinal;    //timeout in millisecond
    i_lasterr: integer;     //last error number
    s_lastmsg: string;      //last message
    s_devname: string;      //name of the device
    b_comhex : boolean;     //hexadizcimal data in string to transfer if it is true

    t_conn: TConnBase;      //connection
    t_prot: TProtBase;      //protocol of communication

    t_rbuf, t_wbuf: TCharBuffer;
  strict private
    function GetStateString : string;

  protected
    procedure PostEvent(const event: EDeviceEvent; const ok: boolean);
    function Sync(): boolean; virtual;
    function TryToReady: boolean; virtual;
    function CheckAnswer(): boolean; virtual; abstract;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property State : EDeviceState read e_state;
    property StateString : string read GetStateString;
    property HexComm : boolean read b_comhex write b_comhex;
    property Timeout : cardinal read c_timeout write c_timeout;

    function ConfigDevice(const ini: TMemIniFile): Boolean; virtual;abstract;
    function FreeDevice(): Boolean; virtual;
    function Connect(): Boolean; virtual;
    function Disconnect: boolean; virtual;
    function SendStr(const sData: string; const bAns: boolean = true): Integer; virtual;
    function RecvStr(var sdata: string): Integer; virtual;
    function GetLastError(var msg: string): Integer; virtual; abstract;
  end;
  PDeviceBase = ^TDeviceBase;

const
  C_TIMEOUT_MSEC: Cardinal = 30000; //default timeout 30000 milli seconds to wait for response
  C_TIMEOUT_ONCE = 2000; //

  C_ERR_NOERROR = $0000; // no error
  C_ERR_UNKNOWN = $8000; // base error number as unknown

  //define sets of device states, which are allowed for each event
  C_DEV_STATES: array[LOW(EDeviceEvent)..HIGH(EDeviceEvent)] of DeviceStateSet = (
                [DS_NONE, DS_CONFIGURED], //allowed states for config
                [DS_CONFIGURED, DS_CONNECTED], //allowed states for connect
                [DS_CONNECTED, DS_COMERROR], //allowed states for sync
                [DS_READY], //allowed states for send
                [DS_WAITING], //allowed states for recv
                [DS_CONNECTED, DS_READY, DS_WAITING, DS_COMERROR], //allowed states for disconnect
                [DS_NONE, DS_CONFIGURED, DS_CONNECTED, DS_READY, DS_WAITING, DS_COMERROR]  //allowed states for free
                );

  CSTR_DEV_TIMEOUT    : string = 'TIMEOUT';
  CSTR_DEV_DESCRIPTION: string = 'DESCRIPTION';
  CSTR_DEV_PRODUCER   : string = 'PRODUCER';
  CSTR_DEV_TYPE       : string = 'TYPE';

implementation

const
  CSTR_DEV_STATES : array[LOW(EDeviceState)..HIGH(EDeviceState)] of string = (
                  'unusable',
                  'configured',
                  'connected',
                  'communicable',
                  'waiting',
                  'device error'
                  );

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
  c_timeout := C_TIMEOUT_MSEC;
  i_lasterr := 0;
  b_comhex  := false;

  t_rbuf := TCharBuffer.Create;
  t_wbuf := TCharBuffer.Create;
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
  FreeAndNil(t_rbuf);
  FreeAndNil(t_wbuf);
	inherited Destroy;
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
function TDeviceBase.GetStateString: string;
begin
  result := CSTR_DEV_STATES[e_state];
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

// =============================================================================
// Class        : TDeviceBase
// Function     : FreeDevice
//                release device, undo what is done in ConfigDevice
// Parameter    : --
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.FreeDevice(): boolean;
begin
  result := Disconnect();
  PostEvent(DE_FREE, result);;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : Connect
//                connects to the device (FlashRunner)
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.Connect: boolean;
begin
  result := false;
  if ((e_state in C_DEV_STATES[DE_CONNECT]) and assigned(t_conn)) then begin
    result := t_conn.Connect();
    PostEvent(DE_CONNECT, result);
  end;
  result := result and (e_state in C_DEV_STATES[DE_DISCONNECT]);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Disconnect
//                disconnect from the device
// Parameter    : --
// Return       : true, if the device is disconnected successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.Disconnect(): boolean;
begin
  result := t_conn.Disconnect();
  PostEvent(DE_DISCONNECT, result);
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : SendStr
//                send string to device
// Parameter    : str, a string to send
// Return       : integer, which counts char, which are sent
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.SendStr(const sData: string; const bAns: boolean): integer;
var i_len: integer;
begin
  result := 0;
  if TryToReady() then begin
    //clear read-buffer of t_ser
    t_conn.RecvData(t_rbuf, C_TIMEOUT_ONCE);
    t_rbuf.Clear;

    //send string and wait til write-buffer is completely sent
    i_len := t_wbuf.WriteStr(sData);
    result := t_conn.SendData(t_wbuf, c_timeout);

    PostEvent(DE_SEND, (result = i_len));
    if (not bAns) then PostEvent(DE_RECV, true);
  end;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : RecvStr
//                receiv string from device
// Parameter    : str, a string for receiving
// Return       : integer, which counts char, which are received
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.RecvStr(var sdata: string): integer;
var b_ok: boolean;
begin
  result := 0;
  if (e_state in C_DEV_STATES[DE_RECV]) then begin
    t_rbuf.Clear;
    result := t_conn.RecvData(t_rbuf, c_timeout);
    sdata := t_rbuf.ReadStr(false);
    b_ok := CheckAnswer();
    PostEvent(DE_RECV, b_ok);
  end;
end;

end.
