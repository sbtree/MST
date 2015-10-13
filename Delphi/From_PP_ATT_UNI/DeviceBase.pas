// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/  defines and uses constant C_DEV_STATES
//                                  renames function and changes its returned value
//==============================================================================
unit DeviceBase;

interface
uses Classes, IniFiles, SysUtils;
type
// =============================================================================
// Enumeration  : EDeviceState
// Description  : This enumeration describes current state of the device.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceState = (
                  DS_NONE, //none state, in which the device object is just created and not yet configured
                  DS_CONFIGURED, //configured state, in which the device is configured
                  DS_CONNECTED, //connected state, in which the device is connected
                  DS_COMMOK, //communication ok state, in which the device is ready to communicate
                  DS_WAITING, //waiting state, in which the device is waiting for answer
                  DS_COMMERR //communication error state, in which an error exists in the last communication
                  );
  DeviceStateSet = set of EDeviceState;
// =============================================================================
// Enumeration  : EDeviceEvent
// Description  : This enumeration describes events, which function of IDeviceInterf is called.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceEvent = (
                  DE_CONFIG, //event, which configs device, e.g. ConfigDevice is called
                  DE_CONNECT, //event, which connects device, e.g. Connect is called
                  DE_CHECKCOMM, //event, which tries to communicate with device, e.g. CheckComm is called
                  DE_SEND, //event, which sends data to device, e.g. SendData is called
                  DE_RECV, //event, which receives data from device, e.g. RecvData is called
                  DE_DISCONNECT, //event, which disconnects from device, e.g. Disconnect is called
                  DE_FREE //event, which releases device, e.g. FreeDevice is called
                  );

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
    c_timeout: cardinal;  //timeout in millisecond
    i_lasterr: integer;  //last error number
    s_lastmsg: string;  //last message

  strict private
    function  GetDeviceStateText : string;
    function  GetLastErrorText(): string;

  protected
    procedure PostEvent(const event: EDeviceEvent; const ok: boolean);
    procedure ClearError();
    procedure SetStateError();
    function  SetTimeout(const msec: string): boolean;

  public
    constructor Create(owner: TComponent); override;
    destructor  Destroy; override;

    property DeviceState : EDeviceState read e_state;
    property DeviceStateText : string read GetDeviceStateText;
    property LastError : integer read i_lasterr;
    property LastErrorText : string read GetLastErrorText;
    property Timeout : cardinal read c_timeout;

    function TryToReady: boolean; virtual;

    function ConfigDevice(const ini: TMemIniFile): Boolean; virtual; abstract;
    function FreeDevice(): Boolean; virtual; abstract;
    function Connect(): Boolean; virtual; abstract;
    function CheckComm(): boolean; virtual;
    function Disconnect: boolean; virtual; abstract;
    function SendStr(const data: string; const ans: boolean = true): boolean; virtual; abstract;
    function RecvStr(var data: string): Integer; virtual; abstract;
  end;

const
  C_TIMEOUT_MSEC: Cardinal = 30000;  //default timeout 30000 milli seconds for communication
  C_DELAY_ONCE: Cardinal = 30;  //delay 30 milli seconds for communication error
  CSTR_DEV_STATE : array[EDeviceState] of string = ('not configured','configured','connected','ok','waiting','error');
  //define sets of device states, which are allowed for each event
  C_DEV_INSTATES: array[EDeviceEvent] of DeviceStateSet = (
                [DS_NONE], //allowed states for ConfigDevice
                [DS_CONFIGURED], //allowed states for Connect
                [DS_CONNECTED, DS_COMMERR, DS_COMMOK], //allowed states for CheckComm
                [DS_COMMOK], //allowed states for SendStr
                [DS_WAITING], //allowed states for RecvStr
                [DS_CONNECTED, DS_COMMOK, DS_WAITING, DS_COMMERR], //allowed states for Disconnect
                [DS_CONFIGURED, DS_CONNECTED, DS_COMMOK, DS_WAITING, DS_COMMERR]  //allowed states for FreeDevice
                );
                
  C_DEV_OUTSTATES: array[EDeviceEvent] of DeviceStateSet = (
                [DS_CONFIGURED, DS_CONNECTED, DS_COMMOK, DS_WAITING, DS_COMMERR], //all possible states presented as configurated
                [DS_CONNECTED, DS_COMMOK, DS_WAITING, DS_COMMERR], //all possible states presented as connected
                [DS_COMMOK], //all possible states presented as communication ok
                [DS_COMMOK, DS_WAITING], //all possible states presented as sending finished
                [DS_COMMOK, DS_COMMERR], //all possible states presented as receiving finisched 
                [DS_NONE, DS_CONFIGURED], //all possible states presented as disconnected
                [DS_NONE]  //all possible states presented as device released
                );

  C_DEV_ERR_OK      = $000000000;
  C_DEV_ERR_CONF    = $000000001;
  C_DEV_ERR_CONN    = $000000002;
  C_DEV_ERR_SEND    = $000000003;
  C_DEV_ERR_RECV    = $000000004;
  C_DEV_ERR_DISC    = $000000005;
  C_DEV_ERR_FREE    = $000000006;
  C_DEV_ERR_TIMEOUT = $000000007;
  C_DEV_ERR_ANSW    = $000000008;
  C_DEV_ERR_STATE   = $000000009;

implementation
uses StrUtils;
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
  ClearError();
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
// Parameter    : msec, milliseconds in string to be set
// Return       : true, if the given parameter is accepted. Otherwise false.
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ returned value is changed
// =============================================================================
function TDeviceBase.SetTimeout(const msec: string): boolean;
var i_msec: integer;
begin
  i_msec := -1;
  result := TryStrToInt(msec, i_msec);
  if (result and (i_msec >= 0)) then  begin
    c_timeout := cardinal(i_msec);
    result := true;
  end else result := false;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : GetLastErrorText
//                return a string to descript the last error
// Parameter    : --
// Return       : string to descript the last error
// Exceptions   : --
// First author : 2015-09-29 /bsu/
// =============================================================================
function TDeviceBase.GetLastErrorText(): string;
var s_exinfo: string;
begin
  result := '';
  s_exinfo := IfThen(s_lastmsg = '', '', '['+ s_lastmsg + ']');
  case i_lasterr of
    C_DEV_ERR_OK:     result := 'No error exists ' + s_exinfo;
    C_DEV_ERR_CONF:   result := 'Failed to configurate the device ' + s_exinfo;
    C_DEV_ERR_CONN:   result := 'Failed to connect the device ' + s_exinfo;
    C_DEV_ERR_SEND:   result := 'Failed to send data to device ' + s_exinfo;
    C_DEV_ERR_RECV:   result := 'Failed to receive data from the device ' + s_exinfo;
    C_DEV_ERR_FREE:   result := 'Failed to release the device ' + s_exinfo;
    C_DEV_ERR_TIMEOUT:result := 'Timeout is reached in last communication ' + s_exinfo;
    C_DEV_ERR_ANSW:   result := 'Error in the answer ' + s_exinfo;
    C_DEV_ERR_STATE:  result := 'Wrong state in doing current task ' + s_exinfo;
  end;
end;


// =============================================================================
// Class        : TDeviceBase
// Function     : GetDeviceStateText
//                return a string, which descipts current state of the device
// Parameter    : --
// Return       : string from C_STR_STATE
// Exceptions   : --
// First author : 2015-09-03 /bsu/
// History      :
// =============================================================================
function TDeviceBase.GetDeviceStateText: string;
begin
  result := CSTR_DEV_STATE[e_state];
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : PostEvent
//                change device state, state machine
// Parameter    : event, in which this function is called
//                ok, if the event is successfully executed
//                NOTE: this function has to be called at end of the following
//                functions, if they are overrided in sub class :
//                ConfigDevice, FreeDevice, Connect, Disconnect, CheckComm, SendStr, RecvStr
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure TDeviceBase.PostEvent(const event: EDeviceEvent; const ok: boolean);
begin
  if (e_state in C_DEV_INSTATES[event]) then begin
    case event of
      DE_CONFIG: if ok then e_state := DS_CONFIGURED;
      DE_CONNECT: if ok then e_state := DS_CONNECTED;
      DE_CHECKCOMM: if ok then e_state := DS_COMMOK else e_state := DS_COMMERR;
      DE_SEND: if ok then e_state := DS_WAITING;
      DE_RECV: begin
        if ok then e_state := DS_COMMOK
        else e_state := DS_COMMERR;
      end;
      DE_DISCONNECT: if ok then e_state := DS_CONFIGURED;
      DE_FREE: if ok then e_state := DS_NONE;
    end;
  end;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : ClearError, an auxiliary function
//                assign last error with C_DEV_ERR_OK and last message with empty
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2015-10-07 /bsu/
// History      :
// =============================================================================
procedure TDeviceBase.ClearError();
begin
    i_lasterr := C_DEV_ERR_OK;
    s_lastmsg := '';
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : SetStateError, an auxiliary function
//                set last error and last message with state error
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2015-10-07 /bsu/
// History      :
// =============================================================================
procedure TDeviceBase.SetStateError();
begin
    i_lasterr := C_DEV_ERR_STATE;
    s_lastmsg := 'current state:' + DeviceStateText;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : CheckComm
//                test communication with device and post event CHECKCOMM
// Parameter    : --
// Return       : boolean, true if the communication is ok. false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      : 2015-09-29 /bsu/ use constant C_DEV_STATES
// =============================================================================
function TDeviceBase.CheckComm(): boolean;
begin
  result := (e_state in C_DEV_INSTATES[DE_CHECKCOMM]);
  PostEvent(DE_CHECKCOMM, result);
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : TryToReady, an auxiliary function
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
    DS_CONFIGURED: if Connect() then CheckComm();
    DS_CONNECTED, DS_COMMERR, DS_COMMOK: CheckComm();
    DS_NONE, //not possible. ConfigDevice has firstly to be called with ini outside of this class
    DS_WAITING: //not possible because the answer is expected.
      SetStateError();
  end;
  result := (e_state in C_DEV_OUTSTATES[DE_CHECKCOMM]);
end;

end.
