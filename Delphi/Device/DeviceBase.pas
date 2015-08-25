// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit DeviceBase;

interface
uses Classes, IniFiles;
type
// =============================================================================
// Enumeration  : EDeviceState
// Description  : This enumeration describes current state of the device.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceState = (
                  DS_NONE, //none state, in which the device is not yet configured
                  DS_CONFIGURED, //configured state, in which the device is configured
                  DS_CONNECTED, //connected state, in which the device is connected
                  DS_READY, //ready state, in which the device is ready to communicate
                  DS_BUSY, //busy state, in which the device is communicating with the program
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
                  DE_COORDINATE, //event, which tries to communicate with device, e.g. Coordinate is called
                  DE_SEND, //event, which sends data to device, e.g. SendData is called
                  DE_RECV, //event, which receives data from device, e.g. RecvData is called
                  DE_DISCONNECT, //event, which disconnects from device, e.g. Disconnect is called
                  DE_FREE //event, which releases device, e.g. FreeDevice is called
                  );

  // =============================================================================
  // Interface    : IDeviceInterf
  // Description  : Definition of interface TDeviceBase, in which the basis functionnalities
  //                of measurement device.
  // First author : 2015-08-14 /bsu/
  // History      :
  // =============================================================================
  IDeviceInterf = interface
    function ConfigDevice(const ini: TMemIniFile): Boolean;
    function FreeDevice(): Boolean;
    function Connect(): Boolean;
    function Coordinate(): boolean;
    function SendData(const data: PChar; const ans: boolean = true): Integer;
    function RecvData(var data:PChar): Integer;
    function Disconnect: boolean;
  end;

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
    e_state:EDeviceState;
    i_timeout: cardinal;
    i_lasterr: integer;
  protected
    procedure PostEvent(const event: EDeviceEvent; const ok: boolean);

  public
    property DeviceState : EDeviceState read e_state;

    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function SetTimeout(const msec: integer): integer;
    function GetLastError(): Integer;

//    function ConfigDevice(const ini: TMemIniFile): boolean;virtual;
//    function FreeDevice(): boolean;virtual;
//    function Connect(): boolean;virtual;
//    function Coordinate(): boolean;virtual;
//    function SendData(const data: PChar; const ans: boolean = true): Integer;virtual;
//    function RecvData(var data:PChar): Integer;virtual;
  end;

const
  C_TIMEOUT_MSEC: Cardinal = 30000;  //default timeout 30000 milli seconds for communication
  C_DELAY_MSEC: Cardinal = 20;  //delay 20 milli seconds for communication error

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
  if (msec < 0) then i_timeout := C_TIMEOUT_MSEC
  else i_timeout := msec;
  result := i_timeout;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : GetLastError
//                return last error number
// Parameter    : --
// Return       : integer, last error number
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.GetLastError(): Integer;
begin
  result := i_lasterr;
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : PostEvent
//                change device state, state machine
// Parameter    : event, in which this function is called
//                ok, if the event is successfully executed
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure TDeviceBase.PostEvent(const event: EDeviceEvent; const ok: boolean);
var ds_set: set of EDeviceState;
begin
  case event of
    DE_CONFIG: if ((e_state = DS_NONE) and ok) then e_state := DS_CONFIGURED;
    DE_CONNECT: if ((e_state = DS_CONFIGURED) and ok) then e_state := DS_CONNECTED;
    DE_COORDINATE: begin
      ds_set := [DS_CONNECTED, DS_COMERROR];
      if ((e_state in ds_set) and ok) then e_state := DS_READY;
    end;
    DE_SEND: if ((e_state = DS_READY) and ok) then e_state := DS_BUSY;
    DE_RECV: begin
      if (e_state = DS_BUSY) then begin
        if ok then e_state := DS_READY
        else e_state := DS_COMERROR;
      end;
    end;
    DE_DISCONNECT: begin
      ds_set := [DS_CONFIGURED, DS_CONNECTED, DS_READY, DS_COMERROR];
      if ((e_state in ds_set) and ok) then e_state := DS_CONFIGURED;
    end;
    DE_FREE: if ok then e_state := DS_NONE;
  end;
end;

end.
