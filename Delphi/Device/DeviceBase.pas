// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      : 2016-08-26 /bsu/ changed using general connection object (TConnBase)
//==============================================================================
unit DeviceBase;

interface
uses Classes, ConnBase, TextMessage, StringPairs;

type
// =============================================================================
// Enumeration  : EDeviceState
// Description  : This enumeration describes current state of the device.
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  EDeviceState = (
                  DS_UNKNOWN,     //none state, in which the device object is just created and yet configured
                  DS_CONFIGURED,  //configured state, in which the device is configured
                  DS_CONNECTED,   //connected state, in which the device is connected
                  DS_COMREADY,    //communication ok state, in which the device is ready to communicate
                  DS_BUSY, //busy state, in which the device is transfering data or waiting for answer
                  DS_COMMERR //communication error state, in which an error exists in the last communication
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
                  DE_RESET, //event to reset device
                  DE_FREE //event, which releases device, e.g. FreeDevice is called
                  );
  DeviceStateSet = set of EDeviceState;

  IDeviceBase = interface
    //function SetConfig(const conf: TStringPairs; const cname: string): boolean;
    //function GetState(): EDeviceState;
    function InitDevice(): boolean;
    function SendCommand(const cmd: string): boolean;
    function RecvAnswer(var ans: string): boolean;
    //function SendPacket(packet: array of byte): boolean;
    //function RecvPacket(packet: array of byte): boolean;
    function ReleaseDevice(): boolean;
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
  TDeviceBase=class(TComponent, IDeviceBase, ITextMessengerImpl)
  protected
  var
    e_state: EDeviceState;  //device state
    t_curconn: TConnBase;   //current connection
    s_devname: string;      //name of the device
    t_messenger:TTextMessenger; //for transfering messages
    t_msgrimpl: TTextMessengerImpl;
  private
    function GetStateText : string;

  protected
    function GetCurConnect(): TConnBase;
    //procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO); virtual;
    //procedure UpdateMessage(const text: string; const level: EMessageLevel = ML_INFO); virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function InitDevice(): boolean; virtual;
    function SendCommand(const cmd: string): boolean; virtual;
    function RecvAnswer(var ans: string): boolean; virtual;
    //function SendPacket(packet: array of byte): boolean; virtual;
    //function RecvPacket(packet: array of byte): boolean; virtual;
    function ReleaseDevice(): Boolean; virtual;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    //property Messenger: TTextMessenger read t_messenger write t_messenger;
    property State : EDeviceState read e_state;
    property StateText : string read GetStateText;
    property CurConnect: TConnBase read GetCurConnect;

    //additional functions of device
    function Reset(const cmd: string): boolean; virtual;
  end;
  PDeviceBase = ^TDeviceBase;

const
  C_TIMEOUT_MSEC: Cardinal = 30000; //default timeout 30000 milli seconds to wait for response
  C_TIMEOUT_ONCE = 2000; //
  C_RESET_MSEC: Cardinal = 600000; //default timeout 600000 milli seconds (10 minutes) to wait for resetting

  C_ERR_NOERROR = $0000; // no error
  C_ERR_UNKNOWN = $8000; // base error number as unknown

  //define sets of device states, which are allowed for each event
  C_DEV_INSTATES: array[EDeviceEvent] of DeviceStateSet = (
                  [DS_UNKNOWN], //states, in which config is allowed
                  [DS_CONFIGURED], //allowed states for connect
                  [DS_CONNECTED, DS_COMMERR], //allowed states for sync
                  [DS_COMREADY], //allowed states for send
                  [DS_COMREADY, DS_BUSY], //allowed states for recv
                  [DS_CONNECTED, DS_COMREADY, DS_BUSY, DS_COMMERR], //allowed states for disconnect
                  [DS_CONFIGURED, DS_CONNECTED, DS_COMREADY, DS_BUSY, DS_COMMERR],
                  [DS_UNKNOWN, DS_CONFIGURED, DS_CONNECTED, DS_COMREADY, DS_BUSY, DS_COMMERR]  //allowed states for free
                  );

  CSTR_DEV_TIMEOUT    : string = 'TIMEOUT';
  CSTR_DEV_DESCRIPTION: string = 'DESCRIPTION';
  CSTR_DEV_PRODUCER   : string = 'PRODUCER';
  CSTR_DEV_TYPE       : string = 'TYPE';

implementation
uses Windows, SysUtils, GenUtils, RS232;

const
  CSTR_DEV_STATES : array[EDeviceState] of string = (
                    'unusable',
                    'configured',
                    'connected',
                    'communicable',
                    'waiting',
                    'error'
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
  t_msgrimpl := TTextMessengerImpl.Create();
  t_msgrimpl.OwnerName := ClassName();
  e_state := DS_UNKNOWN;
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
  t_msgrimpl.Free();
	inherited Destroy;
end;


// =============================================================================
// Class        : TDeviceBase
// Function     : GetStateString
//                return a string, which descipts current state of the device
// Parameter    : --
// Return       : string from CSTR_DEV_STATES
// Exceptions   : --
// First author : 2015-09-03 /bsu/
// History      :
// =============================================================================
function TDeviceBase.GetStateText: string;
begin
  result := CSTR_DEV_STATES[e_state];
end;

function TDeviceBase.GetCurConnect(): TConnBase;
begin
  result := t_curconn;
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
function TDeviceBase.ReleaseDevice(): boolean;
begin
  result := false; //Disconnect();
  //todo:
  if result then e_state := DS_UNKNOWN;
end;

function TDeviceBase.InitDevice(): boolean;
begin
  result := false;
  //todo:
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : SendStr
//                send string to device
// Parameter    : str, a string to send
//                bAns, indicates if an answer is expected
// Return       : integer, count of sent chars
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.SendCommand(const cmd: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then
    result := t_curconn.SendStr(cmd);
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : RecvAnswer
//                receiv string from device
// Parameter    : str, a string for receiving
// Return       : integer, count of the received char
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.RecvAnswer(var ans: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then
    result := (t_curconn.RecvStr(ans) > 0);
end;

// =============================================================================
// Class        : TDeviceBase
// Function     : Reset
//                activate connection of the given type
// Parameter    : ct, connection type to be activated
// Return       : true, if the connection is available und can be connected
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TDeviceBase.Reset(const cmd: string): boolean;
begin
  result := false;
  if assigned(t_curconn) then begin
    t_curconn.Disconnect();
    result := t_curconn.Connect();
    if result then
      result := InitDevice();
  end;
end;

end.
