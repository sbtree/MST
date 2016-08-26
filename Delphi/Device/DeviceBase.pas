// =============================================================================
// Module name  : $RCSfile: DeviceBase.pas,v $
// Description  : This unit implements a base class of measurement device
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      : 2016-08-26 /bsu/ changed using general connection object (TConnBase)
//==============================================================================
unit DeviceBase;

interface
uses Classes, IniFiles, ConnBase, TextMessage, StringPairs;

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
                  DS_COMMOK, //communication ok state, in which the device is ready to communicate
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
    function SetConfig(const conf: TStringPairs; const cname: string): boolean;
    function GetState(): EDeviceState;
    function InitDevice(): boolean;
    function SendCommand(const cmd: string): boolean;
    function RecvAnswer(var ans: string): boolean;
    function SendPacket(packet: array of byte): boolean;
    function RecvPacket(packet: array of byte): boolean;
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
  TDeviceBase=class(TComponent, IDeviceBase)
  protected
  var
    e_state: EDeviceState;  //device state
    //c_timeout: cardinal;    //timeout in millisecond
    //i_lasterr: integer;     //last error number
    //s_lastmsg: string;      //last message
    s_devname: string;      //name of the device
    //b_comhex : boolean;     //convert string, in which the hexadicimal data are presented, into hexadicimal value, if it is true
    t_curconn: TConnBase;   //current connection

    //t_rbuf, t_wbuf: TCharBuffer; //buffer for receiving and sending data
    t_rbuf, t_wbuf: array [0..1023] of char; //buffer for receiving and sending data
  private
    function GetStateText : string;

  protected
    //procedure PostEvent(const event: EDeviceEvent; const ok: boolean);
    //function CheckComm(): boolean; virtual;
    //function CheckAnswer(const ans: string): boolean; virtual; abstract;
    //function VerifySendingData(): boolean; virtual; abstract;
    //function VerifyReceivedData(): boolean; virtual; abstract;
    //function ConfigConnections(const ini: TMemIniFile; const secname: string): integer; virtual;
    function GetCurConnect(): TConnBase;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property State : EDeviceState read e_state;
    property StateText : string read GetStateText;
    //property HexComm : boolean read b_comhex write b_comhex;
    //property Timeout : cardinal read c_timeout write c_timeout;
    property CurConnect: TConnBase read GetCurConnect;

    //function ConfigDevice(const ini: TMemIniFile): Boolean; virtual;abstract;
    //function TryToReady(): boolean; virtual;
    //function Connect(): Boolean; virtual;
    //function Disconnect: boolean; virtual;
    //function ActiveConn(const ct: EConnectType): boolean; virtual;

    function SetConfig(const conf: TStringPairs; const cname: string): boolean; virtual;
    function GetState(): EDeviceState;
    function InitDevice(): boolean; virtual;
    function SendCommand(const cmd: string): boolean; virtual;
    function RecvAnswer(var ans: string): boolean; virtual;
    function SendPacket(packet: array of byte): boolean; virtual;
    function RecvPacket(packet: array of byte): boolean; virtual;
    function ReleaseDevice(): Boolean; virtual;

    //
    function Reset(const cmd: string): boolean; virtual;
    //function GetLastError(var msg: string): Integer; virtual; abstract;
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
                  [DS_NONE], //states, in which config is allowed
                  [DS_CONFIGURED], //allowed states for connect
                  [DS_CONNECTED, DS_COMMERR], //allowed states for sync
                  [DS_COMMOK], //allowed states for send
                  [DS_COMMOK, DS_BUSY], //allowed states for recv
                  [DS_CONNECTED, DS_COMMOK, DS_BUSY, DS_COMMERR], //allowed states for disconnect
                  [DS_CONFIGURED, DS_CONNECTED, DS_COMMOK, DS_BUSY, DS_COMMERR],
                  [DS_NONE, DS_CONFIGURED, DS_CONNECTED, DS_COMMOK, DS_BUSY, DS_COMMERR]  //allowed states for free
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
  e_state := DS_NONE;
  //c_timeout := C_TIMEOUT_MSEC;

  //t_rbuf := TCharBuffer.Create;
  //t_wbuf := TCharBuffer.Create;
  ZeroMemory(@t_rbuf, length(t_rbuf));
  ZeroMemory(@t_wbuf, length(t_wbuf));
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

// =============================================================================
// Class        : TDeviceBase
// Function     : PostEvent
//                change device state, state machine
// Parameter    : event, in which this function is called
//                ok, indicates if the event is successfully executed
//                NOTE: this function has to be called at end of the following
//                functions, if they are overrided in sub class :
//                ConfigDevice, FreeDevice, Connect, Disconnect, Sync, SendStr, RecvStr
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
{procedure TDeviceBase.PostEvent(const event: EDeviceEvent; const ok: boolean);
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
end;  }

// =============================================================================
// Class        : TDeviceBase
// Function     : CheckComm
//                test communication with device and post event SYNC
// Parameter    : --
// Return       : boolean, true if the communication is ok. false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
{function TDeviceBase.CheckComm(): boolean;
begin
  result := (e_state in C_DEV_INSTATES[DE_SYNC]);
  if result then e_state := DS_COMMOK;
end; }

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
{function TDeviceBase.TryToReady: boolean;
begin
  case e_state of
    DS_NONE: ; //not possible. ConfigDevice has firstly to be called with ini outside of this class
    DS_CONFIGURED: if Connect() then CheckComm();
    DS_CONNECTED, DS_COMMERR: CheckComm();
    DS_COMMOK: ; //do nothing
    DS_BUSY: ;//not possible because the answer is expected.
  end;
  result := (e_state = DS_COMMOK);
end; }

// =============================================================================
// Class        : TDeviceBase
// Function     : ConfigConnections
//                read configurations from the section secname of ini and instance
//                appropriate connection, e.g.: rs232, usb, ethernet, and so on if
//                they are configurated in ini
// Parameter    : ini, TMemIniFile of configuration
//                secname, name of the section for this device
// Return       : integer, count of the configurated connections
// Exceptions   : --
// First author : 2015-09-18 /bsu/
// History      :
// =============================================================================
{function TDeviceBase.ConfigConnections(const ini: TMemIniFile; const secname: string): integer;
var i: EConnectType; s_inivalue: string;
begin
  result := 0;
  for i := LOW(EConnectType) to HIGH(EConnectType) do begin
    if ini.ValueExists(secname, CSTR_CONN_KEYS[i]) then begin
      s_inivalue := trim(ini.ReadString(secname, CSTR_CONN_KEYS[CT_RS232], ''));
      if not assigned(t_conns[i]) then begin
        case i of
          CT_RS232: begin
            t_conns[i] := TMtxRS232.Create(self);
            if (t_conns[i].Config(s_inivalue)) then begin
              inc(result);
              e_actconn := CT_RS232;
            end;
          end;
          CT_JTAG, CT_GPIB, CT_USB, CT_ETHERNET, CT_CAN, CT_PROFIL: ; //todo
        end;
      end;
    end;
  end;
end;}

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
  if result then e_state := DS_NONE;
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
{function TDeviceBase.Connect: boolean;
begin
  result := false;
  if ((e_state in C_DEV_INSTATES[DE_CONNECT]) and assigned(t_curconn)) then begin
    result := t_curconn.Connect();
    e_state := DS_CONNECTED;
  end;
  result := result and (e_state in C_DEV_INSTATES[DE_DISCONNECT]);
end; }

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
{function TDeviceBase.Disconnect(): boolean;
begin
  result := true;
  if assigned(t_curconn) then
    result := t_curconn.Disconnect();
end; }

// =============================================================================
// Class        : TDeviceBase
// Function     : ActiveConn
//                activate connection of the given type
// Parameter    : ct, connection type to be activated
// Return       : true, if the connection is available und can be connected
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
{function TDeviceBase.ActiveConn(const ct: EConnectType): boolean;
begin
  result := false;
  if assigned(t_conns[ct])  then begin
    result := t_conns[ct].Connect();
    if result then begin
      e_actconn := ct;
      e_state := DS_CONNECTED;
    end;
  end;
end; }

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
