// =============================================================================
// Module name  : $RCSfile: ConnBase.pas,v $
// description  : This unit implements a basis methodes and properties of a class
//                for connection FlashRunner. The types of connection includes Jtag
//                RS232, USB, GPIB, CAN-bus and Profil-Bus.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit ConnBase;

interface
uses Classes, TextMessage, IniFiles, Serial3, SyncObjs, SysUtils, DataBuffer;
const
  C_BUFFER_SIZE_WRITE = 512;
  C_BUFFER_SIZE_READ = 2048;

type
  //enumeration for all connection type
  EConnectType = (CT_UNKNOWN, //unknown connection
                  CT_RS232,   //rs232
                  CT_MTXUSB,  //metronix usb (usbiocom)
                  CT_TEKUSB,  //usb interface to oscilloscope (Tektronix)
                  CT_GPIB,    //GPIB (IEEE 488)
                  CT_ETHERNET,//ethernet
                  CT_JTAG,    //jtag
                  CT_PCAN,    //can-bus over pcan-adapter
                  CT_PROFI    //profi-bus
                  );
  //enumeration of connection states
  EConnectState = ( CS_UNKNOWN,   //unknown state
                    CS_CONFIGURED,//connection is configurated
                    CS_CONNECTED  //connection is connected and in use
                   );

  //definition of a interface for communication
  ICommInterf = interface
    function Config(const sconf: string): boolean; overload;
    function Config(const sconfs: TStrings): boolean; overload;
    function Connect(): boolean;
    function Disconnect: boolean;
    function SendPacket(const pbuf: PByteArray; const wlen: word): boolean;
    function SendStr(const str: string): boolean;
    function RecvPacket(var pbuf: PByteArray; var wlen: word; const bwait: boolean): boolean;
    function RecvStr(var str: string; const bwait: boolean): integer;
    function ExpectStr(var str: string; const swait: string; const bcase: boolean): boolean;
  end;

  //base class of connection
  TCommBase = class(TComponent, ICommInterf, ITextMessengerImpl)
  class function GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
  class function GetConnectTypeName(const etype: EConnectType): string;
  class function GetConnectState(const estate: EConnectState): string;
  protected
    e_type:     EConnectType;       //connection type
    e_state:    EConnectState;      //connection state
    c_timeout:  cardinal;           //timeout in milli seconds
    t_msgrimpl: TTextMessengerImpl; //for transfering messages
    t_rxwait:   TEvent;             //wait event for reading
    t_txwait:   TEvent;             //wait event for writing complete
  protected
    function GetTypeName(): string; virtual;
    function GetStateStr(): string; virtual;
    function PacketToStr(const pbytes: PByteArray; const wlen: Word; const bhex: Boolean = True): string; virtual; abstract;

    function WaitForReceiving(const tend: cardinal): boolean; virtual;
    function WaitForSending(const tend: cardinal): boolean; virtual;
    function WaitForConnecting(const tend: cardinal): boolean; virtual;
    function IsConnected(): boolean; virtual;
    function IsReadReady(): boolean; virtual; abstract;
    function IsWriteComplete(): boolean; virtual; abstract;
    function TryConnect(): boolean; virtual; abstract;
    function TryDisconnect(): boolean; virtual; abstract;

    function InitBuffer(): boolean; virtual; abstract;
    function WriteStrToBuffer(const txstr: string): boolean; virtual; abstract;
    function WritePacketToBuffer(const pbytes: PByteArray; const wlen: word): boolean; virtual; abstract;
    function SendFromBuffer(): boolean; virtual; abstract;
    function ReadStrFromBuffer(): string; virtual; abstract;
    function ReadPacketFromBuffer(var pbytes: PByteArray; var wlen: word): integer; virtual; abstract;
    function RecvToBuffer(): integer; virtual; abstract;
    function ClearBuffer(): integer; virtual; abstract;
    procedure DeinitBuffer(); virtual; abstract;
  public
    //constructor and destructor
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    //delegate interface ITextMessengerImpl
    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;
    //base properties
    property Connected: boolean read IsConnected;
    property ConnectType: EConnectType read e_type;
    property ConnectTypeName: string read GetTypeName;
    property ConnectState: EConnectState read e_state;
    property Timeout: cardinal read c_timeout write c_timeout;

    //implementation of ICommInterf
    function Config(const sconf: string): boolean; overload; virtual;
    function Config(const sconfs: TStrings): boolean; overload; virtual; abstract;
    function Connect(): boolean; virtual;
    function Disconnect(): boolean; virtual;
    function SendPacket(const pbuf: PByteArray; const wlen: word): boolean; virtual;
    function SendStr(const str: string): boolean; virtual;
    function RecvPacket(var pbuf: PByteArray; var wlen: word; const bwait: boolean = true): boolean; virtual;
    function RecvStr(var str: string; const bwait: boolean = true): integer; virtual;
    function ExpectStr(var str: string; const swait: string; const bcase: boolean = false): boolean; virtual;

    //additionnal functions
    function RecvStrTimeout(var str: string; const timeout: cardinal): integer; virtual;
    function RecvStrInterval(var str: string; const timeout: cardinal; const interv: cardinal = 3000): integer; virtual;
    function RecvStrExpected(var str: string; const exstr: string; timeout: cardinal; const bcase: boolean = false): boolean; virtual;

    //sync methods
    procedure SetEventRx();
    procedure ResetEventRx();
    procedure SetEventTx();
    procedure ResetEventTx();
  end;
  PConnBase = ^TCommBase;

const
  //define names of connection types
  CSTR_CONN_KEYS : array[EConnectType] of string = (
                    'UNKNOWN',
                    'RS232',
                    'MTXUSB',
                    'TEKUSB',
                    'GPIB',
                    'ETHERNET',
                    'JTAG',
                    'PCAN',
                    'PROFI'
                    );
  //define connection state
  CSTR_CONN_STATES: array[EConnectState] of string = (
                    'unknown',
                    'configured',
                    'connected'
                    );
  CINT_TIMEOUT_DEFAULT = 2000;//default timeout in milliseconds
  CINT_INTERVAL_DEFAULT = 300;//default interval by reading in milliseconds
  CINT_RECV_INTERVAL = 50;    //milliseconds

implementation
uses Forms, StrUtils, Windows, Registry;

// =============================================================================
// Description  : a class function to get connection type using its name
// Parameter    : conkey, string to represent the name of a connection type
//                val, enum for output, see definition of EConnectType
// Return       : true, if the given conkey is found in the definiation of
//                connection type and the type value will be save in the
//                parameter val. Otherwise false. 
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
class function TCommBase.GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
var i_idx: integer;
begin
  i_idx := IndexText(conkey, CSTR_CONN_KEYS);
  if ((i_idx >= Ord(Low(EConnectType))) and (i_idx <= Ord(High(EConnectType)))) then begin
    val := EConnectType(i_idx);
    result := true;
  end else result := false;
end;

// =============================================================================
// Description  : a class function to get the name of a connection type
// Parameter    : etype, enum, see definition of EConnectType
// Return       : string, name of connection type
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
class function TCommBase.GetConnectTypeName(const etype: EConnectType): string;
begin
  result := CSTR_CONN_KEYS[etype];
end;

// =============================================================================
// Description  : a class function to get string which represents the current
//                state of the connection
// Parameter    : estate, enum, see definition of EConnectState
// Return       : string of current state
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
class function TCommBase.GetConnectState(const estate: EConnectState): string;
begin
  result := CSTR_CONN_STATES[estate];
end;

// =============================================================================
// Description  : get name of the connection type of the object
// Parameter    : --
// Return       : string of current state
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function  TCommBase.GetTypeName(): string;
begin
  result := TCommBase.GetConnectTypeName(e_type);
end;

// =============================================================================
// Description  : get string which represents the current state
// Parameter    : --
// Return       : string of current state
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.GetStateStr(): string;
begin
  result := TCommBase.GetConnectState(e_state);
end;

// =============================================================================
// Description  : wait until the first data arrives.
// Parameter    : tend, end time (current time + timeout)
// Return       : true, if the data arrives
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.WaitForReceiving(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count, c_secs: cardinal; b_wait, b_read: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_read := IsReadReady(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_read) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := format('Waiting for reading: %d', [Round((tend - c_tcur) / 1000)]) + ' ... %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing in gui
        if (tend > c_tcur) then c_secs := Round((tend - c_tcur) / 1000)
        else c_secs := 0;
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [c_secs]));
        c_count := c_tcur;
      end;
      b_read := IsReadReady();
      b_wait := ((not b_read) and (c_tcur < tend));
    until (not b_wait);
  end;
  result := b_read;
end;

// =============================================================================
// Description  : wait until the sending data is acturally sent.
// Parameter    : tend, end time (current time + timeout)
// Return       : true, if the data is sent
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.WaitForSending(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count, c_secs: cardinal; b_wait, b_write: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_write := IsWriteComplete(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_write) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := format('Waiting for completing write: %d', [Round((tend - c_tcur) / 1000)]) + ' ... %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        if (tend > c_tcur) then c_secs := Round((tend - c_tcur) / 1000)
        else c_secs := 0;
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [c_secs]));
        c_count := c_tcur;
      end;
      b_write := IsWriteComplete();
      b_wait := ((not b_write) and (c_tcur < tend));
    until (not b_wait);
  end;
  result := b_write;
end;

// =============================================================================
// Description  : try to connect more times till timeout or the connection is 
//                established if it is not connected
// Parameter    : tend, end time (current time + timeout)
// Return       : true, if it is connected
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.WaitForConnecting(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count, c_secs: cardinal; b_wait: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_wait := ((not Connected) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := format('Waiting for connecting: %d', [Round((tend - c_tcur) / 1000)]) + ' ... %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000.0)]));
    repeat
      Application.ProcessMessages();
      TryConnect();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        if (tend > c_tcur) then c_secs := Round((tend - c_tcur) / 1000)
        else c_secs := 0;
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [c_secs]));
        c_count := c_tcur;
      end;
      b_wait := ((not IsConnected()) and (c_tcur < tend));
    until (not b_wait);
  end;
  result := IsConnected();
end;

// =============================================================================
// Description  : query whether the object is connected
// Parameter    : --
// Return       : true, if it is connected
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.IsConnected(): boolean;
begin
  result := (e_state = CS_CONNECTED);
end;

// =============================================================================
// Description  : constuctor
// Parameter    : --
// Exceptions   : --
// First author : 2016-06-15 /bsu/
// History      :
// =============================================================================
constructor TCommBase.Create(owner: TComponent);
begin
  inherited Create(owner);
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
  e_type := CT_UNKNOWN;
  e_state := CS_UNKNOWN;
  c_timeout := CINT_TIMEOUT_DEFAULT;
  InitBuffer();
  t_rxwait := TEvent.Create(nil, false, false, 'TMtxConn.Rx');
  t_txwait := TEvent.Create(nil, false, false, 'TMtxConn.Tx');
end;

// =============================================================================
// Description  : destuctor
// Parameter    : --
// Exceptions   : --
// First author : 2016-06-15 /bsu/
// History      :
// =============================================================================
destructor TCommBase.Destroy;
begin
  FreeAndNil(t_txwait);
  FreeAndNil(t_rxwait);
  DeinitBuffer();
  t_msgrimpl.Free();
  inherited Destroy();
end;

// =============================================================================
//    Description  : config device using a string, which is formatted as
//                   'PropertyName1:value1|PropertyName2:value2' or
//                   'PropertyName1=value1|PropertyName2=value2'
//    Parameter    : sconf, a formatted string. As empty parameter is not acceptable
//    Return       : true, if the sconf is valid and accepted by the connection
//    First author : 2016-06-17 /bsu/
//    History      :
// =============================================================================
function TCommBase.Config(const sconf: string): boolean;
var t_confs: TStrings; s_conf: string;
begin
  result := false;
  s_conf := ReplaceStr(sconf, ':', '=');
  t_confs := TStringList.Create();
  if (ExtractStrings(['|'], [' ', #9], PChar(s_conf), t_confs) > 0) then
    result := Config(t_confs);
  t_confs.Free();
end;

// =============================================================================
// Description  : establisch connection
//                Note: This function will try to establish the connections
//                more times till timeout if the connection is not establisched
//                by the first one time
// Parameter    : --
// Return       : true, if the connection is established
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-12 /bsu/
// History      :
// =============================================================================
function TCommBase.Connect(): boolean;
begin
  result := false;
  if (e_state in [CS_CONFIGURED, CS_CONNECTED]) then begin
    TryConnect();
    result := WaitForConnecting(GetTickCount() + c_timeout);
    if result then begin
      e_state := CS_CONNECTED;
      t_msgrimpl.AddMessage(format('Successful to make a connection(%s).', [GetTypeName()]));
    end else
      t_msgrimpl.AddMessage(format('Failed to make a connection(%s)', [GetTypeName()]), ML_ERROR);
  end else
    t_msgrimpl.AddMessage(format('The current state (%s) is not suitable for making a connection.', [GetStateStr()]), ML_WARNING);
end;

// =============================================================================
// Description  : disconnect from the device and release the resource
// Parameter    : --
// Return       : true, if the device is disconnected
//                false, otherwise
// Exceptions   : --
// First author : 2016-11-28 /bsu/
// History      :
// =============================================================================
function TCommBase.Disconnect(): boolean;
begin
  if Connected then begin
    result := TryDisconnect();
    if result then begin
      if (e_state in [CS_CONFIGURED, CS_CONNECTED]) then e_state := CS_CONFIGURED;
      t_msgrimpl.AddMessage(format('Successful to disconnect from the device (type=%s).', [GetTypeName()]));
    end else
      t_msgrimpl.AddMessage(format('Failed to disconnect from the device (type=%s).', [GetTypeName()]), ML_ERROR);
  end else
    result := true;
end;

// =============================================================================
// Description  : send data in an array of char
// Parameter    : buf, pointer of the array to send
//                len, length of the array
// Return       : true, if the string is sent successfully
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.SendPacket(const pbuf: PByteArray; const wlen: word): boolean;
var c_tend: cardinal; s_packet: string;
begin
  result := false;
  if Connected then begin
    ClearBuffer();
    c_tend := GetTickCount() + c_timeout;
    s_packet := PacketToStr(pbuf, wlen, true);
    result := WritePacketToBuffer(pbuf, wlen);
    if result then begin
      result := SendFromBuffer();
      if result then result := WaitForSending(c_tend);
      if result then
        t_msgrimpl.AddMessage(format('Successful to send packet: data=%s (%d bytes)', [s_packet, wlen]))
      else
        t_msgrimpl.AddMessage(format('Failed to send packet: data=%s (%d bytes)', [s_packet, wlen]), ML_ERROR)
    end else
      t_msgrimpl.AddMessage(format('Failed to write packet into sending buffer: data=%s (%d bytes)', [s_packet, wlen]), ML_ERROR)
  end else
    t_msgrimpl.AddMessage(format('No data can be sent because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : send a string
// Parameter    : str, the string to send
// Return       : true, if the string is sent successfully
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.SendStr(const str: string): boolean;
var w_len: word; c_tend: cardinal;
begin
  result := false;
  if Connected then begin
    ClearBuffer();
    c_tend := GetTickCount() + c_timeout;
    w_len := length(str);
    result := WriteStrToBuffer(str);
    if result then begin
      result := SendFromBuffer();
      if result then result := WaitForSending(c_tend);
      if result then
        t_msgrimpl.AddMessage(format('Successful to send string: string=%s; length=%d', [str, w_len]))
      else
        t_msgrimpl.AddMessage(format('Failed to send string into sending buffer: string=%s; length=%d', [str, w_len]), ML_ERROR);
    end else
      t_msgrimpl.AddMessage(format('Failed to write string into sending buffer: string=%s; length=%d', [str, w_len]), ML_ERROR);
  end else
    t_msgrimpl.AddMessage(format('No data can be sent because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : receive data and save it in an array of char
// Parameter    : buf, an array variable for output
//                len, length of the array
//                bwait, indicates whether it should wait if no data is not available recently
//                Note: The property Timeout will be used if bwait is equal true.
// Return       : integer, the length of the received data in byte
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.RecvPacket(var pbuf: PByteArray; var wlen: word; const bwait: boolean): boolean;
var c_tend: cardinal; s_packet: string;
begin
  result := false;
  if Connected then begin
    c_tend := GetTickCount() + c_timeout;
    if bwait then WaitForReceiving(c_tend);
    result := (RecvToBuffer() > 0);
    if result then begin
      result := (ReadPacketFromBuffer(pbuf, wlen) > 0);
      if result then begin
        s_packet := PacketToStr(pbuf, wlen, true);
        t_msgrimpl.AddMessage(format('Successful to get packet from receieving buffer: data=%s (%d bytes)', [s_packet, wlen]))
      end else
        t_msgrimpl.AddMessage('Failed to get packet from receieving buffer', ML_ERROR);
    end else
      t_msgrimpl.AddMessage('No packet is received', ML_ERROR);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : receive a string
// Parameter    : str, output string which is received
//                bwait, indicates whether it should wait if no data is not available recently
//                Note: The property Timeout will be used if bwait is equal true.
// Return       : integer, the length of the received string
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.RecvStr(var str: string; const bwait: boolean): integer;
var c_tend: cardinal;
begin
  result := 0;
  if Connected then begin
    c_tend := GetTickCount() + c_timeout;
    if bwait then WaitForReceiving(c_tend);
    result := RecvToBuffer();
    if (result > 0) then begin
      str := ReadStrFromBuffer();
      if (str <> '') then
        t_msgrimpl.AddMessage(format('Successful to get string from receieving buffer: data=%s', [str]))
      else
        t_msgrimpl.AddMessage('Failed to get string from receieving buffer', ML_ERROR);
    end else
      t_msgrimpl.AddMessage('No string is received', ML_ERROR);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

function TCommBase.ExpectStr(var str: string; const swait: string; const bcase: boolean): boolean;
begin
  result := RecvStrExpected(str, swait, c_timeout, bcase);
end;


// =============================================================================
// Description  : force to receive string until the timeout is over
// Parameter    : str, output string which is received
//                timeout, in millisecond
// Return       : integer, the length of the received string
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.RecvStrTimeout(var str: string; const timeout: cardinal): integer;
var tend: cardinal;
begin
  result := 0; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      result := RecvToBuffer();
      if (result > 0) then str := str + ReadStrFromBuffer();
      Application.ProcessMessages();
    until (GetTickCount() >= tend);
    result := length(str);
    if (result > 0) then
      t_msgrimpl.AddMessage(format('Successful to receieve string: %s (length=%d)', [str, result]))
    else
      t_msgrimpl.AddMessage('Nothing is receieved.', ML_WARNING);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : force to receive string until the time is over or the interval is over
//                An interval is allowed between the data blocks if they arrive in more times
// Parameter    : str, output string which is received
//                timeout, time in millisecond
//                interv, an interval in millisecond
// Return       : integer, the length of the received string
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.RecvStrInterval(var str: string; const timeout: cardinal; const interv: cardinal): integer;
var c_time: cardinal; b_break: boolean; tend: cardinal;
begin
  result := 0; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      result := RecvToBuffer();
      if (result > 0) then str := str + ReadStrFromBuffer();
      c_time := GetTickCount() + interv;
      if (c_time > tend) then c_time := tend;
      Application.ProcessMessages();
      b_break := (not WaitForReceiving(c_time));
    until (b_break or (GetTickCount() >= tend));
    result := length(str);
    if (result > 0) then
      t_msgrimpl.AddMessage(format('Successful to receieve string: %s (length=%d)', [str, result]))
    else
      t_msgrimpl.AddMessage('Nothing is receieved.', ML_WARNING);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : force to receive string until the time is over or the expected string arrives
// Parameter    : str, output string which is received
//                exstr, expected string
//                timeout, in millisecond
//                bcase, indicates if case sensitivity should be considered
// Return       : integer, the length of the received string
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TCommBase.RecvStrExpected(var str: string; const exstr: string; timeout: cardinal; const bcase: boolean): boolean;
var tend: cardinal; w_len: word;
begin
  result := false; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      w_len := RecvToBuffer();
      if (w_len > 0) then str := str + ReadStrFromBuffer();
      if bcase then result := ContainsStr(str, exstr)
      else result := ContainsText(str, exstr);
      Application.ProcessMessages();
    until (result or (GetTickCount() >= tend));
    w_len := length(str);
    if (result) then
      t_msgrimpl.AddMessage(format('Successful to receieve string: %s (length=%d)', [str, w_len]))
    else
      t_msgrimpl.AddMessage('The expected string is not receieved.', ML_WARNING);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

// =============================================================================
// Description  : signal the read event
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
procedure TCommBase.SetEventRx();
begin
  t_rxwait.SetEvent();
end;

// =============================================================================
// Description  : unsignal the event of read complete
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
procedure TCommBase.ResetEventRx();
begin
  t_rxwait.ResetEvent();
end;

// =============================================================================
// Description  : signal the event of write complete
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
procedure TCommBase.SetEventTx();
begin
  t_txwait.SetEvent();
end;

// =============================================================================
// Description  : unsignal the event of write complete
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
procedure TCommBase.ResetEventTx();
begin
  t_txwait.ResetEvent();
end;

end.
