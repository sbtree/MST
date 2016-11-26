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
uses Classes, TextMessage, IniFiles, Serial3, SyncObjs, SysUtils;
const
  C_BUFFER_SIZE_DEFAULT = 1024;

type
  //enumeration for all connection type
  EConnectType = (CT_UNKNOWN, //unknown connection
                  CT_RS232,   //rs232
                  CT_USB,     //usb
                  CT_GPIB,    //GPIB (IEEE 488)
                  CT_ETHERNET,//ethernet
                  CT_JTAG,    //jtag
                  CT_CAN,     //can-bus
                  CT_PROFIL   //profil-bus
                  );
  //enumeration of connection states
  EConnectState = ( CS_UNKNOWN,   //unknown state
                    CS_CONFIGURED,//connection is configurated
                    CS_CONNECTED  //connection is connected and in use
                   );

  //definition of a interface for communication
  IConnInterf = interface
    function Config(const sconf: string): boolean; overload;
    function Config(const sconfs: TStrings): boolean; overload;
    function Connect(): boolean;
    function Disconnect: boolean;
    function SendBuf(const pbuf: PByteArray; const wlen: word): boolean;
    function SendStr(const str: string): boolean;
    function RecvBuf(const pbuf: PByteArray; var wlen: word; const bwait: boolean): boolean;
    function RecvStr(var str: string; const bwait: boolean): integer;
    function ExpectStr(var str: string; const swait: string; const bcase: boolean): boolean;
  end;

  //base class of connection
  TConnBase = class(TComponent, IConnInterf, ITextMessengerImpl)
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
    function ReadBufferToStr(): string; virtual;
    function WaitForReading(const tend: cardinal): boolean; virtual;
    function WaitForWriting(const tend: cardinal): boolean; virtual;
    function WaitForConnecting(const tend: cardinal): boolean; virtual;
    function IsConnected(): boolean; virtual;
    function IsReadReady(): boolean; virtual; abstract;
    function IsWriteComplete(): boolean; virtual; abstract;
    function SendData(const pbuf: PByteArray; const wlen: word): boolean; virtual; abstract;
    function RecvData(): integer; virtual; abstract;
    function TryConnect(): boolean; virtual; abstract;
    procedure ClearBuffer(); virtual;
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
    function Disconnect: boolean; virtual; abstract;
    function SendBuf(const pbuf: PByteArray; const wlen: word): boolean; virtual;
    function SendStr(const str: string): boolean; virtual;
    function RecvBuf(const pbuf: PByteArray; var wlen: word; const bwait: boolean = true): boolean; virtual;
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
  PConnBase = ^TConnBase;

const
  //define names of connection types
  CSTR_CONN_KEYS : array[EConnectType] of string = (
                    'UNKNOWN',
                    'RS232',
                    'USB',
                    'GPIB',
                    'ETHERNET',
                    'JTAG',
                    'CAN',
                    'PROFIL'
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
class function TConnBase.GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
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
class function TConnBase.GetConnectTypeName(const etype: EConnectType): string;
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
class function TConnBase.GetConnectState(const estate: EConnectState): string;
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
function  TConnBase.GetTypeName(): string;
begin
  result := TConnBase.GetConnectTypeName(e_type);
end;

// =============================================================================
// Description  : get string which represents the current state
// Parameter    : --
// Return       : string of current state
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TConnBase.GetStateStr(): string;
begin
  result := TConnBase.GetConnectState(e_state);
end;

// =============================================================================
// Description  : build current data in the buffer into a string
//                Note: The property ShowNullChar is applied for null
// Parameter    : --
// Return       : string, the built string
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TConnBase.ReadBufferToStr(): string;
var i: integer;
begin
  result := '';
  {i_cntnull := 0;
  if (w_rlen > 0) then begin
    for i := 0 to w_rlen - 1 do
      if (ba_rbuf[i] = 0) then begin
        inc(i_cntnull);
        ba_rbuf[i] := byte(ch_nullshow);
      end;
    result := string(PAnsiChar(@ba_rbuf[0]));
  end;}
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
function TConnBase.WaitForReading(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait, b_read: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_read := IsReadReady(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_read) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for reading: %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing in gui
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
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
function TConnBase.WaitForWriting(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait, b_write: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_write := IsWriteComplete(); //save its return value in a local variable, because it can be other value for next calling, e.g. an event automatically signaled
  b_wait := ((not b_write) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for completing write: %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
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
function TConnBase.WaitForConnecting(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal; b_wait: boolean;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  b_wait := ((not TryConnect()) and (c_tcur < tend));
  if b_wait then begin
    s_lastmsg := 'Waiting for connecting: %ds';
    t_msgrimpl.AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
    repeat
      Application.ProcessMessages();
      TryConnect();
      c_tcur := GetTickCount();
      if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
        t_msgrimpl.UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
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
function TConnBase.IsConnected(): boolean;
begin
  result := (e_state = CS_CONNECTED);
end;

// =============================================================================
// Description  : clear the buffer
// Parameter    : --
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
procedure TConnBase.ClearBuffer();
begin
  {ZeroMemory(@ba_rbuf, w_rlen);
  w_rlen := 0;}
end;

// =============================================================================
// Description  : constuctor
// Parameter    : --
// Exceptions   : --
// First author : 2016-06-15 /bsu/
// History      :
// =============================================================================
constructor TConnBase.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_UNKNOWN;
  //t_connobj := nil;
  e_state := CS_UNKNOWN;
  c_timeout := CINT_TIMEOUT_DEFAULT;
  t_rxwait := TEvent.Create(nil, false, false, 'TMtxConn.Rx');
  t_txwait := TEvent.Create(nil, false, false, 'TMtxConn.Tx');
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

// =============================================================================
// Description  : destuctor
// Parameter    : --
// Exceptions   : --
// First author : 2016-06-15 /bsu/
// History      :
// =============================================================================
destructor TConnBase.Destroy;
begin
  t_msgrimpl.Free();
  FreeAndNil(t_rxwait);
  FreeAndNil(t_txwait);
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
function TConnBase.Config(const sconf: string): boolean;
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
function TConnBase.Connect(): boolean;
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
// Description  : send data in an array of char
// Parameter    : buf, pointer of the array to send
//                len, length of the array
// Return       : true, if the string is sent successfully
//                false, otherwise
// Exceptions   : --
// First author : 2016-07-15 /bsu/
// History      :
// =============================================================================
function TConnBase.SendBuf(const pbuf: PByteArray; const wlen: word): boolean;
var c_tend: cardinal;
begin
  result := false;
  if Connected then begin
    ClearBuffer();
    c_tend := GetTickCount() + c_timeout;
    result := SendData(pbuf, wlen);
    if result then result := WaitForWriting(c_tend);
    if result then
      t_msgrimpl.AddMessage(format('Successful to send data (%d bytes): %s', [wlen, PAnsiChar(pbuf)]))
    else
      t_msgrimpl.AddMessage(format('Failed to send data (%d bytes): %s', [wlen, PAnsiChar(pbuf)]), ML_ERROR)
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
function TConnBase.SendStr(const str: string): boolean;
var i_len: integer;s_ansi: AnsiString;
begin
  s_ansi := AnsiString(str);
  i_len := Length(s_ansi) ;//* SizeOf(Char);
  //SetLength(ba_str, i_len);
  //Move(s_ansi[1], ba_str[0], i_len);
  //for i := 0 to i_len - 1 do ba_str[i] := byte(str[i + 1]); //convert to ansi string saved in an array of byte
  result := SendBuf(@s_ansi[1], i_len);
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
function TConnBase.RecvBuf(const pbuf: PByteArray; var wlen: word; const bwait: boolean): boolean;
var c_tend: cardinal; i_len, i_offset: integer;
begin
  result := false;
{  if Connected then begin
    c_tend := GetTickCount() + c_timeout;
    if bwait then WaitForReading(c_tend);
    w_rlen := RecvData();
    if w_rlen > 0 then begin
      i_len := result;
      i_offset := 0;
      if (w_rlen > length(buf)) then begin
        i_len := length(buf);
        i_offset := w_rlen - length(buf);
      end;
      Move(ba_rbuf[i_offset], buf, i_len);
      result := w_rlen;
      ClearBuffer();
    end;
    if (result > 0) then
      t_msgrimpl.AddMessage(format('Successful to receieve data (%d bytes): %s', [result, PString(@buf)]))
    else
      t_msgrimpl.AddMessage('Nothing is receieved.', ML_WARNING);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);}
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
function TConnBase.RecvStr(var str: string; const bwait: boolean): integer;
var c_tend: cardinal;
begin
  result := 0; str := '';
  c_tend := GetTickCount() + c_timeout;
  if Connected then begin
    if bwait then  WaitForReading(c_tend);
    result := RecvData();
    if (result > 0) then begin
      str := ReadBufferToStr();
      t_msgrimpl.AddMessage(format('Successful to receieve string (length=%d): %s', [result, str]));
    end else
      t_msgrimpl.AddMessage('Nothing is receieved.', ML_WARNING);
  end else
    t_msgrimpl.AddMessage(format('No data can be received because the connection (%s) is not yet established.', [GetTypeName()]), ML_ERROR);
end;

function TConnBase.ExpectStr(var str: string; const swait: string; const bcase: boolean): boolean;
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
function TConnBase.RecvStrTimeout(var str: string; const timeout: cardinal): integer;
var s_recv: string; tend: cardinal;
begin
  result := 0; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      result := RecvData();
      if (result > 0) then begin
        s_recv := ReadBufferToStr();
        str := str + s_recv;
        ClearBuffer();
      end;
      Application.ProcessMessages();
    until (GetTickCount() >= tend);
    result := length(str);
    if (result > 0) then
      t_msgrimpl.AddMessage(format('Successful to receieve string (length=%d): %s', [result, str]))
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
function TConnBase.RecvStrInterval(var str: string; const timeout: cardinal; const interv: cardinal): integer;
var i_time: cardinal; s_recv: string; b_break: boolean; tend: cardinal;
begin
  result := 0; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      result := RecvData();
      if (result > 0) then begin
        s_recv := ReadBufferToStr();
        str := str + s_recv;
        ClearBuffer();
      end;
      i_time := GetTickCount() + interv;
      if (i_time > tend) then i_time := tend;
      Application.ProcessMessages();
      b_break := (not WaitForReading(i_time));
    until (b_break or (GetTickCount() >= tend));
    result := length(str);
    if (result > 0) then
      t_msgrimpl.AddMessage(format('Successful to receieve string (length=%d): %s', [result, str]))
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
function TConnBase.RecvStrExpected(var str: string; const exstr: string; timeout: cardinal; const bcase: boolean): boolean;
var s_recv: string; tend: cardinal;
begin
  result := false; str := '';
  tend := GetTickCount() + timeout;
  if Connected then begin
    repeat
      if (RecvData() > 0) then begin
        s_recv := ReadBufferToStr();
        str := str + s_recv;
        ClearBuffer();
      end;
      if bcase then result := ContainsStr(str, exstr)
      else result := ContainsText(str, exstr);
      Application.ProcessMessages();
    until (result or (GetTickCount() >= tend));
    if (result) then
      t_msgrimpl.AddMessage(format('Successful to receieve string (length=%d): %s', [length(str), str]))
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
procedure TConnBase.SetEventRx();
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
procedure TConnBase.ResetEventRx();
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
procedure TConnBase.SetEventTx();
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
procedure TConnBase.ResetEventTx();
begin
  t_txwait.ResetEvent();
end;

end.
