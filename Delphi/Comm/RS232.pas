unit RS232;

interface
uses  Classes, Serial3, ConnBase;

type
  ESerialProperty = (
                    SP_PORT,
                    SP_BAUDRATE,
                    SP_PARITY,
                    SP_DATABITS,
                    SP_STOPBITS,
                    SP_FLOWCONTROL
                    );
  PSerial = ^TSerial;

  TMtxRS232 = class(TConnBase)
  class function EnumSerialPort(var sports: TStringList): integer;
  protected
    t_ser :     TSerial;
  protected
    function IsConnected(): boolean; override;
    function RecvStrOnce(): string; virtual;
    function RecvChars(var buf: PChar; const len: integer; const tend: cardinal; const binterval: boolean = false): integer; overload;
    function SendChars(const buf: PChar; const len: integer; const tend: cardinal): boolean; overload;
    function RecvChars(var buf: string; const tend: cardinal; const binterval: boolean = false): integer; overload;
    function SendChars(const buf: string; const tend: cardinal): boolean; overload;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconfs: TStrings): boolean; override;
    function Connect(): boolean; override;
    function Disconnect: boolean; override;
    function SendBuf(const buf: PChar; const len: longword): boolean; override;
    function RecvBuf(var buf: PChar; const len: longword): integer; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = false): integer; override;

    function RecvStrTimeout(var str: string; const tend: cardinal): integer; override;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = CINT_RECV_INTERVAL): integer; override;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; override;
    function WaitForReading(const tend: cardinal): boolean; override;
  end;
  PConnRS232 = ^TMtxRS232;

implementation
uses SysUtils, StrUtils, Windows, GenUtils, Registry, Forms, TextMessage;

type
  SetSerialProperty = function(pser: PSerial; const sval: string): boolean;

const CSTR_RS232_PROPERTIES: array[ESerialProperty] of string =
                  ('PORT', 'BAUDRATE', 'PARITY', 'DATABITS', 'STOPBITS', 'FLOWCONTROL');

var PSerialPropertyCalls: array [ESerialProperty] of SetSerialProperty;

// =============================================================================
// Class        : TConnRS232
// Function     : EnumSerialPort, class function, enumerates all serial ports
//                from Windows-Registry
// Parameter    : sports, an ouput string list in which all ports are found, e.g.
//                ('COM1', 'COM2' ...)
// Return       : integer, count of the found ports
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
class function TMtxRS232.EnumSerialPort(var sports: TStringList): integer;
var t_registry: TRegistry; s_names: TStringList; s_value: string; i: integer;
begin
  sports.Clear;
  t_registry := TRegistry.Create;
  with t_registry do begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Hardware\DeviceMap\SerialComm') then begin
      s_names := TStringList.Create;
      GetValueNames(s_names);
      for i := 0 to s_names.Count - 1 do begin
        s_value := ReadString(s_names[i]);
        s_value := UpperCase(trim(s_value));
        sports.Add(s_value);
      end;
      s_names.Clear;
      FreeAndNil(s_names);
      CloseKey();
    end;
  end;
  FreeAndNil(t_registry);
  result := sports.Count;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the port of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for port to be set
// Return       : true, if the string is a valid number of the port on this comuputer
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetPort(pser: PSerial; const sval: string): boolean;
var t_ports: TStringList; s_in,s_portname: string; i_port: integer;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(s_in, i_port) then begin
    s_portname := 'COM' + sval;
    t_ports := TStringList.Create;
    TMtxRS232.EnumSerialPort(t_ports);
    result := (t_ports.IndexOf(s_portname) >= 0 );
    if result then pser^.Port := i_port;
    t_ports.Clear;
    FreeAndNil(t_ports);
  end;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the baudrate of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for baudrate to be set
// Return       : true, if the string is a number of the valid baudrate (C_VALID_BAUD)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetBaudrate(pser: PSerial; const sval: string): boolean;
const  CSTR_BAUD_VALUES: array[Low(aBaudrates)..High(aBaudrates)] of string = (
                          '110','300','600','1200','2400','4800','9600','14400',
                          '19200','38400','56000','57600','115200','128000','256000');
var i_baud: integer; s_in: string; i_idx: integer;
begin
  result := false;
  s_in := trim(sval);
  i_idx := IndexText(sval, CSTR_BAUD_VALUES);
  if ((i_idx >= Low(aBaudrates)) and (i_idx <= High(aBaudrates))) then begin
    result := TryStrToInt(sval, i_baud);
    if result then pser^.Baudrate := i_baud;
  end;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the parity of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for parity to be set
// Return       : true, if the string is a valid value for parity of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetParity(pser: PSerial; const sval: string): boolean;
const
  CSTR_PA_VALUES: array[eParity] of string = ('NONE', 'ODD', 'EVEN', 'MARK', 'SPACE');
var i_idx: integer; s_in: string;
begin
  result := false;
  s_in := UpperCase(sval);
  i_idx := IndexText(s_in, CSTR_PA_VALUES);
  if ((i_idx >= Ord(paNone)) and (i_idx <= Ord(paSpace))) then begin
    pser^.Parity := eParity(i_idx);
    result := true;
  end;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the data bits of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for data bits to be set
// Return       : true, if the string is a valid value for data bits of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetDataBits(pser: PSerial; const sval: string): boolean;
const
  CSTR_DB_VALUES: array[eDataBits] of string = ('7', '8');
var i_idx: integer; s_in: string;
begin
  result := false;
  s_in := UpperCase(sval);
  i_idx := IndexText(s_in, CSTR_DB_VALUES);
  if ((i_idx >= Ord(d7bit)) and (i_idx <= Ord(d8bit))) then begin
    pser^.DataBits := eDataBits(i_idx);
    result := true;
  end;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the stop bits of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for stop bits to be set
// Return       : true, if the string is a valid value for stop bits of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetStopBits(pser: PSerial; const sval: string): boolean;
const
  CSTR_SB_VALUES: array[eStopBits] of string = ('1', '2');
var i_idx: integer; s_in: string;
begin
  result := false;
  s_in := UpperCase(sval);
  i_idx := IndexText(s_in, CSTR_SB_VALUES);
  if ((i_idx >= Ord(st1bit)) and (i_idx <= Ord(st2bit))) then begin
    pser^.StopBits := eStopBits(i_idx);
    result := true;
  end;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the flow control of pser with
//                this string if it is valid. This function is only used inside
//                of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for hardware flow control to be set
// Return       : true, if the string is a valid value for hardware flow control of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetFlowControl(pser: PSerial; const sval: string): boolean;
const
  CSTR_FC_VALUES: array[eFlowControl] of string = ('NONE', 'RTS_CTS', 'DTR_DSR', 'XON_XOF');
var i_idx: integer; s_in: string;
begin
  result := false;
  s_in := UpperCase(sval);
  i_idx := IndexText(s_in, CSTR_FC_VALUES);
  if ((i_idx >= Ord(fcNone)) and (i_idx <= Ord(fcXON_XOF))) then begin
    pser^.FlowMode := eFlowControl(i_idx);
    result := true;
  end;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : ReadAll, read all chars from buffer of TSerial and save them in rbuf
// Parameter    : sports, an ouput string list in which all ports are found, e.g.
//                ('COM1', 'COM2' ...)
// Return       : integer, count of the found ports
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TMtxRS232.RecvStrOnce(): string;
var ch: char;
begin
  result := '';
  while (t_ser.RxWaiting > 0) do begin
    if t_ser.ReadChar(ch) = 1 then begin
      if ch = Char(0) then result := result + ch_nullshow
      else result := result + ch;
    end;
    Application.ProcessMessages();
  end;
end;

function TMtxRS232.RecvChars(var buf: PChar; const len: integer; const tend: cardinal; const binterval: boolean = false): integer;
var c_recv: char; b_tend: boolean; c_tend: cardinal;
begin
  buf := ''; i_cntnull := 0;
  b_break := false;
  b_tend := (GetTickCount() >= tend);
  result := 0;
  while ((not b_break) and (not b_tend)) do begin
    if (t_ser.ReadChar(c_recv) = 1) then begin
      buf[result] := c_recv;
      Inc(result);
    end;
    if (result >= len) then break;
    if (t_ser.RxWaiting <= 0) then begin
      if binterval then begin
        c_tend := GetTickCount() + c_rinterval;
        if (c_tend > tend) then c_tend := tend;
        b_break := (not WaitForReading(c_tend));
      end;
    end;
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= tend);
  end;
end;

function TMtxRS232.SendChars(const buf: PChar; const len: integer; const tend: cardinal): boolean;
var i: integer; b_tend: boolean; s_recv: string;
begin
  RecvStr(s_recv, false); //clear reading buffer of the serial interface
  b_break := false;
  b_tend := (GetTickCount() >= tend);
  for i := 0 to len - 1 do t_ser.WriteChar(buf[i]);
  while ((not b_break) and (not b_tend) and (t_ser.TxWaiting > 0)) do begin
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= tend);
  end;
  result := (t_ser.TxWaiting <= 0);
end;

function  TMtxRS232.RecvChars(var buf: string; const tend: cardinal; const binterval: boolean): integer;
var c_recv: char; b_tend: boolean; c_tend: cardinal;
begin
  buf := ''; i_cntnull := 0;
  b_break := false;
  b_tend := (GetTickCount() >= tend);
  while ((not b_break) and (not b_tend)) do begin
    if (t_ser.ReadChar(c_recv) = 1) then begin
      if c_recv = char(0) then begin
        c_recv := ch_nullshow;//replace char(0) with ch_null (default is #13)
        Inc(i_cntnull);
      end;
      buf := buf + c_recv;
    end;
    if (t_ser.RxWaiting <= 0) then begin
      if binterval then begin
        c_tend := GetTickCount() + c_rinterval;
        if (c_tend > tend) then c_tend := tend;
        b_break := (not WaitForReading(c_tend));
      end;
    end;
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= tend);
  end;
  result := length(buf);
end;

function TMtxRS232.SendChars(const buf: string; const tend: cardinal): boolean;
var b_tend: boolean; s_recv: string;
begin
  RecvStr(s_recv, false); //clear reading buffer of the serial interface
  b_break := false;
  b_tend := (GetTickCount() >= tend);
  t_ser.WriteString(buf);
  while ((not b_break) and (not b_tend) and (t_ser.TxWaiting > 0)) do begin
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= tend);
  end;
  result := (t_ser.TxWaiting <= 0);
end;

// =============================================================================
// Class        : TConnRS232
// Function     : IsConnected
//                request if the object of TSerial is actived
// Parameter    : --
// Return       : value of s_ser.active (true or false)
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TMtxRS232.IsConnected(): boolean;
begin
  result := t_ser.Active;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Constructor, creates t_ser
// Parameter    :
// Return       :
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
constructor TMtxRS232.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_RS232;
  t_ser := TSerial.Create(self);
  t_connobj := t_ser;

  //default settings
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Destructor, destroys t_ser
// Parameter    :
// Return       :
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
destructor TMtxRS232.Destroy;
begin
  FreeAndNil(t_ser);
  t_connobj := nil;
  inherited Destroy();
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Config
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                Port=8
//                Baudrate=115200'
// Return       : true, if the setting is available(At least PORT and BAUDRATE
//                      have to be set configured)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TMtxRS232.Config(const sconfs: TStrings): boolean;
var i: ESerialProperty; s_conf: string; b_settings: array[ESerialProperty] of boolean;
begin
  for i := LOW(ESerialProperty) to HIGH(ESerialProperty) do begin
    b_settings[i] := false;
    s_conf := sconfs.Values[CSTR_RS232_PROPERTIES[i]];
    b_settings[i] := (PSerialPropertyCalls[i](@t_ser, s_conf));
  end;
  result := (b_settings[SP_PORT] and b_settings[SP_BAUDRATE]); //
  if result then e_state := CS_CONFIGURED;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Connect
//                activate t_ser
// Parameter    : --
// Return       : true, if t_ser is activated
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TMtxRS232.Connect(): boolean;
begin
  t_ser.Active := true;
  result := t_ser.Active;
  if result then e_state := CS_CONNECTED;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Disconnect
//                deactive t_ser
// Parameter    : --
// Return       : true, if t_ser is deactivated
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TMtxRS232.Disconnect: boolean;
begin
  t_ser.Active := false;
  result := (not t_ser.Active);
  if result then e_state := CS_CONFIGURED;
end;

function TMtxRS232.SendBuf(const buf: PChar; const len: longword): boolean;
var i: integer; c_tend: cardinal; b_tend: boolean;
begin
  b_break := false;
  c_tend := GetTickCount() + c_timeout;
  b_tend := (GetTickCount() >= c_tend);
  for i := 0 to len - 1 do begin
    t_ser.WriteChar(buf[i]);
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= c_tend);
    if (b_break or b_tend) then break;
  end;
  while ((not b_break) and (not b_tend) and (t_ser.TxWaiting > 0)) do begin
    Application.ProcessMessages();
    b_tend := (GetTickCount() >= c_tend);
  end;
  result := (t_ser.TxWaiting <= 0);
end;

function TMtxRS232.RecvBuf(var buf: PChar; const len: longword): integer;
begin
  result := RecvChars(buf, len, false);
end;

function TMtxRS232.SendStr(const str: string): boolean;
begin
  result := SendChars(str, GetTickCount() + c_timeout);
end;

function TMtxRS232.RecvStr(var str: string; const bwait: boolean): integer;
begin
  str := '';
  if bwait then begin
    WaitForReading(GetTickCount() + c_timeout);
    str := RecvStrOnce();
    result := length(str);
  end else result := RecvChars(str, GetTickCount() + c_timeout, false);
  AddMessage(format('Receiving: %s', [str]));
end;

function TMtxRS232.RecvStrTimeout(var str: string; const tend: cardinal): integer;
var s_recv: string; b_tend: boolean;
begin
  str := '';
  b_tend := false;
  while (not b_tend) do begin
    s_recv := RecvStrOnce();
    str := str + s_recv;
    b_tend := (GetTickCount() >= tend);
  end;
  result := length(str);
  AddMessage(format('Receiving: %s', [str]));
end;

function TMtxRS232.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
begin
  c_rinterval := interv;
  result := RecvChars(str, tend, true);
  AddMessage(format('Receiving: %s', [str]));
end;

function TMtxRS232.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
var s_temp: string; b_found: boolean; b_timeout: boolean;
begin
  str := '';  b_found := false;
  repeat
    s_temp := ''; c_rinterval := CINT_RECV_INTERVAL;
    if (RecvChars(s_temp, tend, true) > 0) then begin
      str := str + s_temp;
      if bcase then b_found := ContainsStr(str, exstr)
      else b_found := ContainsText(str, exstr);
    end;
    b_timeout := (tend <= GetTickCount());
  until (b_timeout or b_found);
  result := length(str);
end;

function TMtxRS232.WaitForReading(const tend: cardinal): boolean;
var s_lastmsg: string; c_tcur, c_count: cardinal;
begin
  c_tcur := GetTickCount(); c_count := c_tcur;
  s_lastmsg := 'Waiting for reading: %ds';
  AddMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
  while ((t_ser.RxWaiting <= 0) and (c_tcur <= tend)) do begin
    Application.ProcessMessages();
    c_tcur := GetTickCount();
    if (c_tcur - c_count) > 500  then begin //update the message per 0.5 second to avoid flashing
      UpdateMessage(format(s_lastmsg, [Round((tend - c_tcur) / 1000)]));
      c_count := c_tcur;
    end;
  end;
  result := (t_ser.RxWaiting > 0);
end;

initialization
  PSerialPropertyCalls[SP_PORT]       := SetPort;
  PSerialPropertyCalls[SP_BAUDRATE]   := SetBaudrate;
  PSerialPropertyCalls[SP_PARITY]     := SetParity;
  PSerialPropertyCalls[SP_DATABITS]   := SetDataBits;
  PSerialPropertyCalls[SP_STOPBITS]   := SetStopBits;
  PSerialPropertyCalls[SP_FLOWCONTROL]:= SetFlowControl;

end.

