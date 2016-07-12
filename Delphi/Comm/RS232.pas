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
  class function SetPortByStr(pser: PSerial; const sval: string): boolean;
  class function SetBaudrateByStr(pser: PSerial; const sval: string): boolean;
  class function SetParityByStr(pser: PSerial; const sval: string): boolean;
  class function SetDataBitsByStr(pser: PSerial; const sval: string): boolean;
  class function SetStopBitsByStr(pser: PSerial; const sval: string): boolean;
  class function SetFlowControlByStr(pser: PSerial; const sval: string): boolean;
  protected
    t_ser :     TSerial;
  protected
    function RecvStrOnce(): string; virtual;
    function RecvChars(var buf: PChar; const len: integer; const tend: cardinal; const binterval: boolean = false): integer; overload;
    function SendChars(const buf: PChar; const len: integer; const tend: cardinal): boolean; overload;
    function RecvChars(var buf: string; const tend: cardinal; const binterval: boolean = false): integer; overload;
    function SendChars(const buf: string; const tend: cardinal): boolean; overload;
    function SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
    function SetPort(const sval: string): boolean;
    function SetBaudrate(const sval: string): boolean;
    function SetParity(const sval: string): boolean;
    function SetDatabits(const sval: string): boolean;
    function SetStopbits(const sval: string): boolean;
    function SetFlowControl(const sval: string): boolean;
    function IsConnected(): boolean; override;
    function IsReadable(): boolean; override;
    function IsWriteComplete(): boolean; override;
    procedure TryConnect(); override;
    procedure ClearBuffer();
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconfs: TStrings): boolean; override;
    function Disconnect: boolean; override;
    function SendBuf(const buf: PChar; const len: longword): boolean; override;
    function RecvBuf(var buf: PChar; const len: longword): integer; override;
    function SendStr(const str: string): boolean; override;
    function RecvStr(var str: string; const bwait: boolean = false): integer; override;

    function RecvStrTimeout(var str: string; const tend: cardinal): integer; override;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = CINT_RECV_INTERVAL): integer; override;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; override;
  end;
  PConnRS232 = ^TMtxRS232;

implementation
uses SysUtils, StrUtils, Windows, GenUtils, Registry, Forms, TextMessage;

const CSTR_RS232_PROPERTIES: array[ESerialProperty] of string =
                  ('PORT', 'BAUDRATE', 'PARITY', 'DATABITS', 'STOPBITS', 'FLOWCONTROL');

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
class function TMtxRS232.SetPortByStr(pser: PSerial; const sval: string): boolean;
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
class function TMtxRS232.SetBaudrateByStr(pser: PSerial; const sval: string): boolean;
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
class function TMtxRS232.SetParityByStr(pser: PSerial; const sval: string): boolean;
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
class function TMtxRS232.SetDataBitsByStr(pser: PSerial; const sval: string): boolean;
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
class function TMtxRS232.SetStopBitsByStr(pser: PSerial; const sval: string): boolean;
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
class function TMtxRS232.SetFlowControlByStr(pser: PSerial; const sval: string): boolean;
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
  result := ''; i_cntnull := 0;
  while (t_ser.RxWaiting > 0) do begin
    if t_ser.ReadChar(ch) = 1 then begin
      if ch = Char(0) then begin
        result := result + ch_nullshow;
        inc(i_cntnull);
      end else result := result + ch;
    end;
    Application.ProcessMessages();
  end;
end;

function TMtxRS232.RecvChars(var buf: PChar; const len: integer; const tend: cardinal; const binterval: boolean = false): integer;
var c_recv: char; c_tend: cardinal; b_recv: boolean;
begin
  result := 0; i_cntnull := 0;
  repeat
    if (t_ser.ReadChar(c_recv) = 1) then begin
      buf[result] := c_recv;
      Inc(result);
      b_recv := true;
    end else if binterval then begin
      c_tend := GetTickCount() + c_rinterval;
      if (c_tend > tend) then c_tend := tend;
      b_recv := (not WaitForReading(c_tend));
    end else b_recv := false;
    Application.ProcessMessages();
    b_recv := (b_recv and (GetTickCount() < tend) and (result < len));
  until (not b_recv);
end;

function TMtxRS232.SendChars(const buf: PChar; const len: integer; const tend: cardinal): boolean;
var i: integer;
begin
  ClearBuffer(); //clear reading buffer of the serial interface
  for i := 0 to len - 1 do t_ser.WriteChar(buf[i]);
  result := WaitForWriting(tend);
end;

function  TMtxRS232.RecvChars(var buf: string; const tend: cardinal; const binterval: boolean): integer;
var c_recv: char; c_tend: cardinal; b_recv: boolean;
begin
  i_cntnull := 0; buf := '';
  repeat
    if (t_ser.ReadChar(c_recv) = 1) then begin
      if c_recv = Char(0) then begin
        buf := buf + ch_nullshow;
        inc(i_cntnull);
      end else buf := buf + c_recv;
      b_recv := true;
    end else if binterval then begin
      c_tend := GetTickCount() + c_rinterval;
      if (c_tend > tend) then c_tend := tend;
      b_recv := (not WaitForReading(c_tend));
    end else b_recv := false;
    Application.ProcessMessages();
    b_recv := (b_recv and (GetTickCount() < tend));
  until (not b_recv);
  result := length(buf);
end;

function TMtxRS232.SendChars(const buf: string; const tend: cardinal): boolean;
//var i: integer;
begin
  ClearBuffer(); //clear reading buffer of the serial interface
  t_ser.WriteString(buf);
  //for i := 0 to length(buf) - 1 do t_ser.WriteChar(buf[i]);
  result := WaitForWriting(tend);
end;

function TMtxRS232.SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
begin
  result := false;
  case eprop of
    SP_PORT: result := SetPort(sval);
    SP_BAUDRATE: result := SetBaudrate(sval);
    SP_PARITY: result := SetParity(sval);
    SP_DATABITS: result := SetDatabits(sval);
    SP_STOPBITS: result := SetStopbits(sval);
    SP_FLOWCONTROL: result := SetFlowControl(sval);
  end;
end;

function TMtxRS232.SetPort(const sval: string): boolean;
begin
  result := TMtxRS232.SetPortByStr(@t_ser, sval);
end;

function TMtxRS232.SetBaudrate(const sval: string): boolean;
begin
  result := TMtxRS232.SetBaudrateByStr(@t_ser, sval);
end;

function TMtxRS232.SetParity(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TMtxRS232.SetParityByStr(@t_ser, sval);
end;

function TMtxRS232.SetDatabits(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TMtxRS232.SetDataBitsByStr(@t_ser, sval);
end;

function TMtxRS232.SetStopbits(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TMtxRS232.SetStopBitsByStr(@t_ser, sval);
end;

function TMtxRS232.SetFlowControl(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TMtxRS232.SetFlowControlByStr(@t_ser, sval);
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

function TMtxRS232.IsReadable(): boolean;
begin
  result := (t_ser.RxWaiting > 0);
end;

function TMtxRS232.IsWriteComplete(): boolean;
begin
  result := (t_ser.TxWaiting <= 0);
end;

procedure TMtxRS232.TryConnect();
begin
  t_ser.Active := true;
end;

procedure TMtxRS232.ClearBuffer();
var s_recv: string; i_len: integer;
begin
  s_recv := RecvStrOnce();
  i_len := length(s_recv);
  if (i_len > 0) then AddMessage(format('Rx-Buffer (%d bytes) is cleared: %s', [i_len, s_recv]), ML_WARNING);
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
var i: ESerialProperty; s_conf: string;
begin
  result := false;
  if (e_state in [CS_UNKNOWN, CS_CONFIGURED]) then begin
    for i := LOW(ESerialProperty) to HIGH(ESerialProperty) do begin
      s_conf := sconfs.Values[CSTR_RS232_PROPERTIES[i]];
      result := SetProperty(i, s_conf);
      if not result then break;
    end;
    if result then e_state := CS_CONFIGURED;
  end else AddMessage(format('The current state (%s) is not suitable for configuration.', [GetStateStr()]), ML_WARNING);
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
  result := false;
  if Connected then begin
    t_ser.Active := false;
    result := (not t_ser.Active);
    if result then e_state := CS_CONFIGURED;
  end;
end;

function TMtxRS232.SendBuf(const buf: PChar; const len: longword): boolean;
var i: integer; c_tend: cardinal;
begin
  c_tend := GetTickCount() + c_timeout;
  for i := 0 to len - 1 do t_ser.WriteChar(buf[i]);
  result := WaitForWriting(c_tend);
end;

function TMtxRS232.RecvBuf(var buf: PChar; const len: longword): integer;
begin
  result := RecvChars(buf, len, GetTickCount() + c_timeout, false);
end;

function TMtxRS232.SendStr(const str: string): boolean;
begin
  result := SendChars(str, GetTickCount() + c_timeout);
  if result then AddMessage(format('Succeeded to send: %s', [str]))
  else AddMessage(format('Failed to send: %s', [str]), ML_ERROR)
end;

function TMtxRS232.RecvStr(var str: string; const bwait: boolean): integer;
begin
  str := '';
  if bwait then WaitForReading(GetTickCount() + c_timeout);
  str := RecvStrOnce();
  result := length(str);
  if (result > 0) then AddMessage(format('Received: %s', [str]))
  else AddMessage('Nothing is received.', ML_WARNING);
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

end.

