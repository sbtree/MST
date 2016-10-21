// =============================================================================
// Module name  : $RCSfile: RS232.pas,v $
// description  : This unit defines a class, which implements methodes and
//                properties for general communication with RS232.
// Compiler     : Delphi 2007
// Author       : 2015-09-08 /bsu/
// History      :
//==============================================================================
unit RS232;

interface
uses  Classes, Serial3, ConnBase;
const
  C_RS232_BUFFER_SIZE = C_BUFFER_SIZE_DEFAULT;

type
  //enumeration of properties for serial port
  ESerialProperty = (
                    SP_PORT,
                    SP_BAUDRATE,
                    SP_PARITY,
                    SP_DATABITS,
                    SP_STOPBITS,
                    SP_FLOWCONTROL
                    );
  PSerial = ^TSerial;

  //sub class of TConnBase for communication over serial port
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
    function SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
    function SetPort(const sval: string): boolean;
    function SetBaudrate(const sval: string): boolean;
    function SetParity(const sval: string): boolean;
    function SetDatabits(const sval: string): boolean;
    function SetStopbits(const sval: string): boolean;
    function SetFlowControl(const sval: string): boolean;
    function IsConnected(): boolean; override;
    function IsReadComplete(): boolean; override;
    function IsWriteComplete(): boolean; override;
    function SendData(const buf: PAnsiChar; len: word): boolean; override;
    function RecvData(): boolean; override;
    procedure TryConnect(); override;
    procedure ClearBuffer(); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconfs: TStrings): boolean; override;
    function Disconnect: boolean; override;
  end;
  PConnRS232 = ^TMtxRS232;

implementation
uses SysUtils, StrUtils, Windows, Registry, Forms, TextMessage, AnsiStrings;

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

function TMtxRS232.IsReadComplete(): boolean;
begin
  result := (t_ser.RxWaiting > 0);
end;

function TMtxRS232.IsWriteComplete(): boolean;
begin
  result := (t_ser.TxWaiting <= 0);
end;

function TMtxRS232.SendData(const buf: PAnsiChar; len: word): boolean;
var i: integer;
begin
  ClearBuffer(); //clear reading buffer of the serial interface
  for i := 0 to len - 1 do t_ser.WriteChar(buf[i]);
  result := (len > 0);
  //if result then t_txwait.SetEvent();
end;

function TMtxRS232.RecvData(): boolean;
var ch: AnsiChar; len: word;
begin
  len := length(ba_rbuf);
  ZeroMemory(@ba_rbuf, len); w_rlen := 0;
  while (t_ser.RxWaiting > 0) do begin
    if t_ser.ReadChar(ch) = 1 then begin
      if (w_rlen < len) then begin
        ba_rbuf[w_rlen] := ch;
        inc(w_rlen);
      end else break;
    end;
    Application.ProcessMessages();
  end;
  result := (w_rlen > 0);
  //if result then t_rxwait.SetEvent();
end;

procedure TMtxRS232.TryConnect();
begin
  t_ser.Active := true;
end;

procedure TMtxRS232.ClearBuffer();
var c_tend: cardinal;
begin
  c_tend := GetTickCount() + c_timeout;
  repeat
    if RecvData() then  //read out data from read buffer
      t_msgrimpl.AddMessage(format('Rx-Buffer (%d bytes) is cleared', [w_rlen]), ML_WARNING);
  until ((w_rlen <= 0) or (GetTickCount() >= c_tend));
  ZeroMemory(@ba_rbuf, w_rlen);
  w_rlen := 0;
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
  t_ser.Free();
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
    if result then e_state := CS_CONFIGURED
    else t_msgrimpl.AddMessage(format('Failed to configurate the configuration (%s).', [GetTypeName()]), ML_ERROR);
  end else t_msgrimpl.AddMessage(format('The current state (%s) is not suitable for configuration.', [GetStateStr()]), ML_WARNING);
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

end.

