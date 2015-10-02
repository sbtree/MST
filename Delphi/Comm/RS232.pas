unit RS232;

interface
uses  Serial3, ConnBase, Classes, RegExpr, DataBuffer;

type
  ERS232Settings = (
                    RS_PORT,
                    RS_BAUDRATE,
                    RS_PARITY,
                    RS_DATABITS,
                    RS_STOPBITS,
                    RS_HWFLOWCONTROL,
                    RS_SWFLOWCONTROL
                    );
  TConnRS232 = class(TConnBase)
  class function EnumSerialPort(var sports: TStringList): integer;
  protected
    t_ser : TSerial;
  protected
    function ReadAll(var rbuf: array of char): longword; virtual;
    //procedure ReadAll(var rbuf: TCharBuffer); virtual;

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconf: string): boolean;override;
    function IsConnected(): boolean; override;
    function Connect(): boolean;override;
    function Disconnect: boolean;override;
    function SendData(const sbuf: array of char; const len: longword; const timeout: cardinal): boolean; override;
    function RecvData(var rbuf: array of char; const timeout: cardinal): longword; override;
    function RecvTill(var rbuf: array of char; const expects: array of const; const timeout: cardinal): longword; virtual;abstract;
    //function SendData(const sbuf: TCharBuffer; const timeout: cardinal): Integer; override;
    //function RecvData(var rbuf: TCharBuffer; const timeout: cardinal): Integer; override;
    function GetLastError(var msg: string): Integer;override;
  end;
  PConnRS232 = ^TConnRS232;

const
  CSTR_VALID_BAUD: array[0..14] of string = (
                '110','300','600','1200','2400','4800','9600','14400',
                '19200','38400','56000','57600','115200','128000','256000');

  CSTR_RS232_KEYS: array[ERS232Settings] of string = (
                'PORT',
                'BAUDRATE',
                'PARITY',
                'DATABITS',
                'STOPBITS',
                'HWFLOWCONTROL',
                'SWFLOWCONTROL'
                );
                
implementation
uses SysUtils, StrUtils, Windows, GenUtils, Registry;

type
  PSerial = ^TSerial;
  TSetFunction = function(pser: PSerial; const sval: string): boolean;
var C_FUNC_CALLS: array [ERS232Settings] of TSetFunction;

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
    TConnRS232.EnumSerialPort(t_ports);
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
var i_baud: integer; s_in: string;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(sval, i_baud) then begin
    result := (IndexOfStr(CSTR_VALID_BAUD, sval) >= 0 );
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
begin
  //todo
  result := true;
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
begin
  //todo
  result := true;
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
begin
  //todo
  result := true;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the hardware flow control of
//                pser with this string if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for hardware flow control to be set
// Return       : true, if the string is a valid value for hardware flow control of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetHWFlowControl(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

// =============================================================================
// Class        : --
// Function     : checks the given string and sets the software flow control of
//                pser with this string if it is valid. This function is only used inside of this unit.
// Parameter    : pser, pointer of a TSerial to be set
//                sval, string for software flow control to be set
// Return       : true, if the string is a valid value for software flow control of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function SetSWFlowControl(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

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
class function TConnRS232.EnumSerialPort(var sports: TStringList): integer;
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

function TConnRS232.ReadAll(var rbuf: array of char): longword;
var idx: integer; len: longword; ch: char;
begin
  result := 0;
  len := length(rbuf); idx := LOW(rbuf);
  while ((t_ser.RxWaiting > 0) and (result < len)) do 
    if t_ser.ReadChar(ch) = 1 then begin
      rbuf[idx] := ch;
      inc(result);
    end;
end;

{procedure TConnRS232.ReadAll(var rbuf: TCharBuffer);
var ch: char;
begin
  while (t_ser.RxWaiting > 0) do
    if t_ser.ReadChar(ch) = 1 then rbuf.WriteChar(ch);
end; }

// =============================================================================
// Class        : TConnRS232
// Function     : Constructor, creates t_ser
// Parameter    :
// Return       :
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
constructor TConnRS232.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_RS232;
  t_ser := TSerial.Create(self);

  //default settings
  t_ser.CheckParity := false;
  t_ser.DataBits := d8Bit;
  t_ser.NotifyErrors := neNone;
  t_ser.Port := 1;
  t_ser.Baudrate := 9600;
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
destructor TConnRS232.Destroy;
begin
  FreeAndNil(t_ser);
  inherited Destroy();
end;

// =============================================================================
// Class        : TConnRS232
// Function     : IsConnected
//                request, if the object of TSerial is actived
// Parameter    :
// Return       : value of s_ser.active (true or false)
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TConnRS232.IsConnected(): boolean;
begin
  result := t_ser.Active;
end;

// =============================================================================
// Class        : TConnRS232
// Function     : Config
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                'Port:8|Baudrate:115200'
// Return       : true, if the setting is available(At least PORT and BAUDRATE
//                      have to be set configured)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TConnRS232.Config(const sconf: string): boolean;
var s_conf: string; i: ERS232Settings; t_regexp: TRegExpr;
    b_settings: array[LOW(ERS232Settings)..HIGH(ERS232Settings)] of boolean;
begin
  result := false;
  s_conf := UpperCase(sconf);
  t_regexp := TRegExpr.Create;
  for i := LOW(ERS232Settings) to HIGH(ERS232Settings) do begin
    t_regexp.Expression := '(^|\|)[\t\ ]*' + CSTR_RS232_KEYS[i] + '\b[\t\ ]*:([^\|$]*)';
    b_settings[i]:=false;
    if t_regexp.Exec(s_conf) then  begin
      result := (C_FUNC_CALLS[i](@t_ser, t_regexp.Match[2]));
      b_settings[i]:= result;
      if not result then break;
    end;
  end;
  FreeAndNil(t_regexp);
  result := (result and b_settings[RS_PORT] and b_settings[RS_BAUDRATE]);
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
function TConnRS232.Connect(): boolean;
begin
  t_ser.Active := true;
  result := IsConnected();
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
function TConnRS232.Disconnect: boolean;
begin
  t_ser.Active := false;
  result := (not IsConnected());
end;

function TConnRS232.SendData(const sbuf: array of char; const len: longword; const timeout: cardinal): boolean;
begin
  //todo
  result := false;
end;

function TConnRS232.RecvData(var rbuf: array of char; const timeout: cardinal): longword;
begin
  //todo
  result := false;
end;

function TConnRS232.RecvTill(var rbuf: array of char; const expects: array of const; const timeout: cardinal): longword;
begin
  result := 0;
end;

{function TConnRS232.SendData(const sbuf: TCharBuffer; const timeout: cardinal): Integer;
var i_len, i_sent: integer; c_time: cardinal; ch: char;
begin
  i_sent := 0; i_len := sbuf.CountUsed();
  c_time := GetTickCount() + timeout;
  while ((i_sent < i_len) and (GetTickCount() < c_time)) do begin //send data
    if sbuf.ReadChar(ch) then begin
      t_ser.WriteChar(ch);
      inc(i_sent);
    end else break;
  end;
  while ((t_ser.TxWaiting > 0) and (GetTickCount() < c_time)) do Delay(C_DELAY_MSEC);//wait for finishing to send data
  result := (i_sent - t_ser.TxWaiting);
end;

function TConnRS232.RecvData(var rbuf: TCharBuffer; const timeout: cardinal): Integer;
var ch: char; c_time: cardinal;
begin
  result := 0; c_time := GetTickCount() + timeout;
  if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC * 5 );//wait a moment (100 ms) if there is no data, maybe it comes later

  while ((t_ser.RxWaiting > 0) and (GetTickCount() < c_time)) do begin
    if (not rbuf.IsFull()) then begin
      if (t_ser.ReadChar(ch) = 1) then begin
        rbuf.WriteChar(ch);
        inc(result);
      end else Delay();
    end;
  end;
end; }

function TConnRS232.GetLastError(var msg: string): Integer;
begin
  result := i_lasterr;
end;

initialization
  C_FUNC_CALLS[RS_PORT]:= RS232.SetPort;
  C_FUNC_CALLS[RS_BAUDRATE]:= RS232.SetBaudrate;
  C_FUNC_CALLS[RS_PARITY]:= RS232.SetParity;
  C_FUNC_CALLS[RS_DATABITS]:= RS232.SetDataBits;
  C_FUNC_CALLS[RS_STOPBITS]:= RS232.SetStopBits;
  C_FUNC_CALLS[RS_HWFLOWCONTROL]:= RS232.SetHWFlowControl;
  C_FUNC_CALLS[RS_SWFLOWCONTROL]:= RS232.SetSWFlowControl;

end.
