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
uses  Classes, SysUtils, Serial3, ConnBase, DataBuffer;
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
  TRS232 = class(TCommBase)
  class function EnumSerialPort(var sports: TStringList): integer;
  class function SetPortByStr(pser: PSerial; const sval: string): boolean;
  class function SetBaudrateByStr(pser: PSerial; const sval: string): boolean;
  class function SetParityByStr(pser: PSerial; const sval: string): boolean;
  class function SetDataBitsByStr(pser: PSerial; const sval: string): boolean;
  class function SetStopBitsByStr(pser: PSerial; const sval: string): boolean;
  class function SetFlowControlByStr(pser: PSerial; const sval: string): boolean;
  private
  protected
    t_wbuffer:  TByteBuffer;      //buffer for sending data
    t_rbuffer:  TByteBuffer;      //buffer for receiving data
    t_ser :     TSerial;
  protected
    function SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
    function SetPort(const sval: string): boolean;
    function SetBaudrate(const sval: string): boolean;
    function SetParity(const sval: string): boolean;
    function SetDatabits(const sval: string): boolean;
    function SetStopbits(const sval: string): boolean;
    function SetFlowControl(const sval: string): boolean;
    function PacketToStr(const pbytes: PByteArray; const wlen: Word; const bhex: Boolean = True): string; override;
    function IsReadReady(): boolean; override;
    function IsWriteComplete(): boolean; override;
    function TryConnect(): boolean; override;
    function TryDisconnect(): boolean; override;

    function InitBuffer(): boolean; override;
    function WriteStrToBuffer(const txstr: string): boolean; override;
    function WritePacketToBuffer(const pbytes: PByteArray; const wlen: word): boolean; override;
    function SendFromBuffer(): boolean; override;
    function ReadStrFromBuffer(): string; override;
    function ReadPacketFromBuffer(var pbytes: PByteArray; var wlen: word): integer; override;
    function RecvToBuffer(): integer; override;
    function ClearBuffer(): integer; override;
    procedure DeinitBuffer(); override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconfs: TStrings): boolean; override;
  end;
  PRS232 = ^TRS232;

implementation
uses StrUtils,Windows, Registry, Forms, TextMessage;

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
class function TRS232.EnumSerialPort(var sports: TStringList): integer;
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
class function TRS232.SetPortByStr(pser: PSerial; const sval: string): boolean;
var t_ports: TStringList; s_in,s_portname: string; i_port: integer;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(s_in, i_port) then begin
    s_portname := 'COM' + sval;
    t_ports := TStringList.Create;
    TRS232.EnumSerialPort(t_ports);
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
class function TRS232.SetBaudrateByStr(pser: PSerial; const sval: string): boolean;
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
class function TRS232.SetParityByStr(pser: PSerial; const sval: string): boolean;
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
class function TRS232.SetDataBitsByStr(pser: PSerial; const sval: string): boolean;
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
class function TRS232.SetStopBitsByStr(pser: PSerial; const sval: string): boolean;
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
class function TRS232.SetFlowControlByStr(pser: PSerial; const sval: string): boolean;
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

function TRS232.SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
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

function TRS232.SetPort(const sval: string): boolean;
begin
  result := TRS232.SetPortByStr(@t_ser, sval);
end;

function TRS232.SetBaudrate(const sval: string): boolean;
begin
  result := TRS232.SetBaudrateByStr(@t_ser, sval);
end;

function TRS232.SetParity(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TRS232.SetParityByStr(@t_ser, sval);
end;

function TRS232.SetDatabits(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TRS232.SetDataBitsByStr(@t_ser, sval);
end;

function TRS232.SetStopbits(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TRS232.SetStopBitsByStr(@t_ser, sval);
end;

function TRS232.SetFlowControl(const sval: string): boolean;
begin
  if (sval = '') then result := true
  else result := TRS232.SetFlowControlByStr(@t_ser, sval);
end;

// =============================================================================
// Description  : build current data in the buffer into a string
//                Note: The property ShowNullChar is applied for null
// Parameter    : --
// Return       : string, the built string
// Exceptions   : --
// First author : 2016-11-25 /bsu/
// History      :
// =============================================================================
function TRS232.PacketToStr(const pbytes: PByteArray; const wlen: Word; const bhex: Boolean = True): string;
begin
  if (bhex) then
    result := string(t_rbuffer.ReadHex())
  else
    result := string(AnsiString(pbytes));
end;

function TRS232.IsReadReady(): boolean;
begin
  result := (t_ser.RxWaiting > 0);
end;

function TRS232.IsWriteComplete(): boolean;
begin
  result := (t_ser.TxWaiting <= 0);
end;

function TRS232.TryConnect(): boolean;
begin
  t_ser.Active := true;
  if t_ser.Active then e_state := CS_CONNECTED;
  result := IsConnected();
end;

// =============================================================================
// Class        : TConnRS232
// Function     : TryDisconnect
//                deactive t_ser
// Parameter    : --
// Return       : true, if t_ser is deactivated
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      :
// =============================================================================
function TRS232.TryDisconnect(): boolean;
begin
  t_ser.Active := false;
  result := (not t_ser.Active);
end;

function TRS232.InitBuffer(): boolean;
begin
  if not assigned(t_wbuffer) then t_wbuffer := TByteBuffer.Create();
  if not assigned(t_rbuffer) then t_rbuffer := TByteBuffer.Create();
  result := ( t_wbuffer.Resize(C_BUFFER_SIZE_WRITE) and
              t_rbuffer.Resize(C_BUFFER_SIZE_READ)  );
end;

function TRS232.WriteStrToBuffer(const txstr: string): boolean;
var s_ansi: AnsiString;
begin
  s_ansi := AnsiString(txstr);
  result := (t_wbuffer.WriteAnsiStr(s_ansi) > 0);
end;

function TRS232.WritePacketToBuffer(const pbytes: PByteArray; const wlen: word): boolean;
begin
  result := (t_wbuffer.WriteBytes(pbytes, wlen) > 0);
end;

function TRS232.SendFromBuffer(): boolean;
var s_ansi: AnsiString;
begin
  s_ansi := t_wbuffer.ReadAnsiStr();
  t_ser.WriteString(s_ansi);
  result := (length(s_ansi) > 0);
end;

function TRS232.ReadStrFromBuffer(): string;
begin
  RecvToBuffer();
  result := string(t_rbuffer.ReadAnsiStr());
end;

function TRS232.ReadPacketFromBuffer(var pbytes: PByteArray; var wlen: word): integer;
begin
  RecvToBuffer();
  t_rbuffer.ReadBytes(pbytes, wlen);
  result := wlen;
end;

function TRS232.RecvToBuffer(): integer;
var ch: AnsiChar;
begin
  result := 0;
  while ((t_ser.RxWaiting > 0) and (not t_rbuffer.IsFull())) do begin
    if t_ser.ReadChar(ch) = 1 then begin
      t_rbuffer.WriteElement(byte(ch));
      inc(result);
    end;
    Application.ProcessMessages();
  end;
end;

function TRS232.ClearBuffer(): integer;
var s_text: string;
begin
  RecvToBuffer();
  result := t_rbuffer.CountUsed ;
  if (result > 0) then begin
    s_text := string(t_rbuffer.ReadAnsiStr());
    if length(s_text) > 32 then s_text := LeftStr(s_text, 16) + ' ... ' + RightStr(s_text, 16);
    t_msgrimpl.AddMessage(format('Rx-Buffer is cleared: %s (%d bytes)', [s_text, result]), ML_WARNING);
  end;
end;

procedure TRS232.DeinitBuffer();
begin
  if assigned(t_wbuffer) then t_wbuffer.Free();
  if assigned(t_rbuffer) then t_rbuffer.Free();
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
constructor TRS232.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_RS232;
  t_ser := TSerial.Create(self);
  //t_connobj := t_ser;

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
destructor TRS232.Destroy;
begin
  t_ser.Free();
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
function TRS232.Config(const sconfs: TStrings): boolean;
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
    else t_msgrimpl.AddMessage(format('Failed to configurate the connection (%s).', [GetTypeName()]), ML_ERROR);
  end else t_msgrimpl.AddMessage(format('The current state (%s) is not suitable for configuration.', [GetStateStr()]), ML_WARNING);
end;

end.

