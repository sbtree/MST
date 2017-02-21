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
  TSerialAdapter = class(TCommBase)
    class function EnumSerialPort(var sports: TStringList): integer;
    class function CheckOutPort(const sval: string; var ival: integer): boolean;
    class function CheckOutBaudrate(const sval: string; var ival: integer): boolean;
    class function CheckOutParity(const sval: string; var ival: integer): boolean;
    class function CheckOutDataBits(const sval: string; var ival: integer): boolean;
    class function CheckOutStopBits(const sval: string; var ival: integer): boolean;
    class function CheckOutFlowControl(const sval: string; var ival: integer): boolean;
    class function CheckPort(const ival: integer): boolean;
    class function CheckBaudrateSTD(const ival: integer): boolean;
    class function CheckParity(const ival: integer): boolean;
    class function CheckDataBits(const ival: integer): boolean;
    class function CheckStopBits(const ival: integer): boolean;
    class function CheckFlowControl(const ival: integer): boolean;
  protected
    t_wbuffer:  TByteBuffer;      //buffer for sending data
    t_rbuffer:  TByteBuffer;      //buffer for receiving data
    t_ser :     TSerial;          //object to communicate with serial interface
    b_ownser:   boolean;          //indicate if t_ser is created by the adapter
  private
    procedure SetSerialObj(comobj: TSerial);

  protected
    function SetProperty(const eprop: ESerialProperty; const sval: string): boolean; overload;
    function SetProperty(const propname, sval: string): boolean; overload;
    function SetPortByStr(const sval: string): boolean;
    function SetBaudrateByStr(const sval: string): boolean;
    function SetParityByStr(const sval: string): boolean;
    function SetDatabitsByStr(const sval: string): boolean;
    function SetStopbitsByStr(const sval: string): boolean;
    function SetFlowControlByStr(const sval: string): boolean;
    procedure SetDefault();

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
    function ChangeProperties(const sconfs: TStrings): integer;

    property SerialObj: TSerial read t_ser write SetSerialObj;
  end;
  PRS232 = ^TSerialAdapter;

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
class function TSerialAdapter.EnumSerialPort(var sports: TStringList): integer;
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

class function TSerialAdapter.CheckOutPort(const sval: string; var ival: integer): boolean;
begin
  result := false; ival := 0;
  if TryStrToInt(trim(sval), ival) then result := CheckPort(ival);
end;

class function TSerialAdapter.CheckOutBaudrate(const sval: string; var ival: integer): boolean;
begin
  result := false; ival := 0;
  if TryStrToInt(trim(sval), ival) then result := (ival > 0);
end;

class function TSerialAdapter.CheckOutParity(const sval: string; var ival: integer): boolean;
const CSTR_PA_VALUES: array[eParity] of string = ('NONE', 'ODD', 'EVEN', 'MARK', 'SPACE');
var s_in: string;
begin
  ival := -1;
  s_in := trim(sval);
  if not TryStrToInt(s_in, ival) then ival := IndexText(s_in, CSTR_PA_VALUES);
  result := TSerialAdapter.CheckParity(ival);
end;

class function TSerialAdapter.CheckOutDataBits(const sval: string; var ival: integer): boolean;
begin
  result := false; ival := -1;
  if TryStrToInt(trim(sval), ival) then result := TSerialAdapter.CheckDataBits(ival);
end;

class function TSerialAdapter.CheckOutStopBits(const sval: string; var ival: integer): boolean;
begin
  result := false; ival := -1;
  if TryStrToInt(trim(sval), ival) then result := TSerialAdapter.CheckStopBits(ival);
end;

class function TSerialAdapter.CheckOutFlowControl(const sval: string; var ival: integer): boolean;
const CSTR_FC_VALUES: array[eFlowControl] of string = ('NONE', 'RTS_CTS', 'DTR_DSR', 'XON_XOF');
var s_in: string;
begin
  ival := -1;
  s_in := trim(sval);
  if not TryStrToInt(s_in, ival) then ival := IndexText(s_in, CSTR_FC_VALUES);
  result := TSerialAdapter.CheckFlowControl(ival)
end;

class function TSerialAdapter.CheckPort(const ival: integer): boolean;
var t_ports: TStringList; s_portname: string;
begin
  t_ports := TStringList.Create;
  TSerialAdapter.EnumSerialPort(t_ports);
  s_portname := 'COM' + IntToStr(ival);
  result := (t_ports.IndexOf(s_portname) >= 0);
  t_ports.Clear;
  FreeAndNil(t_ports);
end;

class function TSerialAdapter.CheckBaudrateSTD(const ival: integer): boolean;
var i: integer;
begin
  result := false;
  for i := Low(aBaudrates) to High(aBaudrates) do begin
    if ival = aBaudrates[i] then begin
      result := true;
      break;
    end;
  end;
end;

class function TSerialAdapter.CheckParity(const ival: integer): boolean;
begin
  result := ((ival >= Ord(Low(eParity))) and (ival <= Ord(High(eParity))))
end;

class function TSerialAdapter.CheckDataBits(const ival: integer): boolean;
begin
  result := ((ival = 7) or (ival = 8));
end;

class function TSerialAdapter.CheckStopBits(const ival: integer): boolean;
begin
  result := ((ival = 1) or (ival = 2))
end;

class function TSerialAdapter.CheckFlowControl(const ival: integer): boolean;
begin
  result := ((ival >= Ord(Low(eFlowControl))) and (ival <= Ord(High(eFlowControl))))
end;

procedure TSerialAdapter.SetSerialObj(comobj: TSerial);
begin
  if comobj <> t_ser then begin
    if b_ownser then FreeAndNil(t_ser);
    t_ser := comobj;
    b_ownser := false;
  end;
end;

function TSerialAdapter.SetProperty(const eprop: ESerialProperty; const sval: string): boolean;
begin
  result := false;
  case eprop of
    SP_PORT: result := SetPortByStr(sval);
    SP_BAUDRATE: result := SetBaudrateByStr(sval);
    SP_PARITY: result := SetParityByStr(sval);
    SP_DATABITS: result := SetDatabitsByStr(sval);
    SP_STOPBITS: result := SetStopbitsByStr(sval);
    SP_FLOWCONTROL: result := SetFlowControlByStr(sval);
  end;
end;

// =============================================================================
// Description  : set a property of rs232 object
// Parameter    : propname, name of the property in string
//                sval, value of the property in string
// Return       : true, if the setting is done successfully
//                false, otherwise
// Exceptions   : --
// First author : 2017-02-20 /bsu/
// History      :
// =============================================================================
function TSerialAdapter.SetProperty(const propname, sval: string): boolean;
var i_idx: integer;
begin
  result := false;
  i_idx := IndexText(propname, CSTR_RS232_PROPERTIES);
  if ( (i_idx >= ord(Low(ESerialProperty))) and (i_idx <= ord(High(ESerialProperty)))) then
    result := SetProperty(ESerialProperty(i_idx), sval);
end;

// =============================================================================
// Description  : checks the given string and sets the port of pser with this string
//                if it is valid. This function is only used inside of this unit.
// Parameter    : sval, string to set port
// Return       : true, if the string is a valid number of the port on this comuputer
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetPortByStr(const sval: string): boolean;
var i_port: integer;
begin
  result := TSerialAdapter.CheckOutPort(sval, i_port);
  if result then t_ser.Port := i_port
  else t_msgrimpl.AddMessage(format('Failed to set port number (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : checks the given string and sets the baudrate of pser with this string
//                if it is valid.
// Parameter    : sval, string to set baudrate
// Return       : true, if the string is a number of the valid baudrate (C_VALID_BAUD)
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetBaudrateByStr(const sval: string): boolean;
var i_baud: integer;
begin
  result := TSerialAdapter.CheckOutBaudrate(sval, i_baud);
  if result then t_ser.Baudrate := i_baud
  else t_msgrimpl.AddMessage(format('Failed to set baudrate (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : checks the given string and sets the parity of pser with this string
//                if it is valid.
// Parameter    : sval, string to set parity
// Return       : true, if the string is a valid value for parity of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetParityByStr(const sval: string): boolean;
var i_val: integer;
begin
  result := TSerialAdapter.CheckOutParity(sval, i_val);
  if result then t_ser.Parity := eParity(i_val)
  else t_msgrimpl.AddMessage(format('Failed to set parity (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : checks the given string and sets the data bits of t_ser with this string
//                if it is valid.
// Parameter    : sval, string to set data bits
// Return       : true, if the string is a valid value for data bits of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetDatabitsByStr(const sval: string): boolean;
var i_val: integer;
begin
  result := TSerialAdapter.CheckOutDataBits(sval, i_val);
  if result then t_ser.DataBits := eDataBits(i_val)
  else t_msgrimpl.AddMessage(format('Failed to set data bits (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : checks the given string and sets the stop bits of t_ser with this string
//                if it is valid.
// Parameter    : sval, string to set stop bits
// Return       : true, if the string is a valid value for stop bits of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetStopbitsByStr(const sval: string): boolean;
var i_val: integer;
begin
  result := TSerialAdapter.CheckOutStopBits(sval, i_val);
  if result then t_ser.StopBits := eStopBits(i_val)
  else t_msgrimpl.AddMessage(format('Failed to set stop bits (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : checks the given string and sets the flow control of t_ser with
//                this string if it is valid.
// Parameter    : sval, string to set hardware flow control
// Return       : true, if the string is a valid value for hardware flow control of TSerial
//                false, otherwise
// Exceptions   : --
// First author : 2015-09-11 /bsu/
// History      : 2017-02-21 /bsu/ improved after changing of the class function
// =============================================================================
function TSerialAdapter.SetFlowControlByStr(const sval: string): boolean;
var i_val: integer;
begin
  result := TSerialAdapter.CheckOutFlowControl(sval, i_val);
  if result then t_ser.FlowMode := eFlowControl(i_val)
  else t_msgrimpl.AddMessage(format('Failed to set flow control (invalid value: %s).',[sval]), ML_ERROR);
end;

// =============================================================================
// Description  : set properties with default values.
// Parameter    : --
// Return       : --
// Exceptions   : --
// First author : 2017-02-21 /bsu/
// History      :
// =============================================================================
procedure TSerialAdapter.SetDefault();
begin
  if assigned(t_ser) then begin
    t_ser.Port := 1;
    t_ser.Baudrate := 9600;
    t_ser.CheckParity := false;
    t_ser.DataBits := d8Bit;
    t_ser.NotifyErrors := neNone;
    t_ser.FlowMode := fcNone;
  end;
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
function TSerialAdapter.PacketToStr(const pbytes: PByteArray; const wlen: Word; const bhex: Boolean = True): string;
begin
  if (bhex) then
    result := string(t_rbuffer.ReadHex())
  else
    result := string(AnsiString(pbytes));
end;

function TSerialAdapter.IsReadReady(): boolean;
begin
  if assigned(t_ser) then result := (t_ser.RxWaiting > 0)
  else result := false
end;

function TSerialAdapter.IsWriteComplete(): boolean;
begin
  if assigned(t_ser) then result := (t_ser.TxWaiting <= 0)
  else result := false;
end;

function TSerialAdapter.TryConnect(): boolean;
begin
  if assigned(t_ser) then begin
    t_ser.Active := true;
    if t_ser.Active then e_state := CS_CONNECTED;
  end;
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
function TSerialAdapter.TryDisconnect(): boolean;
begin
  if assigned(t_ser) then begin
    t_ser.Active := false;
    result := (not t_ser.Active);
  end else result := true;
end;

function TSerialAdapter.InitBuffer(): boolean;
begin
  if not assigned(t_wbuffer) then t_wbuffer := TByteBuffer.Create();
  if not assigned(t_rbuffer) then t_rbuffer := TByteBuffer.Create();
  result := ( t_wbuffer.Resize(C_BUFFER_SIZE_WRITE) and
              t_rbuffer.Resize(C_BUFFER_SIZE_READ)  );
end;

function TSerialAdapter.WriteStrToBuffer(const txstr: string): boolean;
var s_ansi: AnsiString;
begin
  s_ansi := AnsiString(txstr);
  result := (t_wbuffer.WriteAnsiStr(s_ansi) > 0);
end;

function TSerialAdapter.WritePacketToBuffer(const pbytes: PByteArray; const wlen: word): boolean;
begin
  result := (t_wbuffer.WriteBytes(pbytes, wlen) > 0);
end;

function TSerialAdapter.SendFromBuffer(): boolean;
var s_ansi: AnsiString;
begin
  s_ansi := t_wbuffer.ReadAnsiStr();
  if assigned(t_ser) then t_ser.WriteString(s_ansi);
  result := assigned(t_ser) and (length(s_ansi) > 0);
end;

function TSerialAdapter.ReadStrFromBuffer(): string;
begin
  RecvToBuffer();
  result := string(t_rbuffer.ReadAnsiStr());
end;

function TSerialAdapter.ReadPacketFromBuffer(var pbytes: PByteArray; var wlen: word): integer;
begin
  RecvToBuffer();
  t_rbuffer.ReadBytes(pbytes, wlen);
  result := wlen;
end;

function TSerialAdapter.RecvToBuffer(): integer;
var ch: AnsiChar;
begin
  result := 0;
  if assigned(t_ser) then begin
    while ((t_ser.RxWaiting > 0) and (not t_rbuffer.IsFull())) do begin
      if t_ser.ReadChar(ch) = 1 then begin
        t_rbuffer.WriteElement(byte(ch));
        inc(result);
      end;
      Application.ProcessMessages();
    end;
  end;
end;

function TSerialAdapter.ClearBuffer(): integer;
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

procedure TSerialAdapter.DeinitBuffer();
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
constructor TSerialAdapter.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_RS232;
  t_ser := nil;
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
destructor TSerialAdapter.Destroy;
begin
  if (b_ownser and assigned(t_ser)) then FreeAndNil(t_ser);
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
function TSerialAdapter.Config(const sconfs: TStrings): boolean;
var i: ESerialProperty; s_conf: string;
begin
  result := false;
  if not assigned(t_ser) then begin
    t_ser := TSerial.Create(self);
    b_ownser := true;
    SetDefault();
  end;

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

// =============================================================================
// Class        : TConnRS232
// Function     : SetProperties
//                set properties of rs232 object
// Parameter    : sconfs, a list of name=value pairs for the properties
// Return       : count of the properties, which are set successfully
// Exceptions   : --
// First author : 2017-02-20 /bsu/
// History      :
// =============================================================================
function TSerialAdapter.ChangeProperties(const sconfs: TStrings): integer;
var i: integer; s_prop, s_val: string; b_connected: boolean;
begin
  result := 0;
  if (sconfs.Count > 0) then begin
    b_connected := Connected;
    if b_connected then TryDisconnect();
    for i := 0 to sconfs.Count - 1 do begin
      s_prop := sconfs.Names[i];
      s_val := sconfs.ValueFromIndex[i];
      if SetProperty(s_prop, s_val) then inc(result)
      else break;
    end;
    if b_connected then TryConnect();
  end;
end;

end.

