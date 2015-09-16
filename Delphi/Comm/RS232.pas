unit RS232;

interface
uses  Serial3, ConnBase, Classes, RegExpr;

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

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function Config(const sconf: string): boolean;override;
    function IsConnected(): boolean; override;
    function Connect(): boolean;override;
    function Disconnect: boolean;override;
    function SendData(const data: array of char): Integer;override;
    function RecvData(var data: array of char): Integer;override;
    function GetLastError(var msg: string): Integer;override;
  end;
  PConnRS232 = ^TConnRS232;

const
  C_VALID_BAUD: array[0..14] of string = (
                '110','300','600','1200','2400','4800','9600','14400',
                '19200','38400','56000','57600','115200','128000','256000');

  C_RS232_KEYS: array[LOW(ERS232Settings)..HIGH(ERS232Settings)] of string = (
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
var C_FUNC_CALLS: array [LOW(ERS232Settings)..HIGH(ERS232Settings)] of TSetFunction;

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

function SetBaudrate(pser: PSerial; const sval: string): boolean;
var i_baud: integer; s_in: string;
begin
  result := false;
  s_in := trim(sval);
  if TryStrToInt(sval, i_baud) then begin
    result := (IndexOfStr(C_VALID_BAUD, sval) >= 0 );
    if result then pser^.Baudrate := i_baud;
  end;
end;

function SetParity(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

function SetDataBits(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

function SetStopBits(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

function SetHWFlowControl(pser: PSerial; const sval: string): boolean;
begin
  //todo
  result := true;
end;

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
begin
  result := false;
  s_conf := UpperCase(sconf);
  t_regexp := TRegExpr.Create;
  for i := LOW(ERS232Settings) to HIGH(ERS232Settings) do begin
    t_regexp.Expression := '(^|\|)[\t\ ]*' + C_RS232_KEYS[i] + '\b[\t\ ]*:([^\|$]*)';
    if t_regexp.Exec(s_conf) then  begin
      result := (C_FUNC_CALLS[i](@t_ser, t_regexp.Match[2]));
      if not result then break;
    end;
  end;
  FreeAndNil(t_regexp);
end;

function TConnRS232.Connect(): boolean;
begin
  t_ser.Active := true;
  result := IsConnected();
end;

function TConnRS232.Disconnect: boolean;
begin
  t_ser.Active := false;
  result := (not IsConnected());
end;

function TConnRS232.SendData(const data: array of char): Integer;
begin
  result := 0;
end;

function TConnRS232.RecvData(var data: array of char): Integer;
begin
  result := 0;
end;

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
