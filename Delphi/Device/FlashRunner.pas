// =============================================================================
// Module name  : $RCSfile: FlashRunner.pas,v $
// description  : This unit implements all methodes and properties of class
//                TFlashRunner for device FlashRunner
// Compiler     : Delphi 2007
// Author       : 2015-08-14 /bsu/
// History      :
//==============================================================================
unit FlashRunner;

interface
uses Serial3, Classes, SysUtils, StrUtils, Windows, Forms, DeviceBase, IniFiles;

type
// =============================================================================
// Class        : TFlashRunner
// Description  : Definition of class TFlashRunner for FlashRunner deriving
//                from TDeviceBase
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
  TFlashRunner=class(TDeviceBase, IDeviceInterf)
  protected
    t_ser: TSerial;
  protected
    function ConfigConnByStr(const conf: string): boolean;
    procedure CheckAnswer(const ans: string);

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    function SendStr(const str: string; const ans: boolean = true): integer;
    function RecvStr(var str:string): integer;
    function IntToDMDataStr(const num: integer): string;

    function ConfigDevice(const ini: TMemIniFile): Boolean;
    function FreeDevice(): Boolean;
    function Connect(): Boolean;
    function Coordinate(): boolean;
    function SendData(const data: PChar; const ans: boolean = true): Integer;
    function RecvData(var data:PChar): Integer;
    function Disconnect: boolean;
  end;

implementation
const
  C_ERR_NOANSWER: integer = $7FFFFFFF; //
  C_ERR_TIMEOUT : integer = $7FFFFFFE; //
// =============================================================================
// Class        : --
// Function     : Delay
//                delay for a moment in milli second
// Parameter    : msec, integer of milli seconds
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure Delay(const msec: cardinal);
var i_timeout: cardinal;
begin
  i_timeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() >= i_timeout);
end;

// =============================================================================
// Class        : --
// Function     : IndexOfStr
//                return index of a string in an array
// Parameter    : aArray, in which the string is searched
//                S, string to search
// Return       : index of string in the array. -1 is returnd, if not found
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function IndexOfStr(const aArray: array of string; const S: string): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(aArray) to High(aArray) do
    if SameText(S, aArray[I]) then
    begin
      Result := I;
      Break;
    end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigConnByStr
//                get settings of connection from a given string and config it
// Parameter    : conf, a string of rs232 settings, e.g.
//                'Port:8|Baudrate:115200'
// Return       : true, if the setting is available.
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.ConfigConnByStr(const conf: string): boolean;
const CSTR_KEYS: array[0..1] of string = ('PORT','BAUDRATE');
var
  s_in, s_key, s_val: string;
  t_keyvalues, t_keyval: TStringList;
  i, i_value, i_idx: integer;
begin
  s_in := trim(conf);
  result := false;
  t_keyvalues := TStringList.Create;
  t_keyval := TStringList.Create;
  if (ExtractStrings(['|'], [' ', Char(9)], PChar(s_in), t_keyvalues) > 0) then
  begin
    i := 0;
    result := true;
    repeat
      t_keyval.Clear;
      if (ExtractStrings([':'], [' '], PChar(t_keyvalues[i]), t_keyval) = 2) then
      begin
        s_key := UpperCase(t_keyval[0]);
        s_val := UpperCase(t_keyval[1]);
        i_idx := IndexOfStr( CSTR_KEYS, s_key);
        case i_idx of
        0: //'PORT'
        begin
          result := TryStrToInt(s_val, i_value);
          if result then t_ser.Port := i_value;
        end;
        1: //'BAUDRATE'
        begin
          result := TryStrToInt(s_val, i_value);
          if result then t_ser.Baudrate := i_value;
        end;
        end;
      end;
      inc(i);
    until ((i>=t_keyvalues.Count) or (not (result)));
  end;
  t_keyval.Clear;
  FreeAndNil(t_keyval);
  t_keyvalues.Clear;
  FreeAndNil(t_keyvalues);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : IntToDMDataStr
//                convert an integer into a string with format of data
//                for FlashRunner-command DMSET in word-addressing,
//                e.g. 1050820084($3EA23DF4)->'$3E $00 $A2 $00 $3D $00 $F4 $00'
// Parameter    : num, integer to be converted
// Return       : string in format of DMSET-data
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.IntToDMDataStr(const num: integer): string;
var str_data: string;
begin
  str_data := RightStr(IntToHex(num, 8), 8);
  Insert(' $00 $', str_data, 7);
  Insert(' $00 $', str_data, 5);
  Insert(' $00 $', str_data, 3);
  result := '$' + str_data + ' $00';
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Create
//                constructor of the class, which initializes the instance with
//                default values
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
constructor TFlashRunner.Create(owner: TComponent);
begin
	inherited Create(owner);
  t_ser := TSerial.Create(self);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Destroy
//                destructor of the class, which finilizes the instance
// Parameter    : owner, which instanced this class
// Return       : --
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
destructor TFlashRunner.Destroy;
begin
  FreeDevice();
  FreeAndNil(t_ser);
	inherited Destroy;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Connect
//                connects to the device (FlashRunner)
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Connect: boolean;
var ds_set: set of EDeviceState;
begin
  if e_state = DS_CONFIGURED then begin
    if not t_ser.Active then t_ser.Active := true;
    result := t_ser.Active;
    PostEvent(DE_CONNECT, result);
  end;
  ds_set := [DS_CONNECTED, DS_READY, DS_BUSY, DS_COMERROR];
  result := (e_state in ds_set);
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : Coordinate
//                coordinate with the device
// Parameter    : --
// Return       : true, if the device is accesible in the time of timeout
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.Coordinate(): boolean;
const C_STR_PING: string = 'SPING'; C_STR_PONG: string = 'PONG>'; C_TIMEOUT: cardinal = 3000;
var s_ans: string; ds_set : set of EDeviceState; i_time: cardinal; ch: char;
begin
  result := false;
  ds_set := [DS_CONNECTED, DS_COMERROR];
  if (e_state in ds_set) then
  begin
    i_time := GetTickCount() + C_TIMEOUT;

    //clear buffer of t_ser
    Delay(C_DELAY_MSEC);
    while ((t_ser.RxWaiting > 0) and ( GetTickCount() < i_time)) do t_ser.ReadChar(ch);

    //send string and wait til write-buffer is completely sent
    t_ser.WriteString(C_STR_PING + Char(13));
    repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= i_time));

    //receive string til read-buffer is empty or timeout
    repeat begin
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then s_ans := s_ans + ch
      else Delay(C_DELAY_MSEC);
    end; until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= i_time));

    //verify the received string
    s_ans := trim(s_ans);
    result := SameText(C_STR_PONG,s_ans);
    PostEvent(DE_COORDINATE, result);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SendStr
//                send string to device
// Parameter    : str, a string to send
// Return       : integer, which counts char, which are sent
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SendStr(const str: string; const ans: boolean): integer;
var ch: char; timeout: cardinal;
    ds_set: set of EDeviceState;
begin
  result := 0;
  ds_set := [DS_CONNECTED, DS_COMERROR];
  if (e_state in ds_set) then Coordinate();
  
  if (e_state = DS_READY) then
    begin
    timeout := GetTickCount() + i_timeout;
    //clear read-buffer of t_ser
    repeat if (t_ser.ReadChar(ch) <> 1) then Delay(C_DELAY_MSEC);
    until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));

    t_ser.WriteString(str);
    //wait til write-buffer is completely sent
    repeat if (t_ser.TxWaiting > 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.TxWaiting <= 0) or (GetTickCount() >= timeout));
    result := (length(str) - t_ser.TxWaiting);

    PostEvent(DE_SEND, (result > 0));
    if (not ans) then PostEvent(DE_RECV, true);
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RecvStr
//                receiv string from device
// Parameter    : str, a string for receiving
// Return       : integer, which counts char, which are received
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RecvStr(var str: string): integer;
var ch: char; timeout: cardinal;
begin
  result := 0;
  if e_state = DS_BUSY then begin
    timeout := GetTickCount() + i_timeout;
    //wait til any data arrives
    repeat if (t_ser.RxWaiting <= 0) then Delay(C_DELAY_MSEC);
    until ((t_ser.RxWaiting > 0) or (GetTickCount() >= timeout));

    //prepare string for receiving
    str := '';
    //receive char and save it into the string
    repeat
    begin
      ch := chr(0);
      if (t_ser.ReadChar(ch) = 1) then str := str + ch
      else Delay(C_DELAY_MSEC);
    end;
    until ((t_ser.RxWaiting <= 0) or (GetTickCount() >= timeout));
    result := length(str);
    CheckAnswer(str);
    PostEvent(DE_RECV, (i_lasterr=0));
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : CheckAnswer
//                check, if an error exists in the answer string from device
// Parameter    : ans, the received string
// Return       : integer, error number
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
procedure TFlashRunner.CheckAnswer(const ans: string);
var s_in: string; i_len: integer;
begin
  s_in := trim(ans);
  i_len := length(s_in);
  if (i_len > 0) then
  begin
    if (s_in[i_len] = '!') then //if command generated an error
    begin
      delete(s_in, i_len, 1);
      TryStrToInt(s_in, i_lasterr);
    end
    else i_lasterr := 0;
  end
  else i_lasterr := C_ERR_NOANSWER;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : ConfigDevice
//                config FlashRunner using settings from INI like:
//                [FlashRunner]
//                DESCRIPTION=A universal programmer for writing flash
//                PRODUCER=SMH Technologies
//                TYPE=FR01LAN
//                CONN_RS232=PORT:8|Baudrate:115200
//                TIMEOUT=30000
// Parameter    : ini, a instance of TMemIniFile, in which section [FlashRunner]
//                     is saved
// Return       : true, if the device is configured successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.ConfigDevice(const ini: TMemIniFile): boolean;
const
  C_STR_FR   : string = 'FlashRunner';
//  C_STR_DESC : string = 'DESCRIPTION';
//  C_STR_PROD : string = 'PRODUCER';
//  C_STR_TYPE : string = 'TYPE';
  C_STR_RS232: string = 'CONN_RS232';
  C_STR_TIMEOUT : string = 'TIMEOUT';
var
  s_inivalue: string;
begin
  result := false;
  if (DeviceState = DS_NONE) then
  begin
    //default settings
    t_ser.CheckParity := false;
    t_ser.DataBits := d8Bit;
    t_ser.NotifyErrors := neNone;
    t_ser.Port := 1;
    t_ser.Baudrate := 115200;
    t_ser.Name := C_STR_FR;

    //settings from ini file
    if (ini.SectionExists(C_STR_FR)) then
    begin
      SetTimeout(ini.ReadInteger(C_STR_FR, C_STR_TIMEOUT, C_TIMEOUT_MSEC));
      s_inivalue := trim(ini.ReadString(C_STR_FR, C_STR_RS232, ''));
      result := ConfigConnByStr(s_inivalue);
    end;
    PostEvent(DE_CONFIG, result);;
  end;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : SendData
//                send data to device
// Parameter    : data, a pointer to char buffer to send
// Return       : integer, which counts bytes, which are sent
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.SendData(const data: PChar; const ans: boolean): Integer;
begin
  //to do
  result := 0;
end;

// =============================================================================
// Class        : TFlashRunner
// Function     : RecvData
//                receive data from device
// Parameter    : data, a pointer to char buffer for receiving
// Return       : integer, which counts bytes, which are received
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.RecvData(var data:PChar): Integer;
begin
  //to do
  result := 0;
end;

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
function TFlashRunner.Disconnect(): boolean;
begin
  t_ser.Active := false;
  result := (t_ser.Active = false);
  PostEvent(DE_DISCONNECT, result);
end;


// =============================================================================
// Class        : TFlashRunner
// Function     : FreeDevice
//                release device, undo what is done in ConfigDevice
// Parameter    : --
// Return       : true, if the device is released successfully
//                false, otherwise
// Exceptions   : --
// First author : 2015-08-14 /bsu/
// History      :
// =============================================================================
function TFlashRunner.FreeDevice(): boolean;
begin
  result := Disconnect();
  PostEvent(DE_FREE, result);;
end;
end.
