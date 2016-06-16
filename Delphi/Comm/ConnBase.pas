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
uses Classes, TextMessage, IniFiles, Serial3;
type
  EConnectType = (CT_UNKNOWN, //unknown connection
                  CT_RS232,   //rs232
                  CT_USB,     //usb
                  CT_GPIB,    //GPIB (IEEE 488)
                  CT_ETHERNET,//ethernet
                  CT_JTAG,    //jtag
                  CT_CAN,     //can-bus
                  CT_PROFIL   //profil-bus
                  );

  EConnectState = ( CS_UNKNOWN,   //unknown state
                    CS_CONFIGURED,//connection is configurated
                    CS_CONNECTED  //connection is connected and in use
                   );

  {ICommWithString = interface
    function SendStr(const str: string): boolean;
    function RecvStr(var str: string; const bwait: boolean = false): integer;
    function RecvStrTimeout(var str: string; const tend: cardinal): integer;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer;
    function WaitForReading(const tend: cardinal): boolean;
  end;

  ICommWithBytes = interface
    function SendBytes(const a: array of byte): boolean;
    function RecvBytes(var a: array of byte; const bwait: boolean = false): boolean;
    function RecvStrTimeout(var str: array of byte; const tend: cardinal): integer;
    function RecvStrInterval(var str: array of byte; const tend: cardinal; const interv: cardinal): integer;
    function RecvStrExpected(var str: array of byte; const exstr: array of byte; tend: cardinal; const bcase: boolean = false): integer;
    function WaitForReading(const tend: cardinal): boolean;
  end; }

  TConnBase = class(TComponent)
  class function GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
  class function GetConnectTypeName(const etype: EConnectType): string;

  protected
    e_type:     EConnectType;
    t_connobj:  TObject;
    e_state:    EConnectState;  //connection state
    i_timeout : cardinal;       //timeout in milli seconds
    t_messenger:TTextMessenger;

  protected
    function  IsConnected(): boolean; virtual;
    function  GetTypeName(): string; virtual;
    procedure AddMessage(const text: string; const level: EMessageLevel = ML_INFO);
    procedure UpdateMessage(const text: string; const level: EMessageLevel = ML_INFO);

  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;

    property Connected: boolean read IsConnected;
    property Messenger: TTextMessenger read t_messenger write t_messenger;
    property ConnectType: EConnectType read e_type;
    property ConnectTypeName: string read GetTypeName;
    property ConnectState: EConnectState read e_state;
    property Timeout: cardinal read i_timeout write i_timeout;

    function Config(const sconf: string): boolean; overload; virtual; abstract;
    function Config(const sconfs: TStrings): boolean; overload; virtual; abstract;
    function Connect(): boolean; virtual;
    function Disconnect: boolean; virtual;
    function SendPacket(const a: array of char): boolean; virtual;
    function RecvPacket(var a: array of char; const tend: cardinal): boolean; virtual;
    function SendStr(const str: string): boolean; virtual;
    function RecvStr(var str: string; const bwait: boolean = false): integer; virtual;
    function RecvStrTimeout(var str: string; const tend: cardinal): integer; virtual;
    function RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal = 3000): integer; virtual;
    function RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean = false): integer; virtual;
    function WaitForReading(const tend: cardinal): boolean; virtual;
  end;
  PConnBase = ^TConnBase;

const
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
  CINT_TIMEOUT_DEFAULT = 1000;//default timeout in milliseconds
  CINT_RECV_INTERVAL = 50;    //milliseconds 

implementation
uses SysUtils, StrUtils, Windows, Registry;

class function TConnBase.GetConnectTypeEnum(const conkey: string; var val: EConnectType): boolean;
var i_idx: integer;
begin
  i_idx := IndexText(conkey, CSTR_CONN_KEYS);
  if ((i_idx >= Ord(Low(EConnectType))) and (i_idx <= Ord(High(EConnectType)))) then begin
    val := EConnectType(i_idx);
    result := true;
  end else result := false;
end;

class function TConnBase.GetConnectTypeName(const etype: EConnectType): string;
begin
  result := CSTR_CONN_KEYS[etype];
end;

function TConnBase.IsConnected(): boolean;
begin
  result := false;
  AddMessage('Virtual function ''IsConnected'' should be reimplemented.', ML_WARNING);
end;

function  TConnBase.GetTypeName(): string;
begin
  result := TConnBase.GetConnectTypeName(e_type);
end;

procedure TConnBase.AddMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.AddMessage(format('%s', [text]), ClassName(), level);
  end;
end;

procedure TConnBase.UpdateMessage(const text: string; const level: EMessageLevel);
begin
  if assigned(t_messenger) then begin
    t_messenger.UpdateMessage(format('%s', [text]), ClassName(), level);
  end;
end;

constructor TConnBase.Create(owner: TComponent);
begin
  inherited Create(owner);
  e_type := CT_UNKNOWN;
  t_connobj := nil;
  e_state := CS_UNKNOWN;
  i_timeout := CINT_TIMEOUT_DEFAULT;
  //todo:
end;

destructor TConnBase.Destroy;
begin
  //todo:
  inherited Destroy();
end;

function TConnBase.Connect(): boolean;
begin
  result := false;
  AddMessage('Virtual function ''Connect'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.Disconnect: boolean;
begin
  result := false;
  AddMessage('Virtual function ''Disconnect'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.SendPacket(const a: array of char): boolean;
begin
  result := false;
  AddMessage('Virtual function ''SendPacket'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.RecvPacket(var a: array of char; const tend: cardinal): boolean;
begin
  result := false;
  AddMessage('Virtual function ''RecvPacket'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.SendStr(const str: string): boolean;
begin
  result := false;
  AddMessage('Virtual function ''SendStr'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.RecvStr(var str: string; const bwait: boolean): integer;
begin
  result := 0;
  AddMessage('Virtual function ''RecvStr'' should be reimplemented.', ML_WARNING);
end;

function TConnBase.RecvStrTimeout(var str: string; const tend: cardinal): integer;
var s_recv: string;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    if WaitForReading(tend) then begin
      result := result + RecvStr(s_recv);
      str := str + s_recv;
    end;
  until (tend <= GetTickCount());
end;

function TConnBase.RecvStrInterval(var str: string; const tend: cardinal; const interv: cardinal): integer;
var i_time: cardinal; s_recv: string; b_break: boolean;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    result := result + RecvStr(s_recv);
    str := str + s_recv;
    i_time := GetTickCount() + interv;
    if (i_time > tend) then i_time := tend;
    b_break := WaitForReading(i_time);
  until (b_break or (tend <= GetTickCount()));
end;

function TConnBase.RecvStrExpected(var str: string; const exstr: string; tend: cardinal; const bcase: boolean): integer;
var b_break: boolean; s_recv: string;
begin
  result := 0; str := '';
  repeat
    s_recv := '';
    result := result + RecvStr(s_recv);
    str := str + s_recv;
    if bcase then b_break := ContainsStr(str, exstr)
    else b_break := ContainsText(str, exstr);
  until (b_break or (tend <= GetTickCount()));
end;

function TConnBase.WaitForReading(const tend: cardinal): boolean;
begin
  result := false;
  AddMessage('Virtual function ''WaitForReading'' should be reimplemented.', ML_WARNING);
end;

end.
