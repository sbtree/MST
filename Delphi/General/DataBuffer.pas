unit DataBuffer;

interface
uses Windows, SysUtils;
type
  IBufferInterf = interface
    function BufferSize: Integer;
    function ResetBuffer: Boolean;
    function CountFree: Integer;
    function CountUsed: Integer;
    function IsEmpty: Boolean;
    function IsFull: Boolean;
  end;

  TBufferBase = class(TInterfacedObject, IBufferInterf)
  protected
    i_length  : Integer; //length of buffer
    p_read    : Integer; //pointer to read
    p_write   : Integer; //pointer to write
    b_full    : Boolean; //specifies if the buffer is full
  public
    constructor Create;overload;
    constructor Create(len : Integer);overload;virtual;abstract;
    destructor Destroy;override;

    function BufferSize: Integer;
    function InitBuffer(len:Integer):Boolean;virtual;abstract;
    function ResetBuffer: Boolean;
    function CountFree: Integer;
    function CountUsed: Integer;
    function IsEmpty: Boolean;
    function IsFull: Boolean;
  end;

  TCharBuffer = class(TBufferBase)
  protected
    a_chars    : array of char; //dynamic buffer
  protected
    function IsHexString(const str: string): boolean;
    function WriteHexStr(const str: string): integer;
  public
    constructor Create(len : Integer);override;
    destructor Destroy;override;
    function InitBuffer(len: integer): boolean;override;
    function ReadChar(var ch: char): boolean;
    function ReadStr(const bHex: boolean = false): String;
    function WriteChar(ch: char): boolean;
    function WriteStr(const str: string; const bHex: boolean = false): integer;
  end;

const
  C_BUFFER_SIZE     : integer = 1024;

implementation

constructor TBufferBase.Create;
begin
	inherited Create;
  i_length:= 0;
  p_read  := 0;
  p_write := 0;
  b_full  := false;
end;

destructor TBufferBase.Destroy;
begin
	inherited Destroy;
end;

function TBufferBase.BufferSize: integer;
begin
  result := i_length;
end;

function TBufferBase.ResetBuffer(): boolean;
begin
  p_write:= 0;
  p_read := p_write;
  b_full := false;
  result := true;
end;

function TBufferBase.CountUsed(): integer;
begin
  if b_full then result := i_length
  else if (p_write >= p_read) then result := p_write - p_read
  else result := i_length + p_write - p_read;
end;

function TBufferBase.CountFree(): integer;
begin
  result := i_length - CountUsed();
end;

function TBufferBase.IsFull(): boolean;
begin
  result := (CountUsed() >= i_length);
end;

function TBufferBase.IsEmpty(): boolean;
begin
  result := ((CountUsed() <= 0) and (i_length > 0));
end;

function TCharBuffer.IsHexString(const str: string): boolean;
var i, iLen: integer;
const C_HEX_CHARS: set of char = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','a','b','c','d','e','f'];
begin
  result := true;
  iLen := length(str);
  for i := 1 to iLen do
    if not (str[i] in C_HEX_CHARS) then begin
      result := false;
      break;
    end;
end;

function TCharBuffer.WriteHexStr(const str: string): integer;
var i, iLen: integer; sBuf, sHex: string;
begin
  result := 0;
  if IsHexString(str) then begin
    iLen := length(str);
    if (iLen mod 2) <> 0 then sBuf := '0' + str
    else sBuf := str;
    iLen := (length(sBuf) shr 1);
    result := iLen;
    for i := 1 to iLen  do begin
      sHex := '$' + sBuf[i*2-1] + sBuf[i*2];
      if not WriteChar(Char(StrToInt(sHex))) then begin
        result := i;
        break;
      end;
    end;
  end;
end;

constructor TCharBuffer.Create(len : Integer);
begin
  inherited Create;
  InitBuffer(len);
end;

destructor TCharBuffer.Destroy;
begin
  SetLength(a_chars, 0);
	inherited Destroy;
end;

function TCharBuffer.InitBuffer(len: integer): boolean;
begin
  result := false;
  if (len > 0) then
  begin
    i_length := len;
    SetLength(a_chars, i_length);
    ZeroMemory(a_chars, i_length);
    result := ResetBuffer();
  end;
end;

function TCharBuffer.ReadChar(var ch: char): boolean;
begin
  result := (not IsEmpty());
  if result then
  begin
    ch := a_chars[p_read];
    Inc(p_read);
    p_read := (p_read mod i_length);
    b_full := false;
  end;
end;

function TCharBuffer.ReadStr(const bHex: boolean): String;
var ch: char;
begin
  if bHex then while (ReadChar(ch)) do result := result + format('%.02x',[integer(ch)])
  else result := result + ch;
end;

function TCharBuffer.WriteChar(ch: char): boolean;
begin
  result := (not IsFull());
  if result then
  begin
    a_chars[p_write] := ch;
    Inc(p_write);
    p_write := (p_write mod i_length);
    b_full := (p_write = p_read);
  end;
end;

function TCharBuffer.WriteStr(const str: string; const bHex: boolean): integer;
var i,iLen: integer;
begin
  result := 0;
  iLen := length(str);
  if bHex then result := WriteHexStr(str)
  else for i := 1 to iLen do if (WriteChar(str[i])) then inc(result);
end;

end.
