unit DataBuffer;

interface
uses Windows, SysUtils;
type

  TBufferBase = class
  protected
    i_size  : Integer; //size of buffer
    p_read  : Integer; //pointer to read
    p_write : Integer; //pointer to write
    b_overlap: Boolean; //indicate if write-pointer oversteps reader-pointer
  protected
    function ForwardRead(): boolean;
    function ForwardWrite(): boolean;

  public
    property Size: integer Read i_size;

  public
    constructor Create;
    destructor Destroy;override;

    function Resize(const n: Integer):Boolean; virtual; abstract;
    function Clear: Boolean; virtual;
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
    constructor Create;
    destructor Destroy;override;

    function Resize(const n: Integer):Boolean; override;
    function ReadChar(var ch: char): boolean;
    function ReadStr(const bHex: boolean = false): String;
    function WriteChar(ch: char): boolean;
    function WriteStr(const str: string; const bHex: boolean = false): integer;
  end;

const
  C_BUFFER_SIZE     : integer = 1024;

implementation

function TBufferBase.ForwardRead(): boolean;
begin
  inc(p_read);
  p_read := (p_read mod i_size);
  b_overlap := false;
  result := true;
end;

function TBufferBase.ForwardWrite(): boolean;
begin
  result := false;
  if not b_overlap then begin
    inc(p_write);
    p_write := (p_write mod i_size);
    b_overlap := (p_write = p_read);
    result := true;
  end;
end;

constructor TBufferBase.Create;
begin
	inherited Create;
  i_size  := C_BUFFER_SIZE;
  p_read  := 0;
  p_write := 0;
  b_overlap := false;
end;

destructor TBufferBase.Destroy;
begin
	inherited Destroy;
end;

function TBufferBase.Clear(): boolean;
begin
  p_read := p_write;
  b_overlap := false;
  result := true;
end;

function TBufferBase.CountUsed(): integer;
begin
  if b_overlap then result := i_size
  else result := ((i_size + p_write - p_read) mod i_size);
end;

function TBufferBase.CountFree(): integer;
begin
  result := i_size - CountUsed();
end;

function TBufferBase.IsFull(): boolean;
begin
  result := b_overlap;
end;

function TBufferBase.IsEmpty(): boolean;
begin
  result := (CountUsed() = 0) ;
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

constructor TCharBuffer.Create();
begin
  inherited Create;
  SetLength(a_chars, i_size);
  ZeroMemory(a_chars, i_size);
end;

destructor TCharBuffer.Destroy;
begin
  SetLength(a_chars, 0);
	inherited Destroy;
end;

function TCharBuffer.Resize(const n: Integer):Boolean;
begin
  result := false;
  if (n > 0) then begin
    i_size := n;
    SetLength(a_chars, i_size);
    if p_write >= i_size then p_write := i_size -1;
    if p_read >= i_size then p_read := i_size -1;
    result := true;
  end;
end;

function TCharBuffer.ReadChar(var ch: char): boolean;
begin
  result := (not IsEmpty());
  if result then
  begin
    ch := a_chars[p_read];
    result := ForwardRead();
  end;
end;

function TCharBuffer.ReadStr(const bHex: boolean): String;
var ch: char;
begin
  result := '';
  if bHex then while (ReadChar(ch)) do result := result + format('%.02x',[integer(ch)])
  else while (ReadChar(ch)) do result := result + ch;
end;

function TCharBuffer.WriteChar(ch: char): boolean;
begin
  result := (not IsFull());
  if result then
  begin
    a_chars[p_write] := ch;
    result := ForwardWrite();
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
