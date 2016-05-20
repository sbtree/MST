unit DataBuffer;

interface
uses Windows, SysUtils, GenUtils;
type

  TBufferBase = class
  protected
    i_size  : Integer; //size of buffer
    p_read  : Integer; //pointer to read
    p_write : Integer; //pointer to write
    b_overlap: Boolean; //indicate if write-pointer oversteps reader-pointer
    b_ringed: Boolean; //indicate if p_write is already gone once around
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
    a_chars   : array of char; //dynamic array of char as a ring buffer
    p_reread  : integer;
  protected
    function IsHexString(const str: string): boolean;

  public
    constructor Create;
    destructor Destroy;override;

    function Resize(const n: Integer):Boolean; override;
    function ReadChar(var ch: char): boolean;
    function ReadStr(const bClear: boolean = true): string;
    function ReadHex(): string;
    function HistoryChar(var ch: char; const idx: integer): boolean;
    function HistoryStr(): string;
    function HistoryHex(): string;
    function WriteChar(const ch: char): boolean;
    function WriteStr(const str: string): integer;
    function WriteHex(const str: string): integer;
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
    if not b_ringed then b_ringed := (p_write >= i_size);  
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
  b_ringed:= false;
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
begin
  result := TGenUtils.IsHexText(str);
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

function TCharBuffer.ReadStr(const bClear: boolean): string;
var ch: char; i_start: integer;
begin
  result := ''; i_start := p_read;
  while (ReadChar(ch)) do result := result + ch;
  if (not bClear) then p_read := i_start;
end;

{function TCharBuffer.RereadStr(): string;
var ch: char;
begin
  result := ''; p_read := p_reread;
  while (ReadChar(ch)) do result := result + ch;
end;}

function TCharBuffer.ReadHex(): string;
var ch: char;
begin
  result := '';
  while (ReadChar(ch)) do result := result + format('%.02x',[integer(ch)])
end;

function TCharBuffer.HistoryChar(var ch: char; const idx: integer): boolean;
var p_char: integer;
begin
  if b_ringed then  result := ((idx > 0) and (idx <= CountFree()))
  else  result := ((idx > 0) and (idx <= p_read));

  if result then begin
    p_char := ((p_read + i_size - idx) mod i_size);
    ch := a_chars[p_char];
  end;
end;

function TCharBuffer.HistoryStr(): string;
var ch: char; i: integer;
begin
  result := ''; i := 1;
  while HistoryChar(ch, i) do begin
    result := ch + result;
    inc(i);
  end;
end;

function TCharBuffer.HistoryHex(): string;
var ch: char; i: integer;
begin
  result := ''; i := 1;
  while HistoryChar(ch, i) do begin
    result := format('%.02x', [integer(ch)]) + result;
    inc(i);
  end;
end;

function TCharBuffer.WriteChar(const ch: char): boolean;
begin
  result := (not IsFull());
  if result then
  begin
    a_chars[p_write] := ch;
    result := ForwardWrite();
  end;
end;

function TCharBuffer.WriteStr(const str: string): integer;
var i,iLen: integer;
begin
  result := 0;
  iLen := length(str);
  for i := 1 to iLen do begin
    if (WriteChar(str[i])) then result := i
    else break;
  end;
end;

function TCharBuffer.WriteHex(const str: string): integer;
var i, iLen: integer; sBuf, sHex: string;
begin
  result := 0;
  if IsHexString(str) then begin
    iLen := length(str);
    if (iLen mod 2) <> 0 then sBuf := '0' + str
    else sBuf := str;
    iLen := (length(sBuf) shr 1);
    for i := 1 to iLen  do begin
      sHex := '$' + sBuf[i*2-1] + sBuf[i*2];
      if WriteChar(Char(StrToInt(sHex))) then result := i
      else break;
    end;
  end;
end;

end.
