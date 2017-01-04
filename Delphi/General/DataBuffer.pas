//==============================================================================
// Module name  : $RCSfile: DataBuffer.pas,v $
// Description  : This unit defines a class of ring buffer and some of its subclasses
// Copyright    : (c) Metronix 2014
// Reversion    : $Revision 1.0$
// Compiler     : Delphi XE7
// Author       : 2014-07-14 /bsu/
// History      :
//==============================================================================
unit DataBuffer;

interface
uses Windows, SysUtils;
type

  TRingBuffer<T> = class
  protected
    t_buffer: array of T;
    p_read:   Integer; //pointer to read
    p_write:  Integer; //pointer to write
    b_overlap:Boolean; //indicate if write-pointer oversteps reader-pointer
  private
    procedure ForwardReadPos();
    procedure ForwardWritePos();
  protected
    function GetSize(): integer;
    function GetCountUsed(): integer;
    function GetCountFree(): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    function Resize(const n: Word): Boolean; virtual;
    procedure Clear(); virtual;
    function IsEmpty: Boolean;
    function IsFull: Boolean;
    function GoToLastElement(): integer;
    function ReadElement(var elem: T): boolean;
    function WriteElement(const elem: T): boolean;

    property BufferSize: integer read GetSize;
    property CountUsed: integer read GetCountUsed;
    property CountFree: integer read GetCountFree;
  end;

  TByteBuffer = class(TRingBuffer<Byte>)
  public
    function ReadHex(): string;
    function WriteHex(const str: string): integer;
    function ReadAnsiStr(): AnsiString;
    function ReadBytes(var pbuf: PByteArray; var wlen: word): boolean;
    function WriteBytes(const pbytes: PByteArray; const wlen: word): integer;
    function WriteAnsiStr(const str: AnsiString): integer;
  end;

  TCharBuffer = class(TRingBuffer<Char>)
  public
    function ReadStr(): string;
    function WriteStr(const str: string): integer;
  end;

const
  C_BUFFER_SIZE     : integer = 1024;

implementation
uses StrUtils, GenUtils;

procedure TRingBuffer<T>.ForwardReadPos();
begin
  b_overlap := false;
  inc(p_read);
  p_read := (p_read mod BufferSize);
end;

procedure TRingBuffer<T>.ForwardWritePos();
begin
  inc(p_write);
  p_write := (p_write mod BufferSize);
  b_overlap := (p_write = p_read);
end;

function TRingBuffer<T>.GetSize(): integer;
begin
  result := Length(t_buffer);
end;

function TRingBuffer<T>.GetCountUsed(): integer;
begin
  if IsFull() then result := BufferSize
  else if IsEmpty() then result := 0
  else begin
    if (p_write > p_read) then result := p_write - p_read
    else result := BufferSize + p_write - p_read;
  end;
end;

function TRingBuffer<T>.GetCountFree(): integer;
begin
  result := BufferSize - CountUsed;
end;

constructor TRingBuffer<T>.Create();
begin
  inherited Create();
end;

destructor TRingBuffer<T>.Destroy();
begin
  SetLength(t_buffer, 0);
  inherited Destroy();
end;

function TRingBuffer<T>.Resize(const n: Word): Boolean;
begin
  if IsEmpty() and (p_write = 0) then begin //allow to resize only if the buffer is not used
    SetLength(t_buffer, n);
    result := true;
  end else result := false;
end;

procedure TRingBuffer<T>.Clear();
begin
  p_read := p_write;
  b_overlap := false;
end;

function TRingBuffer<T>.IsEmpty: Boolean;
begin
  result := (p_read = p_write) and (not b_overlap);
end;

function TRingBuffer<T>.IsFull: Boolean;
begin
  result := b_overlap or (BufferSize = 0);
end;

function TRingBuffer<T>.GoToLastElement(): integer;
begin
  result := -1;
  if CountUsed > 0 then begin
    result := CountUsed - 1;
    b_overlap := false;
    p_read := ((p_read + result) mod BufferSize);
  end;
end;

function TRingBuffer<T>.ReadElement(var elem: T): boolean;
begin
  result := (not IsEmpty());
  if result then begin
    elem := t_buffer[p_read];
    ForwardReadPos();
  end;
end;

function TRingBuffer<T>.WriteElement(const elem: T): boolean;
begin
  result := (not IsFull());
  if result then begin
    t_buffer[p_write] := elem;
    ForwardWritePos();
  end;
end;

function TByteBuffer.ReadHex(): string;
var byte_cur: byte;
begin
  result := '';
  while (ReadElement(byte_cur)) do result := result + format('%0.2x', [byte_cur]);
end;

function TByteBuffer.WriteHex(const str: string): integer;
var s_hex, s_byte: string; i, i_len, i_val: integer;
begin
  result := 0; s_hex := trim(str);
  if TGenUtils.IsHexText(s_hex) then begin
    i_len := Length(s_hex);
    if ((i_len mod 2) <> 0) then s_hex := '0' + s_hex;
    i_len := (Length(s_hex) shl 1) - 1; //divided by 2, two hexadicimal digits represent one byte
    for i := 0 to i_len do begin
      s_byte := '$' + MidStr(s_hex, i * 2 + 1, 2);
      TryStrToInt(s_byte, i_val);
      if WriteElement(byte(i_val)) then inc(result)
      else break;
    end;
  end;
end;

function TByteBuffer.ReadAnsiStr(): AnsiString;
var byte_cur: byte;
begin
  result := '';
  while (ReadElement(byte_cur)) do
    if byte_cur <> 0 then
      result := result + AnsiChar(byte_cur);
end;

function TByteBuffer.ReadBytes(var pbuf: PByteArray; var wlen: word): boolean;
var byte_cur: byte; w_buflen: word;
begin
  w_buflen := wlen; wlen := 0;
  while ((wlen < w_buflen) and ReadElement(byte_cur)) do
    if byte_cur <> 0 then begin
      pbuf^[wlen] := byte_cur;
      inc(wlen);
    end;
  result := (wlen > 0);
end;

function TByteBuffer.WriteBytes(const pbytes: PByteArray; const wlen: word): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to wLen - 1 do begin
    if (WriteElement(pbytes^[i])) then inc(result)
    else break;
  end;
end;

function TByteBuffer.WriteAnsiStr(const str: AnsiString): integer;
begin
  result := WriteBytes(@str[1], length(str));
end;

function TCharBuffer.ReadStr(): string;
var ch: char;
begin
  result := '';
  while (ReadElement(ch)) do
    result := result + ch;
end;

function TCharBuffer.WriteStr(const str: string): integer;
var i,iLen: integer;
begin
  result := 0;
  iLen := length(str);
  for i := 1 to iLen do begin
    if (WriteElement(str[i])) then inc(result)
    else break;
  end;
end;

end.
