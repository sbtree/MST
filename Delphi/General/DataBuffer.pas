//==============================================================================
// Module name  : $RCSfile: DataBuffer.pas,v $
// Description  : This unit defines a class of ring buffer and some of its subclasses
// Copyright    : (c) Metronix 2014
// Reversion    : $Revision 1.0$
// Compiler     : Delphi 2007
// Author       : 2014-07-14 /bsu/
// History      :
//==============================================================================
unit DataBuffer;

interface
uses Windows, SysUtils, GenUtils;
type

  TRingBuffer<T> = class
  protected
    t_buffer: array of T;
    p_read:   Integer; //pointer to read
    p_write:  Integer; //pointer to write
    b_overlap:Boolean; //indicate if write-pointer oversteps reader-pointer
  protected
    function ForwardReadPos(): boolean;
    function ForwardWritePos(): boolean;
    function GetSize(): integer;
  public
    constructor Create();
    destructor Destroy(); override;

    function Resize(const n: Integer): Boolean; virtual;
    function Clear: Boolean; virtual;
    function IsEmpty: Boolean;
    function IsFull: Boolean;
    function ReadElement(var elem: T; const bmove: boolean = true): boolean;
    function WriteElement(const elem: T): boolean;

    property BufferSize: integer read GetSize;
  end;

  TByteBuffer = class(TRingBuffer<Byte>)

  end;

  TCharBuffer = class(TRingBuffer<Char>)
  protected
    p_reread  : integer;
  public
    constructor Create;
    destructor Destroy;override;

    function ReadStr(const bmove: boolean = true): string;
    function WriteStr(const str: string): integer;
  end;

const
  C_BUFFER_SIZE     : integer = 1024;

implementation

function TRingBuffer<T>.ForwardReadPos(): boolean;
begin
  b_overlap := false;
  result := (p_read <> p_write);
  if result then begin
    inc(p_read);
    p_read := (p_read mod BufferSize);
  end;
end;

function TRingBuffer<T>.ForwardWritePos(): boolean;
begin
  result := false;
  if not b_overlap then begin
    inc(p_write);
    p_write := (p_write mod BufferSize);
    b_overlap := (p_write = p_read);
    result := true;
  end;
end;

function TRingBuffer<T>.GetSize(): integer;
begin
  result := Length(t_buffer);
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

function TRingBuffer<T>.Resize(const n: Integer): Boolean;
begin
  if (n >= 0) then begin
    SetLength(t_buffer, n);
    result := true;
  end else result := false;
end;

function TRingBuffer<T>.Clear: Boolean;
begin
  p_read := p_write;
  b_overlap := false;
  result := true;
end;

function TRingBuffer<T>.IsEmpty: Boolean;
begin
  result := (p_read = p_write);
end;

function TRingBuffer<T>.IsFull: Boolean;
begin
  result := (p_write + 1 = p_read) or
            ((p_read = 0) and (p_write + 1 = BufferSize));
end;

function TRingBuffer<T>.ReadElement(var elem: T; const bmove: boolean): boolean;
begin
  result := (not IsEmpty());
  if result then elem := t_buffer[p_read];
  if bmove then ForwardReadPos();
end;

function TRingBuffer<T>.WriteElement(const elem: T): boolean;
begin
  result := (not IsFull());
  if result then t_buffer[p_write] := T;
  ForwardWritePos();
end;

constructor TCharBuffer.Create();
begin
  inherited Create();
  p_reread := p_read;
end;

destructor TCharBuffer.Destroy;
begin
	inherited Destroy;
end;

function TCharBuffer.ReadStr(const bmove: boolean): string;
var ch: char; i_start: integer;
begin
  result := ''; i_start := p_read;
  while (ReadElement(ch, bmove)) do result := result + ch;
end;

function TCharBuffer.WriteStr(const str: string): integer;
var i,iLen: integer;
begin
  result := 0;
  iLen := length(str);
  for i := 1 to iLen do begin
    if (WriteElement(str[i])) then result := i
    else break;
  end;
end;

end.
