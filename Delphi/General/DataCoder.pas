unit DataCoder;

interface

type
  TDataCoder<T1, T2> = class
  protected
    a_srclist: array of T1;
    a_destlist: array of T2;
  public
    destructor Destroy(); override;

    function Encode(const src: T1; var dest: T2): boolean;
    function Decode(const src: T2; var dest: T1): boolean;
  End;

  TEscapeCoder = class(TDataCoder<string, byte>)
  public
    constructor Create();
  End;

implementation
uses System.Generics.Collections;

destructor TDataCoder<T1, T2>.Destroy();
begin
  SetLength(a_srclist, 0);
  SetLength(a_destlist, 0);
end;

function TDataCoder<T1, T2>.Encode(const src: T1; var dest: T2): boolean;
var i_idx: integer;
begin
  result := false;
  if TArray.BinarySearch<T1>(a_srclist, src, i_idx) then begin
    if ((i_idx >= LOW(a_destlist)) and (i_idx <= HIGH(a_destlist))) then begin
      dest := a_destlist[i_idx];
      result := true;
    end;
  end;
end;

function TDataCoder<T1, T2>.Decode(const src: T2; var dest: T1): boolean;
var i_idx: integer;
begin
  result := false;
  if TArray.BinarySearch<T2>(a_destlist, src, i_idx) then begin
    if ((i_idx >= LOW(a_srclist)) and (i_idx <= HIGH(a_srclist))) then begin
      dest := a_srclist[i_idx];
      result := true;
    end;
  end;
end;

constructor TEscapeCoder.Create();
begin
  SetLength(a_srclist, 12);
  SetLength(a_destlist, 12);
  a_srclist[0] := '\0';   a_destlist[0] := 0;
  a_srclist[1] := '\c';   a_destlist[1] := 3;
  a_srclist[2] := '\a';   a_destlist[2] := 7;
  a_srclist[3] := '\b';   a_destlist[3] := 8;
  a_srclist[4] := '\t';   a_destlist[4] := 9;
  a_srclist[5] := '\n';   a_destlist[5] := 10;
  a_srclist[6] := '\v';   a_destlist[6] := 11;
  a_srclist[7] := '\f';   a_destlist[7] := 12;
  a_srclist[8] := '\r';   a_destlist[8] := 13;
  a_srclist[9] := '\"';   a_destlist[9] := 34;
  a_srclist[10]:= '\''';  a_destlist[10]:= 39;
  a_srclist[11]:= '\\';   a_destlist[11]:= 92;
end;

end.
