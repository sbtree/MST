//==============================================================================
// A unit, in which some general functions will be implemented.
// Copyright    : (c) Metronix 2014
// Reversion    : $Revision 1.0$
// First author : /bsu/ 2014-07-14
// History      :
//==============================================================================
unit GenUtils;

interface
uses Forms, Windows, IniFiles, Contnrs;

  //file path
  function GetRelativeFilePath(filePath, basePath: string ): string;
  function GetAbsoluteFilePath(filePath, basePath: string): string;

  //string and array
  function IndexOfStr(const aArray: array of string; const S: string): integer;
  function StrToCharArray(const sData: string; var aData: array of char): integer;
  function StrFromCharArray(const aData: array of char): string;
  function IsHexStr(const sData: string): boolean;
  function HexStrToCharArray(const sData: string; var aData: array of char): integer;
  function HexStrFromCharArray(const aData: array of char; const len: integer = -1): string;

  //process, thread and windows message
  procedure Delay(const msec: Cardinal = 10);

const
  C_DELAY_ONCE: Cardinal = 20;      //delay 20 milli seconds for communication in oneshot

implementation
uses SysUtils, StrUtils, Math;

// =============================================================================
// Class        : --
// Function     : GetRelativeFilePath
//                returns a relative file path referring to the give path
// Parameter    : filePath, a file path
//                basePath, a reference path
// Return       : a file path in a string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function GetRelativeFilePath(filePath, basePath: string ): string;
var s_filepath: string;
begin
  s_filepath := GetAbsoluteFilePath(filePath, basePath);
  result:= ExtractRelativePath(IncludeTrailingPathDelimiter(basePath), ExtractFilePath(s_filepath)) + ExtractFileName(s_filepath);
end;

// =============================================================================
// Class        : --
// Function     : GetAbsoluteFilePath
//                returns a absolute file path referring to the give path
// Parameter    : filePath, a file path
//                basePath, a reference path
// Return       : a file path in a string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function GetAbsoluteFilePath(filePath, basePath: string): string;
begin
  if ExtractFileDrive(filePath) <> '' then result := ExpandFileName(filePath)
  else if LeftStr(filePath,1) = SysUtils.PathDelim then result := ExpandFileName(ExtractFileDrive(basePath) + filePath)
  else result := ExpandFileName(IncludeTrailingPathDelimiter(basePath) + filePath);
end;

// =============================================================================
// Class        : --
// Function     : Delay
//                calls ProcessMessages in a loop till the give milliseconds escapes
// Parameter    : msec, count of milli seconds. Defaut is 10 milli seconds
// Return       : --
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
procedure Delay(const msec: Cardinal);
var i_tcount: Cardinal;
begin
  i_tcount := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() > i_tcount);
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
var I: Integer;
begin
  Result := -1;
  for I := Low(aArray) to High(aArray) do
    if SameText(S, aArray[I]) then begin
      Result := I;
      Break;
    end;
end;

// =============================================================================
// Class        : --
// Function     : StrToCharArray
//                copies a string into an array
// Parameter    : sData, a source string to copy
// Return       : count of chars, which are copied
//                -1, size of aData is too small
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function StrToCharArray(const sData: string; var aData: array of char): integer;
var iLen : integer;
begin
  iLen := length(aData);
  if iLen >= length(sData) then begin
    ZeroMemory(PChar(@aData), iLen);
    result := length(sData);
    StrPLCopy(aData, sData, result);
  end else result := -1; //size of aData is too small
end;

// =============================================================================
// Class        : --
// Function     : StrFromCharArray
//                return a string from an array with null-terminal back
// Parameter    : aData, an array
// Return       : string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function StrFromCharArray(const aData: array of char): string;
begin
  result := PChar(@aData);
end;

// =============================================================================
// Class        : --
// Function     : IsHexStr
//                checks, whether all bytes in sData are hexadicmal digits
// Parameter    : sData, a string to check
// Return       : true if all bytes are hexadicimal digits. Otherwise false
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function IsHexStr(const sData: string): boolean;
const C_HEX_CHARS: set of char = ['0','1','2','3','4','5','6','7','8','9',
                                  'A','B','C','D','E','F',
                                  'a','b','c','d','e','f'];
var i, iLen: integer;
begin
  result := true;
  iLen := length(sData);
  for i := 1 to iLen do
    if not (sData[i] in C_HEX_CHARS) then begin
      result := false;
      break;
    end;
end;

// =============================================================================
// Class        : --
// Function     : HexStrToCharArray
//                converts a string from hexadecimal format to an array of char
// Parameter    : sData, a string in hexadecimal format, e.g.: '303132414243'
//                aData to output, in which the result is saved, e.g. result of
//                '303132414243' is:
//                aData[0]=30, aData[1]=31, aData[2]=32, aData[3]=41, aData[4]=42,...
//                NOTE: two hexadicimal digits in sData present one char in aData
// Return       : size of fufilled chars in aData if it succeeds
//                -1, length of aData is too small
//                -2, the given hexadecimal string is invalid
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function HexStrToCharArray(const sData: string; var aData: array of char): integer;
var i, iLen, iChar, iLow: integer; sBuf, sHex: string;
begin
  if IsHexStr(sData) then begin
    iLen := length(sData);
    if (iLen mod 2) <> 0 then sBuf := '0' + sData //fulfills a '0' if the length of sData is odd
    else sBuf := sData;
    iLen := (length(sBuf) shr 1); // divided by 2, two hexadicimal digits present one char
    if iLen <= length(aData) then begin
      iLow := LOW(aData);
      result := iLen;
      for i := 0 to iLen - 1  do begin
        sHex := '$' + MidStr(sBuf, i*2+1, 2);
        TryStrToInt(sHex, iChar); //it muss be successful, because the chars are all checked in IsHexStr(str)
        aData[i + iLow] := Char(iChar)
      end;
    end else result := -1; //length of aData is too small
  end else result := -2; //the hexadecimal string is invalid.
end;

// =============================================================================
// Class        : --
// Function     : HexStrFromCharArray
//                gets a string from a char array. The result is composed of
//                hexadecimal digits.
// Parameter    : aData, an source array of char
//                len, maximal count of the chars, which should be catch from aData. Defaut
//                     value(-1) means, all chars should komplettely be catched
// Return       : a string of hexadecimal digits, in which two digits presents one char of aData
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
function HexStrFromCharArray(const aData: array of char; const len: integer): string;
var i, iLen: integer;
begin
  result := '';
  iLen := length(aData);
  if (len >= 0) then iLen := IfThen(len > iLen, iLen, len);
  for i := LOW(aData) to iLen - 1 do result := result + format('%.02x', [integer(aData[i])]);
end;

end.
