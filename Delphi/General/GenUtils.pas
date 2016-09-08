//==============================================================================
// Module name  : $RCSfile: GenUtils.pas,v $
// Description  : This unit defines global functions, which are not dependent on
//                any other unit except standard unit of delphi.
// Copyright    : (c) Metronix 2014
// Reversion    : $Revision 1.0$
// Compiler     : Delphi 2007
// Author       : 2014-07-14 /bsu/
// History      :
//==============================================================================
unit GenUtils;

interface
uses Classes;

type

  TGenUtils = class
    //file path
    class function GetRelativeFilePath(const filePath, basePath: string ): string;
    class function GetAbsoluteFilePath(const filePath, basePath: string): string;

    //string and array
    class function StrToCharArray(const sData: string; var aData: array of char): integer;
    class function StrFromCharArray(const aData: array of char): string;
    class function IsHexText(const sData: string): boolean;
    class function HexTextToCharArray(const sData: string; var aData: array of char): integer;
    class function HexTextFromCharArray(const aData: array of char; const len: integer = -1): string;
    class function IsAsciiValid(const str: string): boolean;
    class function ClearQuotationMarks(const str: string): string;
    class function ShowStrHex(const str: string): string;

    //process, thread, windows message and so on
    class procedure Delay(const msec: Cardinal = 10);
    class function  EnumComPorts(var sports: TStrings): integer;
    class function  IsComPortValid(const sport: string; var iport: integer): boolean;
  end;

const
  C_DELAY_ONCE: Cardinal = 20;      //delay 20 milli seconds for communication in oneshot

implementation
uses Forms, SysUtils, StrUtils, Math,Windows, Registry;

// =============================================================================
// Class        : TGenUtils
// Function     : returns a relative file path referring to the give path
// Parameter    : filePath, a file path
//                basePath, a reference path
// Return       : a file path in a string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.GetRelativeFilePath(const filePath, basePath: string ): string;
var s_filepath: string;
begin
  s_filepath := GetAbsoluteFilePath(filePath, basePath);
  result:= ExtractRelativePath(IncludeTrailingPathDelimiter(basePath), ExtractFilePath(s_filepath)) + ExtractFileName(s_filepath);
end;

// =============================================================================
// Class        : TGenUtils
// Function     : returns a absolute file path referring to the give path
// Parameter    : filePath, a file path
//                basePath, a reference path
// Return       : a file path in a string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.GetAbsoluteFilePath(const filePath, basePath: string): string;
begin
  if ExtractFileDrive(filePath) <> '' then result := ExpandFileName(filePath)
  else if LeftStr(filePath,1) = SysUtils.PathDelim then result := ExpandFileName(ExtractFileDrive(basePath) + filePath)
  else result := ExpandFileName(IncludeTrailingPathDelimiter(basePath) + filePath);
end;

// =============================================================================
// Class        : TGenUtils
// Function     : calls ProcessMessages in a loop till the give milliseconds escapes
// Parameter    : msec, count of milli seconds. Defaut is 10 milli seconds
// Return       : --
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class procedure TGenUtils.Delay(const msec: Cardinal);
var iTimeout: Cardinal;
begin
  iTimeout := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() > iTimeout);
end;

// =============================================================================
// Class        : TGenUtils
// Function     : copies a string into an array
// Parameter    : sData, a source string to copy
// Return       : count of chars, which are copied
//                -1, size of aData is too small
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.StrToCharArray(const sData: string; var aData: array of char): integer;
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
// Class        : TGenUtils
// Function     : returns a string from an array with null-terminal back
// Parameter    : aData, an array of chars
// Return       : string
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.StrFromCharArray(const aData: array of char): string;
begin
  result := PChar(@aData);
end;

// =============================================================================
// Class        : TGenUtils
// Function     : checks, whether all bytes in sData are hexadicmal digits
// Parameter    : sData, a string to check
// Return       : true if all bytes are hexadicimal digits. Otherwise false
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.IsHexText(const sData: string): boolean;
const C_HEX_CHARS: set of char = ['0','1','2','3','4','5','6','7','8','9',
                                  'A','B','C','D','E','F',
                                  'a','b','c','d','e','f'];
var i, iLen: integer;
begin
  result := true;
  iLen := length(sData);
  for i := 1 to iLen do begin
    if not (sData[i] in C_HEX_CHARS) then begin
      result := false;
      break;
    end;
  end;
end;

// =============================================================================
// Class        : TGenUtils
// Function     : converts a string from hexadecimal format to an array of char
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
class function TGenUtils.HexTextToCharArray(const sData: string; var aData: array of char): integer;
var i, iLen, iChar, iLow: integer; sBuf, sHex: string;
begin
  if IsHexText(sData) then begin
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
// Class        : TGenUtils
// Function     : gets a string from a char array. The result is composed of
//                hexadecimal digits.
// Parameter    : aData, an source array of char
//                len, maximal count of the chars, which should be catch from aData. Defaut
//                     value(-1) means, all chars should komplettely be catched
// Return       : a string of hexadecimal digits, in which two digits presents one char of aData
// Exceptions   : --
// First author : 2015-09-14 /bsu/
// History      :
// =============================================================================
class function TGenUtils.HexTextFromCharArray(const aData: array of char; const len: integer): string;
var i, iLen: integer;
begin
  result := '';
  iLen := length(aData);
  if (len >= 0) then iLen := IfThen(len > iLen, iLen, len);
  for i := LOW(aData) to iLen - 1 do result := result + format('%.02x', [integer(aData[i])]);
end;

// =============================================================================
// Class        : TGenUtils
// Function     : exames if all chars of the given string are valid ascii chars
// Parameter    : str, string to be examed
// Return       : true, if all chars are ascii char.
//                false, otherwise
// Exceptions   : --
// First author : 2015-11-06 /bsu/
// History      :
// =============================================================================
class function TGenUtils.IsAsciiValid(const str: string): boolean;
var i: integer;
begin
  result := true;
  for i := 1 to Length(str) do begin
    if (Char(str[i]) > Char($7F)) then begin
      result := false;
      break;
    end;
  end;
end;

class function TGenUtils.ClearQuotationMarks(const str: string): string;
var p_char: PChar;
begin
  result := trim(str); p_char := PChar(result);
  if (length(result) >= 2) then begin
    if result[1] = '''' then  result := AnsiExtractQuotedStr(p_char, '''')
    else  result := AnsiExtractQuotedStr(p_char, '"');  //???it does not work
  end;
end;

class function TGenUtils.ShowStrHex(const str: string): string;
var i: integer; byte_char: byte;
begin
  result := '';
  for i := 1 to length(str) do begin
    byte_char := byte(str[i]);
    result := result + IntToHex(byte_char, 2);
  end;
end;

// =============================================================================
// Class        : TGenUtils
// Function     : enumerates all serial ports from Windows-Registry
// Parameter    : sports, an output string list in which all ports are found, e.g.
//                ('COM1', 'COM2' ...)
// Return       : integer, count of the found ports
// Exceptions   : --
// First author : 2016-05-20 /bsu/
// History      :
// =============================================================================
class function TGenUtils.EnumComPorts(var sports: TStrings): integer;
var tReg: TRegistry; sPortNames: TStrings; sVal: string; i: integer;
begin
  sports.Clear;
  tReg := TRegistry.Create();
  with tReg do begin
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly('Hardware\DeviceMap\SerialComm') then begin
      sPortNames := TStringList.Create;
      GetValueNames(sPortNames);
      for i := 0 to sPortNames.Count - 1 do begin
        sVal := ReadString(sPortNames[i]);
        sVal := UpperCase(trim(sVal));
        if sVal <> '' then sports.Add(sVal);
      end;
      sPortNames.Clear();
      FreeAndNil(sPortNames);
      CloseKey();
    end;
  end;
  FreeAndNil(tReg);
  result := sports.Count;
end;

// =============================================================================
// Class        : TGenUtils
// Function     : checks if the given string is a valid  number of serial port.
//                saves it in iport if valid. Otherwise assigns iport = -1
// Parameter    : sport, port number represented in string
//                iport, output of the port number represented in integer if sport is valid
// Return       : true, if sport is a valid number of the serial port on this comuputer
//                false, otherwise
// Exceptions   : --
// First author : 2016-05-20 /bsu/
// History      :
// =============================================================================
class function TGenUtils.IsComPortValid(const sport: string; var iport: integer): boolean;
var sComPorts: TStrings; sPortname: string; iPortnr: integer;
begin
  result := false; iport := -1;
  if TryStrToInt(sport, iPortnr) then begin
    sPortname := 'COM' + IntToStr(iportnr);
    sComPorts := TStringList.Create();
    EnumComPorts(sComPorts);
    result := (sComPorts.IndexOf(sPortname) >= 0 );
    if result then iport := iPortnr;
    sComPorts.Clear();
    FreeAndNil(sComPorts);
  end;
end;

end.
