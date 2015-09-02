//==============================================================================
// A unit, in which some general functions will be implemented.
// Copyright    : (c) Metronix 2014
// Reversion    : $Revision 1.0$
// First author : /bsu/ 2014-07-14
// History      :
//==============================================================================
unit GenUtils;

interface
uses Forms, Windows, IniFiles;

  function GetRelativeFilePath(filePath, basePath: string ): string;
  function GetAbsoluteFilePath(filePath, basePath: string): string;
  //function HexToString(const strHex: string): string;

  procedure Delay(const msec: Cardinal);

type
  TNameValueSet=class(THashedStringList)

  end;

implementation
uses SysUtils, StrUtils;

function GetRelativeFilePath(filePath, basePath: string ): string;
var s_filepath: string;
begin
  s_filepath := GetAbsoluteFilePath(filePath, basePath);
  result:= ExtractRelativePath(IncludeTrailingPathDelimiter(basePath), ExtractFilePath(s_filepath)) + ExtractFileName(s_filepath);
end;

function GetAbsoluteFilePath(filePath, basePath: string): string;
begin
  if ExtractFileDrive(filePath) <> '' then result := ExpandFileName(filePath)
  else if LeftStr(filePath,1) = SysUtils.PathDelim then result := ExpandFileName(ExtractFileDrive(basePath) + filePath)
  else result := ExpandFileName(IncludeTrailingPathDelimiter(basePath) + filePath);
end;

procedure Delay(const msec: Cardinal);
var i_tcount: Cardinal;
begin
  i_tcount := GetTickCount() + msec;
  repeat Application.ProcessMessages();
  until (GetTickCount() > i_tcount);
end;

end.
