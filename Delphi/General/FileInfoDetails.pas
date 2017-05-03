unit FileInfoDetails;

interface
uses Windows;

type
  TFileInfoDetails = class
  private
    b_valid:  boolean;
    c_len, c_handle: cardinal;
    p_fi:   PVSFixedFileInfo;
    s_fipath: string;
  protected
    p_buf:  pointer;
  protected
    procedure FreeBuf();
    function FileInfoString(const skey: string): string;

    function GetCompanyName(): string;
    function GetFileDescription(): string;
    function GetFileVersion(): string;
    function GetInternalName(): string;
    function GetOriginalName(): string;
    function GetCopyRight(): string;
    function GetTrademark(): string;
    function GetProductName(): string;
    function GetProductVersion(): string;
    function GetComments(): string;
    function GetPrivateBuild(): string;
    function GetSpecialBuild(): string;
    function GetMetronixVer(): string;
  public
    //constructor and destructor
    constructor Create();
    destructor Destroy; override;

    function GetFileInfoDetails(const sfile: string): boolean;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVersion: string read GetFileVersion;
    property InternalName: string read GetInternalName;
    property OriginalName: string read GetOriginalName;
    property CopyRight: string read GetCopyRight;
    property Trademark: string read GetTrademark;
    property ProductName: string read GetProductName;
    property ProductVersion: string read GetProductVersion;
    property Comments: string read GetComments;
    property PrivateBuild: string read GetPrivateBuild;
    property SpecialBuild: string read GetSpecialBuild;
    property MetronixVersion: string read GetMetronixVer;
  end;

implementation
uses SysUtils;

type
  TLandCodepage = record
    wLanguage,
    wCodePage: word;
  end;
  PLandCodepage = ^TLandCodepage;

procedure TFileInfoDetails.FreeBuf();
begin
  if assigned(p_buf) then FreeMem(p_buf);
  p_buf := nil;
  p_fi := nil;
  b_valid := false;
  s_fipath := '';
end;

function TFileInfoDetails.FileInfoString(const skey: string): string;
var p_str: pointer;
begin
  result := '';
  if b_valid and VerQueryValue(p_buf, PChar(s_fipath + skey), p_str, c_len) then
    result := PChar(p_str);
end;

function TFileInfoDetails.GetCompanyName(): string;
begin
  result := FileInfoString('CompanyName');
end;

function TFileInfoDetails.GetFileDescription(): string;
begin
  result := FileInfoString('FileDescription');
end;

function TFileInfoDetails.GetFileVersion(): string;
begin
  result := FileInfoString('FileVersion');
end;

function TFileInfoDetails.GetInternalName(): string;
begin
  result := FileInfoString('InternalName');
end;

function TFileInfoDetails.GetOriginalName(): string;
begin
  result := FileInfoString('OriginalFileName');
end;

function TFileInfoDetails.GetCopyRight(): string;
begin
  result := FileInfoString('LegalCopyright');
end;

function TFileInfoDetails.GetTrademark(): string;
begin
  result := FileInfoString('LegalTrademarks');
end;

function TFileInfoDetails.GetProductName(): string;
begin
  result := FileInfoString('ProductName');
end;

function TFileInfoDetails.GetProductVersion(): string;
begin
  result := FileInfoString('ProductVersion');
end;

function TFileInfoDetails.GetComments(): string;
begin
  result := FileInfoString('Comments');
end;

function TFileInfoDetails.GetPrivateBuild(): string;
begin
  result := FileInfoString('PrivateBuild');
end;

function TFileInfoDetails.GetSpecialBuild(): string;
begin
  result := FileInfoString('SpecialBuild');
end;

function TFileInfoDetails.GetMetronixVer(): string;
begin
  result := '';
  if (b_valid and assigned(p_fi)) then begin
    If p_fi^.dwFileFlags and VS_FF_PRERELEASE <> 0 Then //test version
      Result := IntToStr(p_fi^.dwFileVersionMS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionMS and $FFFF)
      + '.100012'
      + '.' + IntToStr(p_fi^.dwFileVersionLS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionLS and $FFFF)
    else If p_fi^.dwFileFlags and VS_FF_SPECIALBUILD <> 0 Then //user special version
      Result := IntToStr(p_fi^.dwFileVersionMS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionMS and $FFFF)
      + '.' + GetSpecialBuild()
      + '.' + IntToStr(p_fi^.dwFileVersionLS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionLS and $FFFF)
    else
      Result := IntToStr(p_fi^.dwFileVersionMS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionMS and $FFFF)
      + '.0'
      + '.' + IntToStr(p_fi^.dwFileVersionLS shr 16)
      + '.' + IntToStr(p_fi^.dwFileVersionLS and $FFFF)
  end;
end;

constructor TFileInfoDetails.Create();
begin
  inherited Create();
  p_buf := nil;
  p_fi := nil;
end;

destructor TFileInfoDetails.Destroy;
begin
  FreeBuf();
  inherited Destroy();
end;

function TFileInfoDetails.GetFileInfoDetails(const sfile: string): boolean;
var s_lang: string; p_str: pointer;
begin
  b_valid := false;
  FreeBuf();
  c_len := GetFileVersionInfoSize(PChar(sfile), c_handle);
  if c_len > 0 then begin
    GetMem(p_buf, c_len);
    if GetFileVersionInfo(PChar(sfile), 0, c_len, p_buf) then begin
      b_valid := ( VerQueryValue(p_buf, '\', Pointer(p_fi), c_len) and
                  VerQueryValue(p_buf, '\VarFileInfo\Translation\', p_str, c_len));
      if b_valid then begin
        s_lang := Format('%.4x%.4x', [PLandCodepage(p_str)^.wLanguage, PLandCodepage(p_str)^.wCodePage]);
        s_fipath := '\StringFileInfo\' + s_lang + '\';
      end;
    end;
  end;
  result := b_valid;
end;

end.
