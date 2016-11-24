// =============================================================================
// Module name  : $RCSfile: DllLoader.pas,v $
// Description  : This unit defines a class to load a dll-file under Windows.
//                Three functions(LoadLibrary, FreeLibrary, GetProcAddress and
//                SetDllDirectory)are encapsulted in this unit.
// Compiler     : Delphi 2007, XE7
// Author       : 2016-11-24 /bsu/
// History      :
unit DllLoader;

interface
uses Windows, Classes;
type
  IDLLLoader = interface
    function LoadDLL(const dllfile: string): boolean;
    function GetCurDllFile(): string;
    function GetFunction(const funcname: string): pointer;
    procedure SetDllSearchPath(const spath: string);
    procedure UnloadDLL();
    property CurDllName: string read GetCurDllFile;
    property DllSearchPath: string write SetDllSearchPath;
  end;

  TDLLLoader = class(TInterfacedObject, IDLLLoader)
  protected
    h_dll:      THandle;
    s_dllfile:  string;
    s_path:     string;
  public
    constructor Create();
    destructor Destroy(); override;

    function LoadDLL(const dllfile: string): boolean;
    function GetCurDllFile(): string;
    function GetFunction(const funcname: string): pointer;
    procedure SetDllSearchPath(const spath: string);
    procedure UnloadDLL();

    property CurDllName: string read GetCurDllFile;
    property DllSearchPath: string write SetDllSearchPath;
  end;

implementation
uses SysUtils;

constructor TDLLLoader.Create();
begin
  inherited Create();
  h_dll := 0;
  s_dllfile := '';
  s_path := '';
end;

destructor TDLLLoader.Destroy();
begin
  UnloadDll();
  inherited Destroy();
end;

function TDLLLoader.LoadDLL(const dllfile: string): boolean;
begin
  UnloadDLL();
  if (s_path <> '') then SetDllDirectory(PChar(s_path)); //set high priority for this path by searching dll
  h_dll := loadLibrary(PChar(dllfile));
  SetDllDirectory(nil); //reset the algorithmus of dll searching (Default)

  result := (h_dll <> 0);
  if result then s_dllfile := dllfile;
end;

function TDLLLoader.GetCurDllFile(): string;
begin
  result := s_dllfile;
end;

function TDLLLoader.GetFunction(const funcname: string): pointer;
begin
  if (h_dll <> 0) then
    result := GetProcAddress(h_dll, PChar(funcname))
  else
    result := nil;
end;

procedure TDLLLoader.SetDllSearchPath(const spath: string);
begin
  if DirectoryExists(spath) then s_path := spath;
end;

procedure TDLLLoader.UnloadDLL();
begin
  if (h_dll <> 0) then begin
    if FreeLibrary(h_dll) then begin
      h_dll := 0;
      s_dllfile := '';
    end;
  end;
end;

end.
