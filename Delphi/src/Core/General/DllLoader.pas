unit DllLoader;

interface
uses Windows, Classes, TextMessage;
type
  IDLLLoader = interface
    function LoadDLL(const dllfile: string): boolean;
    function InitFunctions(const funcnames: TStrings; var pfuncs: array of Pointer): boolean;
    procedure SetSearchPath(const spath: string);
    procedure UnloadDLL();
  end;

  TDLLLoader = class(TInterfacedObject, IDLLLoader, ITextMessengerImpl)
  protected
    h_dll:      THandle;
    s_dllfile:  string;
    t_msgrimpl: TTextMessengerImpl;
  public
    constructor Create();
    destructor Destroy(); override;

    property MessengerService: TTextMessengerImpl read t_msgrimpl implements ITextMessengerImpl;

    function LoadDLL(const dllfile: string): boolean;
    function InitFunctions(const funcnames: TStrings; var pfuncs: array of Pointer): boolean;
    procedure SetSearchPath(const spath: string);
    procedure UnloadDLL();
  end;

implementation
constructor TDLLLoader.Create();
begin
  inherited Create();
  h_dll := 0;
  t_msgrimpl := TTextMessengerImpl.Create(ClassName());
end;

destructor TDLLLoader.Destroy();
begin
  t_msgrimpl.Free();
  inherited Destroy();
end;

function TDLLLoader.LoadDLL(const dllfile: string): boolean;
begin
  if (h_dll <> 0) then UnloadDLL();
  h_dll := loadLibrary(PChar(dllfile));
  result := (h_dll <> 0);
  if result then s_dllfile := dllfile;
end;

function TDLLLoader.InitFunctions(const funcnames: TStrings; var pfuncs: array of Pointer): boolean;
begin
  result := false;
  //todo:
end;

procedure TDLLLoader.SetSearchPath(const spath: string);
begin
  //todo:
end;

procedure TDLLLoader.UnloadDLL();
begin
  if FreeLibrary(h_dll) then begin
    h_dll := 0;
    s_dllfile := '';
  end;
end;

end.
