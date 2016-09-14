unit ConfigBase;

interface
uses Classes, StringPairs;

type
  EConfigFormat = (
                CF_UNKNOWN,
                CF_INI,
                CF_XML
                );

  IConfigSection = interface
    {function UpdateConfig(const tsettings: TStrings): boolean;
    function GetConfig(): TStringPairs;
    function GetConfigName(): string;
    property Config: TStringPairs read GetConfig; }
  end;

  TConfigSection = class(TInterfacedObject, IConfigSection)
  protected
    t_conf:     TStringPairs;
    s_confname: string;
  public
    constructor Create(const confname: string);
    destructor Destroy(); override;
  end;

  TConfigBase = class
  
  end;
implementation

constructor TConfigSection.Create(const confname: string);
begin
  inherited Create();
  t_conf := TStringPairs.Create();
  s_confname := confname;
end;

destructor TConfigSection.Destroy();
begin
  t_conf.Free();
  inherited Destroy();
end;

end.
