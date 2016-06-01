unit ProductManager;

interface
uses Classes, PairStrings;
type

  TConfigTree=class
  protected
    t_mysettings: TStringList;
    t_children:   TConfigTree;
  public
    constructor Create();
    destructor Destroy(); override;

    function LoadFromFile(const sfile: string): integer;
    function AddConfig(const secparent, secself: string; conf: TPairStrings): boolean;
    function FindConfig(const secname: string): TPairStrings;

  end;
implementation

constructor TConfigTree.Create();
begin
  t_mysettings := TStringList.Create();
end;

destructor TConfigTree.Destroy();
var i: integer;
begin
  if assigned(t_children) then  t_children.Free();
  for i := 0 to t_mysettings.Count - 1 do if assigned(t_mysettings.Objects[i]) then t_mysettings.Objects[i].Free();
  t_mysettings.Free();
end;

function TConfigTree.LoadFromFile(const sfile: string): integer;
begin
  result := 0;
end;

end.
