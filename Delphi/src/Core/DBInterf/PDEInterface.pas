unit PDEInterface;

interface
uses Datenbank;
type
  TPdeWrapper = class
  protected
    t_infoblock:  t_db_infoblock;
    t_dbconn:     t_db_connection;
  public
    constructor Create();
    destructor Destroy(); override;
  End;

implementation

constructor TPDEWrapper.Create();
begin
  inherited Create();
  t_infoblock := t_db_infoblock.Create();
  t_dbconn := t_db_connection.Create();
end;

destructor TPDEWrapper.Destroy();
begin
  t_dbconn.Free();
  t_infoblock.Free();
end;

end.
