unit DeviceConfig;

interface
uses Classes, SysUtils, StringPairs;

type
  TDeviceConfig = class
  protected
    s_fname:  string;
    t_fstemp: TDateTime;
    t_confs:  TStrings;
  protected

  public
    constructor Create();
    destructor Destroy(); override;

    function UpdateFromFile(const fname: string; const bforce: boolean = false): boolean;
    function GetConfigSection(const secname: string): TStringPairs;
    procedure ClearConfigs();
  end;

  TDeviceConfigurator = class
  protected
    t_devices: TStrings;
  public
    constructor Create();
    destructor Destroy(); override;

    function LoadDeviceSettings(const devname, fname: string): boolean;
    function GetDeviceConfig(const devname: string): TDeviceConfig;
    function GetDeviceConfigSection(const devname, secname: string): TStringPairs;
    procedure ClearDevices();
  end;

implementation
uses IniFiles;

constructor TDeviceConfig.Create();
begin
  inherited Create();
  t_confs := TStringList.Create();
end;

destructor TDeviceConfig.Destroy();
begin
  ClearConfigs();
  t_confs.Free();
  inherited Destroy();
end;

function TDeviceConfig.UpdateFromFile(const fname: string; const bforce: boolean): boolean;
var i: integer; t_ini: TIniFile; t_dtime: TDateTime;
    t_secnames, t_secvals: TStrings; t_conf: TStringPairs;
begin
  result := FileExists(fname);
  if result then begin
    FileAge(fname, t_dtime);
    if (bforce or (t_dtime <> t_fstemp) or (fname <> s_fname)) then begin
      t_ini := TIniFile.Create(fname);
      t_secnames := TStringList.Create();
      t_secvals := TStringList.Create();
      t_ini.ReadSections(t_secnames);
      for i := 0 to t_secnames.Count - 1 do begin
        t_conf := GetConfigSection(t_secnames[i]);
        if (not assigned(t_conf)) then t_conf := TStringPairs.Create();
        t_secvals.Clear();
        t_ini.ReadSectionValues(t_secnames[i], t_secvals);
        t_conf.AddPairs(t_secvals, true);
        t_confs.AddObject(t_secnames[i], t_conf);
      end;
      t_fstemp := t_dtime;
      s_fname := fname;
      t_secvals.Free();
      t_secnames.Free();
      t_ini.Free();
    end;
  end;
end;

function TDeviceConfig.GetConfigSection(const secname: string): TStringPairs;
var i_idx: integer;
begin
  result := nil;
  i_idx := t_confs.IndexOfName(secname);
  if (i_idx >= 0) then result := TStringPairs(t_confs.Objects[i_idx]);
end;

procedure TDeviceConfig.ClearConfigs();
begin
  while (t_confs.Count > 0) do
    if assigned(t_confs.Objects[0]) then begin
      t_confs.Objects[0].Free();
      t_confs.Delete(0);
    end;
end;

constructor TDeviceConfigurator.Create();
begin
  inherited Create();
  t_devices := TStringList.Create();
end;

destructor TDeviceConfigurator.Destroy();
begin
  ClearDevices();
  t_devices.Free();
  inherited Destroy();
end;

function TDeviceConfigurator.LoadDeviceSettings(const devname, fname: string): boolean;
var t_devconf: TDeviceConfig;
begin
  t_devconf := GetDeviceConfig(devname);
  if (not assigned(t_devconf)) then t_devconf := TDeviceConfig.Create();
  result := t_devconf.UpdateFromFile(fname);
end;

function TDeviceConfigurator.GetDeviceConfig(const devname: string): TDeviceConfig;
var i_idx: integer;
begin
  result := nil;
  i_idx := t_devices.IndexOfName(devname);
  if (i_idx >= 0) then result := TDeviceConfig(t_devices.Objects[i_idx]);
end;

function TDeviceConfigurator.GetDeviceConfigSection(const devname, secname: string): TStringPairs;
var t_devconf: TDeviceConfig;
begin
  result := nil;
  t_devconf := GetDeviceConfig(devname);
  if assigned(t_devconf) then result := t_devconf.GetConfigSection(secname);
end;

procedure TDeviceConfigurator.ClearDevices();
begin
  while (t_devices.Count > 0) do
    if assigned(t_devices.Objects[0]) then begin
      t_devices.Objects[0].Free();
      t_devices.Delete(0);
    end;
end;

end.
