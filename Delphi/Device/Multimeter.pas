unit Multimeter;

interface
uses DeviceBase;
<<<<<<< HEAD
type
  TMultimeter=class(TDeviceBase)
  
  end;

=======
{
type
  TMultimeter = class(TDeviceBase)
  public
    constructor Create;
    destructor Destroy; override;
  end;  }
>>>>>>> origin/master
implementation
{
constructor TMultimeter.Create;
begin
	inherited Create;
end;

destructor TMultimeter.Destroy;
begin
	inherited Destroy;
end;
}
end.
