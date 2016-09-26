unit Oscilloscope;

interface
uses DeviceBase, TekUsb;

type
  IOscilloscope = interface
    //1. config
    //2. trigger
    //3. channel
    //4. data
  end;

  TOsciTektronix = class(TDeviceBase, IOscilloscope)
  protected
  
  end;

implementation

end.
