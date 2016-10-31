unit Oscilloscope;

interface
uses Classes, DeviceBase, TekUsb;

type
  IOscilloscope = interface
    //1. config
    //2. trigger
    //3. channel
    //4. data
  end;

  TOscilloscope = class(TDeviceBase, IOscilloscope)
    //1. config
    //2. trigger
    //3. channel
    //4. data
  end;

  TOsciTektronix = class(TOscilloscope, IOscilloscope)
  protected
  
  end;

implementation

end.
