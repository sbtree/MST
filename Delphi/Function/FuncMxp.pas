// =============================================================================
// Module name  : $RCSfile: FuncDmm.pas,v $
// description  : The classes of script functions which are relevant to the
//                products of Metronix (Mxp), are implemented in this unit, e.g.
//                FW/BL-Download, EEPROM-Write etw.
// Compiler     : Delphi 2007
// Author       : 2015-12-18 /bsu/
// History      :
//==============================================================================
unit FuncMxp;

interface
uses Classes, FuncBase, TextMessage;
//1. Download
//   CheckVersion, DoDownload, BL, FW
//2. Query, e.g. OR:0123
//3. SendOnly
//4. RecvOnly
//5. EEPROM function, write service data
//6. Read error buffer
//7. Reset and wait
//8. Paratemeter
implementation

initialization
  //Classes.RegisterClass(YourSubclass);

finalization
  //Classes.UnregisterClass(YourSubclass);
end.
