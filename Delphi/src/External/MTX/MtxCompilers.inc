
// Tabelle / Compiler-Schalter gem‰ﬂ 
// http://docwiki.embarcadero.com/RADStudio/Seattle/de/Bedingte_Compilierung_%28Delphi%29#Vordefinierte_Bedingungen
// http://docwiki.embarcadero.com/RADStudio/Seattle/de/Compiler-Versionen


// |-------------------|------------------------------------------------|---------|----------|
// | Bedingte          | Produkt                                        | Produkt-| Package- |
// | Delphi-VER<nnn>   |                                                | Version | Version  |
// |-------------------|------------------------------------------------|---------|----------|
// | VER300            | Delphi Seattle / C++Builder Seattle            |    23   |    230   |
// | VER290            | Delphi XE8 / C++Builder XE8                    |    22   |    220   |
// | VER280            | Delphi XE7 / C++Builder XE7                    |    21   |    210   |
// | VER270            | Delphi XE6 / C++Builder XE6                    |    20   |    200   |
// | VER260            | Delphi XE5 / C++Builder XE5                    |    19   |    190   |
// | VER250            | Delphi XE4 / C++Builder XE4                    |    18   |    180   |
// | VER240            | Delphi XE3 / C++Builder XE3                    |    17   |    170   |
// | VER230            | Delphi XE2 / C++Builder XE2                    |    16   |    160   |
// |                   | 161 = FireMonkey-Packages beim XE2 Update 2    |    16   |    161   |
// | VER220            | Delphi XE / C++Builder XE (Win32)              |    15   |    150   |
// | VER210            | Delphi 2010 / C++Builder 2010 (Win32)          |    14   |    140   |
// | VER200            | Delphi 2009 / C++Builder 2009 (Win32)          |    12   |    120   |
// | VER190            | Delphi 2007 f¸r .Net *                         |    11   |    110   |
// | VER180 oder VER185| Delphi 2007 / C++Builder 2007 f¸r Win32 *      |    11   |    110   |
// | VER180            | Delphi 2006 / C++Builder 2006 (Win32/.Net) und |    10   |    100   |
// |                   | Delphi/C++Builder 2007 f¸r Win32               |         |          |
// | VER170            | Delphi 2005 (Win32/.Net)                       |     9   |    90    |
// | VER160            | Delphi 8 f¸r .Net                              |     8   |    80    |
// | VER150            | Delphi 7 (und 7.1)                             |     7   |    70    |
// | VER140            | Delphi 6 / C++Builder 6                        |     6   |    60    |
// | VER130            | Delphi 5 / C++Builder 5                        |     5   |    NV    |
// | VER125            | C++Builder 4                                   |     4   |    NV    |
// | VER120            | Delphi 4                                       |     4   |    NV    |
// | VER110            | C++Builder 3                                   |     3   |    NV    |
// | VER100            | Delphi 3                                       |     3   |    NV    |
// | VER93             | C++Builder 1                                   |    NV   |    NV    |
// | VER90             | Delphi 2                                       |     2   |    NV    |
// | VER80             | Delphi 1                                       |     1   |    NV    |
// | VER70             | Borland Pascal 7.0                             |    NV   |    NV    |
// | VER15             | Turbo Pascal f¸r Windows 1.5                   |    NV   |    NV    |
// | VER10             | Turbo Pascal f¸r Windows 1.0                   |    NV   |    NV    |
// | VER60             | Turbo Pascal 6.0                               |    NV   |    NV    |
// | VER55             | Turbo Pascal 5.5                               |    NV   |    NV    |
// | VER50             | Turbo Pascal 5.0                               |    NV   |    NV    |
// | VER40             | Turbo Pascal 4.0                               |    NV   |    NV    |
// |-------------------|------------------------------------------------|---------|----------|

{$IF   CompilerVersion >= 22}
  {$DEFINE DELPHI_XE_UP}
{$ELSEIF CompilerVersion >= 18}
  {$DEFINE DELPHI_2007_UP}
{$IFEND}


{$IFDEF DELPHI_XE_UP}
  {$DEFINE USE_STD_LIB_INSTEAD_OF_TNT_LIB}
{$ENDIF}  