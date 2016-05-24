set Haozip="D:\2345Soft\HaoZip\HaoZipC.EXE"
%Haozip% a -t7z QWorker3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qworker.files
%Haozip% a -t7z QAES3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qaes.files
%Haozip% a -t7z QDigest3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qdigest.files
%Haozip% a -t7z QJson3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qjson.files
%Haozip% a -t7z QLog3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qlog.files
%Haozip% a -t7z QMsgPack3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qmsgpack.files
%Haozip% a -t7z QXML3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qxml.files
%Haozip% a -t7z QMacros3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qmacros.files
%Haozip% a -t7z QDB3.0_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qdb.files
%Haozip% a -t7z -r QDAC3.0WithDemo_%date:~0,4%%date:~5,2%%date:~8,2%.7z @qdac.files
