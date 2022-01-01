program multidtc;
{$mode objfpc}
uses
  SysUtils, Process;

var
  Count : Longint;
  Message : ansistring;
  Output : ansistring;

begin
  if ParamCount>0 then
  begin
    for Count:=1 to ParamCount do
    begin
      Message := ParamStr(Count);
      if RunCommand('dtc',['-I','dtb','-O','dts','-o',ParamStr(Count)+'.dtb2s',ParamStr(Count)],Output) then
        Message := Message + ':' + 'Success'
      else
        Message := Message + ':' + 'Failed';
      Writeln(Message);
    end;
  end
  else Writeln('Usage: ./multidtc [dtb files]   eg ./multidtc *.dtb');
end.
