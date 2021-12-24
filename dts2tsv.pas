program dts2tsv;
{$mode objfpc}
uses
  Classes, SysUtils;

type
  TMode = (name,
           value);

const
  DepthDelimiter : char = ':';
  CellDelimiter : char = chr(9);

var
  Count : Longint;
  InputFile : ansistring;
  OutputFile: ansistring;
  CanWriteOutputFile: boolean;
  OutputArray: array of array of shortstring;
  x,y,ax,ay: longword; //current size of output array
  UserInput: ansistring;

Procedure ProcessInputFile(InputFile: ansistring; FileNumber: LongInt);
var
  AMS: TMemoryStream;
  A,B: byte;
  L: longword;
  AMode: TMode;
  Depth: byte;
  AName: array of shortstring;
  AValue: shortstring;
  TempStr: shortstring;
  CopySpaces: boolean;
begin
  setlength(AName,0);
  Depth:=0;
  AValue:='';
  AMS:=TMemoryStream.Create;
  try
    AMS.LoadFromFile(InputFile);
    //put filename in zero'th row of the correct column
    if AMS.Size>0 then OutputArray[FileNumber,0]:=InputFile;
    AMS.Position:=0;
    AMode:=Name;
    while AMS.Position < AMS.Size do
    begin
      B:=AMS.ReadByte; //also increments AMS.Position
      case AMode of
        Name: begin
                case B of
                  // {
                  123   : begin
                            inc(depth);
                            if depth>length(AName) then
                            begin
                              setlength(AName,depth);
                              AName[depth-1]:='';
                            end;
                            //writeln('depth increased to '+inttostr(depth));
                          end;
                  61    : begin
                            AMode:=Value; // =
                            CopySpaces:=false;
                          end;
                  {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
                                  #(35) *(42) +(43) ,(44) -(45) .(46)  ?(63) @(64) _(95) }
                  35,42..46,48..57,63..90,95,97..122:
                          begin
                            if depth>0 then  AName[depth-1]:=AName[depth-1]+chr(B);
                          end;
                  // }
                  125   : begin
                            AName[depth-1]:='';
                            dec(depth);
                            setlength(AName,depth);
                            if depth>0 then AName[depth-1]:='';
                            //writeln('depth decreased to '+inttostr(depth));
                          end;
                end;
              end;
        Value:begin
                case B of
                  59    : begin    // ; char means name and value done
                            //now we have a complete name and value
                            TempStr:='';
                            for A:=0 to length(AName)-1 do
                            begin
                              if A<>0 then TempStr:=TempStr+DepthDelimiter;
                              TempStr:=TempStr+AName[A];
                            end;
                            ay:=0; //ay is which row we are going to use
                            case y of
                              0: writeln('oops');
                              1: begin //if there are no names then add it
                                   y:=2;
                                   Setlength(OutputArray,x,y);
                                   ay:=1;
                                   OutputArray[0,ay]:=TempStr;
                                   //writeln(inttostr(x)+','+inttostr(y)+':0,'+inttostr(ay)+':'+tempstr);
                                 end;
                              else
                                 //search in name column
                                 begin
                                   L:=1;
                                   while (ay=0) AND (L<y) do
                                   begin
                                     if CompareStr(tempstr,OutputArray[0,L])=0 then ay:=L;
                                     inc(L);
                                   end;
                                   if ay=0 then //no match, make longer and add to last
                                   begin
                                     ay:=y;
                                     inc(y);
                                     Setlength(OutputArray,x,y);
                                     OutputArray[0,ay]:=TempStr;
                                     //writeln(inttostr(x)+','+inttostr(y)+':0,'+inttostr(ay)+':'+tempstr);
                                   end;
                                 end;
                            end;
                            OutputArray[FileNumber,ay]:=AValue;
                            //writeln(inttostr(x)+','+inttostr(y)+':'+inttostr(FileNumber)+','+inttostr(ay)+':'+AValue);
                            AValue:='';
                            AName[depth-1]:='';
                            AMode:=Name;
                          end;
                  //spcae char
                  32:     if CopySpaces AND (depth>0) then AValue:=AValue+chr(B);
                  {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
                                  "(34) #(35) *(42) +(43) ,(44) -(45) .(46) /(47) <(60) >(62) ?(63) @(64)\(92) _(95) }
                  34..35,42..57,60,62..90,92,95,97..122:
                          begin
                            if depth>0 then AValue:=AValue+chr(B);
                            if ((CopySpaces=false) AND ((B=34) OR (B=60))) then CopySpaces:=true;
                            if ((CopySpaces=true) AND ((B=34) OR (B=62))) then CopySpaces:=false;
                          end;
                end;
              end;
      end;
      //writeln(inttostr(AMS.Position)+','+inttostr(AMS.Size)+':done');
    end;
  except
    on E:Exception do
      writeln('File ', InputFile, ' could not be read because: ', E.Message);
  end;
  AMS.Free;
end;

procedure ProcessOutputFile(OutputFile: ansistring);
var
  AMS: TMemoryStream;
  b: byte;
begin
  AMS:=TMemoryStream.Create;
  for ay:=0 to y-1 do
  begin
    for ax:=0 to x-1 do
    begin
      if ax > 0 then AMS.WriteByte(ord(CellDelimiter));
      for b:=1 to length(OutputArray[ax,ay]) do AMS.WriteByte(ord(OutputArray[ax,ay][b]));
    end;
    AMS.WriteByte(13);
    AMS.WriteByte(10);
  end;
  try
    AMS.SaveToFile(OutputFile);
  except
    on E:Exception do
      writeln('File ', OutputFile, ' could not be written because: ', E.Message);
  end;
  AMS.Free;
end;

begin
  if ParamCount>1 then
  begin
    OutputFile:=ParamStr(ParamCount);
    if FileExists(OutputFile) then
    begin
      CanWriteOutputFile:=false;
      Writeln(OutputFile+' exists. Delete (Y/N)?');
      ReadLn(UserInput);
      if length(UserInput)>0 then
      begin
        if (UserInput[1]='y') OR (UserInput[1]='Y') then
        begin
          if DeleteFile(OutputFile) then
            CanWriteOutputFile:=true
            else
            WriteLn('Could not delete '+OutputFile);
        end;
      end;
    end
    else CanWriteOutputFile:=true;

    if CanWriteOutputFile then
    begin
      x:=ParamCount; //first column is name, after that one column per file
      y:=1;
      Setlength(OutputArray,x,y);
      for Count:=1 to ParamCount-1 do
      //process input dts files
      begin
        InputFile := ParamStr(Count);
        Writeln('Input file:'+InputFile);
        ProcessInputFile(InputFile,Count);
      end;
      Writeln('Output file:'+OutputFile);
      ProcessOutputFile(OutputFile);
      Setlength(OutputArray,0,0);
    end;
  end
  else Writeln('Usage: ./dts2tsv [dtb files] [tsv file]  eg ./dtstsv *.dts out.tsv');
end.
