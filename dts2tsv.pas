program dts2tsv;
{$mode objfpc}
uses
  Classes, SysUtils, StrUtils;

type
  TMode = (name,
           value,
           Slash,
           SComment,
           MComment,
           Star);

  TValueReplace = Record
    startindex: word; //where in the string the value starts
    endindex:   word; //where in the string the value ends
    replace:    boolean; //whether the value is to be replaced
    newtext:    ansistring; //what to replace it with
  end;

  TNode = Record
    name: shortstring;
    phandle: shortstring;
  end;

  TCommandSwitch = (none,source,decomp,output);

  TDTSArray = array of array of ansistring;
  PDTSArray = ^TDTSArray;

const
  DepthDelimiter : char = '{';
  UnDepthDelimiter : char = '}';
  CellDelimiter : char = chr(9);
  debug : boolean = false;

var
  Count : Longint;
  SourceFiles: array of ansistring;
  DecompFiles: array of ansistring;
  OutputFile: ansistring;
  OutputArray: array of array of shortstring;
  phandleArray: array of TNode; //array of nodes and their phandle
  phandleArrayOkay: boolean;
  x,y: longword; //current size of output array
  ax,ay: longword;
  DTSArray: TDTSArray; //temp array for processing a single DTS file
  PValuesArray :TDTSArray; //array of index of values that are pHandles (instead of u32)
  LabelArray: TDTSArray;
  ACommandSwitch: TCommandSwitch;

Procedure AddLabelToLabelArray(ALabel,ANodeName: AnsiString);
var
  A: longint;
  NotFound: boolean;
begin
  //ignore 1:1 mappings for now (not sure about this)
  if comparestr(ALabel,ANodeName)<>0 then
  begin
    if length(LabelArray[0])=0 then
    begin
      setlength(LabelArray,2,1);
      LabelArray[0][0]:=ALabel;
      LabelArray[1][0]:=ANodename;
    end
    else
    begin
      A:=0;
      NotFound:=true;
      while NotFound AND (A<length(LabelArray[1]))do
      begin
        if comparestr(ANodeName,LabelArray[1][A])=0 then NotFound:=false;
        inc(A);
      end;
      if NotFound then
      begin
        setlength(LabelArray,2,A+1);
        LabelArray[0][A]:=ALabel;
        LabelArray[1][A]:=ANodename;
      end;
    end;
  end;
end;

Function DeLabel(AString: AnsiString):AnsiString;
var
  A,B: word;
  ALabel: ansistring;
begin
  Result:='';
  ALabel:='';
  if length(AString)>0 then
  begin
    A:=length(AString)+1;
    repeat
      dec(A);
    until (AString[A]=':') OR (A=1);
    for B:=A to length(AString) do
      if AString[B]<>':' then Result:=Result+AString[B];
    //dump out labels for later nodename to label substitution
    if (AString[A]=':') AND (A>1) then
    begin
      for B:=1 to A-1 do ALabel:=ALabel+AString[B];
      AddLabelToLabelArray(ALabel,Result);
    end;
  end;
end;

Procedure AddNodeTophandleArray(AName, Aphandle: shortstring);
var
  L: longword;
begin
  if length(phandleArray)=0 then
  begin
    setlength(phandleArray,1);
    phandleArray[0].name:=AName;
    phandleArray[0].phandle:=Aphandle;
  end
  else
  begin
    if phandleArrayOkay then
    //check if this node of phandle is already in the table
    begin
      for L:=0 to length(phandleArray)-1 do
      begin
        if CompareStr(phandleArray[L].name,AName)=0 then
        begin
          phandleArrayOkay:=false;
          writeln('phandle table error: Node already exists');
        end;
        if CompareStr(phandleArray[L].phandle,Aphandle)=0 then
        begin
          phandleArrayOkay:=false;
          writeln('phandle table error: phandle already exists');
        end;
      end;
    end;
    if phandleArrayOkay then
    begin
      setlength(phandleArray,length(phandleArray)+1);
      phandleArray[length(phandleArray)-1].name:=AName;
      phandleArray[length(phandleArray)-1].phandle:=Aphandle;
    end;
  end;
end;

Function DTSFile2Array(InputFile: ansistring; ADTSArray: PDTSArray):boolean;
var
  AMS: TMemoryStream;
  AMode: TMode;
  OldMode: TMode;
  A,B: byte;
  Depth: byte;
  AName: array of ansistring;
  AValue: ansistring;
  CopySpaces: boolean;
  TempStr: ansistring;
  DataStarted: boolean;
  ValueIsphandle: boolean;
  phandle: shortstring;
begin
  Result:=false;
  DataStarted:=false;
  AMS:=TMemoryStream.Create;
  try
    AMS.LoadFromFile(InputFile);
  except
    on E:Exception do
      writeln('File '+InputFile+' could not be read because: ', E.Message);
  end;
  if AMS.Size>0 then
  begin;
    Depth:=1;
    setlength(AName,1);
    setlength(phandleArray,0);
    phandleArrayOkay:=true;
    AName[0]:='';
    setlength(ADTSArray^,2,1);
    ADTSArray^[0][0]:='';
    ADTSArray^[1][0]:=InputFile;
    Result:=true;
    AMode:=Name;
    AMS.Position:=0;
    while AMS.Position < AMS.Size do
    begin
      A:=AMS.ReadByte; //also increments AMS.Position
      case AMode of
        Name:  begin
          case A of
            // {
            123   : begin
                      DataStarted:=true;
                      inc(depth);
                      if depth>length(AName) then
                      begin
                        setlength(AName,depth);
                        AName[depth-1]:='';
                        phandle:='';
                        ValueIsphandle:=false;
                      end;
                    end;
            // =
            61    : begin
                      if AName[depth-1] = 'phandle' then
                      begin
                        ValueIsphandle:=true;
                        phandle:='';
                      end;
                      AMode:=Value;
                      CopySpaces:=false;
                    end;
            {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
            #(35) &(38) *(42) +(43) ,(44) -(45) .(46) :(58) ?(63) @(64) _(95) }
            35,38,42..46,48..58,63..90,95,97..122:
                    begin
                      if DataStarted then  AName[depth-1]:=AName[depth-1]+chr(A);
                    end;
            // }  end of node, start a new one
            125   : begin
                      AName[depth-1]:='';
                      dec(depth);
                      setlength(AName,depth);
                      if DataStarted AND (depth>0) then
                      begin
                        AName[depth-1]:='';
                        phandle:='';
                        ValueIsphandle:=false;
                      end;
                    end;
            // /
            47    : begin
                      OldMode:=Name;
                      AMode:=Slash;
                    end;
            59    : begin   // ; char means name and value done
                            //now we have a complete name and value
                            //add to output array
                            //only happens here if there is a node without a value
                      //check if there is anything here at all
                      TempStr:='';
                      if length(AName)>0 then
                        for B:=0 to length(AName)-1 do TempStr:=TempStr+AName[B];
                      if length(Tempstr)>0 then
                      begin
                        TempStr:='';
                        for B:=0 to length(AName)-1 do
                        begin
                          if B<>0 then TempStr:=TempStr+DepthDelimiter;
                          TempStr:=TempStr+DeLabel(AName[B]);
                        end;
                        for B:=1 to length(AName)-1 do
                        begin
                          TempStr:=TempStr+UnDepthDelimiter;
                        end;
                        setlength(ADTSArray^[0],length(ADTSArray^[0])+1);
                        setlength(ADTSArray^[1],length(ADTSArray^[0]));
                        ADTSArray^[0][length(ADTSArray^[0])-1]:=TempStr;
                        ADTSArray^[1][length(ADTSArray^[1])-1]:=AValue;
                        //add to phandle table
                        if depth>2 then
                        begin
                          if phandleArrayOkay AND (length(phandle)>0) then
                          begin
                            TempStr:='@';
                            for B:=0 to length(AName)-2 do
                            begin
                              if B<>0 then TempStr:=TempStr+DepthDelimiter;
                              TempStr:=TempStr+AName[B];
                            end;
                            for B:=1 to length(AName)-1 do
                            begin
                              TempStr:=TempStr+UnDepthDelimiter;
                            end;
                            AddNodeTophandleArray(tempstr,phandle);
                          end;
                        end;
                        //clean up
                        AValue:='';
                        AName[depth-1]:='';
                      end;
                    end;
          end;

        end;
        Value: begin
          case A of
            //spcae char
            32:     if CopySpaces AND DataStarted then AValue:=AValue+chr(A);
            {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
            "(34) #(35) &(38) *(42) +(43) ,(44) -(45) .(46) <(60) >(62) ?(63) @(64) [(91) ](93) \(92) _(95) }
            34..35,38,42..46,48..57,60,62..93,95,97..122:
                    begin
                      if DataStarted then
                      begin
                        AValue:=AValue+chr(A);
                        if ValueIsphandle AND (depth>1) then
                        begin
                          phandle:=phandle+chr(A);
                        end;
                      end;
                      if ((CopySpaces=false) AND ((A=34) OR (A=60) OR (A=91))) then CopySpaces:=true;
                      if ((CopySpaces=true) AND ((A=34) OR (A=62) OR (A=93))) then CopySpaces:=false;
                    end;
            // /
            47    : begin
                      OldMode:=Value;
                      AMode:=Slash;
                    end;
            59    : begin   // ; char means name and value done
                            //now we have a complete name and value
                            //add to output array
                      TempStr:='';
                      for B:=0 to length(AName)-1 do
                      begin
                        if B<>0 then TempStr:=TempStr+DepthDelimiter;
                        TempStr:=TempStr+DeLabel(AName[B]);
                      end;
                      for B:=1 to length(AName)-1 do
                      begin
                        TempStr:=TempStr+UnDepthDelimiter;
                      end;
                      setlength(ADTSArray^[0],length(ADTSArray^[0])+1);
                      setlength(ADTSArray^[1],length(ADTSArray^[0]));
                      ADTSArray^[0][length(ADTSArray^[0])-1]:=TempStr;
                      ADTSArray^[1][length(ADTSArray^[1])-1]:=AValue;
                      //add to phandle table
                      if depth>2 then
                      begin
                        if phandleArrayOkay AND (length(phandle)>0) then
                        begin
                          TempStr:='@';
                          for B:=0 to length(AName)-2 do
                          begin
                            if B<>0 then TempStr:=TempStr+DepthDelimiter;
                            TempStr:=TempStr+AName[B];
                          end;
                          for B:=1 to length(AName)-1 do
                          begin
                            TempStr:=TempStr+UnDepthDelimiter;
                          end;
                          AddNodeTophandleArray(tempstr,phandle);
                        end;
                      end;
                      //clean up
                      AValue:='';
                      AName[depth-1]:='';
                      //go back to loading a node name
                      AMode:=Name;
                    end;
         end;
        end;
        Slash: begin
          case A of
            // *
            42: AMode:=MComment;
            // second/
            47: AMode:=SComment;
            else begin
                   case OldMode of
                     Name: if DataStarted then  AName[depth-1]:=AName[depth-1]+'/'+chr(A);
                     Value: if DataStarted then
                            begin
                              AValue:=AValue+'/';
                              if (A=32) then
                              begin
                                if CopySpaces then AValue:=AValue+chr(A);
                              end
                              else AValue:=AValue+chr(A);
                            end;
                   end;
                   AMode:=OldMode;
                 end;
          end;
        end;
        MComment: begin
          case A of
            42: AMode:=Star;
          end;
        end;
        SComment: begin
          case A of
            10: AMode:=OldMode;
          end;
        end;
        Star: begin
          case A of
            47: AMode:=OldMode;
            else AMode:=MComment;
          end;
        end;
      end;
    end;
  end;

  AMS.Free;
end;

Function OutPutFileOkay(aFile:ansistring):boolean;
var
  UserInput: ansistring;
begin
  Result:=false;
  if length(aFile)>0 then
  begin
    if FileExists(aFile) then
    begin
      Writeln(aFile+' exists. Delete (Y/N)?');
      ReadLn(UserInput);
      if length(UserInput)>0 then
      begin
        if (UserInput[1]='y') OR (UserInput[1]='Y') then
        begin
          if DeleteFile(aFile) then
            Result:=true
            else
            WriteLn('Could not delete '+aFile);
        end;
      end;
    end
    else Result:=true;
  end
  else writeln('No output file specified')
end;

Function ValidChar(aChar: char):boolean;
begin
  case ord(aChar) of
    {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
     #(35) &(38) ((40) )(41) *(42) +(43)  -(45)  ?(63) @(64)  \(92) _(95) }
    35,38,40..43,45,48..57,63..90,92,95,97..122: result:=true;
    else result:=false;
  end;
end;

Function FindHandlesIn(aString: ansistring):ansistring;
{<value1> <value2>
 <value1 value2>
 [value1] [value2]
}
var
  A,B: longword;
  ValueIsText: boolean;
  FirstResult: boolean;
begin
  result:='';
  if length(aString)>0 then
  begin
    B:=0;
    ValueIsText:=false;
    FirstResult:=true;
    for A:=1 to length(aString)-1 do
    begin
      if NOT ValueIsText then
      begin
        if aString[A]='"' then
        begin
          result:='';
          ValueIsText:=true;
        end;
        if ((aString[A]='<') OR (aString[A]='[') OR (aString[A]=' ')) AND ValidChar(aString[A+1]) then
        begin
          inc(B);
          if aString[A+1]='&' then
          begin
            if FirstResult then
            begin
              result:=result+'<'+inttostr(B)+'>';
              FirstResult:=false;
            end
            else result:=result+','+'<'+inttostr(B)+'>';
          end;
        end;
      end;
    end;
  end;
end;

Procedure LoadHandles(aFile: ansistring);
var
  ADTSArray: PDTSArray;
  A: longint;
  TempStr: AnsiString;
begin
  //setlength(PValuesArray,2,0);    mistake, resets with every file, move to program start
  setlength(DTSArray,0,0);
  ADTSArray:=@DTSArray;
  writeln('Processing source dts: '+aFile);
  //DTSFile2Array adds the filename
  if DTSFile2Array(aFile, ADTSArray) then
  begin
    if length(DTSArray)=2 then
    begin
      if (length(DTSArray[0])=length(DTSArray[1])) AND (length(DTSArray[0])>1) then
      begin
        for A:=0 to length(DTSArray[0])-1 do
        begin
          TempStr:=FindHandlesIn(DTSArray[1][A]);
          if length(TempStr)>0 then
          begin
            SetLength(PValuesArray[0],Length(PValuesArray[0])+1);
            SetLength(PValuesArray[1],Length(PValuesArray[0]));
            PValuesArray[0][Length(PValuesArray[0])-1]:=DTSArray[0][A];
            PValuesArray[1][Length(PValuesArray[0])-1]:=TempStr;
          end;
        end;
      end;
    end;
  end;
end;

{for each value
1. look in PValuesArray, see if a value is pHandle
2. see if the value number appears in phandlearray
3. if so substitute the number for the phandle
4. put in outputarray}
Function Value2pHandle (aValue,bValue: ansistring): ansistring;
var
  valuelist: array of TValueReplace; //valuestart[0] is index to first char of a value
  //valueend: array of word;   //valueend[0] is index to first char of a value
  A,B: word;
  TempStr: ansistring;
begin
  Result:='';
  setlength(valuelist,0);
  //setlength(valueend,0);
  A:=0;
  //find start and end of each value
  for B:=2 to length(aValue)-1 do
  begin
    if ((aValue[B-1]='<') OR (aValue[B-1]=' ')) AND ValidChar(aValue[B]) then
    begin
      inc(A);
      setlength(valuelist,A);
      //setlength(valueend,A);
      valuelist[A-1].startindex:=B;
      valuelist[A-1].endindex:=0;
      valuelist[A-1].replace:=false;
      valuelist[A-1].newtext:='';
    end;
    if ((aValue[B+1]='>') OR (aValue[B+1]=' ')) AND ValidChar(aValue[B]) then
    begin
      //valueend[A-1]:=B;
      valuelist[A-1].endindex:=B;
    end;
  end;
  if length(valuelist)>0 then
  begin
    //find if value is a phandle in bvalue
    for A:=0 to length(valuelist)-1 do
    begin
      if valuelist[A].endindex>valuelist[A].startindex then
      begin
        if AnsiContainsStr(BValue,'<'+IntToStr(A+1)+'>') then
        begin
          if length(pHandleArray)>0 then
          begin
            Tempstr:='<';
            for B:=valuelist[A].startindex to valuelist[A].endindex do TempStr:=TempStr+AValue[B];
            TempStr:=TempStr+'>';
            for B:=0 to length(pHandleArray)-1 do
            begin
              if pHandleArray[B].phandle=TempStr then
              //got a phandle match!
              begin
                valuelist[A].replace:=true;
                valuelist[A].newtext:=pHandleArray[B].name;
              end;
            end;
          end;
        end;
      end
      else writeln('Error: Value search fail with '+AValue);
    end;
    //if replace flag is set search for phandle and replace with text
    Result:='';
    for A:=0 to length(valuelist)-1 do
    begin
      if valuelist[A].replace then
      begin
        Result:=Result+'<'+valuelist[A].newtext+'>';
      end
      else
      begin
        Result:=Result+'<';
        for B:=valuelist[A].startindex to valuelist[A].endindex do Result:=Result+AValue[B];
        Result:=Result+'>';
      end;
    end;
  end
  else writeln('Error: No values found in '+AValue);
end;

Procedure ProcessDTBSFile(aFile: ansistring; FileNumber: LongInt);
var
  ADTSArray: PDTSArray;
  A,B,C,D,E: longword;
  PValueMatchFound: boolean;
  LabelSearch: ansistring;
  TempStr: ansistring;
begin
  setlength(DTSArray,0,0);
  //new DTSArray starts here, per input file
  ADTSArray:=@DTSArray;
  writeln('Processing decompiled dtb: '+aFile);
  //produces DTSArray & pHandleArray
  //DTSFile2Array also adds the file name in row 0
  if DTSFile2Array(aFile, ADTSArray) then
  begin
    if length(DTSArray)=2 then
    begin
      if (length(DTSArray[0])=length(DTSArray[1])) AND (length(DTSArray[0])>1) then
      begin
        {for each value
        1. look in PValuesArray, see if a value is pHandle
        2. see if the value number appears in phandlearray
        3. if so substitute the number for the phandle
        4. put in outputarray}

        //writeln('length(DTSArray[0]):'+inttostr(length(DTSArray[0]))+' length(PValuesArray[0])'+inttostr(length(PValuesArray[0])));
        for A:=0 to length(DTSArray[0])-1 do  //row 0 is file name
        begin
          PValueMatchFound:=false;
          if A=0 then
          begin
            OutputArray[FileNumber+1][0]:=aFile;
          end
          else
          begin
            if length(PValuesArray[0])>0 then
            begin
              for B:=0 to length(PValuesArray[0])-1 do
              begin
                if NOT PValueMatchFound then
                begin
                  if comparestr(DTSArray[0][A],PValuesArray[0][B])=0 then //writeln(DTSArray[0][A]+' matches '+PValuesArray[0][B]);
                  begin
                    PValueMatchFound:=true;
                    if length(DTSArray[1][A])>0 then
                    begin
                      if DTSArray[1][A][1]='<' then DTSArray[1][A]:=Value2pHandle(DTSArray[1][A],PValuesArray[1][B]);
                      //writeln(DTSArray[1][A]);
                    end;
                  end;
                end;
              end;
              //look in labelarray
              if NOT PValueMatchFound then
              begin
                if length(labelarray[0])>0 then
                begin
                  for B:=0 to length(labelarray[0])-1 do
                  begin
                    if ANSIcontainsstr(DTSArray[0][A],LabelArray[1][B]) then
                    //writeln(DTSArray[0][A]+' contains '+LabelArray[1][B]+' maps to '+LabelArray[0][B]);
                    begin
                      E:=0;
                      LabelSearch:='';
                      for C:=length(DTSArray[0][A]) downto 1 do
                      begin
                        if E=0 then
                        begin
                          LabelSearch:='';
                          for D:=C to length(DTSArray[0][A]) do
                          begin
                            LabelSearch:=LabelSearch+DTSArray[0][A][D];
                          end;
                          if ANSIcontainsstr(LabelSearch,LabelArray[1][B]) then
                          begin
                            E:=C; //index in DTSArray[0][A] where LabelArray entry starts
                          end;
                        end;
                      end;
                      if E>0 then
                      begin
                        E:=E+length(LabelArray[1][B]); //index in DTSArray[0][A] where first char after LabelArray entry
                        LabelSearch:='&'+LabelArray[0][B]; //switch nodename for label
                        if E<=length(DTSArray[0][A]) then
                        begin
                          for C:=E to length(DTSArray[0][A]) do
                            LabelSearch:=LabelSearch+DTSArray[0][A][C];

                          for C:=0 to length(PValuesArray[0])-1 do
                          begin
                            if NOT PValueMatchFound then
                            begin
                              //if comparestr(LabelSearch,PValuesArray[0][C])=0 then  //doesn't work because extra }} on end
                              if ansicontainsstr(LabelSearch,PValuesArray[0][C]) then
                              begin
                                LabelSearch:=copy(LabelSearch,1,length(PValuesArray[0][C]));
                                if comparestr(LabelSearch,PValuesArray[0][C])=0 then

                                begin
                                  PValueMatchFound:=true;
                                  if length(DTSArray[1][A])>0 then
                                  begin
                                    if DTSArray[1][A][1]='<' then DTSArray[1][A]:=Value2pHandle(DTSArray[1][A],PValuesArray[1][C]);

                                  end;
                                end;

                              end;
                            end;
                          end;
                        end
                        else writeln('oops');
                      end;
                    end;
                  end;
                end;
              end;
            end;
            //values that can be swapped for phandles has now been done
            //load each line into output file
            //filenumber=0 for first file
            ay:=0; //ay is which row we are going to use
            case y of
              0: writeln('Error: Something wrong with outputarray');
              1: begin //if there are no names then add it
                   y:=2;
                   Setlength(OutputArray,x,y);
                   ay:=1;
                   OutputArray[0,ay]:=DTSArray[0][A];
                 end;
              else
                 //search in name column
                 begin
                   C:=1;
                   while (ay=0) AND (C<y) do
                   begin
                     if CompareStr(DTSArray[0][A],OutputArray[0,C])=0 then ay:=C;
                     inc(C);
                   end;
                   if ay=0 then //no match, make longer and add to last
                   begin
                     ay:=y;
                     inc(y);
                     Setlength(OutputArray,x,y);
                     OutputArray[0,ay]:=DTSArray[0][A];
                   end;
                 end;
            end;
            OutputArray[FileNumber+1,ay]:=DTSArray[1][A];



          end;
        end;
      end;
    end;
  end;
end;

Procedure OutputArray2OutPutFile(OutputFile: ansistring);
var
  AMS: TMemoryStream;
  A: word;
begin
  AMS:=TMemoryStream.Create;
  for ay:=0 to y-1 do
  begin
    for ax:=0 to x-1 do
    begin
      if ax > 0 then AMS.WriteByte(ord(CellDelimiter));
      for A:=1 to length(OutputArray[ax,ay]) do AMS.WriteByte(ord(OutputArray[ax,ay][A]));
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


//main program
begin
  if ParamCount>0 then
  begin
    //load in all the filenames
    setlength(SourceFiles,0);
    setlength(DecompFiles,0);
    setlength(PValuesArray,2,0);
    setlength(LabelArray,2,0);
    OutputFile:='';
    ACommandSwitch:=none;
    for count:=1 to ParamCount do
    begin
      case ParamStr(Count) of
        '-s','-S': ACommandSwitch:=source;
        '-d','-D': ACommandSwitch:=decomp;
        '-o','-O': ACommandSwitch:=output;
        else case ACommandSwitch of
               source: begin
                         setlength(SourceFiles,length(SourceFiles)+1);
                         SourceFiles[length(SourceFiles)-1]:=ParamStr(Count);
                       end;
               decomp: begin
                         setlength(DecompFiles,length(DecompFiles)+1);
                         DecompFiles[length(DecompFiles)-1]:=ParamStr(Count);
                       end;
               output: begin
                         OutPutFile:=ParamStr(Count);
                         ACommandSwitch:=none;
                       end;
             end;
      end;
    end;
    //process the files
    if OutputFileOkay(OutPutFile) then
    begin
      if length(SourceFiles)>0 then
      begin
        //create a big list of nodes that have values which are phandles
        //PValuesArray,2,n
        for Count:=0 to length(SourceFiles)-1 do LoadHandles(SourceFiles[Count]);
      end
      else
      begin
        writeln('No source files provided. phandle decoding will not be possible.');
      end;
      //setlength(OutputArray,0,0);
      x:=length(DecompFiles)+1; //first column is names, after that one per Decompfile
      y:=1;                     //first row is filename
      setlength(OutputArray,x,y);
      OutPutArray[0][0]:='';
      if length(DecompFiles)>0 then
      begin
        //Process the decompiled dtb files
        //Makes a DTSArray & pHandleArray
        //replaces Values with pHandles
        //sends result to outputfile
        for Count:=0 to length(DecompFiles)-1 do ProcessDTBSFile(DecompFiles[Count],Count);
        OutputArray2OutPutFile(OutputFile);
      end
      else
      begin
        writeln('No decompiled dtb files provided.');
        Writeln('Usage: ./dts2tsv -s [dts files] -d [decompiled dtb files] -o output file');
      end;
    end;
  end
  else
  //no input parameters
  begin
    Writeln('Usage: ./dts2tsv -s [dts files] -d [decompiled dtb files] -o output file');
  end;
end.
