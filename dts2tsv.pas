program dts2tsv;
{$mode objfpc}
uses
 Classes, SysUtils, StrUtils;

type
  TCommandSwitch = (none,source,combined,decomp,output,defines,handles);
  PDataItem = ^TDataItem;
  TDataItem = record
    next: PDataItem;
    previous: PDataItem;
    data: TMemoryStream;
    position: Int64;
  end;

  TMode = (NodeName,
           NodeValue,
           NodeLabel,
           Slash,
           SComment,
           MComment,
           Star);

  TName = Record
    nodename: ansistring;
    nodelabel: ansistring;
  end;

  TNameArray = array of TName;

  PNameArray = ^TNameArray;

  TNode = Record
    namearray     :array of Tname;
    nodevalue: ansistring;
    phandle: ansistring;
    pvaluelist: ansistring;
  end;

  TDTSArray = array of TNode;

  PDTSArray = ^TDTSArray;

  TDefines = record
    Name: ansistring;
    Value: ansistring;
  end;

  TValueReplace = Record
    startindex: word; //where in the string the value starts
    endindex:   word; //where in the string the value ends
    replace:    boolean; //whether the value is to be replaced
    newtext:    ansistring; //what to replace it with
  end;

  PStringList = ^TStringList;

const
  DepthDelimiter : char = '{';
  UnDepthDelimiter : char = '}';
  CellDelimiter : char = chr(9);
  PathDelimiter : char = '/';

var
  ACommandSwitch: TCommandSwitch;
  DTSNodeArray: TDTSArray;
  DTSStringList: TStringList;
  DecompFiles: array of ansistring;
  OutputArray: array of array of ansistring;
  phandleArray: array of TNode; //array of nodes and their phandle
  phandleArrayOkay: boolean;
  Count1,Count2: LongInt;
  InputFile,OutputFile,DTSOutputfile: ansistring;
  PathToSourceFiles: ansistring;
  CurrentData: TDataItem;
  ProcessDefines: boolean;
  Showphandles: boolean;
  DefinesArray: array of TDefines;
  CurrentNodeLabel: ansistring;
  x,y: longword; //current size of output array
  ax,ay: longword;

function NameArrayToText(aNameArray: PNameArray):ansistring;
var
  A: longint;
begin
  Result:='';
  for A:=0 to length(aNameArray^)-1 do
  begin
    if A>0 then Result:=Result+DepthDelimiter;
    Result:=Result+aNameArray^[A].nodename;
  end;
  for A:=1 to length(aNameArray^)-1 do
  begin
    Result:=Result+UnDepthDelimiter;
  end;
end;

function CompareNameArray(NameArray1,NameArray2: PNameArray):boolean;
var
  A: longint;
begin
  Result:=false;
  if length(NameArray1^)=length(NameArray2^) then
  begin
    A:=0;
    repeat
      inc(A);
    until (NameArray1^[A-1].nodename<>NameArray2^[A-1].nodename) OR (A=length(NameArray1^));
    if A=length(NameArray1^) then
      if NameArray1^[A-1].nodename=NameArray2^[A-1].nodename then Result:=true;
  end;
end;

Procedure AddNodeTophandleArray(AName: PNameArray; Aphandle: shortstring);
var
  L: longword;
begin
  if length(phandleArray)=0 then
  begin
    setlength(phandleArray,1);
    setlength(phandleArray[0].namearray,length(AName^));
    for L:=0 to length(AName^)-1 do phandleArray[0].namearray[L]:=AName^[L];
    phandleArray[0].phandle:=Aphandle;
  end
  else
  begin
    if phandleArrayOkay then
    //check if this node of phandle is already in the table
    begin
      for L:=0 to length(phandleArray)-1 do
      begin
        if CompareNameArray(@phandleArray[0].namearray,AName) then
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
      setlength(phandleArray[length(phandleArray)-1].namearray,length(AName^));
      for L:=0 to length(AName^)-1 do phandleArray[length(phandleArray)-1].namearray[L]:=AName^[L];
      phandleArray[length(phandleArray)-1].phandle:=Aphandle;
    end;
  end;
end;


procedure LoadDefines(aFile: ansistring);
var
  AStringList: TStringList;
  A,B: longint;
  state: byte;
  HashPos, Field1start, Field1end, Field2start, Field2end, Bracket: longint;
  NameMatch: boolean;
begin
  AStringList:=TStringList.Create;
  AStringList.LoadFromFile(aFile);
  if AStringList.Count>0 then
  begin
    for A:=0 to AStringList.Count-1 do
    begin
      if ANSIContainsStr(AStringList[A],'#define') then
      begin
        state:=0;
        HashPos:=0;
        Field1start:=0;
        Field1end:=0;
        Field2start:=0;
        Field2end:=0;
        Bracket:=0;
        for B:=1 to length(AStringList[A]) do
        begin
            case state of
              //beginning
              0: if length(AStringList[A])>=(B+6) then
                 begin
                   if copy(AStringList[A],B,7)='#define' then
                   begin
                     HashPos:=B;
                     state:=1;
                   end;
                 end;
              //look for first char after #define
              1: if B>(HashPos+6) then
                 begin
                   if ord(AStringList[A][B])>32 then
                   begin
                     Field1start:=B;
                     state:=2;
                   end;
                 end;
               //look for end char of field1
               2: if ord(AStringList[A][B])<=32 then
                  begin
                    Field1end:=B-1;
                    state:=3;
                  end;
               //look for first char after field1
               3: if ord(AStringList[A][B])>32 then
                  begin
                    Field2start:=B;
                    if AStringList[A][B]='(' then inc(bracket);
                    if B=length(AStringList[A]) then
                    begin
                      Field2end:=B;
                      state:=5;
                    end
                    else state:=4;
                  end;
               //look for field2 end
               4: begin
                    if B=length(AStringList[A]) then
                    begin
                      Field2end:=B;
                      state:=5;
                    end
                    else
                    begin
                      if bracket=0 then
                      begin
                        if ord(AStringList[A][B])<=32 then
                        begin
                          Field2end:=B;
                          state:=5;
                        end;
                        if AStringList[A][B]='(' then inc(bracket);
                      end;
                      if (AStringList[A][B]=')') AND (bracket>0) then dec(bracket);
                    end;
                  end;
            end;
        end;
        if state=5 then
        begin
          B:=0;
          if length(DefinesArray)=0 then
          begin
            setlength(DefinesArray,1);
            DefinesArray[0].Name:=copy(AStringList[A],field1start,field1end-field1start+1);
            DefinesArray[0].Value:=copy(AStringList[A],field2start,field2end-field2start+1);
          end
          else
          begin
            NameMatch:=false;
            repeat
              if DefinesArray[B].Name=copy(AStringList[A],field1start,field1end-field1start+1) then NameMatch:=true
                                                                                               else inc(B);
            until NameMatch OR (B=length(DefinesArray));
            if NameMatch then
            begin
              DefinesArray[B].Value:=copy(AStringList[A],field2start,field2end-field2start+1);
            end
            else
            begin
              setlength(DefinesArray,length(DefinesArray)+1);
              DefinesArray[length(DefinesArray)-1].Name:=copy(AStringList[A],field1start,field1end-field1start+1);
              DefinesArray[length(DefinesArray)-1].Value:=copy(AStringList[A],field2start,field2end-field2start+1);
            end;
          end;
        end;
      end;
    end;
  end;
  AStringList.Free;
end;

function CheckDefines(aString: AnsiString):AnsiString;
var
  A,B,C: longint;
  StartPos: array of longint;
  EndPos: array of longint;
  bString: ansistring;
  //debug: boolean;

begin
  Result:=AString;
  if length(DefinesArray)>0 then
  begin
    for A:=0 to length(DefinesArray)-1 do
    begin
      setlength(StartPos,0);
      setlength(EndPos,0);
      B:=length(DefinesArray[A].Name);
      if length(Result)>=B then
      begin
        C:=1;
        repeat
          bString:=copy(Result,C,B);
          if comparestr(copy(Result,C,B),DefinesArray[A].Name)=0 then
          begin
            setlength(StartPos,length(StartPos)+1);
            setlength(EndPos,length(StartPos));
            StartPos[length(StartPos)-1]:=C;
            EndPos[length(StartPos)-1]:=C+B-1;
          end;
          inc(C);
        until (B+C-1)>length(Result);
        if length(StartPos)>0 then
        begin
          bString:=Result;
          if StartPos[0]>1 then Result:=copy(bString,1,StartPos[0]-1);
          for C:=0 to length(StartPos)-1 do
          begin
            Result:=Result+DefinesArray[A].Value;
            if length(StartPos)>(C+1) then Result:=Result+copy(bString,EndPos[C]+1,StartPos[C+1]-EndPos[C]-1)
                                      else Result:=Result+copy(bString,EndPos[C]+1,length(aString)-EndPos[C]);
          end;
        end;
      end;
    end;
  end;
end;

function wherestr(aString, bstring: ansistring): longint;
var
  A,B,c: longint;
begin
  result:=0;
  if ansicontainsstr(astring,bstring) then
  begin
    A:=1;
    repeat
      if copy(astring,A,length(bstring))=bstring then result:=A else inc(A);
    until ((length(bstring)+A)>length(astring)) OR (result<>0);
  end;
end;


{removes command strings (not yet supported)
processing of commands needs to be implemented}
function CheckRemoves(aString: Ansistring):AnsiString;
var
  A,B: longint;

begin
  result:=aString;
  A:=wherestr(aString,'/dts-v1/');
  if A>0 then
  begin
    if A=1 then
    begin
      if length(astring)>8 then result:=copy(astring,9,length(astring)-8)
                           else Result:='';
    end
    else
    begin
      result:=copy(astring,1,A-1);
      if length(astring)>(A+7) then result:=result+copy(astring,A+8,length(astring)-7-A);
    end;
  end;

  A:=wherestr(aString,'/delete-property/');
  if A>0 then
  begin
    if A=1 then
    begin
      if length(astring)>17 then result:=copy(astring,18,length(astring)-178)
                           else Result:='';
    end
    else
    begin
      result:=copy(astring,1,A-1);
      if length(astring)>(A+16) then result:=result+copy(astring,A+17,length(astring)-16-A);
    end;
  end;

  A:=wherestr(aString,'/delete-node/');
  if A>0 then
  begin
    if A=1 then
    begin
      if length(astring)>13 then result:=copy(astring,14,length(astring)-13)
                           else Result:='';
    end
    else
    begin
      result:=copy(astring,1,A-1);
      if length(astring)>(A+12) then result:=result+copy(astring,A+13,length(astring)-12-A);
    end;
  end;

  A:=wherestr(aString,'/omit-if-no-ref/');
  if A>0 then
  begin
    if A=1 then
    begin
      if length(astring)>16 then result:=copy(astring,17,length(astring)-16)
                           else Result:='';
    end
    else
    begin
      result:=copy(astring,1,A-1);
      if length(astring)>(A+15) then result:=result+copy(astring,A+16,length(astring)-15-A);
    end;
  end;

  A:=wherestr(aString,'/bits/');
  if A>0 then
  begin
    if length(astring)>(A+8) then
    begin
      B:=A+5;
      //first first space or tab
      repeat
        inc(B);
      until (ord(astring[B])=32) OR (ord(astring[B])=9) OR (B=length(astring));
      ///first non space or tab
      repeat
        inc(B);
      until ((ord(astring[B])<>32) AND (ord(astring[B])<>9)) OR (B=length(astring));
      //next space or tab
      repeat
        inc(B);
      until (ord(astring[B])=32) OR (ord(astring[B])=9) OR (B=length(astring));
      if B<=length(astring) then
      begin
        if A>0 then
        begin
          if A=1 then
          begin
            result:=copy(astring,(B+1),length(astring)-B);
          end
          else
          begin
            result:=copy(astring,1,A-1)+copy(astring,(B+1),length(astring)-B);
          end;
        end;
      end;
    end;
  end;
end;

{loads in dts file, then all includes, put all the includes in
a single stringlist, swaps defines for defined values
the procedure can call itself}
procedure ProcessDTSFile(aPath,aFile:AnsiString; aStringList:PStringList; aDataItem:PDataItem);
var
  A: byte;
  B: longword;
  AString,BString: ansistring;
  AMode: TMode;
  OldMode: TMode;
  pNewDataItem: PDataItem;

begin
  writeln('Opening file: '+aFile);
  aStringList^.Append('//Opening file: '+aFile);
  AString:='';
  BString:='';
  AMode:=NodeName;
  try
    aDataItem^.data.LoadFromFile(AFile);
  except
    on E:Exception do
      writeln('File '+AFile+' could not be read because: ', E.Message);
  end;
  if aDataItem^.data.Size>0 then
  begin
    aDataItem^.data.Position:=0;
    while aDataItem^.data.Position < aDataItem^.data.Size do
    begin
      A:=aDataItem^.data.ReadByte;
      case AMode of
        NodeName  : begin
                  case A of
                    0..8,11..31: ;
                    9: AString:=AString+'   ';
                    59{;},123{ open curl bracket }: begin
                          AString:=AString+chr(A);
                          AString:=CheckDefines(AString);
                          AString:=CheckRemoves(AString);
                          aStringList^.Append(AString);
                          AString:='';
                        end;
                    47    : begin
                              OldMode:=NodeName;
                              AMode:=Slash;
                            end;
                    10{LF}: if AnsiContainsStr(AString,'#include') then
                            begin
                              writeln('got include:'+AString);
                              BString:='';
                              B:=0;
                              //search for " or <
                              repeat
                                inc(B);
                              until (ord(AString[B])=34) OR (ord(AString[B])=60) OR (B=length(AString));
                              case ord(AString[B]) of
                                34: begin
                                      if B<length(AString) then
                                      begin
                                        repeat
                                          inc(B);
                                          if ord(AString[B])<>34 then BString:=BString+AString[B];
                                        until (ord(AString[B])=34) OR (B=length(AString));
                                      end;
                                      AString:='';
                                      if length(Bstring)>0 then
                                      begin
                                        BString:=aPath+BString;
                                        writeln('Includes:'+Bstring);
                                        if FileExists(Bstring) then
                                        begin
                                          writeln(Bstring+' exists');
                                          new(pNewDataItem);
                                          aDataItem^.next:=pNewDataItem;
                                          pNewDataItem^.position:=0;
                                          pNewDataItem^.next:=nil;
                                          pNewDataItem^.previous:=aDataItem;
                                          pNewDataItem^.data:=TMemoryStream.Create;
                                          ProcessDTSFile(aPath,Bstring,aStringList,pNewDataItem);
                                          Writeln('Returning to: '+aFile);
                                          aDataItem^.next:=nil;
                                          dispose(pNewDataItem);
                                          aStringList^.Append('//Returning to: '+aFile);
                                        end
                                        else
                                        begin
                                          Writeln('Error: '+Bstring+' does not exist. Enter to continue.');
                                          readln();
                                          Writeln('Continuing without '+Bstring+'. Results will be incomplete.');
                                        end;
                                      end;
                                    end;
                                60: begin
                                      if B<length(AString) then
                                      begin
                                        repeat
                                          inc(B);
                                          if ord(AString[B])<>62 then BString:=BString+AString[B];
                                        until (ord(AString[B])=62) OR (B=length(AString));
                                      end;
                                      AString:='';
                                      if length(Bstring)>0 then
                                      begin
                                        //remove leading directory structure
                                        B:=length(Bstring);
                                        repeat
                                          if ord(BString[B])<>47 then dec(B);
                                        until (ord(BString[B])=47) OR (B=0);
                                        Bstring:=copy(BString,B+1,length(Bstring)-B);
                                        //writeln('Includes:'+Bstring);
                                        BString:=aPath+BString;
                                        if ProcessDefines then
                                        begin
                                          if FileExists(Bstring) then
                                          begin
                                            writeln(Bstring+' exists');
                                            LoadDefines(BString);
                                          end
                                          else
                                          begin
                                            Writeln('Error: '+Bstring+' does not exist. Enter to continue.');
                                            ReadLn();
                                            Writeln('Continuing without '+Bstring+'. Results will be incomplete.');
                                          end;
                                        end
                                        else
                                        begin
                                          writeln(Bstring+' not processed with -h option');
                                        end;
                                      end;
                                    end;
                              end;
                            end;
                    else AString:=AString+chr(A);
                  end;
                end;

        Slash: begin
                case A of
                  // *
                  42: AMode:=MComment;
                  // second/
                  47: AMode:=SComment;
                  else begin
                         AString:=AString+'/';
                         aDataItem^.data.Position:=aDataItem^.data.Position-1;
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
    Writeln('Finished: '+aFile);
    aStringList^.Append('//Leaving: '+aFile);
  end;
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

function ExtractNodeLabel(aString:ansistring): ansistring;
var
  A,B: word;
begin
  CurrentNodeLabel:='';
  Result:='';
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
      for B:=1 to A-1 do CurrentNodeLabel:=CurrentNodeLabel+AString[B];
    end;
  end;
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



procedure AddItemToNodeArray(AName: PNameArray; AValue: AnsiString; ANodeArray: PDTSArray);
var
  A,ALen,C: longint;
  B,BLen, D: byte;
  Modifier: boolean;
  BName: TNameArray;
  Found: boolean;
  AString: ansistring;
begin
  Modifier:=false;
  setlength(BName,0);
  if length(AName^)>0 then
  begin
    if length(AName^[0].nodename)>0 then
    begin
      if AName^[0].nodename[1]='&' then Modifier:=true;
    end;
    if Modifier then
    begin
      if length(ANodeArray^)>0 then
      begin
        Found:=false;
        A:=0;
        ALen:=length(ANodeArray^);
        repeat
          if length(ANodeArray^[A].namearray)>0 then
          begin
            B:=0;
            BLen:=length(ANodeArray^[A].namearray);
            repeat
              {if a nodename has a label that matches a later &label, then put everything in the &label structure with label swapped for the nodename that has the label}
              if ANodeArray^[A].namearray[B].nodelabel=copy(AName^[0].nodename,2,length(AName^[0].nodename)-1) then
              begin
                Found:=true;
                setlength(BName,length(AName^)+B);
                for C:=0 to length(AName^)+B-1 do
                begin
                  if C<=B then BName[C].nodename:=ANodeArray^[A].namearray[C].nodename
                          else BName[C].nodename:=AName^[C-B].nodename;
                end;
                setlength(ANodeArray^,length(ANodeArray^)+1);
                setlength(ANodeArray^[length(ANodeArray^)-1].namearray, length(BName));
                for C:=0 to length(BName)-1 do
                begin
                  ANodeArray^[length(ANodeArray^)-1].namearray[C].nodename:=ExtractNodeLabel(BName[C].nodename);
                  ANodeArray^[length(ANodeArray^)-1].namearray[C].nodelabel:=CurrentNodeLabel;
                end;
                ANodeArray^[length(ANodeArray^)-1].nodevalue:=AValue;
                ANodeArray^[length(ANodeArray^)-1].phandle:='';
                ANodeArray^[length(ANodeArray^)-1].pvaluelist:=FindHandlesIn(ANodeArray^[length(ANodeArray^)-1].nodevalue);
              end;
              inc(B);
            until Found OR (B=BLen);
          end;
          inc(A);
        until Found OR (A=ALen);



      end; //if length(NodeArray)>0
    end //if Modifier
    else
    begin
      setlength(ANodeArray^,length(ANodeArray^)+1);
      setlength(ANodeArray^[length(ANodeArray^)-1].namearray, length(AName^));
      for B:=0 to length(AName^)-1 do
      begin
        ANodeArray^[length(ANodeArray^)-1].namearray[B].nodename:=ExtractNodeLabel(AName^[B].nodename);
        ANodeArray^[length(ANodeArray^)-1].namearray[B].nodelabel:=CurrentNodeLabel;
      end;
      ANodeArray^[length(ANodeArray^)-1].nodevalue:=AValue;
      ANodeArray^[length(ANodeArray^)-1].phandle:='';
      ANodeArray^[length(ANodeArray^)-1].pvaluelist:=FindHandlesIn(ANodeArray^[length(ANodeArray^)-1].nodevalue);
    end; //if Modifier else
  end;
end;

procedure StringListToNodeArray(aStringList: pStringList; aNodeArray: pDTSArray);
var
  liney,linex: longint;
  B: byte;
  AMode: Tmode;
  OldMode: TMode;
  Depth: byte;
  AName: TNameArray;
  AValue: ansistring;
  CopySpaces: boolean;
  ValueIsphandle: boolean;
  phandle: ansistring;
  CharsSinceLastData: ansistring;
begin
  AMode:=NodeName;
  depth:=0;
  setlength(AName,0);
  setlength(phandleArray,0);
  phandleArrayOkay:=true;
  CharsSinceLastData:='';
  if aStringlist^.Count>0 then
  begin
    liney:=0;
    while liney<aStringlist^.Count do
    begin
      if length(aStringlist^[liney])>0 then
      begin
        linex:=1;
        while linex<=length(aStringlist^[liney]) do
        begin

          case AMode of
            NodeName: begin
              case ord(aStringlist^[liney][linex]) of
                  // {
                  123   : begin
                            inc(depth);
                            if depth>=length(AName) then
                            begin
                              setlength(AName,depth+1);
                              AName[depth].nodename:='';
                              if depth=1 then
                              begin
                                if linex>1 then
                                begin
                                  AName[depth-1].nodename:='';
                                  for B:=1 to linex-1 do
                                  begin
                                    case ord(aStringlist^[liney][B]) of
                                      //chars that can appear as depth0
                                      {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
                  #(35) &(38) *(42) +(43) ,(44) -(45) .(46) /(47) :(58) ?(63) @(64) _(95) }
                  35,38,42..47,48..58,63..90,95,97..122: AName[depth-1].nodename:=AName[depth-1].nodename+aStringlist^[liney][B];
                                    end;
                                  end;
                                end;
                              end;
                              phandle:='';
                              ValueIsphandle:=false;
                            end;
                          end;
                  // =
                  61    : begin
                            if AName[depth].nodename = 'phandle' then
                            begin
                              ValueIsphandle:=true;
                              phandle:='';
                            end;
                            AMode:=NodeValue;
                            CopySpaces:=false;
                          end;
                  {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
                  #(35) &(38) *(42) +(43) ,(44) -(45) .(46) :(58) ?(63) @(64) _(95) }
                  35,38,42..46,48..58,63..90,95,97..122:
                          begin
                            if depth>0 then
                              AName[depth].nodename:=AName[depth].nodename+aStringlist^[liney][linex];
                            CharsSinceLastData:=CharsSinceLastData+aStringlist^[liney][linex];
                          end;
                  // }  end of node, start a new one
                  125   : begin
                            AName[depth].nodename:='';
                            dec(depth);
                            setlength(AName,depth+1);
                            AName[depth].nodename:='';
                            phandle:='';
                            ValueIsphandle:=false;
                            CharsSinceLastData:='';

                          end;
                  // /
                  47    : begin
                            OldMode:=NodeName;
                            AMode:=Slash;
                          end;
                  59    : begin   // ; char means name and value done
                                  //now we have a complete name and value
                                  //add to output array
                                  //only happens here if there is a node without a value
                            //check if there is anything here at all
                            if length(CharsSinceLastData)>0 then
                            begin
                              if (NOT valueisphandle) or showphandles then
                                AddItemToNodeArray(@AName,AValue,ANodeArray);
                              if (length(phandle)>0) then
                              begin
                                if phandleArrayOkay then AddNodeTophandleArray(@AName,phandle)
                                                    else writeln('Error: Something wrong with phandle table.');
                              end;
                              //clean up
                              AValue:='';
                              {if depth>0 then }AName[depth].nodename:='';
                            end;
                          end;
                end;

              end;  //end NodeName

            NodeValue: begin
              case ord(aStringlist^[liney][linex]) of
                //spcae char
                32:     if CopySpaces {AND DataStarted} then AValue:=AValue+aStringlist^[liney][linex];
                {valid name chars a-z (97-122) A-Z (65-90) 0-9 (48-57)
                "(34) #(35) &(38) *(42) +(43) ,(44) -(45) .(46) <(60) >(62) ?(63) @(64) [(91) ](93) \(92) _(95) }
                34..35,38,42..46,48..57,60,62..93,95,97..122:
                        begin
                          //if DataStarted then
                          begin
                            AValue:=AValue+aStringlist^[liney][linex];
                            if ValueIsphandle AND (depth>0) then
                            begin
                              phandle:=phandle+aStringlist^[liney][linex];
                            end;
                          end;
                          if ((CopySpaces=false) AND ((ord(aStringlist^[liney][linex])=34) OR (ord(aStringlist^[liney][linex])=60) OR (ord(aStringlist^[liney][linex])=91))) then CopySpaces:=true;
                          if ((CopySpaces=true) AND ((ord(aStringlist^[liney][linex])=34) OR (ord(aStringlist^[liney][linex])=62) OR (ord(aStringlist^[liney][linex])=93))) then CopySpaces:=false;
                        end;
                // /
                47    : begin
                          OldMode:=NodeValue;
                          AMode:=Slash;
                        end;
                59    : begin   // ; char means name and value done
                                //now we have a complete name and value
                                //add to output array
                          if (NOT valueisphandle) or showphandles then
                                AddItemToNodeArray(@AName,AValue,ANodeArray);
                          if (length(phandle)>0) then
                          begin
                            if phandleArrayOkay then AddNodeTophandleArray(@AName,phandle)
                                                else writeln('Error: Something wrong with phandle table.');
                          end;
                          //clean up
                          AValue:='';
                          AName[depth].nodename:='';
                          //go back to loading a node name
                          AMode:=NodeName;
                        end;
             end;
            end;  //NodeValue end

            Slash: begin
              case ord(aStringlist^[liney][linex]) of
                // *
                42: AMode:=MComment;
                // second/
                47: AMode:=SComment;
                else begin
                       case OldMode of
                         NodeName: {if DataStarted then } if depth>0 then AName[depth].nodename:=AName[depth].nodename+'/'+aStringlist^[liney][linex];
                         NodeValue: {if DataStarted then}
                                begin
                                  AValue:=AValue+'/';
                                  if (ord(aStringlist^[liney][linex])=32) then
                                  begin
                                    if CopySpaces then AValue:=AValue+aStringlist^[liney][linex];
                                  end
                                  else AValue:=AValue+aStringlist^[liney][linex];
                                end;
                       end;
                       AMode:=OldMode;
                     end;
              end;
            end;
            MComment: begin
              case ord(aStringlist^[liney][linex]) of
                42: AMode:=Star;
              end;
            end;
            SComment: begin
                if linex=length(aStringlist^[liney]) then
                begin
                  AMode:=OldMode;
                end;
            end;
            Star: begin
              case ord(aStringlist^[liney][linex]) of
                47: AMode:=OldMode;
                else AMode:=MComment;
              end;
            end;
          end;
          inc(linex);
        end;
      end;
      inc(liney);
    end;
  end;
end;

function DTB2SFileToStringList(aFile: ansistring; AStringList: PStringList):boolean;
begin
  AStringList^.LoadFromFile(aFile);
  result:=true;
end;

{for each value
1. look in PValuesArray, see if a value is pHandle
2. see if the value number appears in phandlearray
3. if so substitute the number for the phandle
4. put in outputarray}

Function Value2pHandle (aValue,bValue: ansistring): ansistring;
var
  valuelist: array of TValueReplace; //valuestart[0] is index to first char of a value
  A,B: word;
  TempStr: ansistring;
begin
  Result:='';
  setlength(valuelist,0);
  A:=0;
  //find start and end of each value
  for B:=2 to length(aValue)-1 do
  begin
    if ((aValue[B-1]='<') OR (aValue[B-1]=' ')) AND ValidChar(aValue[B]) then
    begin
      inc(A);
      setlength(valuelist,A);
      valuelist[A-1].startindex:=B;
      valuelist[A-1].endindex:=0;
      valuelist[A-1].replace:=false;
      valuelist[A-1].newtext:='';
    end;
    if ((aValue[B+1]='>') OR (aValue[B+1]=' ')) AND ValidChar(aValue[B]) then
    begin
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
              //got a phandle match
              begin
                valuelist[A].replace:=true;
                valuelist[A].newtext:=namearraytotext(@pHandleArray[B].namearray);
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



Procedure ProcessDTB2SFile(aFile: ansistring; FileNumber: LongInt);
var
  ADTSArray: TDTSArray;
  DTBStringList: TStringList;
  A,B,C,D,E: longword;
  PValueMatchFound: boolean;
  LabelSearch: ansistring;
  TempStr: ansistring;
begin
  setlength(ADTSArray,0);
  DTBStringList:=TStringList.Create;
  //new DTSArray starts here, per input file
  writeln('Processing decompiled dtb: '+aFile);
  //produces DTSArray & pHandleArray
  //DTSFile2Array also adds the file name in row 0
  if DTB2SFileToStringList(aFile,@DTBStringList) then
  begin
    StringListToNodeArray(@DTBStringList, @ADTSArray);
      if length(ADTSArray)>1 then
      begin
        {for each value
        1. look in PValuesArray, see if a value is pHandle
        2. see if the value number appears in phandlearray
        3. if so substitute the number for the phandle
        4. put in outputarray}
        OutputArray[FileNumber+1][0]:=aFile;
        for A:=0 to length(ADTSArray)-1 do  //row 0 is file name ??
        begin
          PValueMatchFound:=false;
          if length(ADTSArray)>0 then
          begin
            for B:=0 to length(DTSNodeArray)-1 do
            begin
              if NOT PValueMatchFound then
              begin
                //search for nodes that have values containing phandles
                if CompareNameArray(@ADTSArray[A].namearray,@DTSNodeArray[B].namearray) then
                  if length(DTSNodeArray[B].pvaluelist)>0 then
                begin
                  PValueMatchFound:=true;
                  if length(ADTSArray[A].nodevalue)>0 then
                  begin
                    if ADTSArray[A].nodevalue[1]='<' then ADTSArray[A].nodevalue:=Value2pHandle(ADTSArray[A].nodevalue,DTSNodeArray[B].pvaluelist);
                  end;
                end;
              end;
            end;

          end;
          //values that can be swapped for phandles has now been done
          //load each line into output file
          //filenumber=0 for first file
          ay:=0; //ay is which row we are going to use
          tempstr:='';
          if length(ADTSArray[A].namearray)>0 then
          begin
             for B:=0 to length(ADTSArray[A].namearray)-1 do
             begin
               if B>0 then tempstr:=tempstr+DepthDelimiter;
               tempstr:=tempstr+ADTSArray[A].namearray[B].nodename;
             end;
             for B:=1 to length(ADTSArray[A].namearray)-1 do tempstr:=tempstr+UnDepthDelimiter;
          end;
          case y of
            0: writeln('Error: Something wrong with outputarray');
            1: begin //if there are no names then add it
                 y:=2;
                 Setlength(OutputArray,x,y);
                 ay:=1;
                 OutputArray[0,ay]:=tempstr;
               end;
            else
               //search in name column
               begin
                 C:=1;
                 while (ay=0) AND (C<y) do
                 begin
                   if CompareStr(tempstr,OutputArray[0,C])=0 then ay:=C;
                   inc(C);
                 end;
                 if ay=0 then //no match, make longer and add to last
                 begin
                   ay:=y;
                   inc(y);
                   Setlength(OutputArray,x,y);
                   OutputArray[0,ay]:=tempstr;
                 end;
               end;
          end;
          OutputArray[FileNumber+1,ay]:=ADTSArray[A].nodevalue;
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
    ProcessDefines:=true;
    showphandles:=false;
    InputFile:='';
    OutputFile:='';
    dtsOutputFile:='';
    PathToSourceFiles:='';
    DTSStringList:=TStringList.Create;
    SetLength(DefinesArray,0);
    setlength(DecompFiles,0);
    setlength(DTSNodeArray,0);
    ACommandSwitch:=none;
    for Count1:=1 to ParamCOunt do
    begin
      case ParamStr(Count1) of
      '-s','-S': ACommandSwitch:=source;
      '-c','-C': ACommandSwitch:=combined;
      '-d','-D': ACommandSwitch:=decomp;
      '-o','-O': ACommandSwitch:=output;
      //if selected don't process defines (by default process them)
      '-h','-H': begin
                   ProcessDefines:=false;
                   ACommandSwitch:=defines;
                 end;
      //if selected show phandles (by default do not)
      '-p','-P': begin
                   ShowPhandles:=true;
                   ACommandSwitch:=handles;
                 end;
        else case ACommandSwitch of
          source :begin
                    if InputFile='' then InputFile:=ParamStr(Count1)
                                    else writeln(InputFile+' already specified.');
                  end;
          combined: begin
                    if dtsOutputFile='' then dtsOutputFile:=ParamStr(Count1)
                                        else writeln(dtsOutputFile+' already specified.');
                  end;
          decomp: begin
                    setlength(DecompFiles,length(DecompFiles)+1);
                    DecompFiles[length(DecompFiles)-1]:=ParamStr(Count1);
                  end;
          output: begin
                    if OutputFile='' then OutputFile:=ParamStr(Count1)
                                     else writeln(OutputFile+' already specified.');
                  end;
          defines,handles: ;
        end;
      end;
    end;

    if length(InputFile)>0 then
    begin
      //figure out path prefix
      Count1:=0;
      for Count2:=1 to length(InputFile) do
      begin
        if InputFile[Count2]=PathDelimiter then Count1:=Count2;
      end;
      if Count1>0 then
      begin
        PathToSourceFiles:=copy(InputFile,1,Count1);
      end;
      //set up the initial data store
      CurrentData.position:=0;
      CurrentData.next:=nil;
      CurrentData.previous:=nil;
      CurrentData.data:=TMemoryStream.Create;
      ProcessDTSFile(PathToSourceFiles,InputFile,@DTSStringList,@CurrentData);

      StringListToNodeArray(@DTSStringList,@DTSNodeArray);

      if OutputFileOkay(DTSOutPutFile) then DTSStringList.SaveToFile(DTSOutPutFile);

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
        x:=length(DecompFiles)+1; //first column is names, after that one per Decompfile
        y:=1;                     //first row is filename
        setlength(OutputArray,x,y);
        OutPutArray[0][0]:='';
        for Count1:=0 to length(DecompFiles)-1 do ProcessDTB2SFile(DecompFiles[Count1],Count1);
        if OutputFileOkay(OutPutFile) then OutputArray2OutPutFile(OutputFile);
      end
      else
      begin
        writeln('No decompiled dtb files provided.');
        Writeln('Usage: ./dts2tsv -s [dts file] -d [list of decompiled dtb files] -o [output file]');
      end;
    end;
  end
  else Writeln('Usage: ./dts2tsv -s [dts file] -d [list of decompiled dtb files] -o [output file]');
end.
