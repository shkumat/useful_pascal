const   ABOUT0= 'Filter of CSV lines; ver.1.01 (2023-07-24); author Oleksii Shkumat.       '
;       USAGE1= 'Usage : findcsv [/?] [/d] [/n] [/v] [/s]  [InputFileName] [OutputFileName]'
;       USAGE2= '            /d   - delimiter                                    ;         '
;       USAGE3= '            /q   - text is qualified                            ;         '
;       USAGE4= '            /s   - first lines to skip                          ;         '
;       USAGE5= '            /n   - print also line numbers                      ;         '
;       USAGE6= '            /v   - inversion-mode - print only invalid lines    ;         '
;       USAGE7= ' examples:                                                                '
;       USAGE8= '          type SomeFile.csv | findcsv  /d;   /v   > InvalidLines.csv      '
;       USAGE9= '          type SomeFile.csv | findcsv  /d09  /q  /s5  > OutputFile.csv    '
;
var     I,J             :   Integer
;       S               :   ShortString
;       LineNum         :   LongInt     =   0
;       FirstLinesToSkip:   Integer     =   0
;       SampleDelimCount:   Integer     =   -1
;       InputFile       :   Text
;       OutputFile      :   Text
;       InputFileName   :   ShortString =   ''
;       OutputFileName  :   ShortString =   ''
;       PrintLineNumbers:   Boolean     =   FALSE
;       InversionMode   :   Boolean     =   FALSE
;       Qualified       :   Boolean     =   FALSE
;       Delimiter       :   Char        =   #44
; {$I-}   
function Int(S:ShortString):Integer;
var     Code            :   Integer;
begin
    Val(S,Result,Code);
    if Code<>0 then
        Result:=-1;
end;

function EndOfFile:Boolean;
begin
    if InputFileName>'' then
        Result:=EOF(InputFile)
    else
        Result:=EOF(Input);  
end;

procedure ProcessLine();
var     Line            :   AnsiString  =   ''
;       DelimCount      :   Integer     =   0
;       Scope           :   Boolean     =   TRUE ;
begin
    if InputFileName>'' then
        ReadLn(InputFile,Line)
    else
        ReadLn(Line);
    Inc(LineNum);
    if LineNum<=FirstLinesToSkip then
        exit;    
    for I:=1 to Length(Line) do
        if (Qualified) and (Line[I]=#34) then
            Scope:= not Scope
        else
            if (Scope) and (Line[I]=Delimiter) then
                Inc(DelimCount);
    if  (LineNum = (FirstLinesToSkip+1)) then
        SampleDelimCount:=DelimCount;
    if  ((SampleDelimCount=0) or (DelimCount<>SampleDelimCount))<>InversionMode then
        exit;
    if OutputFileName>'' then
        if PrintLineNumbers then
            WriteLn(OutputFile,'Line ',LineNum,' : ',Line)
        else
            WriteLn(OutputFile,Line)
    else
        if PrintLineNumbers then
            WriteLn('Line ',LineNum,' : ',Line)
        else
            WriteLn(Line);
end;

begin
    for I:=1 to ParamCount do
        begin
            S:=LowerCase(ParamStr(I));
            if (S='?') or (S='/?') or (S='-?') or (S='/h') or (S='-h') or (S='-help') or (S='--help') then
                begin
                    Writeln(ABOUT0);Writeln('');Writeln(USAGE1);Writeln(USAGE2);Writeln(USAGE3);Writeln(USAGE4);
                    Writeln(USAGE5);Writeln(USAGE6);Writeln(USAGE7);Writeln(USAGE8);Writeln(USAGE9);
                    exit;
                end;                   
            if (S='/q') or (S='-q') then 
               Qualified:= TRUE;  
            if (S='/v') or (S='-v') then 
               InversionMode:= TRUE; 
            if (S='/n') or (S='-n') then 
               PrintLineNumbers:= TRUE;  
            if Length(S)>2 then
                begin
                    if (Copy(S,1,2)='/s') or (Copy(S,1,2)='-s') then
                        begin
                            FirstLinesToSkip:=Int(Copy(S,3,Length(S)-2));
                            if FirstLinesToSkip<0 then
                                exit;
                        end;
                    if (Copy(S,1,2)='/d') or (Copy(S,1,2)='-d') then
                        if S[3]>#57 then
                            Delimiter:=S[3]
                        else
                            begin
                                J:=Int(Copy(S,3,Length(S)-2));
                                if (J<0) or (J>255) then
                                    exit;
                                Delimiter:=Chr(J);
                            end;
                end;
            if (S[1]<>'/') and (S[1]<>'-') then
                if InputFileName = '' then
                    InputFileName:= S
                else
                    OutputFileName:= S;
    end;        
    if InputFileName>'' then
        begin
            Assign(InputFile,InputFileName); 
            ReSet(InputFile);
            if OutputFileName>'' then
                begin
                    Assign(OutputFile,OutputFileName); 
                    ReWrite(OutputFile);
                end;
        end;
    while not EndOfFile do ProcessLine;
    if InputFileName>'' then Close(InputFile);
    if OutputFileName>'' then Close(OutputFile);
end.