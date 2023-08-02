const   ABOUT0= 'Filter of forbidden characters; ver.1.03 (2023-08-02); author Oleksii Shkumat.'
;       USAGE1= 'Usage : find0 [/?] [/t] [/n] [/v] [/f]   [InputFileName]  [OutputFileName]    '
;       USAGE2= '            /n   - print also line numbers                 ;                  '
;       USAGE3= '            /v   - inversion-mode - print only valid lines ;                  '
;       USAGE4= '            /t   - consider tab as a valid character       ;                  '
;       USAGE5= '            /f   - fix forbidden characters                .                  '
;       USAGE6= ' example:                                                                     '
;       USAGE7= '          type SomeFile.csv | find0  /t /v > OutputFile.csv                   '
;
var     I               :   Integer
;       S               :   ShortString
;       LineNum         :   LongInt     =   0
;       InputFile       :   Text
;       OutputFile      :   Text
;       InputFileName   :   ShortString =   ''
;       OutputFileName  :   ShortString =   ''
;       FixingMode      :   Boolean     =   FALSE
;       InversionMode   :   Boolean     =   FALSE
;       EnableTabChar   :   Boolean     =   FALSE
;       PrintLineNumbers:   Boolean     =   FALSE
;
{$I-}
function EndOfFile:Boolean;
begin
    if InputFileName>'' then
        Result:=EOF(InputFile)
    else
        Result:=EOF(Input);
end;

procedure ProcessLine();
var     Line        :  AnsiString = ''
;       LineIsValid :  Boolean    = TRUE;
begin
    if InputFileName>'' then
        ReadLn(InputFile, Line)
    else
        ReadLn(Line);
    Inc(LineNum);
    for I:=1 to Length(Line) do
        if ( Ord( Line[I] ) < 32 ) and ( ( Ord( Line[I] ) <> 9 ) or ( not EnableTabChar ) ) then
                if FixingMode then
                    Line[I]:=#32
                else
                    LineIsValid:=FALSE;
    if ( LineIsValid<>InversionMode ) and ( not FixingMode ) then
        Exit;
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
            S:=LowerCase( ParamStr(I) );
            if (S='?') or (S='/?') or (S='-?') or (S='/h') or (S='-h') or (S='-help') or (S='--help') then
                begin
                    Writeln(ABOUT0);Writeln('');Writeln(USAGE1);Writeln(USAGE2);Writeln(USAGE3);
                    Writeln(USAGE4);Writeln(USAGE5);Writeln(USAGE6);Writeln(USAGE7);
                    Exit;
                end;
            if (S='/t') or (S='-t') then
               EnableTabChar:= TRUE;
            if (S='/n') or (S='-n') then
               PrintLineNumbers:= TRUE;
            if (S='/v') or (S='-v') then
               InversionMode:=TRUE;
            if (S='/f') or (S='-f') then
               FixingMode:=TRUE;
            if (S[1]<>'/') and (S[1]<>'-') then
                if InputFileName='' then
                    InputFileName:=S
                else
                    OutputFileName:=S;
    end;
    if InputFileName>'' then
        begin
            Assign(InputFile, InputFileName);
            ReSet(InputFile);
            if OutputFileName>'' then
                begin
                    Assign(OutputFile, OutputFileName);
                    ReWrite(OutputFile);
                end;
        end;
    while not EndOfFile do
        ProcessLine;
    if InputFileName>'' then
        Close(InputFile);
    if OutputFileName>'' then
        Close(OutputFile);
end.