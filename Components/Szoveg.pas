
{
	S T R I N G E K K E L   K A P C S O L A T O S
	---------------------------------------------
		    eljárásgyüjtemény
}

UNIT Szoveg;


INTERFACE

Uses System.SysUtils, WinProcs, System.Classes, System.IniFiles;

	{ --------  Nagybetűre/kisbetűre konvertálás ----------}
	Function Upper( s : string ):string;
	Function Lower( s : string ):string;

  { ------- Első betűt nagybetűre konvertál -------}
	Function UpperFirst( s : string ):string;
  function UpperFirstName(name: string; mode: integer): string;

	{ ------- Egy stringben egy másikat keres =>
	OUT	: sorszám, ha talál, egyébként = 0 }
	Function StrSearch( miben,mit:string):word;

  {-------- Megszámolja a substring előfordulásait }
  Function StrCount( miben,mit:string):word;

  { Egy string n. delimiterekkel határolt szavát adja, vagy ''-et}
  Function StrCountD( miben,delimiter:string; n:word):string;

  {Egy sub-string n. előfordulásának pozíciójával tér vissza}
  function CountPos ( miben,mit : string; n:integer ):integer;

  { 1.--------  Részstring Space-ekkel való feltöltése --------
	IN	: i1	= hossz a string ball szélétől
		  s	= a string }
	Function LSpace(i1 : integer;  s :string) : string;

  { 1.1 ------- SPACE konkatenációja ---------}
	Function Space(i1 : integer) : string;

	{ 2.---- Egy stringből a bal oldali Space-eket levágja ----- }
	Function Ltrim(s:string):string;

	{ 3.---- Egy stringből a jobb oldali Space-eket levágja ----- }
	Function Rtrim(s:string):string;

	{ 4.---- Egy string mindkét végéről a Space-eket levágja ----- }
	Function Alltrim(s:string):string;

  Function LeftString(s:string;poz:integer):string;
  Function RightString(s:string;poz:integer):string;

  {Beszúr egy substringet a p pozíciónál, a string hossza a sub hosszával nő}
  Function InsSub(s,sub:string;p:integer):string;
  {Töröl egy substringet ha talál, a string hossza a sub hosszával csökken}
  Function DelSub(s,sub:string):string;

	{ ---- Bevezető 0-k Space-ra cserélése -----
	IN :	s	= sz mstring }
	Function ZeroSpace( s : string ):string;

  // Levágja a string bevezető nulláit
  function TrimZero(cStr: string): string;

	Function Hex( dec_szam : Word ):String;

	Function Replicate( kar : String; szor : Integer ):String;

	{ Egy num. értéket bevezető '0'-kal stringgé alakit }
	Function ZeroNum( sz,hosz : Word ):String;
        { Lecserél egy részstringet }
	Function Stuff( miben, mit, mire :string ):string;

	Function PadL( mit,mivel : string; hossz:integer ): string;

	Function PadR( mit,mivel : string; hossz:integer ): string;

	Function PadC( mit, mivel : string; hossz:integer ): string;

  { Egy mondat-string n. szavával tér vissza }
  Function Szo( szov : string;n : word ):string;


  Function F_Path(fn:string):string;
  Function F_Name(fn:string):string;
  Function F_Ext(fn:string):string;

  Function CsakBetu(s: String): string;

  { DOS szöveget WINDOWS szöveggé konvertál }
	Function ASCIIToWIN(s: string): string;
  { WINDOWS szöveget DOS szöveggé konvertál }
  Function WINToASCII(s: string): string;
  { A DELPHI.INI-ben regisztrált komponensek listáját adja }
  Function GetComponentsTypes:TStringList;
  Function GetComponentsPages:TStringList;

  function GetSectionNemes(iFile: TInifile):TStringList;
  function GetSectionValues(iFile: TInifile;Section:string):TStringList;

  {Csak számjegyek vizsgálata}
  Function IsNum(s:string):boolean;

  {------- Az egész számot szövegessé alakítja --------}
  FUNCTION SzamBetuvel( sz : Longint ):string;

  function HunUpper(ch: Char): Char;
  function HunLower(ch: Char): Char;

  // A hh:mm:ss formátumú stringet a nap törtrészévé konvertálja
  function TimeStringToTime(tStr: string): DOUBLE;

IMPLEMENTATION

Function Upper( s : string ):string;
begin
   Result:=AnsiUpperCase(s);
end;

Function Lower;
begin
   Result:=AnsiLowerCase(s);
end;

{ ------- Első betűt nagybetűre konvertál -------}
Function UpperFirst( s : string ):string;
begin
  s:=Lower(s);
  If Length(s)>0 then s:=AnsiUpperCase(Copy(s,1,1))+Copy(s,2,1000);
  Result := s;
end;

Function LSpace;
Var ix  : Integer;
 Begin
   For ix:=1 to i1  do
     s[ix]:=' ';
   LSpace:=s;
  end;

Function Space;
Var ix  : Integer;
    s: String;
Begin
   s:='';
   For ix:=1 to i1  do
     s:=s+' ';
   Space:=s;
end;

Function StrSearch;
Var  s1,s2 : Array[0..255] of char;
     p	: PChar;
begin
	StrPCopy( s1,miben );
	StrPCopy( s2,mit );
	p := StrPos( s1,s2 );
	IF p<>nil then StrSearch := p-s1+1
	else StrSearch:=0;
end;

{-------- Megszámolja a substring előfordulásait }
Function StrCount( miben,mit:string):word;
var p: integer;
begin
  Result:=0;
  repeat
       p:=Pos(mit,miben);
       If p>0 then begin
          Result:=Result+1;
          miben:=Copy(miben,p+Length(mit),Length(miben)-p);
       end;
  until p=0;
end;

{ A miben string n. delimiterekkel határolt szavát adja, az első és utolsó
 szavaknál nem szükséges az elválasztó}
Function StrCountD( miben,delimiter:string; n:word):string;
var p1,p2: word;
begin
  Result := '';
  If n>0 then begin
     p1:=CountPos(miben,delimiter,n-1);
     p2:=CountPos(miben,delimiter,n);
     If p2>p1 then Result := Copy(miben,p1+1,p2-p1-1)
     else if p1>0 then Result := Copy(miben,p1+1,Length(miben));
  end;
end;

Function Ltrim;
Var i	: integer;
begin
        Ltrim:='';
	For i:=1 to Length(s) do
	begin
		If s[i]>' ' then
                begin
		   Ltrim:=Copy(s,i,Length(s)-i+1);
		   Exit;
                end;
	end;
end;

Function Rtrim;
Var i	: integer;
begin
        Rtrim:='';
	For i:=Length(s) downto 1 do
	begin
		If s[i]>' ' then
		begin
		   Rtrim:=Copy(s,1,i);
		   Exit;
                end;
	end;
end;

Function Alltrim;
begin Alltrim := Ltrim(Rtrim(s)); end;

Function LeftString(s:string;poz:integer):string;
begin
  If poz<Length(s) then Result:=Copy(s,1,poz)
  else Result:=s;
end;

Function RightString(s:string;poz:integer):string;
begin
  If poz<Length(s) then begin
     Delete(s,1,poz-1);
     Result:=s;
  end
  else Result:='';
end;

{Beszúr egy substringet a p pozíciónál, a string hossza a sub hosszával nő}
Function InsSub(s,sub:string;p:integer):string;
begin
  result := Copy(result,1,p-1)+sub+Copy(result,p+Length(sub),Length(s));
end;

{Töröl egy substringet ha talál, a string hossza a sub hosszával csökken}
Function DelSub(s,sub:string):string;
var us,usub,r : string;
    n: integer;
begin
   us := UpperCase(s);
   usub := UpperCase(sub);
   n := Pos(usub,us);
   R := s;
   If n>0 then Delete(R,n,Length(usub));
   Delsub := r;
end;

Function ZeroSpace;
Var  i   : integer;
begin
     i := 1;
	While s[i] < '1' do
        begin
                s[i] := ' ';
                Inc(i);
        end;
end;

Function Hex;
Var hmar	: byte;
    hsz		: Word;
    hexszam     : String;
Const hx : String = '0123456789ABCDEF';
begin
        hsz := 100; hexszam := '';
	While hsz <> 0 do
	begin
		hsz := dec_szam div 16;
		hmar := dec_szam mod 16;
		hexszam := hx[ hmar+1 ] + Hexszam;
		dec_szam := hsz;
	end;
        Hex := hexszam;
end;

{
   Replicate = egy karekter megsokszoroz sa
}
Function Replicate;
Var	i	: Integer;
	r	: String;
begin
	r := '';
	For i:=1 to szor do r := r + kar;
	Replicate := r;
end;


{ Zeronum	= Egy num. értéket bevezető '0'-kal stringgé alakit }

Function ZeroNum;
Var	s	: String;
begin
	Str( sz,s );
	ZeroNum := Replicate( '0',hosz-Length(s)) + s;
end;

{ Stuff	= egy stringben karaktereket cserél le }

Function Stuff;
Var	i	: integer;
	szov	: String;
	b	: String;
begin
	szov := '';
	For i:=1 to Length(miben) do
	begin
		b := Copy(miben,i,1);
		If b=mit then b:=mire;
		szov := szov + b;
	end;
	Stuff := szov
end;



{
	PadL		= Egy string ballra igazit sa
	---------------------------------------------
	mit	= a forr s string;
	hossz	= az igazit si hossz;
	mivel	= kieg‚szˇt” karakterek;
}
Function PadL;
Var	szo	: String;
begin
	szo  := Alltrim(mit);
	If Length( szo ) < hossz then
		 PadL := szo + Replicate( mivel,hossz-Length(szo) )
	else	 PadL := szo;
end;

{
	PadR		= Egy string jobbra igazitása
	---------------------------------------------
	mit	= a forr s string;
	hossz	= az igazit si hossz;
	mivel	= kieg‚szˇt” karakterek;
}
Function PadR;
Var	szov	: String;
	szo	: String;
begin	
	szov := '';
	szo  := Alltrim(mit);
	If Length( szo ) < hossz then
		 PadR := Replicate( mivel,hossz-Length(szo)) + szo
	else	 Padr := szo;
end;

{
	PadC		= Egy string kozepre igazit sa
	----------------------------------------------
	mit	= a forrás string;
	hossz	= az igazitási hossz;
	mivel	= kiegészítő karakterek;
}
Function PadC;
Var	szov	: String;
	szo	: String;
	h	: Integer;
	fel1,fel2	: integer;
begin
	szov := '';
	szo  := Alltrim(mit);
	h    := hossz - Length(szo);
	If Length( szo ) < hossz then
	begin
		fel1 := h div 2;
		fel2 := h-fel1;
		PadC := Replicate( mivel,fel1) + szo + Replicate( mivel,fel2 )
	end
	else	PadC := szo;
end;


{ Egy szov-string n. szav val t‚r vissza }
Function Szo;
Var	hossz    : word;
	hanyadik : word;
	i	 : word;
	ujszo	 : string;
begin
if alltrim(szov)<>'' then begin
	hossz := Length(szov);
	hanyadik := 1;
	i := 0;
	ujszo := '';
	szo := '';
	Repeat
		Inc(i);
		If (szov[i] > #32) and (i<hossz+1) then
		If hanyadik<n then begin
			Inc(hanyadik);
    			Repeat Inc(i);
			Until (szov[i]<#33) or (i=hossz+1);
		end
		else begin
		If i<(hossz+1) then begin
                        While (szov[i]<#33) or (i=hossz+1) do Inc(i);
			Repeat
				ujszo := ujszo + szov[i];
				Inc(i);
			Until (szov[i]<#33) or (i=hossz+1);
			szo := ujszo;
			exit;
		end;
		end;
	Until i>hossz;
end;
end;

Function F_Path(fn:string):string;
var s: string;
begin
 s:=ExtractFilePath(fn);
 Result:=Copy(s,1,Length(s)-1);
end;

Function F_Name(fn:string):string;
var s: string;
begin
 s:=ExtractFileName(fn);
 Result:=Copy(s,1,Pos('.',s)-1);
end;

Function F_Ext(fn:string):string;
begin
 Result:=Copy(ExtractFileExt(fn),2,3);
end;

Function CsakBetu(s: String): string;
var ii: integer;
    b: string;
begin
  Result := '';
//  s:=ASCIIToWIN(s);
  For ii:=1 to Length(s) do begin
      b:=Copy(s,ii,1);
      if b=#0 then exit;
      If b>#31 then Result:=Result+b;
  end;
end;

{DOS szöveget WINDOWS szöveggé konvertál}
Function ASCIIToWIN(s: string): string;
var s1: array[0..1000000] of Char;
begin
(*
  OEMToChar(PChar(s),s1);
  Result := String(s1);
*)
end;

{WINDOWS szöveget DOS szöveggé konvertál}
Function WINToASCII(s: string): string;
var s1,s2: Pchar;
begin
(*
  s1:='';s2:='';
  s1:=StrPCopy(s1,s);
  AnsiToOEM(s1,s2);
  Result:=StrPas(s2);
*)
end;

{Egy sub-string n. elüfordulásának pozíciójával tér vissza,
 ha n-nél kevesebbszer fordul elő vagy egyáltalán nem, akkor =0}
function CountPos ( miben,mit : string; n:integer ):integer;
var p,i: integer;
begin
     p:=0; Result:=0;
  If n>0 then begin
     for i:=1 to n do begin
         p := Pos(mit,miben);
         If (p=0) then break;
         Result:=Result+p;
         miben:=Copy(miben,p+Length(mit),Length(miben));
     end;
     If (n>i) or (p=0) then Result:=0
     else Result := Result + (i-1) * (Length(mit)-1);
  end;
end;

Function GetComponentsTypes:TStringList;
var DiFile: TiniFile;
    sts: TStringList;
    i,j,n: integer;
    s,t: string;
begin
  sts := TStringList.Create;
  DiFile:= TiniFile.Create('DELPHI.INI');
  DiFile.ReadSection('COMPLIB.DCL.Palette',sts);
  Result:=TStringList.Create;
  Result.Sorted:=True;
  For i:=0 to sts.Count-1 do begin
      s:=DiFile.ReadString('COMPLIB.DCL.Palette',sts.Strings[i],'');
      If s<>'' then begin
         n:=StrCount(s,';');
         For j:=1 to n+1 do begin
            t:=StrCountD(s,';',j);
            If t<>'' then Result.Add(t);
         end;
      end;
  end;
  DiFile.Free;
  sts.Free;
end;

Function GetComponentsPages:TStringList;
var DiFile: TiniFile;
begin
  Result:=TStringList.Create;
  DiFile:= TiniFile.Create('DELPHI.INI');
  DiFile.ReadSection('COMPLIB.DCL.Palette',Result);
  DiFile.Free;
end;

{ GetSectionNemes(inifilenév)
  Kigyüjti egy stringlistbe a section-k neveit}
function GetSectionNemes(iFile: TInifile):TStringList;
var T:TEXTFILE;
    sor:string;
begin
  Result:=nil; 
  If ifile<>nil then begin
     Result:=TStringList.Create;
     Try
     AssignFile(T,iFile.FileName);
     Reset(T);
     While not EOF(T) do begin
       ReadLn(T,sor); sor:=AllTrim(sor);
       If (Pos('[',sor)=1) and (Pos(']',sor)>0) then begin
          Result.Add(Copy(sor,Pos('[',sor)+1,Pos(']',sor)-2));
       end;
     end;
     Finally
       CloseFile(T);
     end;
  end;
end;

function GetSectionValues(iFile: TInifile;Section:string):TStringList;
var sts: TStringList;
    i: integer;
    s: string;
begin
  Result:=TStringList.Create;
  sts := TStringList.Create;
  iFile.ReadSection(Section,sts);
  For i:=0 to sts.Count-1 do begin
      s:=iFile.ReadString(Section,sts.Strings[i],'');
      If s<>'' then begin
         Result.Add(s);
      end;
  end;
  sts.Free;
end;

Function IsNum(s:string):boolean;
var i:integer;
begin
  Result := True;
  For i:=1 to Length(s) do
      If not (s[i] in ['0'..'9']) then begin
         Result := False;
         Break;
      end;
end;

FUNCTION SzamBetuvel( sz: longint ):string;
Var szsz,i  : longint;
    elojel  : string;
Const ns : Array[0..4] of string[9] = ('','ezer ','millió ','milliárd ','billió ');

      (*1000 alatti számok betüvé konvertálása*)
      FUNCTION szaztizenegy( sz : Longint ):string;
      var NumStr: string;
      Const nk : Array[1..2,0..9] of string[10] =
            (('nulla','egy','kettő','három','négy','öt','hat','hét','nyolc','kilenc'),
            ('','tizen','huszon','harminc','negyven','ötven','hatvan','hetven',
	     'nyolcvan','kilencven'));
            nr : Array[1..3] of string[4] = ('tiz','száz','ezer');
      begin
        NumStr := ZeroNum(sz,3);
        If sz>0 then result:='' else Result:='nulla';
        IF sz > 99 then
	   Result := Result+nk[1][ StrToInt( Copy( NumStr,1,1 ))] +'száz';
        sz := sz MOD 100;
        IF (sz > 9) and (sz <> 10) and (sz <> 20) then
	   Result := Result + nk[2][ StrToInt( Copy( NumStr,2,1 ))];
        IF (sz MOD 10)>0 then
	   Result := Result + nk[1][ StrToInt( Copy( NumStr,3,1 ))];
        IF sz=10 then Result := Result+'tíz';
        IF sz=20 then Result := Result+'húsz';
      end;

begin
        Result := '';
        i:=0;
        If sz=0 then Result:='nulla';
        If sz<0 then elojel:='- ' else elojel:='';
        While sz<>0 do begin
           szsz := Abs(sz) mod 1000;
           sz   := sz div 1000;
           If szsz<>0 then Result := szaztizenegy(szsz)+ns[i]+Result;
           Inc(i);
        end;
        Result := Stuff(Alltrim(Result),' ','-');
        Result := elojel+Result;
end;

// A személyneveket átalakítja az alábbi formákba:
// mod=0 - végig nagybetűs;
// mod=1 - Csak a nevek első betűi nagybetűsek, a többi kisbetűs
function UpperFirstName(name: string; mode: integer): string;
var s,sz: string;
    i   : integer;
    elsobetu: boolean;
Const kis : string = 'áéíóöőúüű';
      nagy: string = 'ÁÉÍÓÖŐÚÜŰ';
begin
sz := LTrim(name);
Result := sz;
if sz <> '' then begin
Try
   i:=1;
      Result := '';
      elsobetu:= True;
      Case mode of
      0: BEGIN
           Result := UpperCase(sz);
         END;
      1:
         while szo(sz,i)<>'' do begin
                s := szo(sz,i);
                Result := Result + UpperFirst(LowerCase(s))+' ';
                Inc(i);
         end;
      2: Result := UpperFirst(LowerCase(sz));
      end;
finally
  Result := LTrim(Result);
  for i:=1 to Length(Result) do begin
      s:=Result[i];
      if mode=0 then begin
         if (Pos(s,kis)>0) then
            Result[i] := nagy[Pos(s,kis)];
      end;
      if mode=1 then begin
         If (elsobetu) and (Pos(s,kis)>0) then begin
            Result[i] := nagy[Pos(s,kis)];
            elsobetu := False;
         end;
         If (not elsobetu) and (Pos(s,nagy)>0) then begin
            Result[i] := kis[Pos(s,nagy)];
            elsobetu := False;
         end;
         elsobetu := (s=' ');
      end;
      if mode=2 then begin
         If (elsobetu) and (Pos(s,kis)>0) then
            Result[i] := nagy[Pos(s,kis)];
         if (not elsobetu) and (Pos(s,nagy)>0) then
            Result[i] := kis[Pos(s,nagy)];
         elsobetu := False;
      end;
  end;
end;
end;
end;

function HunUpper(ch: Char): Char;
begin
  Result := Upper(Ch)[1];
end;

function HunLower(ch: Char): Char;
begin
  Result := Lower(Ch)[1];
end;

  // Levágja a string bevezető nulláit
function TrimZero(cStr: string): string;
var i: integer;
begin
  Result := cStr;
  i:=1;
  While Result[i]='0' do begin
    Result := Copy(Result,i+1,1000);
    Inc(i);
  end;
end;

// A hh:mm:ss formátumú stringet a nap törtrészévé konvertálja
function TimeStringToTime(tStr: string): DOUBLE;
Var H,M,S: integer;
    n    : integer;
begin
  n:=StrCount(tStr,':');
  H := 0;
  M := 0;
  S := 0;
  Result := 0;
if Length(Alltrim(tStr))>0 then begin
  H := StrToInt(TrimZero(StrCountD(tStr,':',1)));
  if n<2 then
  M := StrToInt(TrimZero(StrCountD(tStr,':',2)));
  if n>=2 then
  S := StrToInt(TrimZero(StrCountD(tStr,':',3)));
  Result := (3600*H+60*M+S)/(24*3600);
end;
end;

BEGIN
end.
