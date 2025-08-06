{
	N U M E R I K U S     E L J  R  S O K
	---------------------------------------
}
UNIT Szamok;

INTERFACE
Uses System.SysUtils, WinTypes, ptd_calc;

type
   EPowerException = class(Exception)
end;

	{ -------- Elõjel vizsgálat ----------
	OUT : szam > 0	= 1
	      szam = 0  = 0
	      szam < 0  = -1	}
	Function Sgn(szam : extended):shortint;

	{ -------- Fok átváltása radiánra ---- }
	Function Radian(f : extended):extended;

	{ -------- Radián átváltása fokra ---- }
	Function Fok(rad : extended):extended;

	{ --- Egy szakasz (vektor) irányszögének meghatározása ---
	IN	dx,dy	: szakasz v‚gpontj‚nek az alapponthoz
			  viszonyitott koord. kl”nbs‚ge;
	OUT     fok
			  270
		       180    0 = 360
			   90			}
	Function Fokkeres(dx,dy : integer):extended;

	{------- Hatvanyozas  (A az N-ediken)--------}
        function Power(X, N : real) : extended;

        {------- Osztási maradék képzés ------
        IN :     sz = osztandó
                 o  = osztó
        OUT:     maradek = [0..1] pl 0.34303 }
        Function Maradek( sz,o : extended ):extended;

        {A legközelebbi 'mire' értékre kerekít:
           pl (2.8,2 -> 2 vagy 1.3,3->0, -2.8,0.5->-3)}
        function Kerekit(szam,mire:extended):extended;

        {------- Tangens szögfüggvény ---------
        IN  :    szog    = szög [rad]  }
        Function Tan( szog : extended ):extended;

        {------- Kotangens szögfüggvény ---------
        IN  :    szog    = szög [rad]  }
        Function Cot( szog : extended ):extended;

	Function DecToHex( dec_szam : Word ):String;
	Function HexToDec( hex_szam : string ):word;
	Function DecToBin( dec_szam : Word ):String;
	Function BinToDec( bin_szam : string ):word;

        Function SetBit(szam:byte;bit:integer;ertek:integer):byte;
        Function GetBit(szam:byte;bit:integer):byte;
        Function IsBit(szam:byte;bit:integer):boolean;
        Function InversBit(szam:byte;bit:integer):byte;

      {--------- Pointerek -----------}
//       function AddPtr(BASE:Pointer;Offs:Longint):Pbyte;

IMPLEMENTATION

function Kerekit(szam,mire:extended):extended;
begin
  Result:=mire*(Int(szam/mire)+Round(Maradek(szam,mire)));
end;

Function Sgn(szam : extended):shortint;
begin
     Sgn := 1;
     If szam=0 then Sgn :=  0;
     If szam<0 then Sgn := -1;
end;

Function Radian(f : extended):extended;
begin
     Radian := f * pi /180;
end;

Function Fok(rad : extended):extended;
begin
     Fok := rad * 180 /pi;
end;

Function Fokkeres(dx,dy : integer):extended;
Var      elojel_x       : shortint;
         elojel_y       : shortint;
         f              : extended;
begin
     elojel_x:=Sgn(dx);
     elojel_y:=Sgn(dy);
     f := 0;
     If dx<>0 then f := Abs( Arctan( dy/dx ) )
     else If dy>=0 then f:=pi/2 else f:=3*pi/2;
     Case elojel_x of
          1  : If elojel_y=-1 then f:=2*pi-f;
          0  : If elojel_y=-1 then f:=f+3*pi/4;
          -1 : Case elojel_y of
                    1 : f:=pi-f;
                    0 : f:=pi;
                   -1 : f:=f+pi;
               end;
     end;
     f := Fok(f);
     Fokkeres := f;
end;

function Power(X, N : real) : extended;
var
  t : longint;
  r : real;
  isInteger : boolean;
begin

   if N = 0 then begin
      result := 1.0;
      exit;
   end;

   if X = 1.0 then begin
      result := 1.0;
      exit;
   end;

   if X = 0.0 then begin
      if N > 0.0 then
         begin
           result := 0.0;
           exit;
         end
      else
        raise EPowerException.Create('Infinite Result');
   end;

   if (X > 0) then
      try
         result := exp(N * ln(X));
         exit;
      except
         raise EPowerException.Create('Overflow/Underflow Result');
   end;


{ X is negative but we still may compute the result if n is an integer}
{ try and get integer portion of n into a longint, it will be quicker to
}       { compute odd n}
  try
     t := trunc(n);
     if (n - t) = 0 then
        isInteger := true
     else
        isInteger := False;
  except
     {Bit superfluous as result will probably underflow/overflow anyway}
     r := int(n);
     if (n - r) = 0 then
        begin
           isInteger := true;
           if frac(r/2) = 0.5 then
              t := 1
           else
              t := 2;
        end
     else
        isInteger := False;
  end;

  if isInteger then
      begin
         {n is an integer}
         if odd(t) then
            {n is odd}
            try
               result := -exp(N * ln(-X));
               exit;
            except
               raise EPowerException.Create('Overflow/Underflow Result');
            end
         else
            {n is even}
            try
               result := exp(N * ln(-X));
               exit;
            except
               raise EPowerException.Create('Overflow/Underflow Result');
            end;
      end
   else
      raise EPowerException.Create('Complex Result');

end;


Function Maradek;
begin
     Result := sz/o - Trunc(sz/o);
end;

Function Tan;
Var s,c : extended;
begin
     s := sin(szog); c := cos(szog);
     If c=0 then tan:=10E+6 else Tan := sin(szog)/cos(szog);
end;

Function Cot;
begin
     Cot := cos(szog)/sin(szog);
end;

Function DecToHex( dec_szam : Word ):String;
begin
   Result:=Format('%x',[dec_szam]);
end;

Function HexToDec( hex_szam : string ):word;
var   i: integer;
Const hx : String = '0123456789ABCDEF';
begin
  Result:=0;
  For i:=Length(hex_szam) downto 1 do begin
      Result:=Result+(Pos(hex_szam[i],hx)-1)*Trunc(Power(16,Length(hex_szam)-i));
  end;
end;

Function DecToBin( dec_szam : Word ):String;
var   i: integer;
      mar: word; sz:real;
Const hx : String = '0123456789ABCDEF';
begin
  Result:='';
  For i:=0 to 15 do begin
      mar := dec_szam Mod 2;
      dec_szam := dec_szam div 2;
      Result:=hx[mar+1]+Result;
  end;
{  Until dec_szam=0;}
end;

Function BinToDec( bin_szam : string ):word;
var   i: integer;
Const hx : String = '0123456789ABCDEF';
begin
  Result:=0;
  For i:=Length(bin_szam) downto 1 do begin
      Result:=Result+(Pos(bin_szam[i],hx)-1)*Trunc(Power(2,Length(bin_szam)-i));
  end;
end;

Function SetBit(szam:byte;bit:integer;ertek:integer):byte;
var bi: extended;
    bitt: byte;
begin
    bi:=bit;
    bitt:=Trunc(Power(2,bi));
    Result := szam;
    If ertek=0 then begin
       If GetBit(szam,bit)=1 then Result:=szam xor bitt;
    end else Result := szam or bitt;
end;

Function GetBit(szam:byte;bit:integer):byte;
var bi: extended;
begin
    bi:=bit;
    bit:=Trunc(Power(2,bi));
    If (szam and bit)>0 then
        Result := 1
    else
        Result := 0;
end;

Function IsBit(szam:byte;bit:integer):boolean;
begin
    Result := GetBit(szam,bit)=1;
end;

Function InversBit(szam:byte;bit:integer):byte;
begin
   If Getbit(szam,bit)=1 then
      Result:=SetBit(szam,bit,0)
   else
      Result:=SetBit(szam,bit,1);
end;


(*
function AddPtr(BASE:Pointer;Offs:Longint):Pbyte;
assembler;
asm
        MOV     AX,word ptr [OFFS+2]
        MOV     BX,word ptr [OFFS+0]
        ADD     BX,word ptr [BASE+0]
        ADC     AX,0
        MUL     SelectorInc
        ADD     AX,word ptr [BASE+2]
        MOV     DX,AX
        MOV     AX,BX
end;
*)
end.
