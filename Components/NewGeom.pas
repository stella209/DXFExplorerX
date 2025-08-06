 { Geometriai transzformációk
  ==========================
  Nagy vonalas ábrák (rajzok, térképek) szerkesztésére
  Ált. jelölések:
       p       : kérdéses pont
       porigo  : centrum pont
       szog    : szög fokokban
       tav     : távolság

  A végtelen értéke = 1E+-30}

unit NewGeom;

interface
Uses
  Winapi.Windows, System.Classes, Vcl.Graphics, System.SysUtils, Szamok, System.Math;

Const MaxReal : double = 10e+300;
      MinReal : double = 10e-300;

Type el = (bal,jobb,also,felso); kinnkod = set of el;

  TPoint2D = record
    X, Y: double;
  end;

  TPoint3D = record
    X, Y, Z: double;
  end;

  TRect2D = record
    case Integer of
    0: (x1,y1,x2,y2 : double);
    1: (P1,P2: TPoint2d);
  end;

  Tegyenes = record
    case Integer of
    0: (x1,y1,x2,y2 : double);
    1: (P1,P2: TPoint2d);
  end;

  Tegyenesfgv = record
    a: double;  {egyenes iránytangense  }
    b: double;  {egyenes tengelymetszete}
  end;

  TTeglalap = record
    case Integer of
    0: (a,b,c,d : TPoint2d); {A négy csúcspont kooordinátája}
    1: (p : array[0..3] of TPoint2d);
  end;

  TKor    = record
    u,v,r: double;      {u,v=kör középpont x,y koord.; r=sugár}
  end;

  TKorfgv = record     {x2+y2+dx+ey+f=0}
    d,e,f: double;
  end;

  T2Point2d = record
    case Integer of
    0: (p1,p2 : TPoint2d);
    1: (r: TRect2d);
  end;

  T3Point2d = record
    p1,p2,p3 : TPoint2d;
  end;

  pPoints2d = ^TPoint2d;        // List for points inner use
  pPoints3d = ^TPoint3d;        // List for points inner use

  PPointArray = ^TPointArray;
  TPointArray   = array of TPoint;

  PPoint2dArray = ^TPoint2dArray;
  TPoint2dArray = array of TPoint2d;

var xbal,xjobb,yalso,yfelso: double;
    dPoints                : TList;            // Temporary Point list
    oldTeglalap,teglalap   : TTeglalap;


// dPoints list
procedure InitdPoints;

Function Rad(fok:double):double;

procedure cls(Ca: TCanvas; co: TColor);
procedure clsRect(Ca: TCanvas; t:TRect; co: TColor);
procedure SetPen(Ca:TCanvas;color:TColor;width:integer;style:TPenStyle;mode:TPenMode);
procedure SetFont(Ca:TCanvas;FontName:string;Size:integer;Color:TColor;Style:TFontStyles);
procedure ShowLine(ca:TCanvas;x1,y1,x2,y2:integer);
procedure ShowCross(ca:TCanvas;x,y,d:integer);
procedure RotText(ca:TCanvas; x,y:integer; szoveg:string; szog:integer);
  { Téglalap elforgatása a Cent-rumja körül: w,h : szélesség,hosszúság}
function RotateTegla(Cent: TPoint2d; w,h : double; Angle: double): TTeglalap; overload;
  { Téglalap elforgatása külsõ Cent-rum körül }
procedure RotateTegla(var tegla: TTeglalap; Cent: TPoint2d; Angle: double); overload;
procedure DrawTegla(Ca: TCanvas; t: TTeglalap);

function AdjustAngle(sz: double): double;
Function Eltolas(p:TPoint2d;tav,szog:double):TPoint2d;
Function Elforgatas(p,porigo:TPoint2d;szog:double):TPoint2d;
Function VisszTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
Function FuggTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
Function TengelyesTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
Function KozeppontosTukrozes(p,porigo:TPoint2d):TPoint2d;
Function Nagyitas(p,porigo:TPoint2d;n:double):TPoint2d;

{Egyéb segédrutinok}

Function KeTPontTavolsaga(x1,y1,x2,y2: double): double; overload;
Function KeTPontTavolsaga(p1,p2: TPoint2d): double; overload;
function PontSzakaszTavolsaga(p,p1,p2: TPoint2d): double;
Function SzakaszSzog(x1,y1,x2,y2: double): double;
function SzogFelezo(alfa,beta: double): double;
function Szogdiff(alapszog,szog:double):double;
function RelSzogdiff(alfa1,alfa2:double):double; overload;
function RelSzogdiff(alfa1,alfa2,alfa3:double):double; overload;
Function FelezoPont(p1,p2:TPoint2d):TPoint2d;
Function OsztoPont(p1,p2:TPoint2d;arany:double):TPoint2d;
Function KeTPontonAtmenoEgyenes(x1,y1,x2,y2:double):Tegyenesfgv; overload;
Function KeTPontonAtmenoEgyenes(p1,p2: TPoint2d):Tegyenesfgv; overload;
Function EgypontonAtmenoMeroleges(e1:Tegyenesfgv;p1:TPoint2D):Tegyenesfgv;
function PontEgyenesTavolsaga(e1:Tegyenesfgv;p:TPoint2d):double;
{ Egy ponton átmenõ adott iránytangensû egyenes egyenletét
  adja: p1 = pont, a = iránytangens }
Function Egyenes1(p1:TPoint2d;a:double):Tegyenesfgv;
{ Két ponton p1,p2 átmenõ egyenes iránytangensét adja}
Function Egyenes2(p1,p2:TPoint2d):double;
{ Két egyenes metszéspontjának koord.-ját adja}
Function Egyenes12(e1,e2:Tegyenes):TPoint2d;
Function KetEgyenesMetszespontja(ef1,ef2:Tegyenesfgv):TPoint2d;
//Function KetSzakaszMetszespontja(p11,p12,p21,p22:TPoint2d: var mp:TPoint2d) boolean;
{ Rajta van-e a pont a vonalszakaszon }
Function Vonalonvan(e : Tegyenes; p: TPoint2d; tures: double):boolean;
{p1,p2 irányított szakaszt d távolsággal eltolja önmagával ||-an;
       Direct = True/False akkor a szakasz jobb/bal partján }
function SzakaszParhuzamosEltolas(p1,p2:TPoint2d;d:double;Direct:boolean):TEgyenes;
Function Kozben(a,b,x,tures: double): boolean;
{ Két végponttól r1,r2 távolságban lévõ pont koor.-áit adja p-ban,
  ha nincs metszéspont = False }
Function Ivmetszes(u1,v1,r1,u2,v2,r2: double;var p:TPoint2D):boolean;
{ True = a 3 oldal valóban háromszöget alkot}
Function HaromszogEgyenlotlenseg(d1,d2,d3:double):boolean;
Function Bemeres(u1,v1,r1,u2,v2,r2: double;var p:TPoint2D):boolean;

{   Egyenes vágó algoritmus:
    Meghatározza egy szakasznak a képernyõre esõ részét
    xi,yi : a szakasz végpontjai,
    t     : a metszendõ téglalap alakú terület
}
Function Clip(var x1,y1,x2,y2:double;t:TRect2D):boolean;
function SzakaszSzakaszMetszes(p11,p12,p21,p22:TPoint2D; var mp:TPoint2d):boolean;  overload;
function SzakaszSzakaszMetszes(Rec1,Rec2:TRect2D; var mp:TPoint2d):boolean; overload;
Function IsSzakaszNegyszogMetszes(p1,p2:TPoint2D;t:TRect2D):boolean; overload;
Function SzakaszNegyszogMetszes(var p1,p2:TPoint2D;t:TRect2D):boolean; overload;
Function PontInKep(x,y:double;t:TRect2D):boolean;
{Elõállítja a kör egyenlet 0-ra redukált alakját}
Function SetKorfgv(u,v,r:double):Tkorfgv;
Function Masodfoku(a,b,c:double;var p12: TPoint2d):integer;
Function IsKorEgyenesMetszes(u,v,r:double; a,b: double):boolean;
Function IsAblakSzakaszMetszes(u,v,r:double; p: TRect2d):boolean;
Function IsAblakNegyszogMetszes(u,v,r:double; p: TRect2d):boolean;
Function IsAblakEllipszisMetszes(u,v,r:double; p: TRect2d):boolean;
{Van-e kör-egyenes metszés és hány ponton}
Function KorEgyenesMetszes(u,v,r:double; a,b: double;var p12:TRect2d):integer;
{3 ponton átmenõ kör értékeit adja: (u,v,r) }
Function HaromPontbolKor(p1,p2,p3:TPoint2D):TPoint3D;
{Körív rajtolás: Ca canvasra, p1,p2,p3 pontokon megy át}
procedure KorivRajzol(Ca:TCanvas;pp1,pp2,pp3:TPoint2D);
{p1,p2 a teglalap egy oldala fix, pk=külsõ futópont a || oldalon}
function KorivbolHarompont(u,v,r,StartAngle,endAngle:double):T3Point2d;
Function HaromPontbolTeglalap(p1,p2,pk:TPoint2D):TTeglalap;
function CorrectRect(t:TRect):TRect;
function CorrectRealRect(t:TRect2D):TRect2D;
function RectInRect2D(OutRect,InRect: TRect2D): boolean;
function IntersectRect2D(Rect1,Rect2: TRect2D): boolean;

{ ---------- Polygon processes ----------- }
Function GetPoligonRect(pSTM:TMemoryStream):TRect2D;
function IsPointInPoligon(pStm:TMemoryStream;p:TPoint2D):boolean; overload;
function IsPointInPoligon(aList: Array of TPoint2d; p: TPoint2D):boolean; overload;
function GetPoligonArea(pStm:TMemoryStream):double;
function IsDirectPoligon(pStm:TMemoryStream):boolean; overload;
function IsDirectPoligon(Pprior,Pymax,Pnext: TPoint2d):boolean; overload;
function PoligonLatoszog(aList: Array of TPoint2d; p: TPoint2D;
                                var maxIndex,minIndex: integer;
                                var MaxAngle, MinAngle: double):boolean;

function IsEqualPoint2d(P1, P2: TPoint2D; d: double): boolean;
function Point2D(X, Y: double): TPoint2D;
function Rect2D(X1, Y1, X2, Y2: double): TRect2D;
function RoundPoint(P: TPoint2D): TPoint;
function FloatPoint(P: TPoint): TPoint2D;
function Point3D(X, Y, Z: double): TPoint3D;
function Angle2D(P: TPoint2D): double;
function Dist2D(P: TPoint2D): double;
function Dist3D(P: TPoint3D): double;
function RelAngle2D(PA, PB: TPoint2D): double;
function RelDist2D(PA, PB: TPoint2D): double;
function RelDist3D(PA, PB: TPoint3D): double;
procedure Rotate2D(var P: TPoint2D; Angle2D: double);
procedure RelRotate2D(var P: TPoint2D; PCentr: TPoint2D; Angle2D: double);
procedure Move2D(var P: TPoint2D; Angle2D, Distance: double);
function Between(PA, PB: TPoint2D; Preference: double): TPoint2D;
function DistLine(A, B, C: double; P: TPoint2D): double;
function Dist2P(P, P1, P2: TPoint2D): double;
function DistD1P(DX, DY: double; P1, P: TPoint2D): double;
function NearLine2P(P, P1, P2: TPoint2D; D: double): Boolean;
function AddPoints(P1, P2: TPoint2D): TPoint2D;
function SubPoints(P1, P2: TPoint2D): TPoint2D;
function GeomPower(Base, Exponent: double): double;
//function PointInRect2D(p: TPoint2D; OutRect: TRect2d): boolean;

procedure MullPoint3d(var p: TPoint3d; coeff: double);

function Invert(Col: TColor): TColor;
function Dark(Col: TColor; Percentage: Byte): TColor;
function Light(Col: TColor; Percentage: Byte): TColor;
function Mix(Col1, Col2: TColor; Percentage: Byte): TColor;
function MMix(Cols: array of TColor): TColor;
function Log(Base, Value: double): double;
function Modulator(Val, Max: double): double;
function M(I, J: Integer): Integer;
function Tan(Angle2D: double): double;
procedure Limit(var Value: Integer; Min, Max: Integer);
function Exp2(Exponent: Byte): Word;
function GetSysDir: String;
function GetWinDir: String;

{3D rutins}
Procedure d3Coord(x, y, z : Real; {coordinates} a, b : Real; {View angles}
                 Var newx, newy : Integer); {return coordinates}
{Gömb koordináta körök}
procedure RotEllipse(ca:TCanvas;porigo:TPoint;a,b:integer;szog:double);
procedure RotEllipseArc(ca:TCanvas;porigo:TPoint;a,b:integer;szog:double;
                        fi1,fi2:integer);
Function GlobeAxis(ca:TCanvas;porigo:TPoint;R:integer;theta,fi:double):TRect;
procedure GlobeSzelessegiKor(ca:TCanvas;porigo:TPoint;R:integer;
                             theta,fi:double;
                             delta:double);

{Geodézia}
Function  Elometszes(a,b:TPoint2D;alfa,beta:real):TPoint2D;

{ Statistic}

// Linear Regression
{************* LinearLeastSquares *******************}
 function LinearLeastSquares(data: TPoint2DArray; var M,B, R: double): boolean;
 {Line "Y = mX + b" is linear least squares line for the input array, "data",
  of TRealPoint. R : Correlation Coefficient (0..1).
  Result = False then error}


implementation

Function Rad(fok:double):double;
begin
  Result := fok*pi/180;
end;

procedure cls(Ca: TCanvas; co: TColor);
Var DC : HDC;
    c  : TRect;
begin
  with Ca do
  begin
{      DC := GetDC(Ca.Handle);}
      Brush.style:=bsSolid;
      Brush.color:=co;
      Pen.Color:=co;
      Pen.Mode:=pmCopy;
      c:=cliprect;
      Rectangle(c.left,c.top,c.right,c.bottom);
{      RestoreDC(Ca.Handle,DC);}
  end;
end;

procedure ClsRect(Ca: TCanvas; t:TRect; co: TColor);
Var DC : HDC;
begin
  with Ca do
  begin
      DC := GetDC(Ca.Handle);
      Pen.Color:=co; Pen.Width:=4;
      brush.style:=bsSolid;
      Brush.color:=co; Pen.Color:=co;
      Rectangle(t.left,t.top,t.right,t.bottom);
      ReleaseDC(Ca.Handle,DC);
      RestoreDC(Ca.Handle,DC);
  end;
end;

{Grafikus toll beállítása}
procedure SetPen(ca:TCanvas;color:TColor;width:integer;style:TPenStyle;mode:TPenMode);
begin
  ca.pen.Color:=color;
  ca.pen.width:=width;
  ca.pen.style:=style;
  ca.pen.mode :=mode;
end;

procedure SetFont(Ca:TCanvas;FontName:string;Size:integer;Color:TColor;Style:TFontStyles);
begin
  ca.Font.Name    := FontName;
  ca.Font.Size    := Size;
  ca.Font.Color   := Color;
  ca.Font.Style   := Style;
end;

procedure ShowLine(ca:TCanvas;x1,y1,x2,y2:integer);
begin ca.MoveTo(x1,y1); ca.LineTo(x2,y2);end;

procedure ShowCross(ca:TCanvas;x,y,d:integer);
begin
  ShowLine(ca,x-d,y,x+d,y);
  ShowLine(ca,x,y-d,x,y+d);
end;

procedure RotText(ca:TCanvas; x,y:integer; szoveg:string; szog:integer);
var th: THandle;
    tf: TLogfont;
    hf: HFont;
begin
  Getobject(Ca.Font.handle,SizeOf(tf),@tf);
  tf.lfEscapement:=szog;
  tf.lfOutPrecision:=OUT_TT_ONLY_PRECIS;
  hf:=CreateFontIndirect(tf);
  th:=SelectObject(Ca.Handle,hf);
  TextOut(Ca.Handle,x,y,PChar(szoveg),Length(szoveg));
  DeleteObject(hf);
  SelectObject(Ca.Handle,th);
end;


Function KeTPontTavolsaga(x1,y1,x2,y2: double): double;
begin
  Result := Sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))
end;

Function KeTPontTavolsaga(p1,p2: TPoint2d): double; overload;
begin
  Result := Sqrt((p2.x-p1.x)*(p2.x-p1.x) + (p2.y-p1.y)*(p2.y-p1.y))
end;

{ Megméri a pontból a szkaszra bocsátott merõlegesen a távolságot.
  Ha nincs metszéspont, akkor a két végpontból mért táv.-ok kisebbikét adja}
function PontSzakaszTavolsaga(p,p1,p2: TPoint2d): double;
var fgv,mer: TEgyenesFgv;
    mp     : TPoint2d;
begin
    Result := 0;
    fgv := KeTPontonAtmenoEgyenes(p1,p2);
    mer := EgypontonAtmenoMeroleges(fgv,p);
    mp  := KetEgyenesMetszespontja(fgv,mer);
    if Kozben(p1.x,p2.x,p.x,0) and Kozben(p1.y,p2.y,p.y,0)
    then Result := RelDist2d(p,mp)
    else
        Result := Min(RelDist2d(p,p1),RelDist2d(p,p2));
end;

{ x1,y1 a kezdõpontból kiindulva megadja a szakasz irányszögét rad-ban}
Function SzakaszSzog(x1,y1,x2,y2: double): double;
begin
  Result:=Relangle2d(Point2d(x1,y1),Point2d(x2,y2));
end;


function SzogFelezo(alfa,beta: double): double;
begin
end;

{alapszög és szög eltérése direkt irányban, óramutató járásával ellentétesen
    0..2*pi rad.}
function Szogdiff(alapszog,szog:double):double;
begin
Try
  szog := szog - alapszog;
  If szog<0 then szog:=2*pi+szog;
  If szog>=2*pi then szog:=szog-2*pi;
  Result := szog;
except
  Result := 0;
end;
end;

// Relatív szög eltérés alfa2-nek az alfa1 irányhoz képest  +/- irányokban
// Result : >0 ha alfa2 direkt irányú eltérés (0..pi)
//          <0 ha alfa2 indirekt .. (2*pi..pi); pl: -1 = 360-57.4
function RelSzogdiff(alfa1,alfa2:double):double;
begin
  Result := Szogdiff(alfa1,alfa2);
  if Result>pi then Result := Result-2*pi;
end;

{Köriv 3 pontja meghatároz egy középponti szöget:
       Result >0 pozitív szög (óramutató járásával ellentétes irányában);
       Result <0  negativ szög (óramutató járásával megegyezõ irányában)}
function RelSzogdiff(alfa1,alfa2,alfa3:double):double;
var szd12,szd13: double;
begin
  alfa1 := 2*pi*Frac(alfa1/(2*pi));
  alfa2 := 2*pi*Frac(alfa2/(2*pi));
  alfa3 := 2*pi*Frac(alfa3/(2*pi));
  szd12:=SzogDiff(alfa1,alfa2);
  szd13:=SzogDiff(alfa1,alfa3);
  if szd12>szd13 then Result:=-(2*pi-szd13)
  else Result:=szd13;
end;

Function Eltolas(p:TPoint2d;tav,szog:double):TPoint2d;
begin
end;

{ ELFORGATAS( pont,elforgatás centruma,szöge )}
Function Elforgatas(p,porigo:TPoint2d;szog:double):TPoint2d;
var c,s : double;
begin
  c := COS(szog); s := SIN(szog);  {szög radiánban}
  p.x := p.x - porigo.x;
  p.y := p.y - porigo.y;
  Result.x := p.x * c + p.y * s + porigo.x;
  Result.y := p.y * c - p.x * s + porigo.y;
end;

Function VisszTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
begin
end;

Function FuggTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
begin
end;

Function TengelyesTukrozes(p:TPoint2d;e:Tegyenes):TPoint2d;
begin
end;

Function KozeppontosTukrozes(p,porigo:TPoint2d):TPoint2d;
begin
end;

Function Nagyitas(p,porigo:TPoint2d;n:double):TPoint2d;
begin
  Result.x := porigo.x + (p.x-porigo.x)*n;
  Result.y := porigo.y + (p.y-porigo.y)*n;
end;

Function FelezoPont(p1,p2:TPoint2d):TPoint2d;
begin
  Result.x := (p1.x+p2.x)/2;
  Result.y := (p1.y+p2.y)/2;
end;

{A p1,p2 szakasz arany részekre osztja és az osztóponttal tér vissza.
 pl. arány = 1/4 : 0.25 a p1 ponthoz közelebbi az osztópont}
Function OsztoPont(p1,p2:TPoint2d;arany:double):TPoint2d;
begin
  Result.x := p1.x+(p2.x-p1.x)*arany;
  Result.y := p1.y+(p2.y-p1.y)*arany;
end;

{Ha a = 10e+30!, akkor az egyenes || az y tengellyek és b=x1 pl.(x=5),
 ha a=0, akkor viszont || az x tengellyel pl. (y=3)}
Function KetPontonAtmenoEgyenes(x1,y1,x2,y2:double):Tegyenesfgv;
begin
 If x1<>x2 then begin
   Result.a := (y2 - y1)/(x2 - x1);
   Result.b := y1 - (Result.a * x1);
   if Result.a>10e+6 then begin
      Result.a:=10e+30;
      Result.b := 0;
   end;
 end else
 If x1=x2 then begin
    Result.a:=10e+30;
    Result.b:=x1;
 end else
 If y1=y2 then begin Result.a:=0; Result.b:=y1; end;
end;

Function KeTPontonAtmenoEgyenes(p1,p2: TPoint2d):Tegyenesfgv; overload;
begin
  Result := KetPontonAtmenoEgyenes(p1.x,p1.y,p2.x,p2.y);
end;

Function EgypontonAtmenoMeroleges(e1:Tegyenesfgv;p1:TPoint2D):Tegyenesfgv;
begin
If (Abs(e1.a)<10e+37) and (Abs(e1.a)>10e-37) then begin
  Result.a:=-1/e1.a;
  Result.b:=p1.y-Result.a*p1.x;
end;
If (Abs(e1.a)>=10e+37) then begin
  Result.a:= 0;
  Result.b:=p1.y;
end;
If (Abs(e1.a)<=10e-37) then begin
  Result.a:= 10e+37;
  Result.b:= p1.x;
end;
end;

function PontEgyenesTavolsaga(e1:Tegyenesfgv;p:TPoint2d):double;
var e2:Tegyenesfgv;
    p1:TPoint2d;
begin
  e2 := EgypontonAtmenoMeroleges(e1,p);
  p1 := KetEgyenesMetszespontja(e1,e2);
  Result := KeTPontTavolsaga(p1.x,p1.y,p.x,p.y);
end;

{ Egy ponton átmenõ adott iránytangensû egyenes egyenletét
  adja: p1 = pont, a = iránztangens }
Function Egyenes1(p1:TPoint2d;a:double):Tegyenesfgv;
begin
  Result.a := a;
  Result.b := p1.y - (a*p1.x);
end;

{ Két ponton p1,p2 átmenõ egyenes iránytangensét adja}
Function Egyenes2(p1,p2:TPoint2d):double;
begin
 If Abs(p1.x-p2.x)>1e-30 then
  Result := (p2.y - p1.y)/(p2.x - p1.x)
 else
  Result := 1e+30
end;

{ Két egyenes metszéspontjának koord.-ját adja}
Function Egyenes12(e1,e2:Tegyenes):TPoint2d;
var ef1,ef2: Tegyenesfgv;
begin
   ef1.a := (e1.y2 - e1.y1)/(e1.x2 - e1.x1);
   ef2.a := (e2.y2 - e2.y1)/(e2.x2 - e2.x1);
   ef1.b := e1.y1 - (ef1.a * e1.x1);
   ef2.b := e2.y1 - (ef2.a * e2.x1);
   Result.x := (ef1.b - ef2.b) / (ef2.a - ef1.a);
   Result.y := ef1.a * Result.x + ef1.b;
end;

{Function KetEgyenesMetszespontja(ef1,ef2:Tegyenesfgv):TPoint2d;
begin
  If Abs(ef2.a - ef1.a)>1e-30 then begin
   Result.x := (ef1.b - ef2.b) / (ef2.a - ef1.a);
   Result.y := ef1.a * Result.x + ef1.b;
  end else
   Result:=Point2d(1e+30,1e+30);
end;}

Function KetEgyenesMetszespontja(ef1,ef2:Tegyenesfgv):TPoint2d;
begin
Try
  If (Abs(ef1.a)<1e+30) AND (Abs(ef2.a)<1e+30) AND (ef2.a <> ef1.a) then begin
   Result.x := (ef1.b - ef2.b) / (ef2.a - ef1.a);
   Result.y := ef1.a * Result.x + ef1.b;
  end else begin
   If (Abs(ef1.a)>=1e+30) and (Abs(ef2.a)<1e+30) then begin
      Result.x := ef1.b;
      Result.y := ef2.a * ef1.b + ef2.b;
   end;
   If (Abs(ef2.a)>=1e+30) and (Abs(ef1.a)<1e+30) then begin
      Result.x := ef2.b;
      Result.y := ef1.a * ef2.b + ef1.b;
   end;
   {ef1 merõleges az x tengelyre és ef2-re}
   If (Abs(ef1.a)>10000) and (Abs(ef2.a)<0.001) then begin
      Result.x := ef1.b;
      Result.y := ef2.b;
   end;
   {ef2 merõleges az x tengelyre és ef1-re}
   If (Abs(ef2.a)>10000) and (Abs(ef1.a)<0.001) then begin
      Result.x := ef2.b;
      Result.y := ef1.b;
   end;
  end;
Except
   Exit;
End;
end;

{p1,p2 irányított szakaszt d távolsággal eltolja önmagával ||-an;
       Direct = True/False akkor a szakasz jobb/bal partján }
function SzakaszParhuzamosEltolas(p1,p2:TPoint2d;d:double;Direct:boolean):TEgyenes;
var p11,p21: TPoint2d;  {Eredeti->eltolt szakasz végpontjai}
    tg_szakasz : double;
    Angle : double;
begin
   tg_szakasz := SzakaszSzog(p1.x,p1.y,p2.x,p2.y);
   p11:=p1; p21:=p2;
   Case Direct of
   True : Angle:=tg_szakasz+pi/2;
   False: Angle:=tg_szakasz-pi/2;
   end;
   Move2d(p11,Angle,d);
   Move2d(p21,Angle,d);
   Result.x1:=p11.x;
   Result.y1:=p11.y;
   Result.x2:=p21.x;
   Result.y2:=p21.y;
end;

{ Rajta van-e a pont a vonalszakaszon
  tures kb. 400 legyen }
Function Vonalonvan(e : Tegyenes; p: TPoint2d; tures: double):boolean;
var d: double;
begin
  {A pontnak az egyenestõl való távolsága = d}
  d := p.x*(e.y1-e.y2)-p.y*(e.x1-e.x2)+(e.x1*e.y2)-(e.y1*e.x2);
  if Abs(d)<=tures then Result:=True else Result:=False;
end;

Function Kozben(a,b,x,tures: double): boolean;
var k: double;
begin
  If a>b then begin
     k:=a; a:=b; b:=k;
  end;
  Result := (a-tures<=x) and (x<=b+tures);
end;

{Két kör metszéspontjait adja:
 In:     u,v,r = a kör középpontjának x,y koord.-ja és r a sugár
         p1,p2 = a metszéspontok
 Out:    0 = Ha a körök nem metszik egymást; 1-2 = a metszéspontok száma}
Function Ivmetszes(u1,v1,r1,u2,v2,r2: double;var p:TPoint2D):boolean;
var d,a,b,x,y,szog: double;
    kpx,kpy: double;
begin
     Result := False;
     d:=KetPonttavolsaga(u1,v1,u2,v2);
     szog:=SzakaszSzog(u1,v1,u2,v2);
  If HaromszogEgyenlotlenseg(d,r1,r2) and (d <= (r1+r2)) then begin
     {Origóba eltolva és x tengelyre beforgatva a szakaszt}
     x := ((d*d)-(r2*r2)+(r1*r1))/(2*d);
     y := sqrt((r1*r1)-(x*x));
     p.x := x; p.y:=y;
     Rotate2D(p,szog);            {Elforgatas a szakasz irányába}
     p.x := u1+p.x; p.y:=v1+p.y;  {Visszatolva az eredeti helyére}
     Result := True;
  end;
end;

{ A szakasz egyik végétõl r1, rá merõlegesen r2 távolságban lévõ pont helyzete
 In:     u,v,r = a kör középpontjának x,y koord.-ja és r a sugár
         p = a metszéspont}
Function Bemeres(u1,v1,r1,u2,v2,r2: double;var p:TPoint2D):boolean;
var d,szog: double;
begin
     d:=KetPonttavolsaga(u1,v1,u2,v2);
     szog:=SzakaszSzog(u1,v1,u2,v2);
     p.x := r1; p.y:=r2;
     Rotate2D(p,szog);          {Elforgatas a szakasz irányába}
     p.x := u1+p.x; p.y:=v1+p.y;
     Result := True;
end;

Function HaromszogEgyenlotlenseg(d1,d2,d3:double):boolean;
begin
  Result := (d1+d2>d3) and (d1+d3>d2) and (d3+d2>d1);
end;

function Point2D(X, Y: double): TPoint2D;
begin
  Point2D.X := X;
  Point2D.Y := Y;
end;

function Rect2D(X1, Y1, X2, Y2: double): TRect2D;
begin
  Rect2D.X1 := X1; Rect2D.X2 := X2;
  Rect2D.Y1 := Y1; Rect2D.Y2 := Y2;
end;

function RoundPoint(P: TPoint2D): TPoint;
begin
  RoundPoint.X := Round(P.X);
  RoundPoint.Y := Round(P.Y);
end;

function FloatPoint(P: TPoint): TPoint2D;
begin
  FloatPoint.X := P.X;
  FloatPoint.Y := P.Y;
end;

function Point3D(X, Y, Z: double): TPoint3D;
begin
  Point3D.X := X;
  Point3D.Y := Y;
  Point3D.Z := Z;
end;

function Angle2D(P: TPoint2D): double;
begin
  if P.X = 0 then
  begin
    if P.Y > 0 then Result := Pi / 2;
    if P.Y = 0 then Result := 0;
    if P.Y < 0 then Result := Pi / -2;
  end
  else
    Result := Arctan(P.Y / P.X);

  if P.X < 0 then
  begin
    if P.Y < 0 then Result := Result + Pi;
    if P.Y >= 0 then Result := Result - Pi;
  end;

  If Result < 0 then Result := Result + 2 * Pi;
end;

function Dist2D(P: TPoint2D): double;
var gyok: double;
begin
Try
  Result := 0;
  if (ABS(P.x)<1E+10) and (ABS(P.y)<1E+10) {and (p.x<>NAN)} then
  begin
  gyok := P.X * P.X + P.Y * P.Y;
  if gyok>0 then
     Result := Sqrt(gyok);
  end;
except
  Result := 0;
end;
end;

function Dist3D(P: TPoint3D): double;
begin
  Dist3d := Sqrt(P.X * P.X + P.Y * P.Y + P.Z * P.Z);
end;

function RelAngle2D(PA, PB: TPoint2D): double;
begin
  Result := Angle2D(Point2D(PB.X - PA.X, PB.Y - PA.Y));
end;

function RelDist2D(PA, PB: TPoint2D): double;
begin
  Result := Dist2D(Point2D(PB.X - PA.X, PB.Y - PA.Y));
end;

function RelDist3D(PA, PB: TPoint3D): double;
begin
  RelDist3D := Dist3D(Point3D(PB.X - PA.X, PB.Y - PA.Y, PB.Z - PA.Z));
end;

procedure Rotate2D(var P: TPoint2D; Angle2D: double);
var
  Temp: TPoint2D;
begin
  Temp.X := P.X * Cos(Angle2D) - P.Y * Sin(Angle2D);
  Temp.Y := P.X * Sin(Angle2D) + P.Y * Cos(Angle2D);
  P := Temp;
end;

procedure RelRotate2D(var P: TPoint2D; PCentr: TPoint2D; Angle2D: double);
var
  Temp: TPoint2D;
begin
  Temp := SubPoints(P, PCentr);
  Rotate2D(Temp, Angle2D);
  P := AddPoints(Temp, PCentr);
end;

procedure Move2D(var P: TPoint2D; Angle2D, Distance: double);
var
  Temp: TPoint2D;
begin
  Temp.X := P.X + (Cos(Angle2D) * Distance);
  Temp.Y := P.Y + (Sin(Angle2D) * Distance);
  P := Temp;
end;

function Between(PA, PB: TPoint2D; Preference: double): TPoint2D;
begin
  Between.X := PA.X * Preference + PB.X * (1 - Preference);
  Between.Y := PA.Y * Preference + PB.Y * (1 - Preference);
end;

function DistLine(A, B, C: double; P: TPoint2D): double;
begin
Try
  Result := 0;
  if (A<>0) AND (B<>0) then
  Result := (A * P.X + B * P.Y + C) / Sqrt(Sqr(A) + Sqr(B));
except
  exit;
end;
end;

function Dist2P(P, P1, P2: TPoint2D): double;
begin
  Result := DistLine(P1.Y - P2.Y, P2.X - P1.X, -P1.Y * P2.X + P1.X * P2.Y, P);
end;

function DistD1P(DX, DY: double; P1, P: TPoint2D): double;
begin
  Result := DistLine(DY, -DX, -DY * P1.X + DX * P1.Y, P);
end;

function NearLine2P(P, P1, P2: TPoint2D; D: double): Boolean;
begin
  Result := False;
  if DistD1P(-(P2.Y - P1.Y), P2.X - P1.X, P1, P) * DistD1P(-(P2.Y - P1.Y), P2.X - P1.X, P2, P) <= 0 then
    if Abs(Dist2P(P, P1, P2)) < D then Result := True;
end;

function AddPoints(P1, P2: TPoint2D): TPoint2D;
begin
  AddPoints := Point2D(P1.X + P2.X, P1.Y + P2.Y);
end;

function SubPoints(P1, P2: TPoint2D): TPoint2D;
begin
  SubPoints := Point2D(P1.X - P2.X, P1.Y - P2.Y);
end;

// Egyenlõ-e a 2 pont; d a tûrés
function IsEqualPoint2d(P1, P2: TPoint2D; d: double): boolean;
begin
  Result := KetPontTavolsaga(p1.x,p1.y,p2.x,p2.y)<=d;
end;

function Invert(Col: TColor): TColor;
begin
  Invert := not Col;
end;

function Dark(Col: TColor; Percentage: Byte): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(Col); G := GetGValue(Col); B := GetBValue(Col);
  R := Round(R * Percentage / 100);
  G := Round(G * Percentage / 100);
  B := Round(B * Percentage / 100);
  Dark := RGB(R, G, B);
end;

function Light(Col: TColor; Percentage: Byte): TColor;
var
  R, G, B: Byte;
begin
  R := GetRValue(Col); G := GetGValue(Col); B := GetBValue(Col);
  R := Round(R * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  G := Round(G * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  B := Round(B * Percentage / 100) + Round(255 - Percentage / 100 * 255);
  Light := RGB(R, G, B);
end;

function Mix(Col1, Col2: TColor; Percentage: Byte): TColor;
var
  R, G, B: Byte;
begin
  R := Round((GetRValue(Col1) * Percentage / 100) + (GetRValue(Col2) * (100 - Percentage) / 100));
  G := Round((GetGValue(Col1) * Percentage / 100) + (GetGValue(Col2) * (100 - Percentage) / 100));
  B := Round((GetBValue(Col1) * Percentage / 100) + (GetBValue(Col2) * (100 - Percentage) / 100));
  Mix := RGB(R, G, B);
end;

function MMix(Cols: array of TColor): TColor;
var
  I, R, G, B, Length: Integer;
begin
  Length := High(Cols) - Low(Cols) + 1;
  R := 0; G := 0; B := 0;
  for I := Low(Cols) to High(Cols) do
  begin
    R := R + GetRValue(Cols[I]);
    G := G + GetGValue(Cols[I]);
    B := B + GetBValue(Cols[I]);
  end;
  R := R div Length;
  G := G div Length;
  B := B div Length;
  MMix := RGB(R, G, B);
end;

function Log(Base, Value: double): double;
begin
  Log := Ln(Value) / Ln(Base);
end;

function GeomPower(Base, Exponent: double): double;
begin
  Result := Ln(Base) * Exp(Exponent);
end;

function Modulator(Val, Max: double): double;
begin
  Modulator := (Val / Max - Round(Val / Max)) * Max;
end;

function M(I, J: Integer): Integer;
begin
  M := ((I mod J) + J) mod J;
end;

function Tan(Angle2D: double): double;
begin
  Tan := Sin(Angle2D) / Cos(Angle2D);
end;

procedure Limit(var Value: Integer; Min, Max: Integer);
begin
  if Value < Min then Value := Min;
  if Value > Max then Value := Max;
end;

function Exp2(Exponent: Byte): Word;
var
  Temp, I: Word;
begin
  Temp := 1;
  for I := 1 to Exponent do
    Temp := Temp * 2;
  Result := Temp;
end;

function GetSysDir: String;
var
  Temp: array[0..255] of Char;
begin
  GetSystemDirectory(Temp, 256);
  Result := StrPas(Temp);
end;

function GetWinDir: String;
var
  Temp: array[0..255] of Char;
begin
  GetWindowsDirectory(Temp, 256);
  Result := StrPas(Temp);
end;

{   Egyenes vágó algoritmus:
    Meghatározza egy szakasznak a képernyõre esõ részét
    xi,yi : a szakasz végpontjai,
    t     : a metszendõ téglalap alakú terület
}
Function Clip(var x1,y1,x2,y2:double;t:TRect2D):boolean;
 label return;
 var  c,c1,c2: kinnkod; x,y: double;

procedure Kod(x,y:double; var c :kinnkod);
begin
  c:=[ ];
  If x<xbal then c:=[bal] else if x>xjobb then c:=[jobb];
  If y<yalso then c:=c+[also] else if y>yfelso then c:=c+[felso];
end;

begin
  Result:=False;
  xbal:=t.x1; xjobb:=t.x2;
  yalso:=t.y1; yfelso:=t.y2;
    Kod(x1,y1,c1); Kod(x2,y2,c2);
  while (c1<>[ ]) or (c2<>[ ]) do begin
     If (c1*c2)<>[ ] then goto return;
     c:=c1; if c=[ ] then c:=c2;
     If bal in c then begin {metszés a bal élen}
        y:=y1+(y2-y1)*(xbal-x1)/(x2-x1);
        x:=xbal end else
     If jobb in c then begin {metszés a jobb élen}
        y:=y1+(y2-y1)*(xjobb-x1)/(x2-x1);
        x:=xjobb end else
     If also in c then begin {metszés az alsó élen}
        x:=x1+(x2-x1)*(yalso-y1)/(y2-y1);
        y:=yalso end else
     If felso in c then begin {metszés a felsõ élen}
        x:=x1+(x2-x1)*(yfelso-y1)/(y2-y1);
        y:=yfelso end;
     if c=c1 then begin
        x1:=x;y1:=y;Kod(x,y,c1)
     end else begin
        x2:=x;y2:=y;Kod(x,y,c2)
     end;
  end;
  Result:=True;
return: end;

{ Van-e a két szakasznak metszéspontja?  }
function SzakaszSzakaszMetszes(p11,p12,p21,p22:TPoint2D; var mp:TPoint2d):boolean;
var e1,e2: Tegyenesfgv;
    s: TEgyenes;
    d: double;

    function Egyenes(p1,p2: TPoint2d): TEgyenes;
    begin
      Result.P1 := p1;
      Result.P2 := p2;
    end;

begin
  Result := False;
  d:=10e-2;

  if IsEqualPoint2d(p11,p21,d) and (not IsEqualPoint2d(p12,p22,d)) then exit;
  if IsEqualPoint2d(p11,p22,d) and (not IsEqualPoint2d(p12,p21,d)) then exit;
  if IsEqualPoint2d(p12,p21,d) and (not IsEqualPoint2d(p11,p22,d)) then exit;
  if IsEqualPoint2d(p12,p22,d) and (not IsEqualPoint2d(p12,p21,d)) then exit;

  e1 := KeTPontonAtmenoEgyenes(p11.x,p11.y,p12.x,p12.y);
  e2 := KeTPontonAtmenoEgyenes(p21.x,p21.y,p22.x,p22.y);
  if e1.a<>e2.a then begin  // Ha nem párhuzamosak
     mp  := KetEgyenesMetszespontja(e1,e2);
//     Result := Vonalonvan( Egyenes(p11,p12),mp,0.1);
     Result := PontInKep(mp.x,mp.y,Rect2d(p11.x,p11.y,p12.x,p12.y))
            and PontInKep(mp.x,mp.y,Rect2d(p21.x,p21.y,p22.x,p22.y));
  end;
end;

function SzakaszSzakaszMetszes(Rec1,Rec2:TRect2D; var mp:TPoint2d):boolean;
begin
  Result := SzakaszSzakaszMetszes(Point2d(Rec1.x1,Rec1.y1),Point2d(Rec1.x2,Rec1.y2),
                        Point2d(Rec2.x1,Rec2.y1),Point2d(Rec2.x2,Rec2.y2),mp);
end;


{   Szakasz vágó rutin:
    Megvizsgálja, hogy a szakasz metszi-e a t téglalap alakú területet.
    Ha igen -> meghatározza a bele esõ szakasz végpontjait: p1,p2
    és True értékkel tér vissza
}
Function SzakaszNegyszogMetszes(var p1,p2:TPoint2D;t:TRect2D):boolean;
label return;
var k1,k2: kinnkod;
begin
  Result:=False;
  k1:=[]; k2:=[];
  {Vizsgálat a t 4 élére, hogy kivül esik-e a szakasz}
  If (p1.x<t.x1) and (p2.x<t.x1) then goto return;
  If (p1.x>t.x2) and (p2.x>t.x2) then goto return;
  If (p1.y<t.y1) and (p2.y<t.y1) then goto return;
  If (p1.y>t.y2) and (p2.y>t.y2) then goto return;
  {A szakasz teljesen a képernyõn van}
  {egyébként vágni kell}
  Result := Clip(p1.x,p1.y,p2.x,p2.y,t);
return:
end;

//  Csak azt vizsgálja, hogy van e p1,p2 szkasznak a t téglalappal metszése
Function IsSzakaszNegyszogMetszes(p1,p2:TPoint2D;t:TRect2D):boolean;
var pp1,pp2: TPoint2D;
begin
  pp1 := p1; pp2 := p2;
  Result := SzakaszNegyszogMetszes(pp1,pp2,t);
end;

{Egy térképi pont rajta van-e a képterületen?}
Function PontInKep(x,y:double;t:TRect2D):boolean;
var tt: TRect2D;
begin
Try
  Result:=False;
  tt:=CorrectRealRect(t);
  if tt.x1 = tt.x2 then
  begin
    tt.x1 := tt.x1 - 0.01;
    tt.x2 := tt.x2 + 0.01;
  end;
  if tt.y1 = tt.y2 then
  begin
    tt.y1 := tt.y1 - 0.01;
    tt.y2 := tt.y2 + 0.01;
  end;
  If (x>=tt.x1) and (x<=tt.x2) and (y>=tt.y1) and (y<=tt.y2) then
     Result:=True;
except
End;
end;

{Elõállítja a kör egyenlet 0-ra redukált alakját}
Function SetKorfgv(u,v,r:double):Tkorfgv;
begin
  With Result do begin
    d := -2*u; e := -2*v; f := (4*sqr(r)-sqr(d)-sqr(e))/4;
  end;
end;

{Másodfokú egyenlet két megoldása: a,b,c egyenlet paraméterek,
           Result: a megoldások száma}
Function Masodfoku(a,b,c:double;var p12: TPoint2d):integer;
var d: double;
begin
  d := sqr(b)-4*a*c;
  IF d<0 then Result:=0;
  IF d=0 then begin
     Result:=1;
     p12.x := -b/(2*a);
     p12.y := p12.x;
  end;
  IF d>0 then begin
     Result:=2;
     d := sqrt(d);
     p12.x := (-b+d)/(2*a);
     p12.y := (-b-d)/(2*a);
  end;
end;


{Van-e kör-egyenes metszés és hány ponton}
Function IsKorEgyenesMetszes(u,v,r:double; a,b: double):boolean;
var kor: TKorfgv;
    x12: TPoint2d;
begin
Try
If Abs(a)<10e+20 then begin
  kor := SetKorfgv(u,v,r);
  Result := Masodfoku(sqr(a)+1, 2*a*b+kor.d+a*kor.e, b*(b+kor.e)+kor.f, x12)>0;
end else If (u-r<=b) and (u+r>=b) then Result:=True;
except
  Result := False;
end;
end;

{Viysgálja hogy az u,v középpontú r sugarú négyzeten a p szakasz áthalad-e}
Function IsAblakSzakaszMetszes(u,v,r:double; p: TRect2d):boolean;
var ve : TEgyenesfgv;
    x12: TPoint2d;
    x1,y1,x2,y2: double;
label return;
begin
  Result := False;
  If (p.x1<u-r) and (p.x2<u-r) then goto return;
  If (p.x1>u+r) and (p.x2>u+r) then goto return;
  If (p.y1<v-r) and (p.y2<v-r) then goto return;
  If (p.y1>v+r) and (p.y2>v+r) then goto return;
  ve := KeTPontonAtmenoEgyenes(p.x1,p.y1,p.x2,p.y2);
  If Abs(ve.a)>10e+3 then begin
     Result:=Kozben(u-r,u+r,ve.b,0); goto return;
  end;
  If Abs(ve.a)<0.01 then
     Result:=Kozben(v-r,v+r,ve.b,0)
  else begin
     Result:=Kozben(u-r,u+r,((v-r)-ve.b)/ve.a,0);
     If Result then goto return;
     Result:=Kozben(u-r,u+r,((v+r)-ve.b)/ve.a,0);
     If Result then goto return;
     Result:=Kozben(v-r,v+r,ve.a*(u-r)+ve.b,0);
     If Result then goto return;
     Result:=Kozben(v-r,v+r,ve.a*(u+r)+ve.b,0);
  end;
return:end;

{Kör és egyenes metszése:
     In : u,v,r kör középpont x,y és sugara,
          a,b   az egyenes egyenletének paraméterei
          p12   a metszéspontok rekordja
     Result: a megoldások száma}
Function KorEgyenesMetszes(u,v,r:double; a,b: double;var p12:TRect2d):integer;
var kor: TKorfgv;
    p1,p2,p3,c: double;
    x12: TPoint2d;
begin
  kor := SetKorfgv(u,v,r);
  c  := -1;
  Result := Masodfoku(sqr(a)+1, 2*a*b+kor.d+a*kor.e, b*(b+kor.e)+kor.f, x12);
  Case Result of
  1: begin
          p12.x1 := x12.x;
          p12.x2 := x12.x;
          p12.y1 := x12.x; p12.y2 := x12.x;
     end;
  2: begin
          p12.x1 := x12.x;
          p12.x2 := x12.x;
          p12.y1 := x12.x;
          p12.y2 := x12.x;
     end;
  end;
end;

{3 ponton átmenõ kör értékeit adja: (u,v,r),
   ha a 3 pont egy egyenesre esik, akkor:
      Result=felezõpont,x,y; a sugár pedig = -1 }
Function HaromPontbolKor(p1,p2,p3:TPoint2D):TPoint3D;
var e1,e2 : Tegyenesfgv;
    m1,m2 : Tegyenesfgv;
    f1,f2 : TPoint2d;
    c     : TPoint2d;
begin
{ If ((p1.x=p2.x) and (p2.x=p3.x)) or ((p1.x=p2.x) and (p2.x=p3.x)) then begin}
 Try
   e1 := KeTPontonAtmenoEgyenes(p1.x,p1.y,p2.x,p2.y);
   e2 := KeTPontonAtmenoEgyenes(p3.x,p3.y,p2.x,p2.y);
   f1 := FelezoPont(p1,p2);
   m1 := EgypontonAtmenoMeroleges(e1,f1);
   f2 := FelezoPont(p3,p2);
   m2 := EgypontonAtmenoMeroleges(e2,f2);
   c  := KetEgyenesMetszespontja(m1,m2);
   Result.x := c.x;
   Result.y := c.y;
   Result.z := RelDist2D(c,p1);
 except
   Result.x := Felezopont(p1,p3).x;
   Result.y := Felezopont(p1,p3).y;
   Result.z := -1;
 end;
end;

{Körív rajtolás: Ca canvasra, p1,p2,p3 pontokon megy át}
procedure KorivRajzol(Ca:TCanvas;pp1,pp2,pp3:TPoint2D);
var c:TPoint3D;
    alfa1,alfa2,alfa3:double;
    alf1,alf2,alf3:double;
begin
 Try
  c:=HaromPontbolKor(pp1,pp2,pp3);
  If (c.z>0) and (c.y<MaxInt) then begin
{  Ca.Ellipse(Trunc(c.x-2),Trunc(c.y-2),Trunc(c.x+2),Trunc(c.y+2));}

  alfa1 := RelAngle2D(Point2d(c.x,c.y),pp1);
  alfa2 := RelAngle2D(Point2d(c.x,c.y),pp2);
  alfa3 := RelAngle2D(Point2d(c.x,c.y),pp3);

  If ((alfa1<alfa2) and (alfa2<alfa3))
     or ((alfa3>alfa2) and (alfa1>alfa3))
     or ((alfa1<alfa2) and (alfa3<alfa1))
  then
     Ca.Arc(Trunc(c.x-c.z),Trunc(c.y-c.z),Trunc(c.x+c.z),Trunc(c.y+c.z),
                   Trunc(pp3.x),Trunc(pp3.y),Trunc(pp1.x),Trunc(pp1.y))
  else
     Ca.Arc(Trunc(c.x-c.z),Trunc(c.y-c.z),Trunc(c.x+c.z),Trunc(c.y+c.z),
                   Trunc(pp1.x),Trunc(pp1.y),Trunc(pp3.x),Trunc(pp3.y));
  end else begin
     Ca.Moveto(Trunc(pp1.x),Trunc(pp1.y));
     Ca.Lineto(Trunc(pp3.x),Trunc(pp3.y));
  end;
 except
  exit;
 end;
end;

function KorivbolHarompont(u,v,r,StartAngle,endAngle:double):T3Point2d;
VAR felszog : double;
begin
  Result.p1 := Point2d(u+R*COS(StartAngle),v+R*SIN(StartAngle));
  If StartAngle<EndAngle then
     felszog := StartAngle+SzogDiff(StartAngle,endAngle)/2
  else
     felszog := StartAngle+(EndAngle+(2*pi-StartAngle))/2;
  Result.p2 := Point2d(u+R*COS(felszog),v+R*SIN(felszog));
  Result.p3 := Point2d(u+R*COS(endAngle),v+R*SIN(endAngle));
end;

{p1,p2 a teglalap egy oldala fix, pk=külsõ futópont a || oldalon}
Function HaromPontbolTeglalap(p1,p2,pk:TPoint2D):TTeglalap;
var e1,e2,ek : Tegyenesfgv;
    m1,m2 : Tegyenesfgv;
    alfa  : double;
begin
   Result.a := p1;
   Result.b := p2;
 If (p1.y <> p2.y) and (p1.x <> p2.x) then begin
   e1 := KeTPontonAtmenoEgyenes(p1.x,p1.y,p2.x,p2.y);
   alfa := Egyenes2(p1,p2);
   ek := Egyenes1(pk,alfa);
   m1 := EgypontonAtmenoMeroleges(e1,p1);
   m2 := EgypontonAtmenoMeroleges(e1,p2);
   Result.c := KetegyenesMetszespontja(m2,ek);
   Result.d := KetegyenesMetszespontja(m1,ek);
 end
 else begin
   {Ha a bázisvonal || az x tengellyel}
   If p1.y = p2.y then begin
      Result.c := Point2d(p2.x,p2.y+(pk.y-p2.y));
      Result.d := Point2d(p1.x,p1.y+(pk.y-p1.y));
   end;
   {Ha a bázisvonal || az y tengellyel}
   If p1.x = p2.x then begin
      Result.c := Point2d(p2.x+(pk.x-p2.x),p2.y);
      Result.d := Point2d(p1.x+(pk.x-p1.x),p1.y);
   end;
 end;
end;

{Viysgálja hogy az u,v középpontú r sugarú négyzeten a p befoglalójú
 ellipszis kerületi íve áthalad-e}
Function IsAblakEllipszisMetszes(u,v,r:double; p: TRect2d):boolean;
var a,b,ux,uy,y: double;
    pp: TRect2d;
begin
  pp:=CorrectRealRect(p);
  If PontInKep(u,v,pp) then
  begin
  a := (pp.x2-pp.x1)/2;   {ellipszis félnagytengelye = a}
  b := (pp.y2-pp.y1)/2;   {ellipszis félkistengelye = b}
  ux := u-(pp.x1+a);      {Keresõ pont eltolva}
  uy := v-(pp.y1+b);
  y  := b*sqrt(1-sqr(ux/a)); {ell.pont y értéke a keresõ pont x értéke mellett}
  Result := PontInKep(ux,y,Rect2d(ux-r,uy-r,ux+r,uy+r));
  If not result then Result := PontInKep(ux,-y,Rect2d(ux-r,uy-r,ux+r,uy+r));
  end else Result:=False;
end;

{Viysgálja hogy az u,v középpontú r sugarú négyzeten a p téglalap
 kerületi vonala áthalad-e}
Function IsAblakNegyszogMetszes(u,v,r:double; p: TRect2d):boolean;
Var pp: TRect2d;
begin
  pp:=CorrectRealRect(p);
  Result := PontInKep(u,v,Rect2d(pp.x1-r,pp.y1-r,pp.x2+r,pp.y2+r)) and
     not PontInKep(u,v,Rect2d(pp.x1+r,pp.y1+r,pp.x2-r,pp.y2-r));
end;

    {Normal rectangle vizsgálata és átalakítás: bal alsó-jobb felsõ sarokká.
    pl Rect(-1,4,5,-3) => Rect(-1,-3,5,4)}
    function CorrectRealRect(t:TRect2D):TRect2D;
    var k: double;
    begin
      result:=t;
      With Result do begin
        If x1>x2 then begin k:=x1; x1:=x2; x2:=k; end;
        If y1>y2 then begin k:=y1; y1:=y2; y2:=k; end;
      end;
    end;

    function CorrectRect(t:TRect):TRect;
    var k: integer;
    begin
      result:=t;
      With Result do begin
        If Left>Right then begin k:=Left; Left:=Right; Right:=k; end;
        If Top>Bottom then begin k:=Top; Top:=Bottom; Bottom:=k; end;
      end;
    end;

{ Poligon befoglaló téglalap meghatározása
  pSTM stream TPoint2d alapban tartalmazza a poligon csúcspontjait;
  Result = befoglaló téglalap}
Function GetPoligonRect(pSTM:TMemoryStream):TRect2D;
var i,meret: longint;
    p  : TPoint2d;
    MinX,MaxX,MinY,MaxY: double;
begin
  MinX := MaxReal;
  MaxX := MinReal;
  MinY := MaxReal;
  MaxY := MinReal;
  pSTM.Seek(0,0);
  meret := pSTM.size div SizeOf(TPoint2d);
  For i:=1 to meret do begin
      pSTM.Read(p,SizeOf(TPoint2d));
      If p.x<MinX then MinX:=p.x;
      If p.x>MaxX then MaxX:=p.x;
      If p.y<MinY then MinY:=p.y;
      If p.y>MaxY then MaxY:=p.y;
  end;
  Result := Rect2D(MinX,MinY,MaxX,MaxY);
end;

{Vizsgálja, hogy a p pont a pStm stream-en tárolt (TPoint2d rekordok)
 poligon belsejében helyezkedik-e el}
function IsPointInPoligon(pStm:TMemoryStream;p:TPoint2D):boolean;
var j  : longint;
    pCrossPoint: TPoint2d;
    p1,p2: TPoint2d;
    E: TEgyenesFgv;
    AboveCount : integer;
begin
   Result := False;
   AboveCount := 0;
   pSTM.Seek(0,0);
   pSTM.Read(p1,SizeOf(TPoint2d));
   For j:=2 to (pSTM.Size div SizeOf(TPoint2d)) do begin
       pSTM.Read(p2,SizeOf(TPoint2d));
       If Kozben(p1.x,p2.x,p.x,0) then begin
          E := KetpontonAtmenoEgyenes(p1.x,p1.y,p2.x,p2.y);
          pCrossPoint.y := E.a*p.x+E.b;
          If pCrossPoint.y > p.y then Inc(AboveCount);
       end;
       p1 := p2;
   end;
   Result := (AboveCount mod 2)=1;
end;

function IsPointInPoligon(aList: Array of TPoint2d; p: TPoint2D):boolean;
Type
   PPoint = ^TPoint2d;
var j  : longint;
    pCrossPoint: TPoint2d;
    p1,p2: TPoint2d;
    E: TEgyenesFgv;
    AboveCount : integer;
begin
   Result := False;
   AboveCount := 0;
   p1 := PPoint(@aList[0])^;
   For j:=Low(Alist)+1 to High(aList) do begin
       p2 := PPoint(@aList[j])^;
       If Kozben(p1.x,p2.x,p.x,0) then begin
          E := KetpontonAtmenoEgyenes(p1.x,p1.y,p2.x,p2.y);
          pCrossPoint.y := E.a*p.x+E.b;
          If pCrossPoint.y > p.y then Inc(AboveCount);
       end;
       p1 := p2;
   end;
   Result := (AboveCount mod 2)=1;
end;


function GetPoligonArea(pStm:TMemoryStream):double;
var j  : longint;
    p1,p2: TPoint2d;
begin
   Result := 0;
   pSTM.Seek(0,0);
   pSTM.Read(p1,SizeOf(TPoint2d));
   For j:=2 to (pSTM.Size div SizeOf(TPoint2d)) do begin
       pSTM.Read(p2,SizeOf(TPoint2d));
       Result := Result + (p2.x-p1.x)*(p2.y+p1.y)/2;
       p1 := p2;
   end;
   Result := Abs(Result);
end;

{Meghatározza a poligon körüljárási (sodrási) irányát:
  True : direkt (óramutató járásával ellentétes) körüljárás
  False: indirekt (óramutató járásával egyezõ) körüljárás
}
function IsDirectPoligon(pStm:TMemoryStream):boolean;
Var szog1,szog2,sz : double;
    p1,p2,p3,p4 : TPoint2d;
begin
   {A kiinduló él irányszögének meghatározása}
   pSTM.Seek(0,0);
   pSTM.Read(p1,SizeOf(TPoint2d));
   pSTM.Read(p2,SizeOf(TPoint2d));
   Szog1 := SzakaszSzog(p1.x,p1.y,p2.x,p2.y);
   {A befutó él irányszögének meghatározása}
   pSTM.Seek(-2*SizeOf(TPoint2d),2);          {Utolsó pont kiolvasása}
   pSTM.Read(p3,SizeOf(TPoint2d));
   pSTM.Read(p4,SizeOf(TPoint2d));
   If (p4.x<>p1.x) or (p4.y<>p1.y) then begin
       p3:=p4; p4:=p1;
   end;  {Zárt/nyitott a poligon?}
   Szog2 := SzakaszSzog(p4.x,p4.y,p3.x,p3.y);
   sz := Szogdiff(szog1,szog2);
   Result := sz<pi;
end;

// A poligon max. y értékéhez tartozó Pymax, ill. a sorrendben elõzõ és
// következõ pontja alapján meghatározza a poligon körüljárási irányát
// a ymax pontba befutó élek szögei alapján
function IsDirectPoligon(Pprior,Pymax,Pnext: TPoint2d):boolean;
Var szog1,szog2,sz : double;
    p1,p2,p3,p4 : TPoint2d;
begin
   {A kiinduló él irányszögének meghatározása}
   Szog1 := SzakaszSzog(Pprior.x,Pprior.y,Pymax.x,Pymax.y);
   {A befutó él irányszögének meghatározása}
   Szog2 := SzakaszSzog(Pnext.x,Pnext.y,Pymax.x,Pymax.y);
   sz := Szogdiff(szog1,szog2);
   Result := sz>=pi;
end;

// A fgv. azt a szögtartományt adja vissza (Max,Min), melyben a poligon látszik
//   egy külsõ p pontból.
//   maxIndex,minIndex : a max. ill. min. látószöghöz tartozó pontok indexe
// Ha belsõ pont => False értékkel tér vissza (nem hajtható végre a számítás!)
function PoligonLatoszog(aList: Array of TPoint2d; p: TPoint2D;
                                var maxIndex,minIndex: integer;
                                var MaxAngle, MinAngle: double):boolean;
Type
   PPoint = ^TPoint2d;
var j: integer;
    oldDir,Dir: boolean;
    p1,p2: TPoint2d;
    alapszog,szog,diff: double;
begin
   maxIndex := 0;
   minIndex := 0;
   MaxAngle := 0;
   MinAngle := 0;
//   Result := not IsPointInPoligon(aList,p);
   Result := True;
   if Result then begin
      oldDir     := True;
      p1 := PPoint(@aList[0])^;
      alapszog := RelAngle2d(p,p1);
      For j:=1 to High(aList) do begin
          p2 := PPoint(@aList[j])^;
          szog := RelAngle2d(p,p2);
          diff := RelSzogDiff(alapszog,szog);
             if diff>=0 then
                if diff>MaxAngle then begin
                   MaxAngle:=diff;
                   maxIndex:=j;
                end;
             if diff<0 then
                if diff<MinAngle then begin
                   MinAngle:=diff;
                   minIndex:=j;
                end;
          if oldDir<>Dir then begin
             oldDir := Dir;
          end;
      end;
      if MaxAngle=0 then MaxAngle:=alapszog
      else
      MaxAngle := alapszog + MaxAngle;
      if MinAngle=0 then MinAngle:=alapszog
      else
      MinAngle := alapszog + MinAngle;
   end;
end;

{Egy P(x,y,z) térbeli koordinátáit képernyõpont Pk(newx,newy)-á átszámítja}
Procedure d3Coord(x, y, z : Real; {coordinates} a, b : Real; {View angles}
                 Var newx, newy : Integer); {return coordinates}
Var
  Xd, Yd, Zd : Real;
begin
  Xd := cos(a * pi / 180) * cos(b * pi / 180);
  Yd := cos(b * pi / 180) * sin(a * pi / 180);
  Zd := -sin(b * pi / 180);
  {Set coordinates For X/Y system}
  If (zd+x)<>0 then
     newx:= round(-z * Xd / Zd + x)
  else newx:=0;
  If (zd+y)<>0 then
     newy:= round(-z * Yd / Zd + y)
  else newy:=0;
end;

{ RotEllipse = Elforgatott ellipszis;
  ca         : Canvas
  porigo     : középpont koordináták
  a,b        : fél nagy és kis tengely hossza
  szog       : elforgatás szöge rad-ban }
procedure RotEllipse(ca:TCanvas;porigo:TPoint;a,b:integer;szog:double);
begin
   RotEllipseArc(ca,porigo,a,b,szog,0,360);
end;

{Csak az elforgatott ellipszis fi1,f2 közötti ívet rajzolja meg}
procedure RotEllipseArc(ca:TCanvas;porigo:TPoint;a,b:integer;szog:double;
                        fi1,fi2:integer);
var i:integer;
    p,p1,p2 : TPoint2d;
    j,si,co: real;
begin
    For i:=fi1 to fi2 do begin
        j:=i*pi/180;
        p1.x:=a*sin(j); p1.y:=b*cos(j);
        p2.x:=a*sin((i+1)*pi/180); p2.y:=b*cos((i+1)*pi/180);
        p.x:=0; p.y:=0;
        p1:=Elforgatas(p1,p,szog);
        p2:=Elforgatas(p2,p,szog);
        ca.MoveTo(Trunc(porigo.x+p1.x),Trunc(porigo.y+p1.y));
        ca.LineTo(Trunc(porigo.x+p2.x),Trunc(porigo.y+p2.y));
    end;
end;

{Az R sugarú gömb tengelyét rajzolja meg: theta szöggel oldal irányban,
 fi szöggel pedig a látóirányban megdöntött. [radiánban]}
Function GlobeAxis(ca:TCanvas;porigo:TPoint;R:integer;theta,fi:double):TRect;
var Rp: integer;   {Fél tengely hossza}
    xp,yp: integer;{Fél tengely origótól való eltérése}
begin
   Rp := Trunc(R * cos(fi));
   xp := Trunc(Rp * sin(theta));
   yp := Trunc(Rp * cos(theta));
   ca.MoveTo(porigo.x-xp,porigo.y-yp);
   ca.LineTo(porigo.x+xp,porigo.y+yp);
   Result:=Rect(porigo.x-xp,porigo.y-yp,porigo.x+xp,porigo.y+yp);
end;

procedure GlobeSzelessegiKor(ca:TCanvas;porigo:TPoint;R:integer;
                             theta,fi:double;
                             delta:double);
var a,b: integer;  {Ferde ellipszis nagy-kis féltengelyeinek hossza}
    Rp: double;      {Fél tengely hossza}
    xp,yp: double;   {Fél tengely origótól való eltérése}
    xd,yd: integer;  {A szélességi kör középpontja}
    p: Tpoint2d;
    lp: double;
    epszilon: double;
    deltaMax,deltaMin: double;   {Szélességi kör max és min}
    deltafok : double;           {Szélességi kör fokokban}
begin
   {a szélességi kör limitek meghatározása}
   if (fi>=0) then begin
    deltaMax := 90;
    deltaMin := -90 + RadToDeg(fi);
   end else begin
    deltaMax := 90 + RadToDeg(fi);
    deltaMin := -90;
   end;
   deltafok := RadToDeg(delta);
If Kozben(deltaMin,deltaMax,deltafok,0) then begin
   a  := Trunc(R * cos(delta));  {szélességi kör fél nagytengelye}
   b  := Trunc(a * sin(fi));     {szélességi kör fél kistengelye}
   Rp := R * cos(fi);            {gömb fél tengelyének hossza}
   yp := Rp * sin(delta);        {szél.kör középpontjának távolsága a gömb középpontjától }
   p  := Elforgatas(Point2D(porigo.x,porigo.y-yp),Point2D(porigo.x,porigo.y),theta);
   {Elforgatott szél.kör középpontja}
   xd := Trunc(p.x);
   yd := Trunc(p.y);
   ca.rectangle(xd-2,yd-2,xd+2,yd+2);
   {A látható kistengely hossza}
   lp := yp * tan(fi);

   If Abs(lp)>=Abs(a) then
    RotEllipse(ca,Point(xd,yd),a,b,theta)
   else begin
    epszilon := RadToDeg(arcsin(lp/a));
    epszilon := 90+epszilon;
(*    If fi>=0 then
        If (delta>=0) then epszilon := 90+epszilon
        else epszilon := 90-epszilon
    else
        If (delta>=0) then epszilon := 90+epszilon
        else epszilon := 90-epszilon;*)
    RotEllipseArc(ca,Point(xd,yd),Round(a),Round(b),theta,0,Round(Epszilon));
    RotEllipseArc(ca,Point(xd,yd),Round(a),Round(b),theta,360-Round(epszilon),360);
   end;

end;
end;

{ELÕMETSZÉS: in  : a,b a bázisvonal két végpontja,
                   alfa,béta a végpontokból mért irányszög
             out : a keresett pont térképi koordinátái
}
Function  Elometszes(a,b:TPoint2D;alfa,beta:real):TPoint2D;
begin
  Result.x := a.x+((b.x-a.x)*cot(alfa)-(b.y-a.y))/(cot(alfa)+cot(beta));
  Result.y := a.y+((b.y-a.y)*cot(alfa)+(b.x-a.x))/(cot(alfa)+cot(beta));
end;

function RectInRect2D(OutRect,InRect: TRect2D): boolean;
Var R0,R : TRect2D;
begin
  R0 := CorrectRealRect(OutRect);
  R  := CorrectRealRect(InRect);
  Result := (R.x1>=R0.x1) and (R.x2<=R0.x2) and (R.y1>=R0.y1) and (R.y2<=R0.y2);
end;

function IntersectRect2D(Rect1,Rect2: TRect2D): boolean;
Var R1,R2 : TRect2D;
begin
  R1 := CorrectRealRect(Rect1);
  R2 := CorrectRealRect(Rect2);
  Result := not ((r1.x1 > r2.x2) or (r2.x1 > r1.x2) or
                 (r1.y1 > r2.y2) or (r2.y1 > r1.y2));
(*
  Result := PontInKep(r1.x1,r1.y1,R2) or PontInKep(r1.x2,r1.y1,R2) or
            PontInKep(r1.x1,r1.y2,R2) or PontInKep(r1.x2,r1.y2,R2) or
            PontInKep(r2.x1,r2.y1,R1) or PontInKep(r2.x2,r2.y1,R1) or
            PontInKep(r2.x1,r2.y2,R1) or PontInKep(r2.x2,r2.y2,R1); *)
end;

// Minden szöget 0..360 közötti tartományba konvertál
function AdjustAngle(sz: double): double;
    begin
      Result := 360*Frac(sz/360);
      if Result<0 then Result := 360+Result;
    end;

procedure DrawTegla(Ca: TCanvas; t: TTeglalap);
begin
  Ca.MoveTo(Round(t.a.x),Round(t.a.y));
  Ca.LineTo(Round(t.b.x),Round(t.b.y));
  Ca.LineTo(Round(t.c.x),Round(t.c.y));
  Ca.LineTo(Round(t.d.x),Round(t.d.y));
  Ca.LineTo(Round(t.a.x),Round(t.a.y));
end;

  { Téglalap elforgatása a Cent-rumja körül: w,h : szélesség,hosszúság}
function RotateTegla(Cent: TPoint2d; w,h : double; Angle: double): TTeglalap;
begin
  with Result do begin
  a:=Point2d(Cent.x-w/2,Cent.y+h/2);
  RelRotate2d(a,Cent,Angle);
  b:=Point2d(Cent.x+w/2,Cent.y+h/2);
  RelRotate2d(b,Cent,Angle);
  c:=Point2d(Cent.x+w/2,Cent.y-h/2);
  RelRotate2d(c,Cent,Angle);
  d:=Point2d(Cent.x-w/2,Cent.y-h/2);
  RelRotate2d(d,Cent,Angle);
  end;
end;

  { Téglalap elforgatása külsõ Cent-rum körül: Angle szögdiff. radiánban}
procedure RotateTegla(var tegla: TTeglalap; Cent: TPoint2d; Angle: double); overload;
begin
  with tegla do begin
  RelRotate2d(a,Cent,Angle);
  RelRotate2d(b,Cent,Angle);
  RelRotate2d(c,Cent,Angle);
  RelRotate2d(d,Cent,Angle);
  end;
end;

procedure MullPoint3d(var p: TPoint3d; coeff: double);
    begin
      p.X := p.x * coeff;
      p.y := p.y * coeff;
      p.z := p.z * coeff;
    end;

procedure InitdPoints;
begin
  if dPoints=nil then
     dPoints := TList.Create;
  dPoints.Clear;
end;

{************* LinearLeastSquares *******************}
 function LinearLeastSquares(data: TPoint2DArray; var M,B, R: double): boolean;
 {Line "Y = mX + b" is linear least squares line for the input array, "data",
  of TRealPoint. R : Correlation Coefficient (0..1).
  Result = False then error}
var
  SumX, SumY, SumX2, SumY2, SumXY: extended;
  Sx,Sy :extended;
  n, i: Integer;
begin
  n := Length(data); {number of points}
  SumX := 0.0;  SumY := 0.0;
  SumX2 := 0.0;  SumY2:=0.0;
  SumXY := 0.0;

  for i := 0 to n - 1 do
  with data[i] do
  begin
    SumX := SumX + X;
    SumY := SumY + Y;
    SumX2 := SumX2 + X*X;
    SumY2 := SumY2 + Y*Y;
    SumXY := SumXY + X*Y;
  end;

  if (n*SumX2=SumX*SumX) or (n*SumY2=SumY*SumY)
  then
  begin
//    showmessage('LeastSquares() Error - X or Y  values cannot all be the same');
    Result := False;
    M:=0;
    B:=0;
  end
  else
  begin
    M:=((n * SumXY) - (SumX * SumY)) / ((n * SumX2) - (SumX * SumX));  {Slope M}
    B:=(sumy-M*sumx)/n;  {Intercept B}
    Sx:=sqrt(Sumx2-sqr(sumx)/n);
    Sy:=Sqrt(Sumy2-sqr(Sumy)/n);
    r:=(Sumxy-Sumx*sumy/n)/(Sx*sy);
    Result := True;
    //RSquared:=r*r;
  end;
end;

initialization
   dPoints := TList.Create;
finalization
   dPoints.Free;
end.


