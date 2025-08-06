
{
  Besier és B_spline görbék unitja (DELPHI 1.0)
  Author : Agócs László 2002 @StellaSoft
}

unit Bspline_New;

interface
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, NewGeom;

type

 TBSplineDrawMod = (bspNone,bspFrame,bspCorners,bspFrameCorners);

 TBSplineAlgoritm = (bsaBezier,
                     bsaBSplinePeriodic,
                     bsaBSplineNonPeriodic,
                     bsaRomSplinePeriodic,
                     bsaRomSplineNonPeriodic
                     );

 TBSplineMod =         {BSpline szerkesztési mód}
      (bsmNone,
       bsmDraw,        {BSpline támpontok rajzolása}
       bsmMove,        {támpont mozgatása}
       bsmTotalMove,   {Teljes BSpline elmozgatása}
       bsmIns,         {új támpont beszúrása}
       bsmDel,         {támpont törlése}
       bsmSearch,      {támpont keresés}
       bsmSearchPoint  {BSpline kerületi pont keresés}
       );

 CurveDataArray = array[-1..1000] of TPoint3d;


var
 BSPDRAWMODE  : TBSplineDrawMod;
 BSPMODE      : TBSplineMod;
 BSplineAlgoritm    : TBSplineAlgoritm;
 BSplinePointsCount : word;       {BSpline támpontok száma}
 BSplineFirst       : boolean;    {BSpline elsõ pont?}
 BSplineActualIndex : integer;    {Az aktuális támpont indexe a tömbben}

procedure Spline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;
                 spAlgoritm:TBSplineAlgoritm);

procedure drawBezier(CA:TCanvas;var d:array of TPoint3d;nPoints,nSteps:word);
procedure InitBSpline(var dd:CurveDataArray;var nPoints:word);
procedure drawBSpline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;
                        periodic:boolean);
procedure drawBSplineReferencePoints(CA:TCanvas;diameter:integer;NumText:boolean;
                        var dd:CurveDataArray;nPoints:word);
function  IsBSplinePoint(x,y:real;tures:integer;var dd:CurveDataArray;nPoints,
                        nSteps:word):boolean;
function  IsBSplineReferencePoint(x,y:real;tures:integer;var dd:CurveDataArray;
                                 nPoints:word; var pointN:integer):boolean;
procedure InsertBSplinePoint(insPoint:TPoint3d;index:integer;
                             var dd:CurveDataArray;nPoints:word);
procedure DeleteBSplinePoint(index:integer;var dd:CurveDataArray;nPoints:word);

PROCEDURE Spline_Calc (Ap, Bp, Cp, Dp: TPoint3D; T, D: Real; Var X, Y: Real);
PROCEDURE BSpline_ComputeCoeffs ( var dd:CurveDataArray; N: Integer; Var Ap, Bp, Cp, Dp: TPoint3D);
PROCEDURE Catmull_Rom_ComputeCoeffs ( var dd:CurveDataArray; N: Integer; Var Ap, Bp, Cp, Dp: TPoint3D);
PROCEDURE BSpline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint);
PROCEDURE Catmull_Rom_Spline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);

implementation

procedure Spline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;
                 spAlgoritm:TBSplineAlgoritm);
begin
  Case spAlgoritm of
       bsaBezier             : drawBezier(CA,dd,nPoints,nSteps);
       bsaBSplinePeriodic    : drawBSpline(CA,dd,nPoints,nSteps,True);
       bsaBSplineNonPeriodic : drawBSpline(CA,dd,nPoints,nSteps,False);
       bsaRomSplinePeriodic  : CatMull_Rom_Spline(CA,dd,nPoints,nSteps,True);
       bsaRomSplineNonPeriodic : CatMull_Rom_Spline(CA,dd,nPoints,nSteps,False);
  end;
end;

procedure InitBSpline(var dd:CurveDataArray;var nPoints:word);
 var
  i  : integer;
begin
 for i:=Low(dd) to High(dd) do dd[i]:=Point3d(0,0,0);
 nPoints := 0;
end;

procedure drawBezier(CA:TCanvas;var d:array of TPoint3d;nPoints,nSteps:word);
 const nsa=1/16; nsb=2/3;
 var
  i,i2,i3,xx,yy:integer;
  t,tm3,t2,t2m3,t3,t3m3,nc1,nc2,nc3,nc4,step:real;
begin
 step:=1/nSteps;
{ for i:=1 to nPoints do begin}
If nPoints>3 then
 for i2:=0 to pred(nPoints) div 4 do begin
  i:=i2*4;
  t:=0.0;
  for i3:=pred(nSteps) downto 0 do begin
   t:=t+step;
   tm3:=t*3.0; t2:=t*t; t2m3:=t2*3.0; t3:=t2*t; t3m3:=t3*3.0;
   nc1:=1-tm3+t2m3-t3;
   nc2:=t3m3-2.0*t2m3+tm3;
   nc3:=t2m3-t3m3;
   nc4:=t3;
   xx:=round(nc1*d[i].x+nc2*d[succ(i)].x+nc3*d[i+2].x+nc4*d[i+3].x);
   yy:=round(nc1*d[i].y+nc2*d[succ(i)].y+nc3*d[i+2].y+nc4*d[i+3].y);
   If i=0 then ca.MoveTo(xx,yy);
   ca.LineTo(xx,yy);
   end;
  end;
end;

procedure drawBSpline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;periodic:boolean);
 const nsa=1/6; nsb=2/3;
 var
  j,i,i2,xx,yy:integer;
  t,ta,t2,t2a,t3,t3a,nc1,nc2,nc3,nc4,step:real;
  xx1,yy1,xx2,yy2: integer;
begin
 step:=1/nSteps;
 If periodic then begin
    dd[-1]:=dd[1]; dd[0]:=dd[nPoints];
    dd[nPoints+1]:=dd[1]; dd[nPoints+2]:=dd[2];
    dd[nPoints+3]:=dd[3]; dd[nPoints+4]:=dd[4];
 end else begin
    dd[-1]:=dd[1]; dd[0]:=dd[1];
    dd[nPoints+1]:=dd[nPoints]; dd[nPoints+2]:=dd[nPoints];
    dd[nPoints+2]:=dd[nPoints]; dd[nPoints+3]:=dd[nPoints];
 end;
 if not periodic then ca.MoveTo(Round(dd[1].x),Round(dd[1].y));
 for i:=0 to nPoints do begin
  t:=0.0;
  for i2:=pred(nSteps) downto 0 do begin
   t:=t+step;
   ta:=t*0.5; t2:=t*t; t2A:=t2*0.5; t3:=t2*t; t3A:=t3*0.5;
   nc1:=-nsa*t3+t2A-ta+nsa;
   nc2:=t3a-t2+nsb;
   nc3:=-t3a+t2a+ta+nsa;
   nc4:=nsa*t3;
   xx:=round(nc1*dd[i].x+nc2*dd[succ(i)].x+nc3*dd[i+2].x+nc4*dd[i+3].x);
   yy:=round(nc1*dd[i].y+nc2*dd[succ(i)].y+nc3*dd[i+2].y+nc4*dd[i+3].y);
   If (i=0) and periodic then ca.MoveTo(xx,yy);
{     ca.Rectangle(xx-2,yy-2,xx+2,yy+2);}
   ca.LineTo(xx,yy);
   end;
  end;
end;

{ drawBSplineReferencePoints : Megrajzolja a BSpline támpontjait
  ----------------------------
  In:  CA            : Canvas rajzfelület a rajzoláshoz
       diameter      : támpontok mérete
       NumText       : írja-e ki a támpont sorszámokat
       dd            : támpontok tömbje;
       nPoints       : támpontok száma a tömbben;
}
procedure drawBSplineReferencePoints(CA:TCanvas;diameter:integer;NumText:boolean;
                      var dd:CurveDataArray;nPoints:word);
 var
  i  : integer;
begin
 for i:=nPoints downto 1 do begin
   Ca.Rectangle(Round(dd[i].x-diameter),Round(dd[i].y-diameter),
                Round(dd[i].x+diameter),Round(dd[i].y+diameter));
   If NumText then Ca.TextOut(Round(dd[i].x+4),Round(dd[i].y+4),IntToStr(i));
 end;
end;

{ IsBSplineReferencePoint = A BSpline görbe pontjának létét vizsgálja
  -----------------------   x,y koordináták közelében
  In:  x,y           : a vizsgálandó geometriai hely koordinátái;
       tures         : az érzékelés sugara;
       dd            : támpontok tömbje;
       nPoints       : támpontok száma a tömbben;
       pointN        : a megtalált támpont indexe a dd tömbben;
  Out: True  = x,y vizsgálandó pont tures-nél kisebb távolságra esik valamelyik
               támponttól;
       False = nincs ilyen pont.
}

function IsBSplinePoint(x,y:real;tures:integer;var dd:CurveDataArray;nPoints,nSteps:word):boolean;
 const nsa=1/6; nsb=2/3;
 var
  i,i2  : integer;
  xx,yy : real;
  t,ta,t2,t2a,t3,t3a,nc1,nc2,nc3,nc4,step:real;
  xx1,yy1,xx2,yy2: integer;
  dx,dy : real;
begin
 Result:=False;
 step:=1/nSteps;
 dd[-1]:=dd[1]; dd[0]:=dd[nPoints];
 dd[nPoints+1]:=dd[1]; dd[nPoints+2]:=dd[2];
 dd[nPoints+3]:=dd[3]; dd[nPoints+4]:=dd[4];
 for i:=0 to nPoints do begin
  t:=0.0;
  for i2:=pred(nSteps)downto 0 do begin
   t:=t+step;
   ta:=t*0.5; t2:=t*t; t2A:=t2*0.5; t3:=t2*t; t3A:=t3*0.5;
   nc1:=-nsa*t3+t2A-ta+nsa;
   nc2:=t3a-t2+nsb;
   nc3:=-t3a+t2a+ta+nsa;
   nc4:=nsa*t3;
   xx:=round(nc1*dd[i].x+nc2*dd[succ(i)].x+nc3*dd[i+2].x+nc4*dd[i+3].x);
   yy:=round(nc1*dd[i].y+nc2*dd[succ(i)].y+nc3*dd[i+2].y+nc4*dd[i+3].y);

   dx := Abs(xx-x);
   dy := Abs(yy-y);
   If (tures>dx) and (tures>dy) then begin
      Result:=True;
      Exit;
   end;

  end;
end;
end;

{ IsBSplineReferencePoint = A támpont létét vizsgálja x,y koordináták közelében
  -----------------------
  In:  x,y           : a vizsgálandó geometriai hely koordinátái;
       tures         : az érzékelés sugara;
       dd            : támpontok tömbje;
       nPoints       : támpontok száma a tömbben;
       pointN        : a megtalált támpont indexe a dd tömbben;
  Out: True  = x,y vizsgálandó pont tures-nél kisebb távolságra esik valamelyik
               támponttól;
       False = nincs ilyen pont.
}
function IsBSplineReferencePoint(x,y:real;tures:integer;var dd:CurveDataArray;
                                 nPoints:word; var pointN:integer):boolean;
 var
  i  : integer;
  dx,dy : real;
begin
 Result:=False;
 pointN := -1;
 for i:=nPoints downto 0 do begin
   dx := Abs(dd[i].x-x);
   dy := Abs(dd[i].y-y);
   If (tures>dx) and (tures>dy) then begin
      pointN := i;
      Result:=True;
      Exit;
   end;
 end;
end;

{ InsertBSplinePoint = Egy uj BSpline támpontot szúr be a támpontokat
  ------------------   tartalmazó tömb index-el megadott helyére
  In:  insPoint      : a beszúrandó támpont koordinátái;
       index         : az új pont beszúrási helyének tömbindexe;
       dd            : támpontok tömbje;
       nPoints       : támpontok száma a tömbben egyel növelve;
}
procedure InsertBSplinePoint(insPoint:TPoint3d;index:integer;
                             var dd:CurveDataArray;nPoints:word);
 var
  i  : integer;
begin
 {Az index-edik elemtõl a tömbelemek léptetése +1 index-el}
 for i:=nPoints downto index do dd[i+1]:=dd[i];
 dd[index]:=insPoint;
 nPoints  :=nPoints+1;
end;

{ DeleteBSplinePoint = Egy BSpline támpont törlése a támpontokat
  ------------------   tartalmazó tömb index-el megadott helyérõl
  In:  insPoint      : a beszúrandó támpont koordinátái;
       index         : az új pont beszúrási helyének tömbindexe;
       dd            : támpontok tömbje;
       nPoints       : támpontok száma a tömbben egyel növelve;
}
procedure DeleteBSplinePoint(index:integer;var dd:CurveDataArray;nPoints:word);
 var
  i  : integer;
begin
 {Az index-edik elemtõl a tömbelemek léptetése -1 index-el}
 for i:=index to nPoints do dd[i]:=dd[i+1];
 nPoints  :=nPoints-1;
end;


PROCEDURE Spline_Calc (Ap, Bp, Cp, Dp: TPoint3D; T, D: Real; Var X, Y: Real);
VAR T2, T3: Real;
BEGIN
   T2 := T * T;                                       { Square of t }
   T3 := T2 * T;                                      { Cube of t }
   X := ((Ap.X*T3) + (Bp.X*T2) + (Cp.X*T) + Dp.X)/D;  { Calc x value }
   Y := ((Ap.Y*T3) + (Bp.Y*T2) + (Cp.Y*T) + Dp.Y)/D;  { Calc y value }
END;

PROCEDURE BSpline_ComputeCoeffs (var dd:CurveDataArray; N: Integer;
                                 Var Ap, Bp, Cp, Dp: TPoint3D);
BEGIN
   Ap.X := -dd[N-1].X + 3*dd[N].X - 3*dd[N+1].X + dd[N+2].X;
   Bp.X := 3*dd[N-1].X - 6*dd[N].X + 3*dd[N+1].X;
   Cp.X := -3*dd[N-1].X + 3*dd[N+1].X;
   Dp.X := dd[N-1].X + 4*dd[N].X + dd[N+1].X;
   Ap.Y := -dd[N-1].Y + 3*dd[N].Y - 3*dd[N+1].Y + dd[N+2].Y;
   Bp.Y := 3*dd[N-1].Y - 6*dd[N].Y + 3*dd[N+1].Y;
   Cp.Y := -3*dd[N-1].Y + 3*dd[N+1].Y;
   Dp.Y := dd[N-1].Y + 4*dd[N].Y + dd[N+1].Y;
END;

PROCEDURE Catmull_Rom_ComputeCoeffs (var dd:CurveDataArray; N: Integer;
                                    Var Ap, Bp, Cp, Dp: TPoint3D);
BEGIN
   Ap.X := -dd[N-1].X + 3*dd[N].X - 3*dd[N+1].X + dd[N+2].X;
   Bp.X := 2*dd[N-1].X - 5*dd[N].X + 4*dd[N+1].X - dd[N+2].X;
   Cp.X := -dd[N-1].X + dd[N+1].X;
   Dp.X := 2*dd[N].X;
   Ap.Y := -dd[N-1].Y + 3*dd[N].Y - 3*dd[N+1].Y + dd[N+2].Y;
   Bp.Y := 2*dd[N-1].Y - 5*dd[N].Y + 4*dd[N+1].Y - dd[N+2].Y;
   Cp.Y := -dd[N-1].Y + dd[N+1].Y;
   Dp.Y := 2*dd[N].Y;
END;

PROCEDURE BSpline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint);
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint3D;
BEGIN
   dd[-1] := dd[1];
   dd[0] := dd[1];
   dd[N+1] := dd[N];
   dd[N+2] := dd[N];
   For I := 0 To N Do Begin
     BSpline_ComputeCoeffs(dd, I, Ap, Bp, Cp, Dp);
     Spline_Calc(Ap, Bp, Cp, Dp, 0, 6, Lx, Ly);
     For J := 1 To Resolution Do Begin
       Spline_Calc(Ap, Bp, Cp, Dp, J/Resolution, 6, X, Y);
       If j=1 then CA.MoveTo(Round(Lx), Round(Ly))
       else CA.LineTo(Round(X), Round(Y));
       Lx := X; Ly := Y;
     End;
   End;
END;

PROCEDURE Catmull_Rom_Spline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint3D;
BEGIN
 If periodic then begin
    dd[0]:=dd[N];
    dd[N+1]:=dd[1];
    N:=N+1;
 end else begin
    dd[0] := dd[1];
    dd[N+1] := dd[N];
 end;
   For I := 1 To N-1 Do Begin
     Catmull_Rom_ComputeCoeffs(dd, I, Ap, Bp, Cp, Dp);
     Spline_Calc(Ap, Bp, Cp, Dp, 0, 2, Lx, Ly);
     For J := 1 To Resolution Do Begin
       Spline_Calc(Ap, Bp, Cp, Dp, J/Resolution, 2, X, Y);
       CA.MoveTo(Round(Lx), Round(Ly));
       CA.LineTo(Round(X), Round(Y));
       Lx := X; Ly := Y;
     End;
   End;
END;

end.
