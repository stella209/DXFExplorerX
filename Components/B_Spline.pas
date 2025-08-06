
{
  Besier �s B_spline g�rb�k unitja (DELPHI 1.0)
  Author : Ag�cs L�szl� 2002 @StellaSoft
}

unit B_spline;

interface
uses
  Winapi.Windows, System.SysUtils, Winapi.Messages, System.Classes,
  VCL.Graphics, System.Math, NewGeom;

type

 TBSplineDrawMod = (bspNone,bspFrame,bspCorners,bspFrameCorners);

 TBSplineAlgoritm = (bsaBezier,
                     bsaBSplinePeriodic,
                     bsaBSplineNonPeriodic,
                     bsaRomSplinePeriodic,
                     bsaRomSplineNonPeriodic
                     );

 TBSplineMod =         {BSpline szerkeszt�si m�d}
      (bsmNone,
       bsmDraw,        {BSpline t�mpontok rajzol�sa}
       bsmMove,        {t�mpont mozgat�sa}
       bsmTotalMove,   {Teljes BSpline elmozgat�sa}
       bsmIns,         {�j t�mpont besz�r�sa}
       bsmDel,         {t�mpont t�rl�se}
       bsmSearch,      {t�mpont keres�s}
       bsmSearchPoint  {BSpline ker�leti pont keres�s}
       );

 PPointer = ^TPoint2d;
 pCurveDataArray= ^CurveDataArray;
 CurveDataArray = array[-1..5000] of TPoint2d;

 TGLPointF  = TPoint2d;
 TGLPointsF = array of TPoint2d;

var
 BSPDRAWMODE        : TBSplineDrawMod;
 BSPMODE            : TBSplineMod;
 BSplineAlgoritm    : TBSplineAlgoritm;
 BSplinePointsCount : word;       {BSpline t�mpontok sz�ma}
 BSplineFirst       : boolean;    {BSpline els� pont?}
 BSplineActualIndex : integer;    {Az aktu�lis t�mpont indexe a t�mbben}

procedure Spline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;
                 spAlgoritm:TBSplineAlgoritm);

procedure SplineXP(CA:TCanvas;var DataArray: Array of TPoint;nSteps:word;
                 spAlgoritm:TBSplineAlgoritm);

procedure drawBezier(CA:TCanvas;var d:CurveDataArray;nPoints,nSteps:word);
procedure InitBSpline(var dd:CurveDataArray;var nPoints:word);
procedure drawBSpline(CA:TCanvas;var dd:CurveDataArray;nPoints,nSteps:word;
                        periodic:boolean);
procedure drawBSplineReferencePoints(CA:TCanvas;diameter:integer;NumText:boolean;
                        var dd:CurveDataArray;nPoints:word);
function  IsBSplinePoint(x,y:real;tures:integer;var dd:CurveDataArray;nPoints,
                        nSteps:word):boolean;
function  IsBSplineReferencePoint(x,y:real;tures:integer;var dd:CurveDataArray;
                                 nPoints:word; var pointN:integer):boolean;
procedure InsertBSplinePoint(insPoint:TPoint2d;index:integer;
                             var dd:CurveDataArray;nPoints:word);
procedure DeleteBSplinePoint(index:integer;var dd:CurveDataArray;nPoints:word);

PROCEDURE Spline_Calc (Ap, Bp, Cp, Dp: TPoint2D; T, D: Real; Var X, Y: Real);
PROCEDURE BSpline_ComputeCoeffs ( var dd:CurveDataArray; N: Integer; Var Ap, Bp, Cp, Dp: TPoint2D);
PROCEDURE Catmull_Rom_ComputeCoeffs ( var dd:CurveDataArray; N: Integer; Var Ap, Bp, Cp, Dp: TPoint2D);
PROCEDURE BSpline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint);
PROCEDURE Catmull_Rom_Spline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);

procedure GetBezierPoints(var d:CurveDataArray;nPoints,nSteps:word);
procedure GetBezierPathPoints(var d:CurveDataArray;nPoints,nSteps:word);
procedure GetBSplinePoints(var dd:CurveDataArray;nPoints,nSteps:word;periodic:boolean);
PROCEDURE GetSplinePoints(var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);

procedure DrawCubicCurve(Canvas: TCanvas;
  const Points: array of TPoint; Steps: cardinal);

procedure InitdPoints;

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

procedure SplineXP(CA:TCanvas;var DataArray: Array of TPoint;nSteps:word;
                 spAlgoritm:TBSplineAlgoritm);
Var
   dd: CurveDataArray;
   nPoints: integer;
   i:  integer;
begin
  InitBSpline(dd,BSplinePointsCount);
  nPoints := High(DataArray)+1;
  InitdPoints;
  for I := 0 to nPoints+1 do begin
      dd[i] := Point2d(DataArray[i-1].x,DataArray[i-1].y);
  end;

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
 for i:=Low(dd) to High(dd) do dd[i]:=Point2d(0,0);
 nPoints := 0;
end;

procedure drawBezier(CA:TCanvas;var d:CurveDataArray;nPoints,nSteps:word);
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
//  xx1,yy1,xx2,yy2: integer;
//  p : pPoints;
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
//        ca.Rectangle(xx-2,yy-2,xx+2,yy+2);
   ca.LineTo(xx,yy);
   end;
  end;
end;

procedure GetBSplinePoints(var dd:CurveDataArray;nPoints,nSteps:word;periodic:boolean);
 const nsa=1/6; nsb=2/3;
 var
  j,i,i2        : integer;
  xx,yy         : double;
  t,ta,t2,t2a,t3,t3a,nc1,nc2,nc3,nc4,step : double;
//  xx1,yy1,xx2,yy2: integer;
  p : pPoints2d;
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
 for i:=-1 to nPoints do begin
  t:=0.0;
  for i2:=pred(nSteps) downto 0 do begin
   t:=t+step;
   ta:=t*0.5; t2:=t*t; t2A:=t2*0.5; t3:=t2*t; t3A:=t3*0.5;
   nc1:=-nsa*t3+t2A-ta+nsa;
   nc2:=t3a-t2+nsb;
   nc3:=-t3a+t2a+ta+nsa;
   nc4:=nsa*t3;
   xx:=nc1*dd[i].x+nc2*dd[succ(i)].x+nc3*dd[i+2].x+nc4*dd[i+3].x;
   yy:=nc1*dd[i].y+nc2*dd[succ(i)].y+nc3*dd[i+2].y+nc4*dd[i+3].y;
   GetMem(P,SizeOf(TPoint2d));
   p^.x := xx;
   p^.y := yy;
   dPoints.Add(p);
   end;
  end;
end;

{ drawBSplineReferencePoints : Megrajzolja a BSpline t�mpontjait
  ----------------------------
  In:  CA            : Canvas rajzfel�let a rajzol�shoz
       diameter      : t�mpontok m�rete
       NumText       : �rja-e ki a t�mpont sorsz�mokat
       dd            : t�mpontok t�mbje;
       nPoints       : t�mpontok sz�ma a t�mbben;
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

{ IsBSplineReferencePoint = A BSpline g�rbe pontj�nak l�t�t vizsg�lja
  -----------------------   x,y koordin�t�k k�zel�ben
  In:  x,y           : a vizsg�land� geometriai hely koordin�t�i;
       tures         : az �rz�kel�s sugara;
       dd            : t�mpontok t�mbje;
       nPoints       : t�mpontok sz�ma a t�mbben;
       pointN        : a megtal�lt t�mpont indexe a dd t�mbben;
  Out: True  = x,y vizsg�land� pont tures-n�l kisebb t�vols�gra esik valamelyik
               t�mpontt�l;
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
  p : pPoints2d;
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
//   GetMem(P,SizeOf(TPoint2d));
   p^.x := xx;
   p^.y := yy;
   dPoints.Add(p);

   dx := Abs(xx-x);
   dy := Abs(yy-y);
   If (tures>dx) and (tures>dy) then begin
      Result:=True;
//      Exit;
   end;

  end;
end;
end;

{ IsBSplineReferencePoint = A t�mpont l�t�t vizsg�lja x,y koordin�t�k k�zel�ben
  -----------------------
  In:  x,y           : a vizsg�land� geometriai hely koordin�t�i;
       tures         : az �rz�kel�s sugara;
       dd            : t�mpontok t�mbje;
       nPoints       : t�mpontok sz�ma a t�mbben;
       pointN        : a megtal�lt t�mpont indexe a dd t�mbben;
  Out: True  = x,y vizsg�land� pont tures-n�l kisebb t�vols�gra esik valamelyik
               t�mpontt�l;
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

{ InsertBSplinePoint = Egy uj BSpline t�mpontot sz�r be a t�mpontokat
  ------------------   tartalmaz� t�mb index-el megadott hely�re
  In:  insPoint      : a besz�rand� t�mpont koordin�t�i;
       index         : az �j pont besz�r�si hely�nek t�mbindexe;
       dd            : t�mpontok t�mbje;
       nPoints       : t�mpontok sz�ma a t�mbben egyel n�velve;
}
procedure InsertBSplinePoint(insPoint:TPoint2d;index:integer;
                             var dd:CurveDataArray;nPoints:word);
 var
  i  : integer;
begin
 {Az index-edik elemt�l a t�mbelemek l�ptet�se +1 index-el}
 for i:=nPoints downto index do dd[i+1]:=dd[i];
 dd[index]:=insPoint;
 nPoints  :=nPoints+1;
end;

{ DeleteBSplinePoint = Egy BSpline t�mpont t�rl�se a t�mpontokat
  ------------------   tartalmaz� t�mb index-el megadott hely�r�l
  In:  insPoint      : a besz�rand� t�mpont koordin�t�i;
       index         : az �j pont besz�r�si hely�nek t�mbindexe;
       dd            : t�mpontok t�mbje;
       nPoints       : t�mpontok sz�ma a t�mbben egyel n�velve;
}
procedure DeleteBSplinePoint(index:integer;var dd:CurveDataArray;nPoints:word);
 var
  i  : integer;
begin
 {Az index-edik elemt�l a t�mbelemek l�ptet�se -1 index-el}
 for i:=index to nPoints do dd[i]:=dd[i+1];
 nPoints := nPoints-1;
end;


PROCEDURE Spline_Calc (Ap, Bp, Cp, Dp: TPoint2D; T, D: Real; Var X, Y: Real);
VAR T2, T3: Real;
BEGIN
   T2 := T * T;                                       { Square of t }
   T3 := T2 * T;                                      { Cube of t }
   X := ((Ap.X*T3) + (Bp.X*T2) + (Cp.X*T) + Dp.X)/D;  { Calc x value }
   Y := ((Ap.Y*T3) + (Bp.Y*T2) + (Cp.Y*T) + Dp.Y)/D;  { Calc y value }
END;

PROCEDURE BSpline_ComputeCoeffs (var dd:CurveDataArray; N: Integer;
                                 Var Ap, Bp, Cp, Dp: TPoint2D);
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
                                    Var Ap, Bp, Cp, Dp: TPoint2D);
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
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint2D;
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

(*
PROCEDURE Catmull_Rom_Spline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint2D;
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
*)

PROCEDURE Catmull_Rom_Spline (CA:TCanvas; var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint2D;
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

PROCEDURE GetSplinePoints(var dd:CurveDataArray; N, Resolution:longint;
                        periodic:boolean);
VAR I, J: Integer; X, Y, Lx, Ly: Real; Ap, Bp, Cp, Dp: TPoint2D;
  p : pPoints2d;
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
       GetMem(P,SizeOf(TPoint2d));
       p^.x := Lx;
       p^.y := Ly;
       dPoints.Add(p);
       Lx := X; Ly := Y;
     End;
   End;
END;

procedure GetBezierPoints(var d:CurveDataArray;nPoints,nSteps:word);
 const nsa=1/16; nsb=2/3;
 var
  i,i2,i3:integer;
  xx,yy : double;
  t,tm3,t2,t2m3,t3,t3m3,nc1,nc2,nc3,nc4,step:real;
  p : pPoints2d;
begin
 step:=1/nSteps;
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
   xx:=nc1*d[i].x+nc2*d[succ(i)].x+nc3*d[i+2].x+nc4*d[i+3].x;
   yy:=nc1*d[i].y+nc2*d[succ(i)].y+nc3*d[i+2].y+nc4*d[i+3].y;
       GetMem(P,SizeOf(TPoint2d));
       p^.x := xx;
       p^.y := yy;
       dPoints.Add(p);
   end;
  end;
end;

procedure GetBezierPathPoints(var d:CurveDataArray;nPoints,nSteps:word);
var BMP: TBitmap;
    p: array of TPoint;
    i: integer;
    FPathPoints: array of TPoint;
    FPathTypes: array of Byte;
    FNumber: Integer;
    PointIdx: integer;

    procedure AddPoint(xx,yy: integer);
    Var pp : pPoints2d;
    begin
       GetMem(pp,SizeOf(TPoint2d));
       pp^.x := xx/10;
       pp^.y := yy/10;
       dPoints.Add(pp);
    end;

begin
  Try
    // d points convert to TPointArray
    SetLength( p, nPoints );
    for I := 0 to Pred(nPoints) do
        p[i] := Point( Round(10*d[i].x),Round(10*d[i].y) );
    // Draw path on the canvas
    BMP := TBitmap.Create;
    BMP.Width := 10000;
    BMP.Height:= 10000;
    BeginPath(BMP.Canvas.handle);
      BMP.Canvas.PolyBezier(p);
    EndPath(BMP.Canvas.handle);
    //'Flatten' the path ...
    FlattenPath(BMP.Canvas.handle);

    // Get the outline points
    FNumber := GetPath(BMP.Canvas.Handle, Pointer(nil^), Pointer(nil^), 0);

    IF FNumber>0 then begin
       SetLength(FPathPoints, FNumber);
       SetLength(FPathTypes, FNumber);
       FNumber := GetPath(BMP.Canvas.Handle, FPathPoints[0], FPathTypes[0], FNumber);

       PointIdx := 0;

    while PointIdx < FNumber do begin

        CASE FPathTypes[PointIdx] of
        PT_MOVETO:
        begin
            AddPoint(FPathPoints[PointIdx].x,FPathPoints[PointIdx].y);
            inc(PointIdx, 1);
        end;
        PT_LINETO:
        begin
            AddPoint(FPathPoints[PointIdx].x,FPathPoints[PointIdx].y);
            inc(PointIdx, 1);
        end;
        PT_BEZIERTO:
        begin
            AddPoint(FPathPoints[PointIdx].x,FPathPoints[PointIdx].y);
            AddPoint(FPathPoints[PointIdx+1].x,FPathPoints[PointIdx+1].y);
            AddPoint(FPathPoints[PointIdx+2].x,FPathPoints[PointIdx+2].y);
            inc(PointIdx, 3);
        end;
        PT_LINETO or PT_CLOSEFIGURE:
        begin
            AddPoint(FPathPoints[PointIdx].x,FPathPoints[PointIdx].y);
            inc(PointIdx, 1);
        end;
        END;

    end;
    end;

  Finally
    BMP.Free;
  End;
end;

procedure InitdPoints;
begin
  if dPoints=nil then
     dPoints := TList.Create;
  dPoints.Clear;
end;

//==================================================================
procedure DrawCubicCurve(Canvas: TCanvas;
  const Points: array of TPoint; Steps: cardinal);

  function Interpolate(const p1, p2, p3, p4: TPoint;
    t: single): TPoint;

    function cubic(v1, v2, v3, v4, t: single): single;
    begin
      result:= v2 + t*((-v1 + v3) +
       t*((2*v1 - 2*v2 + v3 - v4) +
       t*(-v1 + v2 - v3 + v4)));
    end;
  begin
    Result.x:= round(cubic(p1.x, p2.x, p3.x, p4.x, t));
    Result.y:= round(cubic(p1.y, p2.y, p3.y, p4.y, t));
  end;

var
  i, s: integer;
  p, p1, p2, p3, p4: TPoint;
begin
  if Length(Points) < 2 then
    exit;

  Canvas.MoveTo(Points[0].x, Points[0].y);

  p2:= Points[0];
  p3:= Points[0];
  p4:= Points[1];

  for i:= 0 to High(Points)-1 do
  begin
    p1:= p2;
    p2:= p3;
    p3:= p4;
    if i+2 < Length(Points) then
      p4:= Points[i+2];

    for s:= 1 to Steps do
    begin
      p:= Interpolate(p1, p2, p3, p4, s / Steps);

      Canvas.LineTo(p.x, p.y);
    end;
  end;
end;
//==================================================================

{ The following methods are translated into Pascal from ReactOS source.
    calc_curve_bezier_endp
    calc_curve_bezier
    BEZIERMIDDLE
    BezierCheck
    GDI_InternalBezier
    GDI_Bezier
    GenCurvePoints  }
// Calculates Bezier points from cardinal spline endpoints.
procedure calc_curve_bezier_endp(xend, yend, xadj, yadj, tension: Single;
   var x, y: Single);
begin
   // tangent at endpoints is the line from the endpoint to the adjacent point
   x := tension * (xadj - xend) + xend;
   y := tension * (yadj - yend) + yend;
end;

// Calculates Bezier points from cardinal spline points.
procedure calc_curve_bezier(const pts: TGLPointsF; tension: Single;
   var x1, y1, x2, y2: Single);
var
   xdiff, ydiff: Single;
begin
   // calculate tangent
   xdiff := pts[2].X - pts[0].X;
   ydiff := pts[2].Y - pts[0].Y;

   // apply tangent to get control points
   x1 := pts[1].X - tension * xdiff;
   y1 := pts[1].Y - tension * ydiff;
   x2 := pts[1].X + tension * xdiff;
   y2 := pts[1].Y + tension * ydiff;
end;

procedure BEZIERMIDDLE(var Mid: TGLPointF; const P1, P2: TGLPointF);
begin
   Mid.x := (P1.x + P2.x) / 2;
   Mid.y := (P1.y + P2.y) / 2;
end;

type
   TGDIBezierPoints = array[0..3] of TGLPointF;

{
* BezierCheck helper function to check
* that recursion can be terminated
*       Points[0] and Points[3] are begin and endpoint
*       Points[1] and Points[2] are control points
*       level is the recursion depth
*       returns true if the recusion can be terminated
}
function BezierCheck(level: Integer; const Points: TGDIBezierPoints): Boolean;
const
   BEZIERPIXEL = 1;
var
   dx, dy: Single;
begin
   dx := Points[3].x - Points[0].x;
   dy := Points[3].y - Points[0].y;
   if Abs(dy) <= Abs(dx) then // shallow line
   begin
      // check that control points are between begin and end
      if Points[1].x < Points[0].x then
      begin
         if Points[1].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].x < Points[0].x then
      begin
         if Points[2].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dx) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].y - Points[0].y - (dy / dx) * (Points[1].x - Points[0].x)) > BEZIERPIXEL) or
         (Abs(Points[2].y - Points[0].y - (dy / dx) * (Points[2].x - Points[0].x)) > BEZIERPIXEL) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end
   else
   begin // steep line
      // check that control points are between begin and end
      if Points[1].y < Points[0].y then
      begin
         if Points[1].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].y < Points[0].y then
      begin
         if Points[2].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dy) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].x - Points[0].x - (dx / dy) * (Points[1].y - Points[0].y)) > BEZIERPIXEL) or
        (Abs(Points[2].x - Points[0].x - (dx / dy) * (Points[2].y - Points[0].y)) > BEZIERPIXEL) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end;
end;

procedure GDI_InternalBezier(var Points: TGDIBezierPoints; var PtsOut: TGLPointsF;
   var dwOut, nPtsOut: Integer; level: Integer);
var
   Points2: TGDIBezierPoints; // for the second recursive call
begin
  if nPtsOut = dwOut then
  begin
     dwOut := dwOut * 2;
     SetLength(PtsOut, dwOut);
  end;

  if (level = 0) or BezierCheck(level, Points) then // Recursion can be terminated
  begin
     if nPtsOut = 0 then
     begin
        PtsOut[0] := Points[0];
        nPtsOut := 1;
     end;
     PtsOut[nPtsOut] := Points[3];
     Inc(nPtsOut);
  end
  else
  begin
     Points2[3] := Points[3];
     BEZIERMIDDLE(Points2[2], Points[2], Points[3]);
     BEZIERMIDDLE(Points2[0], Points[1], Points[2]);
     BEZIERMIDDLE(Points2[1],Points2[0],Points2[2]);

     BEZIERMIDDLE(Points[1], Points[0],  Points[1]);
     BEZIERMIDDLE(Points[2], Points[1], Points2[0]);
     BEZIERMIDDLE(Points[3], Points[2], Points2[1]);

     Points2[0] := Points[3];

     // do the two halves
     GDI_InternalBezier(Points, PtsOut, dwOut, nPtsOut, level - 1);
     GDI_InternalBezier(Points2, PtsOut, dwOut, nPtsOut, level - 1);
  end;
end;

procedure GDI_Bezier(const Points: TGLPointsF; count: Integer;
   var PtsOut: TGLPointsF; var nPtsOut:Integer);
var
   Bezier, dwOut, i: Integer;
   ptBuf: TGDIBezierPoints;
begin
   dwOut := 150;
   nPtsOut := 0;

   if (count - 1) mod 3 <> 0 then
      Exit;

   SetLength(PtsOut, dwOut);
   for Bezier := 0 to (count - 1) div 3 - 1 do
   begin
      Move(Points[Bezier * 3], ptBuf[0], SizeOf(ptBuf));
      GDI_InternalBezier(ptBuf, PtsOut, dwOut, nPtsOut, 8);
   end;
end;

procedure GenCurvePoints(const points: TGLPointsF; count: Integer;
   var outPoints: TGLPointsF; var outCount: Integer; tension: Single = 0.5);
var
   i, len_pt: Integer;
   x1, x2, y1, y2: Single;
   pt: TGLPointsF;
begin
   outCount := 0;
   if count <= 1 then
      Exit;

   // PolyBezier expects count*3-2 points.
   len_pt := count * 3 - 2;
   SetLength(pt, len_pt);
   tension := tension * 0.3;

   calc_curve_bezier_endp(points[0].X, points[0].Y, points[1].X, points[1].Y,
      tension, x1, y1);

   pt[0] := points[0];
   pt[1].X := x1;
   pt[1].Y := y1;

   for i := 0 to count - 3 do
   begin
      calc_curve_bezier(TGLPointsF(@(points[i])), tension, x1, y1, x2, y2);
      pt[3 * i + 2].X := x1;
      pt[3 * i + 2].Y := y1;
      pt[3 * i + 3] := points[i + 1];
      pt[3 * i + 4].X := x2;
      pt[3 * i + 4].Y := y2;
   end;

   calc_curve_bezier_endp(points[count - 1].X, points[count - 1].Y,
       points[count - 2].X, points[count - 2].Y, tension, x1, y1);
   pt[len_pt - 2].X := x1;
   pt[len_pt - 2].Y := y1;
   pt[len_pt - 1] := points[count - 1];

   GDI_Bezier(pt, len_pt, outPoints, outCount);
end;

end.
