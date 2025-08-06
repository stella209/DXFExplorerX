unit Dgrafik;

interface
uses
  System.SysUtils, WinTypes, WinProcs, Winapi.Messages, System.Classes,
  VCL.Graphics, VCL.Controls,  VCL.StdCtrls, VCL.ExtCtrls, NewGeom;

Type

  TDrawingTool = (dtNone, dtPoint, dtLine, dtInfo, dtRectangle,
       dtRoundRect, dtEllipse, dtFillRect, dtFillRoundRect,
       dtFillEllipse, dtPolyLine, dtIv, dtText,
       dtExtraText,dtBrush);

  TCanvasRect = record
    Pen      : TPen;
    Brush    : TBrush;
    CopyMode : TCopyMode;
    PenPos   : TPoint;
  end;

  TgrPot = record     {Grafikus potenciométer}
       ca : TBitmap;
       enabled: boolean;
       text: string;
       oldx,oldy: integer;
  end;


procedure cls(i: TCanvas; co: TColor);
procedure clsRect(i: TCanvas; t:TRect; co: TColor);
Function  CanvasMent(im:TCanvas):TCanvas;
Procedure CanvasVisszatolt(var im:TCanvas; tc: TCanvas);
procedure SetPen(ca:TCanvas;color:TColor;width:integer;style:TPenStyle;mode:TPenMode);

procedure ShowLine(ca:TCanvas;x1,y1,x2,y2:integer);
procedure ShowCross(ca:TCanvas;x,y,d:integer);
function  KepMove(ca:TCanvas; xm,ym : integer; alapszin: TColor):Trect;
Procedure VonalzoBe(von:Timage;dx:integer;xmin,xmax,beosztas:extended;
                    form:string);
Function  Kerekit(sz:double;tizedes:integer):extended;
Function  Power(alap,kitevo:extended):extended;
Function  Bennevan( t: TRect; p: TPoint):boolean;
Procedure Vegpontrajzol(im:TCanvas; p: TPoint; d: integer);
{ Alakzatrajzolo rutin }
procedure AlakzatRajz(Canv: TCanvas; DrawingTool:TDrawingTool; T, B: TPoint;
              AMode: TPenMode; ujrajz: boolean);
procedure Kereszt(Canv: TCanvas; co: TColor);
procedure RotText(ca:TCanvas; x,y:integer; szoveg:string; szog:integer);
Function  IsLMouseButton:boolean;
procedure RotateBitmap90Degrees(var ABitmap: TBitmap);

{ Grafikus potenciométer }
procedure GrafPotInit(var grp:TgrPot);
procedure GrafPotEnd(var grp:TgrPot);
procedure GrafpotBe(var grp:TgrPot; Canv:TCanvas; x,y:integer);
Procedure GrafpotMozog(var grp:TgrPot; Canv:TCanvas; x,y:integer;
                           text:string;meret:integer);

Var sPal     : TCanvasRect;
    DrawingTool: TDrawingTool;

implementation

procedure GrafPotInit(var grp:TgrPot);
begin
    If grP.ca=nil then begin
       grP.Ca:=TBitmap.Create;
       grP.ca.width :=100;
       grP.ca.height:=40;
       grP.enabled  :=True;
    end;
    grP.text:='';
end;

procedure GrafPotEnd(var grp:TgrPot);
begin
    grP.ca.Free;
end;

procedure GrafpotBe(var grp:TgrPot; Canv:TCanvas; x,y:integer);
begin
  if grP.enabled then begin
     GrafPotInit(grp);
     grP.oldx:=x;
     grP.oldy:=y;
     grP.ca.Canvas.CopyRect(Rect(0,0,100,40),Canv,Rect(x,y,x+100,y+40));
  end else begin
     Canv.CopyRect(Rect(grP.oldx,grP.oldy,grP.oldx+100,grP.oldy+40),
                 grP.ca.Canvas,Rect(0,0,100,40));
  end;
end;

Procedure GrafpotMozog(var grp:TgrPot; Canv:TCanvas; x,y:integer;
                           text:string;meret:integer);
var fm:TFont;
    br:TBrush;
begin
  if grp.enabled then begin
     fm:=Canv.Font;
     Canv.CopyRect(Rect(grP.oldx,grP.oldy,grP.oldx+100,grP.oldy+40),
                   grP.ca.Canvas,Rect(0,0,100,40));
     grP.ca.Canvas.CopyRect(Rect(0,0,100,40),Canv,Rect(x,y,x+100,y+40));
     Canv.Font.Size := meret;
     grP.text:=Text;
     br:=Canv.Brush; Canv.Brush.style:=bsSolid;
     Canv.TextOut(x,y,grP.text);
     Canv.Brush:=br;
     grP.oldx:=x;
     grP.oldy:=y;
     Canv.Font:=fm;
  end;
end;

procedure cls(i: TCanvas; co: TColor);
var pe: TPen; br: TBrush; c: Trect;
begin
  with i as TCanvas do
  begin
      pe:= Pen; br:=Brush; Pen.color:=co;
      brush.style:=bsSolid;
      Brush.color:=co;
      Pen.Color:=co;
      Pen.Mode:=pmCopy;
      c:=cliprect;
      Rectangle(c.left,c.top,c.right,c.bottom);
      Pen:=pe; Brush := br;
  end;
end;

procedure clsRect(i: TCanvas; t:TRect; co: TColor);
var br: TBrush; pe:TPen;
begin
  with i as TCanvas do
  begin
      br := Brush; pe:=Pen;
      Pen.Color:=co; Pen.Width:=4;
      brush.style:=bsSolid;
      Brush.color:=co; Pen.Color:=co;
      Rectangle(t.left,t.top,t.right,t.bottom);
      Brush := br; Pen:=pe;
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

procedure ShowLine(ca:TCanvas;x1,y1,x2,y2:integer);
begin ca.MoveTo(x1,y1); ca.LineTo(x2,y2);end;

procedure ShowCross(ca:TCanvas;x,y,d:integer);
begin
  ShowLine(ca,x-d,y,x+d,y);
  ShowLine(ca,x,y-d,x,y+d);
end;

{ Képterület átmozgatása és a maradék sáv alapszinû törlése }
function KepMove(ca:TCanvas; xm,ym : integer; alapszin: TColor):Trect;
var fRect,cRect: TRect;
begin
  fRect:=ca.cliprect;
  cRect:=fRect;
  OffSetRect(cRect,XM,YM);
  Ca.CopyMode:=cmSrcCopy;
  Ca.CopyRect(cRect,Ca,fRect);
  If (xm<>0) or (ym<>0) then begin
    If xm>0 then fRect:=Rect(0,0,xm,frect.bottom-frect.top);
    If xm<0 then fRect:=Rect(frect.right-frect.left+xm,0,
       frect.right-frect.left,frect.bottom-frect.top);
    If ym>0 then fRect:=Rect(0,0,frect.right-frect.left,ym);
    If ym<0 then fRect:=Rect(0,frect.bottom-frect.top+ym,
       frect.right-frect.left,frect.bottom-frect.top);
    ca.Brush.Style:=bsSolid;
    ca.Brush.Color:=alapszin;
    ca.FillRect(fRect);
    Result:=frect;
  end;
end;

{ V o n a l z o B e  procedure
  ----------------------------
  Egy Image1 grafikus felülethez készit egy másik Image-re
  vonalzó skálát:
  in: von   : a vonalzó Timage neve
      dx    : az eredet grafika kiterjedése
      xmin,xmax : az ábrázolandó koord. tartomány
      beosztás  : a skálaosztás léptéke (pl. 0.2)
      form      : a beosztás számok formátuma pl. '2.1'
  Hogy a vonalzó x vagy y lessz, azt a von:Timage kiterjedésébõl
  dönti el. Ha y irányban hosszabb, akkor y vonalzó lesz.
}
Procedure VonalzoBe(von:Timage;dx:integer;xmin,xmax,beosztas:extended;
                    form:string);
var b:extended;
    mszorzo: extended;     {torzítási tényezõ}
    xvonalzo: boolean;     {True, ha x vonalzó}
    vonas: extended;
    vh: integer;
    y: integer;
    elteres: extended;
    xx,yy: integer;
begin
  With von do begin
    If von.Width>von.Height then xvonalzo:=True else xvonalzo:=False;
    vonas:=xmin;
    b:=beosztas;
    mszorzo:=dx/(xmax-xmin);
    vh:=5;
    While vonas<=xmax do begin
         y:=Trunc(mszorzo*(vonas-xmin));
         if not xvonalzo then y:=dx-y;
         elteres:=KEREKIT(FRAC(vonas),3);
         If (elteres=0.5) then vh:=5 else vh:=2;
         If (elteres=0) or (elteres=1) then begin
          vh:=10;
          if not xvonalzo then begin
             xx:=0; yy:=y;
          end else begin
             xx:=y; yy:=10;
          end;
          Canvas.MoveTo(xx,yy);
          Canvas.TextOut(xx,yy,Format('%'+form+'f',[vonas]));
         end;
          if not xvonalzo then begin
             xx:=Von.Width; yy:=y;
             Canvas.MoveTo(xx,yy);
             Canvas.LineTo(xx-vh,yy);
          end else begin
             xx:=y; yy:=vh;
             Canvas.MoveTo(xx,0);
             Canvas.LineTo(xx,yy);
          end;
      vonas:=vonas+b;
    end;
  end;
end;


{   K e r e k i t (sz,tizedes)
    --------------------------
    Az sz valós számot 'tizedes' jegyre kerekíti.
    pl. Kerekit(12.346,2)  = 12.35
        Kerekit(12.346,0)  = 12
        Kerekit(12.346,-1) = 10
}
Function Kerekit(sz:double;tizedes:integer):extended;
var szorzo: extended;
    s: String;
begin
    szorzo:=Power(10,tizedes);
{
    Result:=Round(sz*szorzo)/szorzo;
}
    s:=IntToStr(Round(sz*szorzo));
    Insert(FormatSettings.DecimalSeparator,s,Length(s)-tizedes+1);
    If sz=0 then result:=0 else
    Result:=StrToFloat(s);
end;

Function Power(alap,kitevo:extended):extended;
begin
    Power := exp( kitevo * ln( alap ));
end;

{ Forrópont megjelenítés kis négyzettel }
Procedure Vegpontrajzol(im:TCanvas; p: TPoint; d: integer);
Var cv : TCanvas;
begin
  cv := im;
  With im do begin
     Pen.Style:=psSolid;
     Pen.Width:=3;
     Pen.Mode:=pmNotXor;
     Rectangle(p.x-d,p.y-d,p.x+d,p.y+d);
  end;
  CanvasVisszatolt(im,cv);
end;

Function CanvasMent(im:TCanvas):TCanvas;
begin
  Result := im;
end;

Procedure CanvasVisszatolt(var im:TCanvas; tc: TCanvas);
begin
  With (im as TCanvas) do
  begin
     Pen     := tc.Pen;
     Brush   := tc.Brush;
     CopyMode:= tc.CopyMode;
     PenPos  := tc.PenPos;
  end;
end;

{ BENNEVAN : A fgv. True értéket ad vissza, ha az adott pont
           benne van a kijelölt alakzat befoglaló téglalapjában}

Function Bennevan( t: TRect; p: TPoint):boolean;
Var hc : HRgn;
begin
  hc := CreateRectRgn(t.left,t.top,t.right,t.bottom);
  Result := PtInRegion(hc,p.x,p.y);
  DeleteObject(hc);
end;


procedure AlakzatRajz(Canv: TCanvas; DrawingTool:TDrawingTool; T,B: TPoint;
           AMode: TPenMode; ujrajz: Boolean);
begin
    Canv.Pen.Mode := AMode;
    If (T.X<>B.x) OR (T.Y<>B.Y) then
    begin
        If ujrajz then
        begin
        end;
    case DrawingTool of
      dtPoint: Canv.Rectangle(T.X-1,T.Y-1,T.X+1,T.Y+1);
      dtLine: begin
                Canv.MoveTo(T.X, T.Y);
                Canv.LineTo(B.X, B.Y);
              end;
      dtRectangle: begin
           Canv.Rectangle(T.X, T.Y, B.X, B.Y);
           end;
      dtEllipse:   Canv.Ellipse(T.X, T.Y, B.X, B.Y);
      dtRoundRect: Canv.RoundRect(T.X, T.Y, B.X, B.Y,
        (T.X - B.X) div 2, (T.Y - B.Y) div 2);
    end;
    case DrawingTool of
      dtFillRect: begin
        Canv.Rectangle(T.X, T.Y, B.X, B.Y);
        end;
      dtFillEllipse: begin
        Canv.Ellipse(T.X, T.Y, B.X, B.Y);
        end;
      dtFillRoundRect: begin
        Canv.RoundRect(T.X, T.Y, B.X, B.Y,
        (T.X - B.X) div 2, (T.Y - B.Y) div 2);
        end;
    end;
    end;
    Canv.Refresh;
end;

procedure Kereszt(Canv: TCanvas; co: TColor);
var t: Trect;
    tp: Tpen;
    dx,dy: integer;
begin
    tp:=Canv.Pen;
    canv.pen.color:= co;
    canv.pen.mode := pmNotXor;
    t := canv.ClipRect;
    dx := (t.right - t.left) div 2;
    dy := (t.bottom - t.top) div 2;
    canv.MoveTo(dx,0); canv.LineTo(dx,t.bottom);
    canv.MoveTo(0,dy); canv.LineTo(t.right,dy);
    Canv.Pen:=tp;
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
  Ca.TextOut(x,y,szoveg);
  DeleteObject(hf);
  SelectObject(Ca.Handle,th);
end;

Function IsLMouseButton:boolean;
begin
if HiWord(GetAsyncKeyState(VK_LBUTTON)) > 0 then Result:=True else Result:=False;
end;

procedure RotateBitmap90Degrees(var ABitmap: TBitmap);

const
  BITMAP_HEADER_SIZE = SizeOf(TBitmapFileHeader) +
SizeOf(TBitmapInfoHeader);

var
  { Things that end in R are for the rotated image. }
  PbmpInfoR: PBitmapInfoHeader;
  bmpBuffer, bmpBufferR: PByte;
  MemoryStream, MemoryStreamR: TMemoryStream;
  PbmpBuffer, PbmpBufferR: PByte;
  PbmpBufferRFirstScanLine, PbmpBufferColumnZero: PByte;
  BytesPerPixel, BytesPerScanLine, BytesPerScanLineR: Integer;
  X, Y, T: Integer;

begin
  {
    Don't *ever* call GetDIBSizes! It screws up your bitmap.
    I'll be posting about that shortly.
  }

  MemoryStream := TMemoryStream.Create;

  {
   To do: Put in a SetSize, which will eliminate any reallocation
   overhead.
  }

  ABitmap.SaveToStream(MemoryStream);

  {
   Don't need you anymore. We'll make a new one when the time comes.
  }
  ABitmap.Free;

  bmpBuffer := MemoryStream.Memory;

  { Set PbmpInfoR to point to the source bitmap's info header. }
  { Boy, these headers are getting annoying. }
  Inc( bmpBuffer, SizeOf(TBitmapFileHeader) );
  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);

  { Set bmpBuffer to point to the original bitmap bits. }
  Inc(bmpBuffer, SizeOf(PbmpInfoR^));
  { Set the ColumnZero pointer to point to, uh, column zero. }
  PbmpBufferColumnZero := bmpBuffer;

  with PbmpInfoR^ do
  begin
    BytesPerPixel := biBitCount shr 3;
    { ScanLines are DWORD aligned. }
    BytesPerScanLine := ((((biWidth * biBitCount) + 31) div 32) * 4);
    BytesPerScanLineR := ((((biHeight * biBitCount) + 31) div 32) * 4);

    { The TMemoryStream that will hold the rotated bits. }
    MemoryStreamR := TMemoryStream.Create;
    {
     Set size for rotated bitmap. Might be different from source size
     due to DWORD aligning.
    }
    MemoryStreamR.SetSize(BITMAP_HEADER_SIZE  + BytesPerScanLineR * biWidth);
  end;

  { Copy the headers from the source bitmap. }
  MemoryStream.Seek(0, soFromBeginning);
  MemoryStreamR.CopyFrom(MemoryStream, BITMAP_HEADER_SIZE);

  { Here's the buffer we're going to rotate. }
  bmpBufferR := MemoryStreamR.Memory;
  { Skip the headers, yadda yadda yadda... }
  Inc(bmpBufferR, BITMAP_HEADER_SIZE);

  {
   Set up PbmpBufferRFirstScanLine and advance it to end of the first scan
   line of bmpBufferR.
  }
  PbmpBufferRFirstScanLine := bmpBufferR;
  Inc(PbmpBufferRFirstScanLine, (PbmpInfoR^.biHeight - 1) * BytesPerPixel);

  { Here's the meat. Loop through the pixels and rotate appropriately. }

  { Remember that DIBs have their origins opposite from DDBs. }
  for Y := 1 to PbmpInfoR^.biHeight do
  begin
    PbmpBuffer := PbmpBufferColumnZero;
    PbmpBufferR := PbmpBufferRFirstScanLine;

    for X := 1 to PbmpInfoR^.biWidth do
    begin
      for T := 1 to BytesPerPixel do
      begin
        PbmpBufferR^ := PbmpBuffer^;
        Inc(PbmpBufferR);
        Inc(PbmpBuffer);
      end;
      Dec(PbmpBufferR, BytesPerPixel);
      Inc(PbmpBufferR, BytesPerScanLineR);
    end;

    Inc(PbmpBufferColumnZero, BytesPerScanLine);
    Dec(PbmpBufferRFirstScanLine, BytesPerPixel);
  end;

  { Done with the source bits. }
  MemoryStream.Free;

  { Now set PbmpInfoR to point to the rotated bitmap's info header. }
  PbmpBufferR := MemoryStreamR.Memory;
  Inc( PbmpBufferR, SizeOf(TBitmapFileHeader) );
  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);

  { Swap the width and height of the rotated bitmap's info header. }
  with PbmpInfoR^ do
  begin
    T := biHeight;
    biHeight := biWidth;
    biWidth := T;
  end;

  ABitmap := TBitmap.Create;

  { Spin back to the very beginning. }
  MemoryStreamR.Seek(0, soFromBeginning);
  ABitmap.LoadFromStream(MemoryStreamR);

  MemoryStreamR.Free;
end;

end.
