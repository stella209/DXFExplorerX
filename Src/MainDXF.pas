unit MainDXF;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, System.IniFiles, Vcl.CheckLst,
  Vcl.Grids, InspCtrl, Vcl.FileCtrl, Vcl.Menus, Vcl.ImgList,
  Al_TSpeedButton, AL_CADTypes,
  AL_GL, AL_CADGL, AL_CAD, al_DXF, CompInsp, Szoveg, NewGeom, janLanguage;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    lbCoord: TLabel;
    ALTimerSpeedButton1: TALTimerSpeedButton;
    ALTimerSpeedButton2: TALTimerSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    CheckBox1: TCheckBox;
    RotTrackBar: TTrackBar;
    CheckBox2: TCheckBox;
    Poligonize: TButton;
    Button2: TButton;
    CheckBox4: TCheckBox;
    StatusBar: TStatusBar;
    alCADSource1: TalCADSource;
    SaveDialog1: TSaveDialog;
    FindDialog1: TFindDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Reopen1: TMenuItem;
    Append1: TMenuItem;
    CheckBox3: TCheckBox;
    PageControl1: TPageControl;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    Sablon: TalCADViewGL;
    PageControl2: TPageControl;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    FilterComboBox1: TFilterComboBox;
    Panel4: TPanel;
    DirectoryListBox1: TDirectoryListBox;
    Panel5: TPanel;
    FileListBox1: TFileListBox;
    Panel6: TPanel;
    DriveComboBox1: TDriveComboBox;
    Panel7: TPanel;
    Edit1: TEdit;
    PageControl3: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    TabSheet12: TTabSheet;
    Panel8: TPanel;
    Label1: TLabel;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SRCEdit: TEdit;
    Panel11: TPanel;
    Button3: TButton;
    Button4: TButton;
    DXFMemo: TMemo;
    Panel9: TPanel;
    ComponentInspector: TComponentInspector;
    ObjectList: TCheckListBox;
    Panel12: TPanel;
    cbShape: TComboBox;
    Panel13: TPanel;
    BlkView: TalCADView;
    BlkList: TListBox;
    LayerGrid: TStringGrid;
    StructTreeView: TTreeView;
    ColorDialog: TColorDialog;
    alCADSource2: TalCADSource;
    Panel10: TPanel;
    Button1: TButton;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit2: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Pate1: TMenuItem;
    View1: TMenuItem;
    Scale1: TMenuItem;
    N2: TMenuItem;
    CentralCross1: TMenuItem;
    CursorCross1: TMenuItem;
    Grid1: TMenuItem;
    Pointsnodes1: TMenuItem;
    Hinted1: TMenuItem;
    SaveAsDXF1: TMenuItem;
    N3: TMenuItem;
    LayerColor1: TMenuItem;
    Help1: TMenuItem;
    HelpContext1: TMenuItem;
    RotLabel: TLabel;
    Label2: TLabel;
    TabSheet3: TTabSheet;
    N4: TMenuItem;
    Language1: TMenuItem;
    English1: TMenuItem;
    Hungarian1: TMenuItem;
    N5: TMenuItem;
    About1: TMenuItem;
    SaveDialog2: TSaveDialog;
    pupObjList: TPopupMenu;
    RefreshList1: TMenuItem;
    N18: TMenuItem;
    SelectAll1: TMenuItem;
    UnselectAll1: TMenuItem;
    InversSelection1: TMenuItem;
    N13: TMenuItem;
    DeleteSelected1: TMenuItem;
    pmStruct: TPopupMenu;
    Expand1: TMenuItem;
    Collapse1: TMenuItem;
    janLanguage1: TjanLanguage;
    RichEdit1: TRichEdit;
    procedure FileListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ALTimerSpeedButton2TimerEvent(Sender: TObject);
    procedure ALTimerSpeedButton1TimerEvent(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SablonMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBox2Click(Sender: TObject);
    procedure RotTrackBarChange(Sender: TObject);
    procedure StructTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure CheckBox1Click(Sender: TObject);
    procedure SablonChangeSelected(Sender: TObject; Curve: TCurve;
      Point: Integer);
    procedure LayerGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure LayerGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SablonMouseLeave(Sender: TObject);
    procedure SablonChangeWindow(Sender: TObject; Cent: TPoint2D; Zoom: Double;
      CursorPos: TPoint);
    procedure BlkListClick(Sender: TObject);
    procedure BlkListDblClick(Sender: TObject);
    procedure ObjectListClick(Sender: TObject);
    procedure RotTrackBarEnter(Sender: TObject);
    procedure RotTrackBarExit(Sender: TObject);
    procedure ObjectListClickCheck(Sender: TObject);
    procedure Append1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Reopen1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure CentralCross1Click(Sender: TObject);
    procedure CursorCross1Click(Sender: TObject);
    procedure Grid1Click(Sender: TObject);
    procedure Pointsnodes1Click(Sender: TObject);
    procedure Hinted1Click(Sender: TObject);
    procedure LayerColor1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure SRCEditChange(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cbShapeChange(Sender: TObject);
    procedure RefreshList1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure UnselectAll1Click(Sender: TObject);
    procedure InversSelection1Click(Sender: TObject);
    procedure DeleteSelected1Click(Sender: TObject);
    procedure Expand1Click(Sender: TObject);
    procedure Collapse1Click(Sender: TObject);
    procedure HelpContext1Click(Sender: TObject);
    procedure English1Click(Sender: TObject);
    procedure Hungarian1Click(Sender: TObject);
  private
    LastRotAngle : single;
    RotBar       : boolean;
    FFileName: string;
    ScaleF,ScaleFactor : integer;
    procedure SetFileName(const Value: string);
    procedure GridInit;
    procedure BlockGridInit;
    procedure LayerGridInit;
    procedure cbShapeInit;
    procedure Rescale(scaleF: integer);
    function FindInMemo(sText: string): boolean;
    function FindNextInMemo(sText: string): boolean;
  public
    dxfObj   : TALCustomDXF;
    stm      : TMemoryStream;
    property FileName: string read FFileName write SetFileName;
  end;

var
  MainForm: TMainForm;
  focim: string;
  iniFile: TInifile;
  iFileName: string;
  lineNumber: integer = 0;
  DefaultDir: string;
  langfile: string;

implementation

{$R *.dfm}

uses _About;

procedure TMainForm.FormActivate(Sender: TObject);
begin
   Update;
   Sleep(2000);
   AboutBox.Close;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  iniFile.WriteString('MAIN','Dir',DirectoryListBox1.Directory );
  iniFile.WriteString('MAIN','Langfile',Langfile);
  iniFile.Free;
  stm.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := focim;
  dxfObj := TALCustomDXF.Create('');
  iFileName:=ChangeFileExt(Application.ExeName,'.ini');
  iniFile := TInifile.Create(iFileName);
  DirectoryListBox1.Directory := iniFile.ReadString('MAIN','Dir',DirectoryListBox1.Directory );
  janLanguage1.InitLanguage(Self);
  DXFMemo.HideSelection := False;
  dxfObj := TALCustomDXF.Create('');
  dxfObj.CadSource := alCADSource1;
  StructTreeView.Items.Clear;
  stm := TMemoryStream.Create;
  PageControl2.ActivePageIndex:=0;
  PageControl3.ActivePageIndex:=0;
  cbShapeInit;
  LastRotAngle := 0;
  RotBar := False;

 // Átméretezi a MainForm-ot ScaleF %-ra, ha kell
 ScaleFactor := 100;
 if ScaleF=0 then
    ScaleF := 100;
 if ScaleF<>100 then
     Rescale(ScaleF);
  if ParamStr(1)<>'' then
     Filename := ParamStr(1);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  dxfObj.Free;
end;

procedure TMainForm.cbShapeChange(Sender: TObject);
var Shape : TDrawMode;
  i: Integer;
begin
  Shape := TDrawMode(cbShape.ItemIndex);
  Sablon.CADSource.FCurveList.SelectAllShape(Shape,True);
  GridInit;
  Sablon.ReCall;
end;

procedure TMainForm.cbShapeInit;
var i: integer;
begin
  cbShape.Clear;
  for I := 0 to High(DrawModeText) do
      cbShape.Items.Add(DrawModeText[i]);
end;

procedure TMainForm.CentralCross1Click(Sender: TObject);
begin
  Sablon.CentralCross := not Sablon.CentralCross;
  CentralCross1.Checked := Sablon.CentralCross;
end;

procedure TMainForm.CheckBox1Click(Sender: TObject);
begin
  Sablon.OpenGLPaint := CheckBox1.Checked;
end;

procedure TMainForm.CheckBox2Click(Sender: TObject);
begin
  Sablon.ShowPoints := CheckBox2.Checked;
  Pointsnodes1.Checked := CheckBox2.Checked;
end;

procedure TMainForm.CheckBox3Click(Sender: TObject);
begin
  Sablon.LayerPaint := CheckBox3.Checked;
  LayerColor1.Checked := CheckBox3.Checked;
end;

procedure TMainForm.CheckBox4Click(Sender: TObject);
begin
  Sablon.Paper.Visible := CheckBox4.Checked;
end;

procedure TMainForm.Collapse1Click(Sender: TObject);
begin
  StructTreeView.FullCollapse;
end;

procedure TMainForm.Copy1Click(Sender: TObject);
begin
  Sablon.CopyToClipboard;
end;

procedure TMainForm.CursorCross1Click(Sender: TObject);
begin
  Sablon.CursorCross := not Sablon.CursorCross;
  CursorCross1.Checked := Sablon.CursorCross;
end;

procedure TMainForm.DeleteSelected1Click(Sender: TObject);
begin
  Sablon.CADSource.FCurveList.DeleteSelectedCurves;
  GridInit;
  Sablon.Dopaint;
end;

procedure TMainForm.English1Click(Sender: TObject);
begin
  English1.Checked := True;
  langfile := Defaultdir+'ENG.lng';
  janLanguage1.InitLanguage(self,LangFile);
  HelpContext1Click(nil);
end;

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Expand1Click(Sender: TObject);
begin
  StructTreeView.FullExpand;
end;

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutBox.Show;
end;

procedure TMainForm.ALTimerSpeedButton1TimerEvent(Sender: TObject);
begin
  Sablon.CentralisZoom := True;
  Sablon.Zoom := 1.05*Sablon.Zoom;
  Sablon.CentralisZoom := False;
end;

procedure TMainForm.ALTimerSpeedButton2TimerEvent(Sender: TObject);
begin
  Sablon.CentralisZoom := True;
  Sablon.Zoom := 0.95*Sablon.Zoom;
  Sablon.CentralisZoom := False;
end;

procedure TMainForm.Append1Click(Sender: TObject);
begin
  Append1.Checked := not Append1.Checked;
  Sablon.Append := Append1.Checked;
end;

procedure TMainForm.BlkListClick(Sender: TObject);
var
  block: TBlock;
begin
if BlkList.ItemIndex>-1 then
begin
  block:=TBlock(Sablon.CADSource.Blocks[BlkList.ItemIndex]);
  BlkView.CADSource.FCurveList := block.FCurveList;
  BlkView.ZoomDrawing;
end;
end;

procedure TMainForm.BlkListDblClick(Sender: TObject);
var block : TBlock;
    H     : integer;
    R     : TRect2d;
begin
// Insert actual block into drawing
(*
   Sablon.SelectAll(False);
   block := Sablon.Blocks[BlkList.ItemIndex];
   R     := block.BoundsRect;
   H := Sablon.MakeCurve('Insert',-1,dmInsert,True,True,False);
   Sablon.Curves[H].BlockParams.BlockName := block.BlockName;
   Sablon.Curves[H].BlockParams.TranslateX := Sablon.Centrum.x;
   Sablon.Curves[H].BlockParams.TranslateY := Sablon.Centrum.y;
   Sablon.Curves[H].AddPoint(Sablon.Centrum.x,Sablon.Centrum.y);
   R := Sablon.CADSource.GetBlockBoundsRect(block,Sablon.Curves[H]);
   Sablon.Curves[H].BoundsRect := R;
   Sablon.Curves[H].Selected := True;
   Sablon.GetDrawExtension;
   Sablon.Repaint;*)
end;

procedure TMainForm.BlockGridInit;
var I,H: integer;
    block: TBlock;
begin
Try
 With Sablon do begin
    BlkList.Clear;
    if CADSource.FBlockList.Count>0 then begin
       For i:=0 to CADSource.FBlockList.Count-1 do begin
           block:=TBlock(CADSource.FBlockList.Items[i]);
           BlkList.Items.Add(PadL(block.BlockName,' ',20)+Inttostr(block.FCurveList.Count));
       end;
    end
    ELSE BlkList.Clear;
    BlkList.Repaint;
 end;
except
 Exit;
end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
   ComponentInspector.Instance:= TComponent(Sablon);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  dxfObj.Regenerate(DXFMemo.Lines);
  dxfObj.GetStructure(StructTreeView.Items);
    GridInit;
    LayerGridInit;
    BlockGridInit;
    BlkView.Repaint;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  if SaveDialog2.Execute then
     DXFMemo.Lines.SaveToFile(SaveDialog2.FileName);
end;

procedure TMainForm.FileListBox1Click(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
  FileName := FileListBox1.Filename;
end;

procedure TMainForm.Grid1Click(Sender: TObject);
begin
  Sablon.Grid.Visible := not Sablon.Grid.Visible;
  Grid1.Checked := Sablon.Grid.Visible;
end;

procedure TMainForm.GridInit;
var I,H: integer;
    Cuv: TCurve;
    s  : string;
begin
Try
 With Sablon do begin
    ObjectList.Clear;
    Recall;
    if CADSource.FcurveList.Count>0 then begin
       For i:=0 to CADSource.FcurveList.Count-1 do begin
           Cuv:=CADSource.FcurveList.Items[i];
           s:=PadL(Cuv.Name,' ',14)+'('+DrawModeText[Ord(Cuv.Shape)]+')-' + Inttostr(Cuv.count);
           if cuv.Shape=dmInsert then
              s := s+'-'+cuv.BlockParams.BlockName;
           ObjectList.Items.Add(s);
           ObjectList.Checked[i] := Cuv.Selected;
           if Selected=Cuv then begin
              GetCurveHandle(Cuv.name,H);
              ObjectList.ItemIndex := i;
           end;
           Application.ProcessMessages;
       end;
    end
    ELSE ObjectList.Clear;
    ObjectList.Repaint;
 end;
except
 Exit;
end;
end;

procedure TMainForm.HelpContext1Click(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet3;
  if English1.Checked then
     RichEdit1.Lines.LoadFromFile(DefaultDir+'ENG_DXFExplorer.rtf');
  if Hungarian1.Checked then
     RichEdit1.Lines.LoadFromFile(DefaultDir+'HUN_DXFExplorer.rtf');
end;

procedure TMainForm.Hinted1Click(Sender: TObject);
begin
  Sablon.Hinted := not Sablon.Hinted;
  Hinted1.Checked := Sablon.Hinted;
end;

procedure TMainForm.Hungarian1Click(Sender: TObject);
begin
  Hungarian1.Checked := True;
  langfile := Defaultdir+'HUN.lng';
  janLanguage1.InitLanguage(self,LangFile);
  HelpContext1Click(nil);
end;

procedure TMainForm.InversSelection1Click(Sender: TObject);
begin
  Sablon.CADSource.FCurveList.InversSelectedCurves;
  GridInit;
  Sablon.Dopaint;
end;

procedure TMainForm.LayerColor1Click(Sender: TObject);
begin
  CheckBox3.Checked := not CheckBox3.Checked;
  LayerColor1.Checked := CheckBox3.Checked;
end;

procedure TMainForm.LayerGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Var
  color: integer;
  br: TColor;
begin
  if (ARow>0) and (ARow<Sablon.LayerCount+1) then
  if ACol=2 then begin
     with LayerGrid.Canvas do begin
         br := Brush.Color;
         color := Sablon.CadSource.Layers[ARow-1].Pen.Color;
         Brush.Color := color;
         FillRect(Rect);
         Brush.Color := br;
     end;
  end;
end;

procedure TMainForm.LayerGridInit;
const grTitle : array[0..4] of string = ('No','Name','Col.','Vis.','LType');
var i,j: integer;
begin
  with LayerGrid do begin
    if Sablon.LayerCount=0 then begin
       RowCount := 2;
       Enabled  := False;
    end
    else begin
       RowCount := Sablon.LayerCount+1;
       Enabled  := True;
    end;
//    LayerGrid.Height := DefaultRowHeight * RowCount + Sablon.LayerCount+4;
    for I := 0 to 4 do
      Cells[i,0] := grTitle[i];
    if Sablon.LayerCount>0 then
    for j := 1 to Pred(RowCount) do
    begin
         Cells[0,j] := IntToStr(j-1);
         Cells[1,j] := Sablon.CadSource.Layers[j-1].Name;
         if Sablon.CadSource.Layers[j-1].Visible then
            Cells[3,j] := 'x'
         else
            Cells[3,j] := ' ';
//         Cells[4,j] := Sablon.CadSource.Layers[j-1].LineTypeName;
    end;
    Repaint;
  end;
end;

procedure TMainForm.LayerGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if ARow>0 then
  if ACol=2 then
  begin
    ColorDialog.Color := Sablon.Layers[ARow-1].Pen.Color;
    if ColorDialog.Execute then begin
       Sablon.Layers[ARow-1].Pen.Color := ColorDialog.Color;
       Sablon.Dopaint;
    end;
  end;
  with LayerGrid do begin
  If ACol = 3 then
  begin
    if Cells[Acol,ARow]='' then
       Cells[Acol,ARow]:='x'
    else
       Cells[Acol,ARow]:='';
    Sablon.Layers[ARow-1].Visible := Cells[Acol,ARow]='x';
    Sablon.Dopaint;
  end;
  ComponentInspector.Instance := TComponent(Sablon.Layers[ARow-1]);
  Panel9.Caption := 'Layer : '+Sablon.Layers[ARow-1].Name;
  end;
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  Sablon.Clear;
end;

procedure TMainForm.ObjectListClick(Sender: TObject);
begin
if ObjectList.Enabled then
Try
//  Sablon.SelectedVisible := True;
  if Sablon.CentralCross then
     Sablon.CurveToCent(ObjectList.ItemIndex);
  ObjectList.Repaint;
  Sablon.Selected := Sablon.Curves[ObjectList.ItemIndex];
  Sablon.Repaint;
except
  Exit;
End;
end;

procedure TMainForm.ObjectListClickCheck(Sender: TObject);
Var Cuv: TCurve;
begin
if ObjectList.Enabled then
begin
  Cuv := Sablon.CADSource.Curves[ObjectList.ItemIndex];
  Cuv.Selected := ObjectList.Checked[ObjectList.ItemIndex] ;
  if Sablon.CentralCross then
     Sablon.CurveToCent(ObjectList.ItemIndex);
  Sablon.repaint;
end;
end;

procedure TMainForm.Open1Click(Sender: TObject);
begin
  FileListBox1.SetFocus;
end;

procedure TMainForm.Pointsnodes1Click(Sender: TObject);
begin
  CheckBox2.Checked    := not CheckBox2.Checked;
  Pointsnodes1.Checked := CheckBox2.Checked;
end;

procedure TMainForm.RefreshList1Click(Sender: TObject);
begin
  GridInit;
  Sablon.Repaint;
end;

procedure TMainForm.Reopen1Click(Sender: TObject);
begin
  Filename := FFileName;
end;

procedure TMainForm.Rescale(scaleF: integer);
var sf: integer;
    s:  string;
begin
  // Form átméretezése ScaleBy (50....150 %)
  PageControl1.ScaleBy(Round((100/ScaleFactor)*scaleF),100);
  PageControl2.ScaleBy(Round((100/ScaleFactor)*scaleF),100);
  WindowState := wsMaximized;
  ScaleFactor := scaleF;
end;

function GetNodeByText(ATree : TTreeView; AValue:String;
                       AVisible: Boolean): TTreeNode;
var
 Node: TTreeNode;
begin
 Result := nil;
 if ATree.Items.Count = 0 then Exit;
 Node := ATree.Items[0];
 while Node<>nil do begin
       if UpperCase(Node.Text) = UpperCase(AValue) then begin
          Result := Node;
          if AVisible then
             Result.MakeVisible;
          Break;
       end;
       Node := Node.GetNext;
 end;
end;


procedure TMainForm.SablonChangeSelected(Sender: TObject; Curve: TCurve;
  Point: Integer);
VAR H: integer;
    node : TTreeNode;
begin
if not Sablon.loading then
begin
  with ComponentInspector do
  if Curve<>nil then begin
    Instance:= TComponent(Curve);
    Panel9.Caption := Curve.Name+' ('+DrawModeText[Ord(Curve.Shape)]+')';
    if Sablon.CADSource.FCurveList.GetCurveHandle(Curve.Name,H) then
    begin
       if Sablon.PrevSelectedIndex>-1 then
          ObjectList.Checked[Sablon.PrevSelectedIndex] := False;
       Sablon.PrevSelectedIndex := H;
       ObjectList.ItemIndex := H;
       ObjectList.Checked[H] := curve.Selected;
    end;
    node := GetNodeByText(StructTreeView,curve.Name,True);
    if node<>nil then begin
       if StructTreeView.Showing then
//          StructTreeView.SetFocus;
          node.Selected := True;
          ActiveControl:=Sablon;
    end;
  end else
    ComponentInspector.Instance:= Sablon;
//  PointGrid.Visible := ComponentInspector.Instance<>Sablon;
//  PointGridInit(curve);
end;
end;

procedure TMainForm.SablonChangeWindow(Sender: TObject; Cent: TPoint2D;
  Zoom: Double; CursorPos: TPoint);
var T: TPoint;
begin
  if (LastRotAngle<>Sablon.RotAngle) then
  begin
     RotTrackBar.Position := Round(Sablon.RotAngle);
  end;
  lastRotAngle := Sablon.RotAngle;
  RotLabel.Caption := FloatToStr(lastRotAngle);
end;

procedure TMainForm.SablonMouseLeave(Sender: TObject);
begin
  if Sablon.Hint_Label<>nil then
     Sablon.Hint_Label.Visible := False;
end;

procedure TMainForm.SablonMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
VAR px,py: double;
begin
  px := Sablon.MapPoint.x;
  py := Sablon.MapPoint.y;
  lbCoord.Caption := Trim(Format('%6.2f',[px]))+' : '+Trim(Format('%6.2f',[py]));
  lbCoord.Update;
end;

procedure TMainForm.SelectAll1Click(Sender: TObject);
begin
  Sablon.SelectAll(True);
  GridInit;
  Sablon.Dopaint;
end;

procedure TMainForm.SetFileName(const Value: string);
VAR fn,ext: string;
  block : TBlock;
begin
  FFileName := Value;
if FFileName<>'' then
Try
//    if not Sablon.STOP then Sablon.STOP := True;

    Screen.Cursor := crHourGlass;
    Sablon.Visible := True;
    if not Sablon.CADSource.Append then
       Sablon.CADSource.Clear;
    Sablon.Repaint;
    Sablon.EnablePaint := false;
    Sablon.Loading := True;
    DXFMemo.Clear;
    fn := Value;
    ext := UpperCase(ExtractFileExt(fn));
    Sablon.EnablePaint := False;

    Sablon.LoadGraphFromFile(FFileName);

    Sablon.EnablePaint := True;
    Sablon.Loading := False;
    Sablon.ZoomDrawing;

    if ext='.DXF' then
    begin
    // Memo-ba beolvassa a fájl tartalmát
      DXFMemo.Lines.LoadFromFile(FFileName);
      dxfObj.DXFFileName := FFileName;
      dxfObj.GetStructure(StructTreeView.Items);
    end;

    GridInit;
    LayerGridInit;
    BlockGridInit;
    BlkView.Repaint;

//    Sablon.STOP := False;

    Caption := focim + ' ['+FFileName+']';
    StatusBar.Panels[0].Text := ExtractFileName(FFileName);
    Screen.Cursor := crDefault;
except
  Screen.Cursor := crDefault;
  Sablon.Visible := False;
  FileName := '';
  Sablon.Loading := False;
end;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
begin
  Sablon.ZoomPaper;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  Sablon.ZoomDrawing;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  PageControl3.Visible := not PageControl3.Visible;
  PageControl2.Visible := not PageControl2.Visible;
end;

procedure TMainForm.SpeedButton7Click(Sender: TObject);
begin
  FindNextInMemo(SRCEdit.Text);
end;

procedure TMainForm.SRCEditChange(Sender: TObject);
begin
  FindInMemo(SRCEdit.Text);
end;

procedure TMainForm.StructTreeViewChange(Sender: TObject; Node: TTreeNode);
var sorszam: integer;
    s1,s2  : integer;
    n,y,y0 : integer;
begin
  sorszam := 0;
//  StructTreeView.Select(Node,[ssLeft]);
  if Node.Data<>nil then begin
     ObjectList.Repaint;
     sorszam := Integer(Node.Data^);
     s1 := DXFMemo.Perform (EM_LineIndex, sorszam, 0);
     s2 := DXFMemo.Perform (EM_LineIndex, sorszam+1, 0);
     DXFMemo.SelStart := s1;
     DXFMemo.SelLength := s2-s1;
     y0 := SendMessage(DXFMemo.Handle,EM_GETFIRSTVISIBLELINE,0,0);
     y := DXFMemo.CaretPos.Y;
     DXFMemo.Perform(EM_LINESCROLL, 0,  Y-Y0);
     if Pos('_',node.Text)>0 then begin
        n := Pos('_',node.Text)+1;
        n := StrToInt(Copy(node.Text,n,100));
        DXFMemo.Refresh;
        ObjectList.ItemIndex := n;
        if Sablon.Showing then begin
           Sablon.Selected := Sablon.Curves[n];
           Sablon.SelectedIndex := n;
        end;
        DXFMemo.Refresh;
        ComponentInspector.Instance := TComponent(Sablon.Curves[n]);
//        Sablon.SelectedVisible := True;
        if Sablon.CentralCross then begin
             Sablon.CurveToCent(n);
             ObjectList.Repaint;
        end;
     end;
     if node.Parent<>NIL then
     BEGIN
     if node.Parent.Text='LAYER' then
        ComponentInspector.Instance :=
          TComponent(Sablon.CADSource.FLayerList.GetLayerByName(node.Text));
(*     if node.Parent.Text='LTYPE' then
        ComponentInspector.Instance :=
          TComponent(Sablon.CADSource.FLineTypeList.GetLinetypeByName(node.Text));*)
     END;

  end;
end;

procedure TMainForm.UnselectAll1Click(Sender: TObject);
begin
  Sablon.SelectAll(False);
  GridInit;
  Sablon.Dopaint;
end;

procedure TMainForm.RotTrackBarChange(Sender: TObject);
begin
  if RotBar then
     Sablon.RotAngle := RotTrackBar.Position;
end;

procedure TMainForm.RotTrackBarEnter(Sender: TObject);
begin
  RotBar := True;
end;

procedure TMainForm.RotTrackBarExit(Sender: TObject);
begin
  RotBar := False;
end;

procedure ScrollLineToTop( Memo: TCustomMemo; LineNumber:Integer);
var
 TempLine:Integer;
begin
  with Memo do
  begin
    // scroll relative to the first visible line
    TempLine:=Perform(EM_GETFIRSTVISIBLELINE,0,0);
    Perform(EM_LINESCROLL,0,(LineNumber-TempLine));
    // move the caret
    SelStart:=Perform( EM_LINEINDEX, LineNumber, 0);
    Perform( EM_SCROLLCARET, 0, 0);
  end;
end;

function TMainForm.FindInMemo(sText: string): boolean;
var i: integer;
    AFirstLine: integer;
begin
  if sText='' then begin
       DXFMemo.SelStart := SendMessage(DXFMemo.Handle, EM_LINEINDEX, 0, 0);
       DXFMemo.SelLength := Length(sText);
       SendMessage(DXFMemo.Handle, EM_SCROLLCARET, 0, 0);
       lineNumber := 0;
       exit;
  end;
  for  I:= lineNumber to DXFMemo.lines.count-1 do
    if UpperCase(Copy(DXFMemo.lines[I],1,Length(sText))) = sText then begin
       AFirstLine := DXFMemo.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
       SendMessage(DXFMemo.Handle, EM_SCROLLCARET, I, 0);
       ScrollLineToTop(DXFMemo,lineNumber);
       DXFMemo.SelStart := SendMessage(DXFMemo.Handle, EM_LINEINDEX, I, 0);
       DXFMemo.SelLength := Length(sText);
       lineNumber := I;
       Exit;
    end;
end;

function TMainForm.FindNextInMemo(sText: string): boolean;
var i: integer;
begin
  if sText='' then begin
       DXFMemo.SelStart := SendMessage(DXFMemo.Handle, EM_LINEINDEX, lineNumber, 0);
       SendMessage(DXFMemo.Handle, EM_SCROLLCARET, 0, 0);
       lineNumber := 0;
       exit;
  end;
  for  I:= lineNumber to DXFMemo.lines.count-1 do
    if Copy(DXFMemo.lines[I],1,Length(sText)) = sText then begin
       SendMessage(DXFMemo.Handle, EM_SCROLLCARET, I, 0);
       ScrollLineToTop(DXFMemo,i);
       DXFMemo.SelStart := SendMessage(DXFMemo.Handle, EM_LINEINDEX, I, 0);
       DXFMemo.SelLength := Length(sText);
       lineNumber := I+1;
       Exit;
    end;
  lineNumber:=I+1;
  if lineNumber>=DXFMemo.lines.count then
  begin
     ShowMessage('End of Text');
     lineNumber := 0;
     DXFMemo.SelStart := SendMessage(DXFMemo.Handle, EM_LINEINDEX, lineNumber, 0);
     SendMessage(DXFMemo.Handle, EM_SCROLLCARET, 0, 0);
     FindInMemo(sText);
     Exit;
  end;
end;

initialization
  focim := 'DXFExplorerX 1.0';
  DefaultDir := ExtractFilePath(Application.Exename);
end.
