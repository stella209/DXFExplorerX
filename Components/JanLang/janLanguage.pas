unit janLanguage;

{ This component is an orginal component created 17-october 1999 by Jan Verhoeven
  and is offered as freeware for free use in any application including commercial applications.
  You are free to copy and modify the source code
  provided you do not remove this comment.

  email:
    jan1.verhoeven@wxs.nl

  website:
    http://members.xoom.com/JanVee/jfdelphi.htm
   }

   // Modified by Agócs László Hungary 2020

   // New stored property are TStrings types
   // In text: Enter = \n
   // Az ini fájl (Lang.ini) le van tiltva.
   // Csak a Language.lng-be ír
   // A LanguageFile változó mentésérõl a hívó peogramnak kell gondoskodni.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TypInfo,Inifiles, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TjanLanguage = class(TComponent)
  private
    FLanguageFile: string;
    FSaveAble: boolean;
    FChangeLanguage: TNotifyEvent;
    function appldir: string;
    function inifile: string;
    procedure SetLanguageFile(const Value: string);
  protected
    oldLanguage : string;
  public
    constructor Create(AOwner:TComponent);override;
    procedure InitLanguage(AForm:TForm); overload;
    procedure InitLanguage(AForm:TForm; AFile: string); overload;
    procedure ChangeLanguage(AForm:Tform);
    function GetLang: string;
    procedure LoadLanguage(AForm: TForm; Afile: string);
    procedure SaveLanguage(AForm: TForm; Afile: string);
  published
    property LanguageFile:string read FLanguageFile write SetLanguageFile;
    property SaveAble: boolean read FSaveAble write FSaveAble;
    property OnChangeLanguage: TNotifyEvent read FChangeLanguage write FChangeLanguage;
  end;

procedure Register;

implementation

const
  cr = chr(13);
  eol = chr(13)+chr(10);
  tab = chr(9);

procedure Register;
begin
  RegisterComponents('Jans 2', [TjanLanguage]);
end;

constructor TjanLanguage.Create(AOwner: TComponent);
Var
  ini:TIniFile;
  iFile: string;
begin
  inherited;
  oldLanguage := '';
  FSaveAble := True;
end;

// Get language file name from application.ini file
function TjanLanguage.GetLang: string;
begin
 Result := LanguageFile;
end;

function TjanLanguage.appldir:string;
begin
 result:=extractfilepath(application.exename);
end;

function TjanLanguage.inifile:string;
begin
  result:=ExtractFilePath(application.exename)+'Lang.ini';
end;

procedure TjanLanguage.ChangeLanguage(AForm: Tform);
var
  ini:TIniFile;
  od:TOpendialog;
begin
 od:=TOpendialog.Create(self) ;
 if (LanguageFile='') or (not FileExists(LanguageFile)) then
    od.InitialDir :=appldir
 else
    od.InitialDir := ExtractFilePath(LanguageFile);
    od.FileName   := LanguageFile;
 od.Filter :='Language Files |*.lng';
 if od.Execute then
 begin
   LanguageFile:=od.FileName ;
   InitLanguage(Aform,LanguageFile);
 end;
 od.free;
end;

procedure TjanLanguage.InitLanguage(AForm: TForm; AFile: string);
begin
  LanguageFile := AFile;
  InitLanguage(AForm);
end;

procedure TjanLanguage.InitLanguage(AForm: TForm);
var
  ini:TIniFile;
begin
 if oldLanguage='' then
    SaveLanguage (Aform,appldir+'Language.lng');
    if LanguageFile<>'' then
       if fileexists(LanguageFile) then
       begin
            LoadLanguage (Aform,LanguageFile);
            oldLanguage := LanguageFile;
       end;
end;

procedure TjanLanguage.LoadLanguage(AForm:TForm;Afile:string);
var c:Tcomponent;
    ini:tinifile;
    langfile:string;
    langs:TStringlist;
    i,p:integer;
    aname,avalue:string;
    afrm,acomp,aprop:string;
    AComponent:Tcomponent;
    PropInfo:PPropInfo;
    ts: TObject;

    procedure split(s:string);
    var p:integer;
    begin
      p:=pos('=',s);
      aname:=copy(s,1,p-1);
      avalue:=copy(s,p+1,length(s));
      avalue:=stringreplace(avalue,'\n',cr,[rfreplaceall]);
      p:=pos('.',aname);
      afrm:=copy(aname,1,p-1);
      aname:=copy(aname,p+1,length(aname));
      p:=pos('.',aname);
      acomp:=copy(aname,1,p-1);
      aprop:=copy(aname,p+1,length(aname));
    end;

begin
Try
  langfile:=Afile;
  ini:=tinifile.Create (langfile);
  langs:=tstringlist.create;
  ini.ReadSectionValues (AForm.name,langs);
  if langs.Count >0 then
  begin

    if Aform <> nil then
       AForm.Caption := ini.ReadString(AForm.name,AForm.Name+'.Caption','');

    for i:=0 to langs.count-1 do
    begin
      split(langs[i]);
      if Aform.name<>Afrm then continue;
      AComponent:=Aform.findcomponent(acomp);
      if AComponent= nil then continue;
      PropInfo:=GetPropInfo(Acomponent,aprop);
      if PropInfo<>nil then begin
       if propinfo^.PropType^.Kind in [tkUString,tkLString,tkString,tkWString] then
         setStrProp(Acomponent,aprop,avalue);
       if propinfo^.PropType^.Kind = tkClass then
       begin
         ts := nil;
         PropInfo:=GetPropInfo(Acomponent,'Lines');
         if PropInfo<>nil then begin
            ts := GetObjectProp(Acomponent,PropInfo,nil);
            if ts <> nil then
            TCustomMemo(Acomponent).text := avalue;
         end;
         PropInfo:=GetPropInfo(Acomponent,'Items');
         if PropInfo<>nil then begin
            ts := GetObjectProp(Acomponent,PropInfo,nil);
            if ts <> nil then begin
            if Acomponent is TCustomListBox then
               TCustomListBox(Acomponent).Items.text := avalue;
            if Acomponent is TCustomComboBox then
               TCustomComboBox(Acomponent).Items.text := avalue;
            if Acomponent is TRadioGroup then
               TRadioGroup(Acomponent).Items.text := avalue;
            end;
         end;
        end;
      end;
    end;
  end;
  ini.free;
  langs.free;
except
  if ini<>nil then ini.free;
  if langs<>nil then langs.free;
end;
end;

procedure TjanLanguage.SaveLanguage(AForm:TForm;Afile:string);
var
    i,j:integer;
    langini:TInifile;
    LangFile:string;
    acap:string;
    ahint:string;
    atext:string;
    PropInfo:PPropInfo;
    TProp : TPropInfo;
    TKind : TTypeKind;
    ts    : TObject;
    m:Tcomponent;
begin
if FSaveAble then
begin
 LangFile:=Afile;
 langini:=TInifile.Create(LangFile);
   PropInfo:=GetPropInfo(Aform,'caption');
      if PropInfo<>nil then
      begin
        acap:=GetStrProp(Aform,PropInfo);
        acap:=stringreplace(acap,cr,'\n',[rfreplaceall]);
        if ((acap<>'')and(acap<>'-')) then
          Langini.WriteString (AForm.name,AForm.name+'.Caption',acap)
      end;
 for i:=0 to Aform.ComponentCount-1 do
 begin
   m:=Aform.components[i];
   PropInfo:=GetPropInfo(m,'caption');
      if PropInfo<>nil then
      begin
        acap:=GetStrProp(m,PropInfo);
        acap:=stringreplace(acap,eol,'\n',[rfreplaceall]);
        acap:=stringreplace(acap,cr,'\n',[rfreplaceall]);
        if ((acap<>'')and(acap<>'-')) then
          Langini.WriteString (AForm.name,AForm.name+'.'+m.name+'.Caption',acap)
      end;
  PropInfo:=GetPropInfo(m,'hint');
      if PropInfo<>nil then
      begin
        ahint:=GetStrProp(m,PropInfo);
        ahint:=stringreplace(ahint,eol,'\n',[rfreplaceall]);
        ahint:=stringreplace(ahint,cr,'\n',[rfreplaceall]);
        if ahint<>'' then
          Langini.WriteString (AForm.name,AForm.name+'.'+m.name+'.Hint',ahint)
      end;
   PropInfo:=GetPropInfo(m,'text');
      if PropInfo<>nil then
      begin
        atext:=GetStrProp(m,PropInfo);
        atext:=stringreplace(atext,eol,'\n',[rfreplaceall]);
        atext:=stringreplace(atext,cr,'\n',[rfreplaceall]);
        if atext<>'' then
          Langini.WriteString (AForm.name,AForm.name+'.'+m.name+'.Text',atext)
      end;
   PropInfo:=GetPropInfo(m,'Lines');
      if PropInfo<>nil then
      begin
        ts := GetObjectProp(m,PropInfo,nil);
        if ts is TStrings then
        begin
        atext:=TStrings(ts).Text;
        atext:=stringreplace(atext,eol,'\n',[rfreplaceall]);
        atext:=stringreplace(atext,cr,'\n',[rfreplaceall]);
        if atext<>'' then
          Langini.WriteString (AForm.name,AForm.name+'.'+m.name+'.Lines',atext)
        end;
      end;
   PropInfo:=GetPropInfo(m,'Items');
      if PropInfo<>nil then begin
        ts := GetObjectProp(m,PropInfo,nil);
        if ts is TStrings then
        begin
        atext:=TStrings(ts).Text;
        atext:=stringreplace(atext,eol,'\n',[rfreplaceall]);
        atext:=stringreplace(atext,cr,'\n',[rfreplaceall]);
        if atext<>'' then
          Langini.WriteString (AForm.name,AForm.name+'.'+m.name+'.Items',atext)
        end;
      end;
 end;
 Langini.Free;
end;
end;


procedure TjanLanguage.SetLanguageFile(const Value: string);
begin
   FLanguageFile := Value;
   if Assigned(FChangeLanguage) then
      FChangeLanguage(Self);
end;

end.
