unit _About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,  ExtCtrls, Buttons, ShellApi, janLanguage;

type
  TAboutBox = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    VerLabel: TLabel;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    janLanguage1: TjanLanguage;
    Label6: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    FDonate: boolean;
    oldLanguage : string;
  public
//    procedure ChangeLanguage;
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}


procedure TAboutBox.FormActivate(Sender: TObject);
begin
  VerLabel.Caption := 'Version 1.0';
  if oldLanguage <> janLanguage1.GetLang then begin
     janLanguage1.InitLanguage(Self);
     oldLanguage := janLanguage1.LanguageFile;
  end;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  janLanguage1.InitLanguage(Self);
  oldLanguage := janLanguage1.LanguageFile;
end;

procedure TAboutBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then close;
end;

procedure TAboutBox.Label6Click(Sender: TObject);
begin
  shellexecute(0,'open',pchar(Label6.Caption),nil,nil,sw_normal);
end;

procedure TAboutBox.SpeedButton1Click(Sender: TObject);
begin
  close;
end;

end.
