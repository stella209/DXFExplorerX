program DXFExplorX;

uses
  Vcl.Forms,
  MainDXF in 'MainDXF.pas' {MainForm},
  _About in '_About.pas' {AboutBox};

{$R *.res}

begin
  AboutBox := TAboutBox.Create(Application);
  AboutBox.Show;
  AboutBox.Update;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
