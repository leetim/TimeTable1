unit uConnectionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls, Graphics,
  Dialogs;

type

  { TForm1 }

  TForm1 = class(TForm)
    IBConnection1: TIBConnection;
    SQLTransaction1: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //IBConnection1.Connected := True;
  //SQLTransaction1.Active := True;
end;

end.

