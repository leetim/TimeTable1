unit uRedactorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

const
  Indent = 5;
  LableHeight = 22;
  LableWidth = 100;
  EditWidth = 100;
  EditHight = 22;

type

  { TRedactorForm }

  TRedactorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FActionButton: TButton;
    FLables: array of TLabel;
    FEdits: array of TWinControl;
  end;

implementation

{$R *.lfm}

{ TRedactorForm }

procedure TRedactorForm.FormCreate(Sender: TObject);
begin
  with Label1 do begin
    Top := Indent;
    Left := Indent;
  end;
  with Label2 do begin
    Top := Indent;
    Left := 2 * Indent + LableWidth;
  end;
end;

end.

