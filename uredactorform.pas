unit uRedactorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

const
  Indent = 5;
  LableHeight = 22;
  LableWidth = 150;
  EditWidth = 150;
  EditHeight = 22;

type
  TArray = array of TControl;

  { TRedactorForm }

  TRedactorForm = class(TForm)
    ActionButton: TButton;
    CancelButton: TButton;
    Panel: TPanel;
    SQLQuery1: TSQLQuery;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FLables: array of TLabel;
    FEdits: array of TWinControl;
    function ComponentCreate(var AArray: TArray; AClass: TControlClass): TControl;
  end;

implementation

{$R *.lfm}

{ TRedactorForm }

procedure TRedactorForm.FormCreate(Sender: TObject);
begin
  with ActionButton do begin
    Left := Indent;
    Width := LableWidth;
    Height := LableHeight;
  end;
  with CancelButton do begin
    Left := Indent + (LableWidth + Indent);
    Width := EditWidth;
    Height := EditHeight;
  end;
end;

function TRedactorForm.ComponentCreate(var AArray: TArray; AClass: TControlClass
  ): TControl;
var
  h: integer;
begin
  SetLength(AArray, Length(AArray) + 1);
  Result := AClass.Create(Self);
  h := High(AArray);
  AArray[h] := Result;
  With Result do begin
    Top := Indent + (LableHeight + Indent) * h;
    If Result is TLabel then
      Left := Indent
    else
      Left := Indent + (Indent + LableWidth);
    Width := LableWidth;
    Height := LableHeight;
    Parent := Panel;
  end;
end;

procedure TRedactorForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

end.

