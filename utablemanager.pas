unit uTableManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, UTable, Forms, DbCtrls, UMeta;

type

  { TTableManager }

  TTableManager = class
    private
      FForm: TTableForm;
      FTable: TMyTable;
      FFields: array of TMyField;
      FMenuItemLink: TMenuItem;
      procedure MakeForm(); virtual;
      procedure OnClickEvent(Sender: TObject);
      procedure OnCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
    public
      constructor Create(ATable: TMyTable);
      function GetMenuItem(AParent: TControl): TMenuItem;
      destructor Destroy(); override;
  end;

implementation

{ TTableManager }

procedure TTableManager.MakeForm();
begin
  Application.CreateForm(TTableForm, FForm);
  with FForm do begin
    Caption := FTable.Caption;
  end;
end;

procedure TTableManager.OnClickEvent(Sender: TObject);
begin
  If FForm = nil then MakeForm();
  (Sender as TMenuItem).Checked := True;
  FForm.ShowOnTop;
end;

procedure TTableManager.OnCloseEvent(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FMenuItemLink.Checked := False;
end;

constructor TTableManager.Create(ATable: TMyTable);
begin
  FTable := ATable;
end;

function TTableManager.GetMenuItem(AParent: TControl): TMenuItem;
begin
  Result := TMenuItem.Create(AParent);
  FMenuItemLink := Result;
  with Result do begin
    Caption := FTable.Caption;
    OnClick := @OnClickEvent;
  end;
end;

destructor TTableManager.Destroy;
var
  f: TMyField;
begin
  inherited Destroy();
  FTable.Free;
  for f in FFields do
    f.Free;
end;

end.

