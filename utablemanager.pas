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

  { TRefrenceTableManager }

  TRefrenceTableManager = class(TTableManager)
    private
      procedure MakeForm(); override;
    public
  end;

  procedure AddTableManager(ATable: TMyTable);
  procedure AddRefrenceTableManager(ATable: TMyTable);
  procedure AddAllManagers();

var
  TableManagers: array of TTableManager;

implementation

procedure AddTableManager(ATable: TMyTable);
begin
  SetLength(TableManagers, Length(TableManagers) + 1);
  TableManagers[High(TableManagers)] := TTableManager.Create(ATable);
end;

procedure AddRefrenceTableManager(ATable: TMyTable);
begin
  SetLength(TableManagers, Length(TableManagers) + 1);
  TableManagers[High(TableManagers)] := TRefrenceTableManager.Create(ATable);
end;

procedure AddAllManagers;
var
  t: TMyTable;
begin
  for t in Tables do
    If t is TTRefrenceTable then
      AddRefrenceTableManager(t)
    else
      AddTableManager(t);
end;

{ TRefrenceTableManager }

procedure TRefrenceTableManager.MakeForm();
begin
  inherited MakeForm();
  FForm.DBNavigator.VisibleButtons := [nbFirst, nbLast, nbNext, nbPrior]
end;

{ TTableManager }

procedure TTableManager.MakeForm();
var
  i: integer;
  s: TStringList;
begin
  Application.CreateForm(TTableForm, FForm);
  With FForm do begin
    OnClose := @OnCloseEvent;
    Caption := FTable.Caption;
    with SQLQuery do begin
      s := FTable.GetSQLCode();
      SQL.AddStrings(s);
      s.Free;
      Active := True;
    end;
    with DBGrid.Columns do
      for i := 0 to Count - 1 do begin
        Items[i].Title.Caption := FTable.Fields[i].Caption;
        Items[i].Width := FTable.Fields[i].Width;
      end;
  end;;
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

initialization
  AddAllManagers();

end.

