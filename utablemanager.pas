unit uTableManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, UTable, Forms, DbCtrls, UMeta, sqldb;

type

  { TTableManager }

  TTableManager = class
    private
      FForm: TTableForm;
      FTable: TMyTable;
      FMenuItemLink: TMenuItem;
      procedure MakeForm(); virtual;
      procedure OnClickEvent(Sender: TObject);
      procedure OnCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      function GetSQLCode(): string; virtual;
    public
      constructor Create(ATable: TMyTable); virtual;
      function GetMenuItem(AParent: TControl): TMenuItem;
  end;

  { TRefrenceTableManager }

  TRefrenceTableManager = class(TTableManager)
    private
      FRefrences: TMyTableArray;
      procedure MakeForm(); override;
      function GetSQLCode: string; override;
    public
      constructor Create(ATable: TMyTable); override;
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
  for t in MetaLibrary.Tables do
    If t is TTRefrenceTable then
      AddRefrenceTableManager(t)
    else
      AddTableManager(t);
end;

{ TRefrenceTableManager }

procedure TRefrenceTableManager.MakeForm();var
  i: integer;
  s: String;
begin
  Application.CreateForm(TTableForm, FForm);
 // AddFields(FForm.SQLQuery);
  With FForm do begin
    OnClose := @OnCloseEvent;
    Caption := FTable.Caption;
    with SQLQuery do begin
      SQL.Add(GetSQLCode());
      Active := True;
    end;
    with DBGrid.Columns do
      for i := 0 to Count - 1 do begin
        Items[i].Title.Caption :=
          (FTable.Fields[i] as TFIDRefrence).RefrenceTable.Fields[1].Caption;
        Items[i].Width :=
          (FTable.Fields[i] as TFIDRefrence).RefrenceTable.Fields[1].Width;
      end;
  end;
//begin
  //inherited MakeForm();
  FForm.DBNavigator.VisibleButtons := [nbFirst, nbLast, nbNext, nbPrior]
end;

function TRefrenceTableManager.GetSQLCode: string;
var
  i: integer;
begin
  Result := 'SELECT ';
  with FTable do begin
    for i := 0 to MaxIndex do begin
      Result += FRefrences[i].Name + '.' + FRefrences[i].Fields[1].Name;
      if i <> MaxIndex then
        Result += ', ';
    end;
    Result += 'FROM ' + Name;
    for i := 0 to High(FRefrences) do begin
      Result += 'INNER JOIN ' + FRefrences[i].Name;
      Result += 'ON ' + FRefrences[i].Name + '.' + FRefrences[i].Fields[0].Name +
        ' = ' + Name + '.' + Fields[i].Name;
    end;
  end;
end;

constructor TRefrenceTableManager.Create(ATable: TMyTable);
var
  i: integer;
begin
  inherited Create(ATable);
  If ATable is TTLessonsTable then
    i := 1;
  with ATable do begin
    SetLength(FRefrences, MaxIndex + 1);
    for i := 0 to MaxIndex do
      FRefrences[i] := (Fields[i] as TFIDRefrence).RefrenceTable;
  end;
end;

{ TTableManager }

procedure TTableManager.MakeForm();
var
  i: integer;
  s: String;
begin
  Application.CreateForm(TTableForm, FForm);
 // AddFields(FForm.SQLQuery);
  With FForm do begin
    OnClose := @OnCloseEvent;
    Caption := FTable.Caption;
    with SQLQuery do begin
      SQL.Add(GetSQLCode());
      Active := True;
    end;
    with DBGrid.Columns do
      for i := 0 to Count - 1 do begin
        Items[i].Title.Caption := FTable.Fields[i].Caption;
        Items[i].Width := FTable.Fields[i].Width;
      end;
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

function TTableManager.GetSQLCode(): string;
begin
  Result := 'SELECT * FROM ' + FTable.Name;
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

initialization
  AddAllManagers();

end.

