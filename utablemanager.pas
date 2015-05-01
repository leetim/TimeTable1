unit uTableManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, UTable, Forms, DbCtrls, UMeta, sqldb,
  DBGrids, StdCtrls, ExtCtrls,  Buttons, uRedactorForm, Graphics, uFilters,
  uRedactor;

type

  { TTableManager }

  TTableManager = class
    private
      FForm: TTableForm;
      FTable: TMyTable;
      FMenuItemLink: TMenuItem;
      FOrderedField: TMyField;
      FDesc: Boolean;
      FFilters: array of TFilter;
      FDeletedPanels: array of TPanel;
      FColumn, FRow: integer;
      FCommited: boolean;
      FUpdateRedactors: array of TRUpdate;
      FDeleteRedactors: array of TRDelete;
      FInsertRedactor: TRInsert;
      procedure MakeForm(); virtual;
      procedure Refresh(); virtual;
      procedure AddFilterEvent(Sender: TObject);
      procedure DeleteFilterEvent(AIndex: Integer);
      procedure ChangeFilterEvent();
      procedure OnItemClickEvent(Sender: TObject);
      procedure OnInsertClick(Sender: TObject);
      procedure OnDeleteClick(Sender: TObject);
      procedure OnEditClick(Sender: TObject);
      procedure OnCommitClick(Sender: TObject);
      procedure OnCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      procedure OnTitleClickEvent(Column: TColumn); virtual; abstract;
      procedure Apply(Sender: TObject);
      function GetSQLCode(): string; virtual;
      procedure SetCommited(AValue: boolean);
      function UpdateRedactorSearch(AID: integer): TRUpdate;
      function DeleteRedactorSearch(AID: integer): TRDelete;
      procedure OnDeleteRedactorEvent(ATag: integer);
    public
      constructor Create(ATable: TMyTable); virtual;
      function GetMenuItem(AParent: TControl): TMenuItem;
      procedure RefreshRedactors();
      property Commited: boolean read FCommited write SetCommited;
  end;

  { TRefrenceTableManager }

  TRefrenceTableManager = class(TTableManager)
    private
      FOrderedFieldPerentTable: TMyTable;
      FRefrences: TMyTableArray;
      FIDs: array of array of string;
      procedure Refresh; override;
      function GetSQLCode: string; override;
      procedure OnTitleClickEvent(Column: TColumn); override;
    public
      constructor Create(ATable: TMyTable); override;
  end;

  { TWithoutRefrenceTableManager }

  TWithoutRefrenceTableManager = class(TTableManager)
    private
      procedure Refresh; override;
      function GetSQLCode: string; override;
      procedure OnTitleClickEvent(Column: TColumn);
  end;

  procedure AddTableManager(ATable: TMyTable);
  procedure AddRefrenceTableManager(ATable: TMyTable);
  procedure AddAllManagers();
  procedure BigRefresh();

var
  TableManagers: array of TTableManager;
  TransactionComponent: TSQLTransaction;

implementation

var
  DeletedRedactors: array of TRedactor;

procedure ClearRedactorFree(ARedactor: TRedactor);
begin
  SetLength(DeletedRedactors, Length(DeletedRedactors) + 1);
  DeletedRedactors[High(DeletedRedactors)] := ARedactor;
end;

procedure ActivateClearRedactorFree();
var
  r: TRedactor;
begin
  for r in DeletedRedactors do
    r.Free;
  SetLength(DeletedRedactors, 0);
end;

procedure AddTableManager(ATable: TMyTable);
begin
  SetLength(TableManagers, Length(TableManagers) + 1);
  TableManagers[High(TableManagers)] :=
  TWithoutRefrenceTableManager.Create(ATable);
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

procedure BigRefresh();
var
  tm: TTableManager;
begin
  for tm in TableManagers do
    tm.RefreshRedactors();
end;

{ TWithoutRefrenceTableManager }

procedure TWithoutRefrenceTableManager.Refresh();
var
  i: integer;
begin
  inherited Refresh();
  with FForm.DBGrid.Columns do begin
    Items[0].Visible := False;
    for i := 1 to Count - 1 do begin
      Items[i].Title.Caption := FTable.Fields[i].Caption;
      Items[i].Width := FTable.Fields[i].Width;
    end;
  end;
end;

function TWithoutRefrenceTableManager.GetSQLCode(): string;
var
  i: integer;
begin
  Result := 'SELECT ';
  with FTable do begin
    for i := 0 to MaxIndex do begin
      Result += Fields[i].Name;
      if i <> MaxIndex then
        Result += ', ';
    end;
    Result += ' FROM ' + Name;
  end;
  Result += inherited GetSQLCode();
  If FOrderedField <> nil then begin
    Result += ' ORDER BY ' + FTable.Name + '.' + FOrderedField.Name;
    if FDesc then Result += ' DESC ';
  end;
end;

procedure TWithoutRefrenceTableManager.OnTitleClickEvent(Column: TColumn);
begin
  If FOrderedField = FTable.Fields[Column.Index] then
    FDesc := not FDesc;
  FOrderedField := FTable.Fields[Column.Index];
  Refresh();
end;

{ TRefrenceTableManager }

procedure TRefrenceTableManager.Refresh();
var
  i: integer;
begin
  Inherited Refresh();
  with FForm.DBGrid.Columns do begin
    Items[0].Visible := False;
    for i := 1 to Count - 1 do
      with (FTable.Fields[i] as TFIDRefrence).RefrenceTable do begin
        Items[i].Title.Caption := Fields[MaxIndex].Caption;
        Items[i].Width := Fields[MaxIndex].Width;
      end;
  end;
end;

function TRefrenceTableManager.GetSQLCode(): string;
var
  i: integer;
begin
  Result := 'SELECT ';
  with FTable do begin
    Result += Name + '.' + IDField.Name + ', ';
    for i := 1 to MaxIndex do begin
      with FRefrences[i] do
        Result += Name + '.' + Fields[MaxIndex].Name;
      if i <> MaxIndex then
        Result += ', ';
    end;
    Result += ' FROM ' + Name;
    for i := 1 to High(FRefrences) do begin
      Result += ' INNER JOIN ' + FRefrences[i].Name;
      Result += ' ON ' + FRefrences[i].Name + '.' + FRefrences[i].IDField.Name +
        ' = ' + Name + '.' + Fields[i].Name;
    end;
  end;
  Result += Inherited GetSQLCode();
  If FOrderedField <> nil then begin
    Result += ' ORDER BY ' + FOrderedFieldPerentTable.Name
      + '.' + FOrderedField.Name;
    if FDesc then Result += ' DESC ';
  end;
end;

procedure TRefrenceTableManager.OnTitleClickEvent(Column: TColumn);
begin
  FOrderedFieldPerentTable :=
    (FTable.Fields[Column.Index] as TFIDRefrence).RefrenceTable;
  with FOrderedFieldPerentTable do begin
    if FOrderedField = Fields[MaxIndex] then
      FDesc := not FDesc;
    FOrderedField := Fields[MaxIndex];
  end;
  Refresh();
end;

constructor TRefrenceTableManager.Create(ATable: TMyTable);
var
  i: integer;
begin
  inherited Create(ATable);
  with ATable do begin
    SetLength(FRefrences, MaxIndex + 1);
    for i := 1 to MaxIndex do
      FRefrences[i] := (Fields[i] as TFIDRefrence).RefrenceTable;
  end;
end;

{ TTableManager }

procedure TTableManager.MakeForm();
begin
  Application.CreateForm(TTableForm, FForm);
  With FForm do begin
    OnClose := @OnCloseEvent;
    DBGrid.OnTitleClick  := @OnTitleClickEvent;
    Caption := FTable.Caption;
    Width := FTable.Width + 2 * Indent + ScrollLength;
    AddFilterButton.OnClick := @AddFilterEvent;
    ApplyButton.OnClick := @Apply;
  end;
end;

procedure TTableManager.Refresh();
var
  i: integer;
  f: TFilter;
begin
  with FForm.SQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Add(Self.GetSQLCode());
    with FForm do begin
      Prepare;
      for i := 0 to High(FFilters) do begin
        f := FFilters[i];
        Params[i].AsString := f.FiltredConst;
      end;
    end;
    Open;
  end;
end;

procedure TTableManager.RefreshRedactors;
var
  r: TRedactor;
begin
  for r in FDeleteRedactors do
    r.RefreshRedactor();
  for r in FUpdateRedactors do
    r.RefreshRedactor();
end;

procedure TTableManager.AddFilterEvent(Sender: TObject);
var
  f: TPanel;
  hf: integer;
begin
  FForm.ApplyButton.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  hf := High(FFilters);
  FFilters[hf] := TFilter.Create(FTable);
  with FForm do begin
    SetLength(FPanels, Length(FFilters));
    FPanels[hf] := FFilters[hf].MakePanel(FForm);
    FPanels[hf].Top := Indent + hf * (ControlHeight + 3 * Indent);
    FPanels[hf].Parent := FForm.FilterPanel;
  end;
  with FFilters[hf] do begin
    Tag := hf;
    DeleteEvent := @DeleteFilterEvent;
    ChangeEvent := @ChangeFilterEvent;
  end;
  If Length(FDeletedPanels) <> 0 then begin
    for f in FDeletedPanels do
      FreeAndNil(f);
    SetLength(FDeletedPanels, 0);
  end;
end;

procedure TTableManager.DeleteFilterEvent(AIndex: Integer);
var
  i: integer;
begin
  if AIndex > High(FFilters) then exit;
  FForm.ApplyButton.Enabled := True;
  FFilters[AIndex].Free();
  SetLength(FDeletedPanels, Length(FDeletedPanels) + 1);
  FDeletedPanels[High(FDeletedPanels)] := FForm.FPanels[AIndex];
  FForm.FPanels[AIndex].Visible := False;
  FForm.FPanels[AIndex] := nil;
  with FForm do
    for i := AIndex to High(FFilters) - 1 do begin
      FFilters[i] := FFilters[i + 1];
      FPanels[i] := FPanels[i + 1];
    end;
  SetLength(FFilters, Length(FFilters) - 1);
  SetLength(FForm.FPanels, Length(FFilters));
  for i := 0 to High(FFilters) do
    with FForm.FPanels[i] do begin
      Top := Indent + i * (ControlHeight + 3 * Indent);
      Tag := i;
      FFilters[i].Tag := i;
    end;
end;

procedure TTableManager.ChangeFilterEvent;
begin
  FForm.ApplyButton.Enabled := True;
end;

procedure TTableManager.OnItemClickEvent(Sender: TObject);
begin
  If FForm = nil then MakeForm();
  FOrderedField := nil;
  FDesc := False;
  Refresh();
  FForm.ShowOnTop;
  (Sender as TMenuItem).Checked := True;
  with FForm do begin
    InsertButton.OnClick := @OnInsertClick;
    EditButton.OnClick := @OnEditClick;
    DeleteButton.OnClick := @OnDeleteClick;
    CommitButton.OnClick := @OnCommitClick;
  end;
end;

procedure TTableManager.OnInsertClick(Sender: TObject);
begin
  If FInsertRedactor = nil then
    FInsertRedactor := TRInsert.Create(FTable);
  FInsertRedactor.Start();
  FInsertRedactor.OnRefreshEvent := @Refresh;
  ActivateClearRedactorFree();
end;

procedure TTableManager.OnDeleteClick(Sender: TObject);
var
  id: integer;
  r: TRedactor;
begin
  id := FForm.DBGrid.DataSource.DataSet.Fields[0].AsInteger;
  r := DeleteRedactorSearch(id);
  r.Start();
  r.OnRefreshEvent := @Refresh;
  (r as TRDelete).OnDeleteRedacor := @OnDeleteRedactorEvent;
  ActivateClearRedactorFree();
end;

procedure TTableManager.OnEditClick(Sender: TObject);
var
  id: integer;
  r: TRedactor;
begin
  id := FForm.DBGrid.DataSource.DataSet.Fields[0].AsInteger;
  r := UpdateRedactorSearch(id);
  r.Start();
  r.OnRefreshEvent := @Refresh;
  ActivateClearRedactorFree();
end;

procedure TTableManager.OnCommitClick(Sender: TObject);
begin
  TransactionComponent.Commit;
  Commited := True;
end;

procedure TTableManager.OnCloseEvent(Sender: TObject;
  var CloseAction: TCloseAction);
var
  r: TRedactor;
begin
  FMenuItemLink.Checked := False;
  FInsertRedactor.CloseForm();
  for r in FUpdateRedactors do
    r.CloseForm();
  for r in FDeleteRedactors do
    r.CloseForm();
end;

procedure TTableManager.Apply(Sender: TObject);
var
  f: TFilter;
begin
  FForm.ApplyButton.Enabled := False;
  for f in FFilters do f.Applied := True;
  Refresh();
end;

function TTableManager.GetSQLCode(): string;
var
  i: integer;
begin
  If Length(FFilters) > 0 then begin
    Result += ' WHERE ';
    for i := 0 to High(FFilters) do begin
      with FFilters[i] do begin
        Result += '(' + FieldName + Operation + ':param' + IntToStr(i) + ')';
        Applied := True;
      end;
      If i <> High(FFilters) then
        Result += ' AND ';
    end;
  end;
end;

procedure TTableManager.SetCommited(AValue: boolean);
begin
  if FCommited = AValue then Exit;
  FCommited := AValue;
  FForm.CommitButton.Enabled := AValue;
end;

function TTableManager.UpdateRedactorSearch(AID: integer): TRUpdate;
var
  r: TRUpdate;
begin
  for r in FUpdateRedactors do
    if r.Check(AID) then Exit(r);
  SetLength(FUpdateRedactors, Length(FUpdateRedactors) + 1);
  Result := TRUpdate.Create(FTable, AID);
  FUpdateRedactors[High(FUpdateRedactors)] := Result;
  Result.Tag := High(FUpdateRedactors);
end;

function TTableManager.DeleteRedactorSearch(AID: integer): TRDelete;
var
  r: TRDelete;
begin
  for r in FDeleteRedactors do
    if r.Check(AID) then Exit(r);
  SetLength(FDeleteRedactors, Length(FDeleteRedactors) + 1);
  Result := TRDelete.Create(FTable, AID);
  FDeleteRedactors[High(FDeleteRedactors)] := Result;
  Result.Tag := High(FDeleteRedactors);
end;

procedure TTableManager.OnDeleteRedactorEvent(ATag: integer);
var
  i: integer;
  r: TRedactor;
begin
  for i := 0 to High(FUpdateRedactors) do begin
    r := FUpdateRedactors[i];
    if r.Tag = ATag then begin
      ClearRedactorFree(r);
      FUpdateRedactors[i] := FUpdateRedactors[High(FUpdateRedactors)];
      FUpdateRedactors[i].Tag := i;
      SetLength(FUpdateRedactors, Length(FUpdateRedactors) - 1);
    end;
  end;
  for i := 0 to High(FDeleteRedactors) do begin
    r := FDeleteRedactors[i];
    if r.Tag = ATag then begin
      ClearRedactorFree(r);
      FDeleteRedactors[i] := FDeleteRedactors[High(FDeleteRedactors)];
      FDeleteRedactors[i].Tag := i;
      SetLength(FDeleteRedactors, Length(FDeleteRedactors) - 1);
    end;
  end;
end;

constructor TTableManager.Create(ATable: TMyTable);
begin
  FTable := ATable;
  FDesc := False;
end;

function TTableManager.GetMenuItem(AParent: TControl): TMenuItem;
begin
  Result := TMenuItem.Create(AParent);
  FMenuItemLink := Result;
  with Result do begin
    Caption := FTable.Caption;
    OnClick := @OnItemClickEvent;
  end;
end;

initialization
  AddAllManagers();
  uRedactor.TransactionComponent:= @TransactionComponent;
  uRedactor.BigRefresh := @BigRefresh;

end.

