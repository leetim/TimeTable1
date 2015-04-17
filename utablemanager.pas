unit uTableManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, UTable, Forms, DbCtrls, UMeta, sqldb,
  DBGrids, StdCtrls, ExtCtrls;

type

  { TFilter }

  TDeleteEvent = procedure(i: Integer) of object;
  TChangeEvent = procedure of Object;
  TFilterOperation = (foIs, foMore, foLess, foLike);

  TFilter = class
    private
      FFiltredConst: string;
      FOperation: string;
      FTable: TMyTable;
      FDeleteEvent: TDeleteEvent;
      FChangeEvent: TChangeEvent;
      FTag: integer;
      FFilterOperation: TFilterOperation;
      FFIlterField: TMyField;
      FFIlterFieldIndex: integer;
      FFilterConst: string;
      FApplied: boolean;
      procedure OnDeleteEvent(Sender: TObject);
      procedure OnConstChange(Sender: TObject);
      procedure OnOperationChange(Sender: TObject);
      procedure OnFieldChange(Sender: TObject);
      function GetFieldName(): string;
      function GetOperation(): string;
      function GetFiltredConst(): string;
    public
      constructor Create(ATable: TMyTable);
      function MakePenel(ASelf: TWinControl): TPanel;
      property DeleteEvent: TDeleteEvent write FDeleteEvent;
      property Tag: integer write FTag;
      property FieldName: string read GetFieldName;
      property Applied: boolean read FApplied write FApplied;
      property Operation: string read GetOperation;
      property FiltredConst: string read GetFiltredConst;
      property ChangeEvent: TChangeEvent write FChangeEvent;
  end;

  { TTableManager }

  TTableManager = class
    private
      FForm: TTableForm;
      FTable: TMyTable;
      FMenuItemLink: TMenuItem;
      FOrderedField: TMyField;
      FFilters: array of TFilter;
      FDeletedPanels: array of TPanel;
      //FPanels: array of TPanel;
      procedure MakeForm(); virtual;
      procedure Refresh(); virtual;
      procedure AddFilterEvent(Sender: TObject);
      procedure DeleteFilterEvent(AIndex: Integer);
      procedure ChangeFilterEvent();
      procedure OnClickEvent(Sender: TObject);
      procedure OnCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      procedure OnTitleClickEvent(Column: TColumn); virtual;
      procedure Apply(Sender: TObject);
      function GetSQLCode(): string; virtual;
    public
      constructor Create(ATable: TMyTable); virtual;
      function GetMenuItem(AParent: TControl): TMenuItem;
  end;

  { TRefrenceTableManager }

  TRefrenceTableManager = class(TTableManager)
    private
      FOrderedFieldPerentTable: TMyTable;
      FRefrences: TMyTableArray;
      procedure MakeForm(); override;
      procedure Refresh; override;
      function GetSQLCode: string; override;
      procedure OnTitleClickEvent(Column: TColumn); override;
    public
      constructor Create(ATable: TMyTable); override;
  end;

  procedure AddTableManager(ATable: TMyTable);
  procedure AddRefrenceTableManager(ATable: TMyTable);
  procedure AddAllManagers();

var
  TableManagers: array of TTableManager;

implementation

var
  Operations: array[0..3] of string;

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

{ TFilter }

procedure TFilter.OnDeleteEvent(Sender: TObject);
begin
  FDeleteEvent(FTag);
end;

procedure TFilter.OnConstChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    If FFilterConst <> Text then
      FFilterConst := Text;
  FApplied := False;
  If FChangeEvent <> nil then
    FChangeEvent();
end;

procedure TFilter.OnOperationChange(Sender: TObject);
begin
  with (Sender as TComboBox) do
    If FFilterOperation <> TFilterOperation(ItemIndex) then
      FFilterOperation := TFilterOperation(ItemIndex);
  FApplied := False;
  If FChangeEvent <> nil then
    FChangeEvent();
end;

procedure TFilter.OnFieldChange(Sender: TObject);
begin
  with (Sender as TComboBox) do begin
    FFIlterFieldIndex := ItemIndex;
    if FTable is TTRefrenceTable then
      FFIlterField := (FTable.Fields[ItemIndex] as TFIDRefrence).RefrenceTable.
        Fields[1]
    else
      FFIlterField := FTable.Fields[ItemIndex];
  end;
  FApplied := False;
  If FChangeEvent <> nil then
    FChangeEvent();
end;

function TFilter.GetFieldName: string;
begin
  if FTable is TTRefrenceTable then
    with (FTable.Fields[FFIlterFieldIndex] as TFIDRefrence).RefrenceTable do
      Result := Name
  else
      Result := FTable.Name;
  Result += '.' + FFIlterField.Name;
end;

function TFilter.GetOperation: string;
begin
  Result := Operations[integer(FFilterOperation)];
end;

function TFilter.GetFiltredConst: string;
begin
  Result := FFilterConst;
  If FFilterOperation = foLike then
    Result += '%';
end;

constructor TFilter.Create(ATable: TMyTable);
var
  i: integer;
  c: TWinControl;
begin
  FTable := ATable;
  FApplied := False;
end;

function TFilter.MakePenel(ASelf: TWinControl): TPanel;
var
  Controls: array of TWinControl;
  c: TWinControl;
  OperationBox, FieldBox: TComboBox;
  ConstEdit: TEdit;
  DeleteButton: TButton;
  i: integer;

  function AddControl(AControl: TWinControl): TWinControl;
  begin
    SetLength(Controls, Length(Controls) + 1);
    Controls[High(Controls)] := AControl;
    Result := AControl;
  end;

begin
  Result := TPanel.Create(nil);
    with Result do begin
      Left := Indent;
      Width := 600;
      Height := PanelHeight;
    end;
    FieldBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
    with FieldBox do begin
      if FTable is TTRefrenceTable then
        for i := 0 to FTable.MaxIndex do
          Items.Add((FTable.Fields[i] as TFIDRefrence).RefrenceTable.
            Fields[1].Caption)
      else
        for i := 0 to FTable.MaxIndex do
          Items.Add(FTable.Fields[i].Caption);
      ItemIndex := 0;
      ReadOnly := True;
      Left := Indent;
      Top := Indent;
      Width := 100;
      Height := 25;
      OnChange := @OnFieldChange;
      OnFieldChange(FieldBox);
    end;
    OperationBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
    with OperationBox do begin
      //Items.Add('...');
      Items.Add('Равно');
      Items.Add('Больше');
      Items.Add('Меньше');
      Items.Add('Содержит');
      ItemIndex := 0;
      ReadOnly := True;
      Parent := Result;
      Left := 100 + Indent;
      Top := Indent;
      Width := 100;
      Height := 25;
      OnChange := @OnOperationChange;
    end;
    ConstEdit := AddControl(TEdit.Create(ASelf)) as TEdit;
    with ConstEdit do begin
      Left := 210;
      Top := Indent;
      Width := 200;
      Height := 25;
      OnChange := @OnConstChange;
    end;
    DeleteButton := AddControl(TButton.Create(ASelf)) as TButton;
    with DeleteButton do begin
      Left := 415;
      Top := Indent;
      Width := 25;
      Height := 25;
      OnClick := @OnDeleteEvent;
    end;
    for c in Controls do
      c.Parent := Result;
end;

{ TRefrenceTableManager }

procedure TRefrenceTableManager.MakeForm();
begin
  inherited MakeForm();
  FForm.DBNavigator.VisibleButtons := [nbFirst, nbLast, nbNext, nbPrior]
end;

procedure TRefrenceTableManager.Refresh;
var
  i: integer;
  f: TFilter;
begin
  with FForm.SQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Add(GetSQLCode());
    with FForm do begin
      Prepare;
      for i := 0 to High(FFilters) do begin
        f := FFilters[i];
        Params[i].AsString := f.FiltredConst;
      end;
    end;
    Open;
  end;
  with FForm.DBGrid.Columns do
    for i := 0 to Count - 1 do begin
      Items[i].Title.Caption :=
        (FTable.Fields[i] as TFIDRefrence).RefrenceTable.Fields[1].Caption;
      Items[i].Width :=
        (FTable.Fields[i] as TFIDRefrence).RefrenceTable.Fields[1].Width;
    end;
end;

function TRefrenceTableManager.GetSQLCode(): string;
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
    Result += ' FROM ' + Name;
    for i := 0 to High(FRefrences) do begin
      Result += ' INNER JOIN ' + FRefrences[i].Name;
      Result += ' ON ' + FRefrences[i].Name + '.' + FRefrences[i].Fields[0].Name +
        ' = ' + Name + '.' + Fields[i].Name;
    end;
  end;
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
  If FOrderedField <> nil then
    Result += ' ORDER BY ' + FOrderedFieldPerentTable.Name
      + '.' + FOrderedField.Name
end;

procedure TRefrenceTableManager.OnTitleClickEvent(Column: TColumn);
begin
  FOrderedFieldPerentTable :=
    (FTable.Fields[Column.Index] as TFIDRefrence).RefrenceTable;
  FOrderedField := FOrderedFieldPerentTable.Fields[1];
  Refresh();
end;

constructor TRefrenceTableManager.Create(ATable: TMyTable);
var
  i: integer;
begin
  inherited Create(ATable);
  with ATable do begin
    SetLength(FRefrences, MaxIndex + 1);
    for i := 0 to MaxIndex do
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
    SQL.Add(GetSQLCode());
    with FForm do begin
      Prepare;
      for i := 0 to High(FFilters) do begin
        f := FFilters[i];
        Params[i].AsString := f.FiltredConst;
      end;
    end;
    Open;
  end;
  with FForm.DBGrid.Columns do
    for i := 0 to Count - 1 do begin
      Items[i].Title.Caption := FTable.Fields[i].Caption;
      Items[i].Width := FTable.Fields[i].Width;
    end;
end;

procedure TTableManager.AddFilterEvent(Sender: TObject);
var
  f: TPanel;
begin
  FForm.ApplyButton.Enabled := True;
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(FTable);
  with FForm do begin
    SetLength(FPanels, Length(FFilters) + 1);
    FPanels[High(FPanels)] := FFilters[High(FFilters)].MakePenel(FForm);
    FPanels[High(FPanels)].Top := Indent + High(FFilters) * (PanelHeight + Indent)
      - FIndentPanel;
    FPanels[High(FPanels)].Parent := FForm.FilterPanel;
  end;
  with FFilters[High(FFilters)] do begin
    Tag := High(FFilters);
    DeleteEvent := @DeleteFilterEvent;
    ChangeEvent := @ChangeFilterEvent;
  end;
  If Length(FDeletedPanels) <> 0 then
    for f in FDeletedPanels do
      FreeAndNil(f);
  with FForm do begin
    ScrollBar1.PageSize := round(FilterPanel.Height / (Length(FPanels) *
      (PanelHeight + Indent) + Indent) * 1000);
    //ScrollBar1.OnChange(ScrollBar1);
  end;
end;

procedure TTableManager.DeleteFilterEvent(AIndex: Integer);
var
  i: integer;
begin
  if AIndex > High(FFilters) then exit;
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
  SetLength(FForm.FPanels, Length(FFilters) - 1);
  for i := 0 to High(FFilters) do
    with FForm.FPanels[i] do begin
      Top := Indent + i * (PanelHeight + Indent);
      Tag := i;
      FFilters[i].Tag := i;
    end;
end;

procedure TTableManager.ChangeFilterEvent;
begin
  FForm.ApplyButton.Enabled := True;
end;

procedure TTableManager.OnClickEvent(Sender: TObject);
begin
  If FForm = nil then MakeForm();
  FOrderedField := nil;
  Refresh();
  FForm.ShowOnTop;
  (Sender as TMenuItem).Checked := True;
end;

procedure TTableManager.OnCloseEvent(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FMenuItemLink.Checked := False;
end;

procedure TTableManager.OnTitleClickEvent(Column: TColumn);
begin
  FOrderedField := FTable.Fields[Column.Index];
  Refresh();
end;

procedure TTableManager.Apply(Sender: TObject);
var
  f: TFilter;
  i: integer;
begin
  FForm.ApplyButton.Enabled := False;
  for f in FFilters do
    if not f.Applied then begin
      Refresh();
      Break;
    end;
end;

function TTableManager.GetSQLCode(): string;
var
  i: integer;
begin
  Result := 'SELECT * FROM ' + FTable.Name;
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
  If FOrderedField <> nil then
    Result += ' ORDER BY ' + FTable.Name + '.' + FOrderedField.Name;
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
  Operations[0] := ' = ';
  Operations[1] := ' > ';
  Operations[2] := ' < ';
  Operations[3] := ' LIKE ';

end.

