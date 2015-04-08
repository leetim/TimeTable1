unit uTableManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, UTable, Forms, DbCtrls, UMeta, sqldb,
  DBGrids, StdCtrls, ExtCtrls;

const
  Indent = 5;
  ScrollLength = 20;

type

  { TFilter }

  TFilter = class
    private
      FPanel: TPanel;
      FTable: TMyTable;
      FControls: array of TWinControl;
      FOperationBox, FFieldBox: TComboBox;
      FConstEdit: TEdit;
      FDeleteButton: TButton;
      function AddControl(AControl: TWinControl): TWinControl;
    public
      constructor Create(ASelf: TComponent; ATable: TMyTable);
      destructor Destroy();
  end;

  { TTableManager }

  TTableManager = class
    private
      FForm: TTableForm;
      FTable: TMyTable;
      FMenuItemLink: TMenuItem;
      FOrderedField: TMyField;
      procedure MakeForm(); virtual;
      procedure Refresh(); virtual;
      procedure OnClickEvent(Sender: TObject);
      procedure OnCloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      procedure OnTitleClickEvent(Column: TColumn); virtual;
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

function TFilter.AddControl(AControl: TWinControl): TWinControl;
begin
  SetLength(FControls, Length(FControls) + 1);
  FControls[High(FControls)] := AControl;
  Result := AControl;
end;

constructor TFilter.Create(ASelf: TComponent; ATable: TMyTable);
var
  i: integer;
  c: TWinControl;
begin
  FTable := ATable;
  FFieldBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
  with FFieldBox do
    for i := 0 to ATable.MaxIndex do
      FFieldBox.Items.Add(ATable.Fields[i].Caption);
  FOperationBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
  FConstEdit := AddControl(TEdit.Create(ASelf)) as TEdit;
  FDeleteButton := AddControl(TButton.Create(ASelf)) as TButton;
  for c in FControls do
    c.Parent := FPanel;
end;

destructor TFilter.Destroy;
var
  c: TWinControl;
begin
  for c in FControls do
    c.Free;
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
begin
  with FForm.SQLQuery do begin
    Active := False;
    SQL.Clear;
    SQL.Add(GetSQLCode());
    Active := True;
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
  end;
end;

procedure TTableManager.Refresh();
var
  i: integer;
begin
  with FForm.SQLQuery do begin
    Active := False;
    SQL.Clear;
    SQL.Add(GetSQLCode());
    Active := True;
  end;
  with FForm.DBGrid.Columns do
    for i := 0 to Count - 1 do begin
      Items[i].Title.Caption := FTable.Fields[i].Caption;
      Items[i].Width := FTable.Fields[i].Width;
    end;
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

function TTableManager.GetSQLCode(): string;
begin
  Result := 'SELECT * FROM ' + FTable.Name;
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

end.

