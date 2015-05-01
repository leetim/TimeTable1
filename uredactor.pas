unit uRedactor;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, Controls, uRedactorForm, Forms, DbCtrls, UMeta,
  sqldb, DBGrids, StdCtrls, ExtCtrls, Buttons;

type

  TDeleteRedactorEvent = procedure(ATag: integer) of object;
  TRefreshEvent = procedure of object;
  TBigRefreshEvent = procedure;


  { TMyObject }

  TMyObject = class
    private
      FValue, FTag: integer;
    public
      constructor Create(AValue: integer);
      property Value: integer read FValue;
      property Tag: integer read FTag write FTag;
  end;

  { TMyObjectManager }

  TMyObjectManager = class
    private
      FObjects: array of TMyObject;
      function GetObjects(j: integer): TMyObject;
    public
      property Objects[j: integer]: TMyObject
        read GetObjects; default;
  end;

  { TRedactor }

  TRedactor = class
    private
      FForm: TRedactorForm;
      FTable: TMyTable;
      FStarted: boolean;
      FTag: integer;
      FObjectManeger: TMyObjectManager;
      FRefreshEvent: TRefreshEvent;
      procedure MakeForm(); virtual;
      function GetSQL(): string; virtual; abstract;
      procedure RefreshForm(); virtual; abstract;
      procedure RefreshCombobox(ACombobox: TComboBox; AField: TFIDRefrence);
      procedure RefreshEdit(AEdit: TEdit; AField: TMyField); virtual;
      procedure OnClose(Sender: TObject; var ACloseAction: TCloseAction);
      procedure OnActionClick(Sender: TObject); virtual;
    public
      constructor Create(ATable: TMyTable);
      destructor Destroy; override;
      function Check(AID: integer): Boolean; virtual;
      procedure Start();
      procedure RefreshRedactor();
      procedure CloseForm();
      property Tag: integer read FTag write FTag;
      property OnRefreshEvent: TRefreshEvent write FRefreshEvent;
  end;

  { TRUpdate }

  TRUpdate = class(TRedactor)
    private
      FID: integer;
      FRecord: array of string;
      procedure MakeForm; override;
      procedure RefreshEdit(AEdit: TEdit; AField: TMyField); override;
      procedure RefreshRecord();
      procedure RefreshForm; override;
      function GetSQL(): string; override;
      procedure OnActionClick(Sender: TObject); override;
    public
      constructor Create(ATable: TMyTable; AID: integer);
      function Check(AID: integer): Boolean; override;
  end;

  { TRInsert }

  TRInsert = class (TRedactor)
    private
      procedure MakeForm; override;
      function GetSQL(): string; override;
      procedure RefreshForm; override;
      procedure OnActionClick(Sender: TObject); override;
  end;

  { TRDelete }

  TRDelete = class (TRUpdate)
    private
      FDeleteRedacorEvent: TDeleteRedactorEvent;
      procedure MakeForm(); override;
      function GetSQL(): string; override;
      procedure OnActionClick(Sender: TObject); override;
      function CheckChanges(): boolean;
    public
      property OnDeleteRedacor: TDeleteRedactorEvent read FDeleteRedacorEvent
        write FDeleteRedacorEvent;
  end;

var
  TransactionComponent: ^TSQLTransaction;
  BigRefresh: TBigRefreshEvent;


implementation

var
  DeletedRedactors: array of TRedactor;

  {Procedures}

  procedure KillAllRedactors();
  var
    r: TRedactor;
  begin
    for r in DeletedRedactors do
      r.Free;
  end;

  procedure KillRedactor(ARedactor: TRedactor);
  begin
    SetLength(DeletedRedactors, Length(DeletedRedactors) + 1);
    DeletedRedactors[High(DeletedRedactors)] := ARedactor;
  end;

{ TMyObjectManager }

function TMyObjectManager.GetObjects(j: integer): TMyObject;
var
  o: TMyObject;
begin
  for o in FObjects do
    if o.Value = j then
      exit(o);
  Result := TMyObject.Create(j);
  SetLength(FObjects, Length(FObjects) + 1);
  FObjects[High(FObjects)] := Result;
end;

{ TMyObject }

constructor TMyObject.Create(AValue: integer);
begin
  FValue := AValue;
end;


{ TRedactor }

procedure TRedactor.MakeForm();
var
  l: TLabel;
  e: TControl;
  i: integer;
begin
  Application.CreateForm(TRedactorForm, FForm);
  FForm.OnClose := @OnClose;
  FForm.ActionButton.OnClick := @Self.OnActionClick;
  With FForm do begin
    for i := 1 to FTable.MaxIndex do begin
      l := ComponentCreate(TArray(FLables), TLabel) as TLabel;
      l.Caption := FTable.Fields[i].Caption;
      if FTable.Fields[i] is TFIDRefrence then begin
        e := ComponentCreate(TArray(FEdits), TComboBox);
        RefreshCombobox(e as TComboBox, FTable.Fields[i] as TFIDRefrence);
      end else
        e := ComponentCreate(TArray(FEdits), TEdit);
    end;
    ActionButton.Top := Indent + (LableHeight + Indent) * (i + 1);
    CancelButton.Top := Indent + (LableHeight + Indent) * (i + 1);
  end;
end;

procedure TRedactor.RefreshRedactor;
var
  i: integer;
  c: TWinControl;
begin
  for i := 0 to High(FForm.FEdits) do begin
    c := FForm.FEdits[i];
    if c is TComboBox then
      RefreshCombobox(c as TComboBox, FTable.Fields[i + 1] as TFIDRefrence);
    if c is TEdit then
      RefreshEdit(c as TEdit, FTable.Fields[i + 1]);
  end;
  RefreshForm();
end;

procedure TRedactor.CloseForm;
begin
  If FForm.Visible then
    FForm.Close;
end;

procedure TRedactor.RefreshCombobox(ACombobox: TComboBox; AField: TFIDRefrence);
var
  i: integer;
begin
  with FForm.SQLQuery1 do begin
    Close;
    SQL.Text := Format('SELECT * FROM %s', [AField.RefrenceTable.Name]);
    Open;
    with ACombobox do begin
      Items.Clear;
      ReadOnly := True;
      while not EOF do begin
        Items.AddObject(Fields[AField.RefrenceTable.MaxIndex].AsString,
          FObjectManeger[Fields[0].AsInteger]);
        Next;
      end;
    end;
  end;
end;

procedure TRedactor.RefreshEdit(AEdit: TEdit; AField: TMyField);
begin

end;

procedure TRedactor.OnClose(Sender: TObject; var ACloseAction: TCloseAction);
begin
  FStarted := False;
end;

procedure TRedactor.OnActionClick(Sender: TObject);
begin
  With FForm.SQLQuery1 do begin
    Close;
    SQl.Text := GetSQL();
    Prepare;
  end;
end;

constructor TRedactor.Create(ATable: TMyTable);
begin
  FTable := ATable;
  FStarted := False;
  FObjectManeger := TMyObjectManager.Create;
  Self.MakeForm();
end;

function TRedactor.Check(AID: integer): Boolean;
begin
  Result := False;
end;

procedure TRedactor.Start;
begin
  FStarted := True;
  FForm.ShowOnTop;
  Self.RefreshRedactor();
end;

destructor TRedactor.Destroy;
begin
  inherited Destroy;
  FForm.Close;
  FreeAndNil(FForm);
end;

{ TRDelete }

procedure TRDelete.MakeForm();
var
  c: TWinControl;
begin
  inherited MakeForm();
  for c in FForm.FEdits do
    c.Enabled := False;
  FForm.ActionButton.Caption := 'Удалить запись';
end;

function TRDelete.GetSQL: string;
begin
  Result := Format('DELETE FROM %s WHERE %s.ID = :id ',
    [FTable.Name, FTable.Name]);
end;

procedure TRDelete.OnActionClick(Sender: TObject);
begin
  If not CheckChanges() then begin
    ShowMessage('Удаление не возможно! На данную запись ссылается другая таблица.');
    exit;
  end;
  With FForm.SQLQuery1 do begin
    Close;
    SQl.Text := GetSQL();
    Prepare;
    ParamByName('id').AsInteger := FID;
    ExecSQL;
    //(TransactionComponent^).Commit;
  end;
  If FRefreshEvent <> nil then
    FRefreshEvent();
  BigRefresh();
  if FDeleteRedacorEvent <> nil then
    FDeleteRedacorEvent(FTag);
end;

function TRDelete.CheckChanges: boolean;
var
  t: TMyTable;
  i: integer;
  f: TMyField;
  s: string;
begin
  for t in MetaLibrary.Tables do
    if t is TTRefrenceTable then
      for i := 1 to FTable.MaxIndex do begin
        f := t.Fields[i];
        if f is TFIDRefrence then
          if (f as TFIDRefrence).RefrenceTable.ClassNameIs(FTable.ClassName) then
            with FForm.SQLQuery1 do begin
              Close;
              SQL.Text := format('SELECT count(*) FROM %s a WHERE a.%s = %d',
                [t.Name, f.Name, FID]);
              Open;
              If Fields[0].AsInteger <> 0 then exit(False);
            end;
      end;
  Result := True;
end;

{ TRInsert }

procedure TRInsert.MakeForm;
begin
  inherited MakeForm;
  FForm.ActionButton.Caption := 'Доавить';
end;

function TRInsert.GetSQL: string;
var
  i: integer;
  sfields, params: string;
begin
  with FTable do
    for i := 1 to MaxIndex do begin
       sfields += Fields[i].Name;
       params += ':' + Fields[i].Name;
       if i <> MaxIndex then begin
         sfields += ', ';
         params += ', ';
       end;
    end;
  Result := Format('INSERT INTO %s (%s) VALUES(%s)', [FTable.Name, sfields, params]);
end;

procedure TRInsert.RefreshForm;
var
  c: TWinControl;
begin
  for c in FForm.FEdits do begin
    if c is TComboBox then
      (c as TComboBox).ItemIndex := 0;
    if c is TEdit then
      (c as TEdit).Text := '';
  end;
end;

procedure TRInsert.OnActionClick(Sender: TObject);
var
  i: integer;
  c: TWinControl;
begin
  With FForm.SQLQuery1 do begin
    Close;
    SQl.Text := GetSQL();
    Prepare;
    for i := 1 to FTable.MaxIndex do
      with ParamByName(FTable.Fields[i].Name) do begin
        c := FForm.FEdits[i - 1];
        if c is TComboBox then
          with (c as TComboBox) do
            AsInteger := (Items.Objects[ItemIndex] as TMyObject).Value;
        if c is TEdit then
          AsString := (c as TEdit).Text;
      end;
    ExecSQL;
    //(TransactionComponent^).Commit;
  end;
  If FRefreshEvent <> nil then
    FRefreshEvent();
  BigRefresh();
end;

{ TRUpdate }

procedure TRUpdate.MakeForm;
begin
  inherited MakeForm();
  FForm.ActionButton.Caption := 'Изменить';
end;

procedure TRUpdate.RefreshEdit(AEdit: TEdit; AField: TMyField);
begin
  with FForm.SQLQuery1 do begin
    Close;
    if AField is TFIDRefrence then
      with (AField as TFIDRefrence).RefrenceTable do
        SQL.Text := Format('SELECT b.%s FROM %s a INNER JOIN %s b on b.ID = a.%s WHERE b.ID = %d',
          [Fields[MaxIndex].Name, FTable.Name, Name, IDField.Name, FID])
      else
        with FTable do
          SQL.Text := Format('SELECT a.%s FROM %s a WHERE a.%s = %d',
            [Fields[MaxIndex].Name, Name, IDField.Name, FID]);
      Open;
      AEdit.Text := Fields[0].AsString;
    end;
end;

procedure TRUpdate.RefreshRecord;
var
  i: integer;
begin
  with FForm.SQLQuery1 do begin
    Close;
    SQL.Text := Format('SELECT * FROM %s a WHERE a.%s = %d',
      [FTable.Name, FTable.IDField.Name, FID]);
    Open;
    SetLength(FRecord, FieldCount);
    for i := 1 to FieldCount - 1 do
      FRecord[i - 1] := Fields[i].AsString;
  end;
end;

procedure TRUpdate.RefreshForm;
var
  c: TWinControl;
  i: integer;
begin
  RefreshRecord();
  for i := 0 to High(FForm.FEdits) do begin
    c := FForm.FEdits[i];
    if c is TComboBox then
      with (c as TComboBox) do ItemIndex :=
        Items.IndexOfObject(FObjectManeger[StrToInt(FRecord[i])]);
    if c is TEdit then
      (c as TEdit).Text := FRecord[i];
  end;
end;

function TRUpdate.GetSQL: string;
var
  Sets: string;
  i: integer;
begin
  for i := 1 to FTable.MaxIndex do begin
    Sets += Format(' %s = :value%d ', [FTable.Fields[i].Name, i]);
    if i <> FTable.MaxIndex then
      Sets += ', ';
  end;
  Result := Format('UPDATE %s SET %s WHERE %s.ID = :id',
    [FTable.Name, Sets, FTable.Name]);
end;

procedure TRUpdate.OnActionClick(Sender: TObject);
var
  i: integer;
  c: TWinControl;
begin
  With FForm.SQLQuery1 do begin
    Close;
    SQl.Text := GetSQL();
    Prepare;
    ParamByName('id').AsInteger := FID;
    for i := 0 to High(FForm.FEdits) do
      with ParamByName('value' + IntToStr(i + 1)) do begin
        c := FForm.FEdits[i];
        if c is TComboBox then
          with (c as TComboBox) do
            AsInteger := (Items.Objects[ItemIndex] as TMyObject).Value;
        if c is TEdit then
          AsString := (c as TEdit).Text;
      end;
    ExecSQL;
    //(TransactionComponent^).Commit;
  end;
  If FRefreshEvent <> nil then
    FRefreshEvent();
  BigRefresh();
end;

constructor TRUpdate.Create(ATable: TMyTable; AID: integer);
begin
  FID := AID;
  FTable := ATable;
  FObjectManeger := TMyObjectManager.Create;
  Self.MakeForm();
  Self.RefreshRecord();
end;

function TRUpdate.Check(AID: integer): Boolean;
begin
  Result := (FID = AID);
end;

end.

