unit uFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMeta, ExtCtrls, Controls, StdCtrls, Graphics, Buttons,
  UTable;

type

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
      FPanel: TPanel;
      procedure OnDeleteEvent(Sender: TObject);
      procedure OnConstChange(Sender: TObject);
      procedure OnOperationChange(Sender: TObject);
      procedure OnFieldChange(Sender: TObject);
      function GetFieldName(): string;
      function GetOperation(): string;
      function GetFiltredConst(): string;
      procedure SetApplied(AValue: boolean);
    public
      constructor Create(ATable: TMyTable);
      function MakePanel(ASelf: TWinControl): TPanel;
      property DeleteEvent: TDeleteEvent write FDeleteEvent;
      property Tag: integer write FTag;
      property FieldName: string read GetFieldName;
      property Applied: boolean read FApplied write SetApplied;
      property Operation: string read GetOperation;
      property FiltredConst: string read GetFiltredConst;
      property ChangeEvent: TChangeEvent write FChangeEvent;
  end;


implementation

var
  Operations: array[0..3] of string;

procedure TFilter.OnDeleteEvent(Sender: TObject);
begin
  FDeleteEvent(FTag);
end;

procedure TFilter.OnConstChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    If FFilterConst <> Text then
      FFilterConst := Text;
  Applied := False;
  If FChangeEvent <> nil then
    FChangeEvent();
end;

procedure TFilter.OnOperationChange(Sender: TObject);
begin
  with (Sender as TComboBox) do
    If FFilterOperation <> TFilterOperation(ItemIndex) then
      FFilterOperation := TFilterOperation(ItemIndex);
  Applied := False;
  If FChangeEvent <> nil then
    FChangeEvent();
end;

procedure TFilter.OnFieldChange(Sender: TObject);
begin
  with (Sender as TComboBox) do begin
    FFIlterFieldIndex := ItemIndex + 1;
    if FTable is TTRefrenceTable then
      with (FTable.Fields[ItemIndex + 1] as TFIDRefrence).RefrenceTable do
        FFIlterField := Fields[MaxIndex]
    else
      FFIlterField := FTable.Fields[ItemIndex + 1];
  end;
  Applied := False;
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

procedure TFilter.SetApplied(AValue: boolean);
begin
  if FApplied = AValue then Exit;
  FApplied := AValue;
  If AValue then
    FPanel.Color := clGreen
  else
    FPanel.Color := clRed;
end;

constructor TFilter.Create(ATable: TMyTable);
begin
  FTable := ATable;
  FApplied := False;
end;

function TFilter.MakePanel(ASelf: TWinControl): TPanel;
var
  Controls: array of TWinControl;
  c: TWinControl;
  OperationBox, FieldBox: TComboBox;
  ConstEdit: TEdit;
  DeleteButton: TBitBtn;
  i: integer;

  function AddControl(AControl: TWinControl): TWinControl;
  begin
    SetLength(Controls, Length(Controls) + 1);
    Controls[High(Controls)] := AControl;
    Result := AControl;
  end;

begin
  Result := TPanel.Create(nil);
  FPanel := Result;
  FPanel.Color := clRed;
    with Result do begin
      Left := Indent;
      Width := 600;
      Height := ControlHeight + 2 * Indent;
    end;
    FieldBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
    with FieldBox do begin
      if FTable is TTRefrenceTable then
        for i := 1 to FTable.MaxIndex do
          Items.Add((FTable.Fields[i] as TFIDRefrence).RefrenceTable.
            Fields[1].Caption)
      else
        for i := 1 to FTable.MaxIndex do
          Items.Add(FTable.Fields[i].Caption);
      ItemIndex := 0;
      ReadOnly := True;
      Left := Indent;
      Top := Indent;
      Width := ControlWidth;
      Height := ControlHeight;
      OnChange := @OnFieldChange;
      OnFieldChange(FieldBox);
    end;
    OperationBox := AddControl(TComboBox.Create(ASelf)) as TComboBox;
    with OperationBox do begin
      Items.Add('Равно');
      Items.Add('Больше');
      Items.Add('Меньше');
      Items.Add('Содержит');
      ItemIndex := 0;
      ReadOnly := True;
      Parent := Result;
      Left := ControlWidth + Indent * 2;
      Top := Indent;
      Width := ControlWidth;
      Height := ControlHeight;
      OnChange := @OnOperationChange;
    end;
    ConstEdit := AddControl(TEdit.Create(ASelf)) as TEdit;
    with ConstEdit do begin
      Left := (ControlWidth + Indent) * 2 + Indent;
      Top := Indent;
      Width := ControlWidth;
      Height := ControlHeight;
      OnChange := @OnConstChange;
    end;
    DeleteButton := AddControl(TBitBtn.Create(ASelf)) as TBitBtn;
    with DeleteButton do begin
      try
        Glyph.LoadFromFile('Images\Del.bmp');
      except
        Caption := 'X';
      end;
      Left := (ControlWidth + Indent) * 3 + Indent;
      Top := Indent;
      Width := ControlHeight;
      Height := ControlHeight;
      OnClick := @OnDeleteEvent;
    end;
    for c in Controls do
      c.Parent := Result;
end;

initialization
  Operations[0] := ' = ';
  Operations[1] := ' > ';
  Operations[2] := ' < ';
  Operations[3] := ' LIKE ';

end.

