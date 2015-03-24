unit UMeta;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Classes, SysUtils;
const
  TypeCount = 4;
//   = ('INTEGER', 'VARCHAR(50)', 'BOOLEAN', 'FLOAT');

type
  TSQLType = (sqlInteger, sqlVarChar, sqlBoolean, sqlFloat);

    { TMyField }

    TMyField = class(TPersistent)
    protected
      FName, FCaption: string;
      FFieldType: TSQLType;
      FWidth: integer;
      function GetCaption(): string; virtual;
      function GetWidth(): integer; virtual;
    public
      property Name: string read FName;
      property Caption: string read GetCaption;
      property FieldType: TSQLType read FFieldType;
      property Width: integer read GetWidth;
  end;

  TMyFieldsArray = array of TMyField;

  { TMyTable }

  TMyTable = class(TPersistent)
    protected
      FName, FCaption: string;
      FFields: array of TMyField;
      function GetIDField(): TMyField;
      function GetNameField(): TMyField;
      procedure AddField(ANewField: TMyField);
    public
      property Name: string read FName;
      property Caption: string read FCaption;
      property IDField: TMyField read GetIDField;
      property NameField: TMyField read GetNameField;
      property Fields: TMyFieldsArray read FFields;
      function GetSQLCode(): TStringList; virtual;
      destructor Destroy; override;
  end;

  { TFID }

  TFID = class(TMyField)
    protected
      FParentTable: TMyTable;
    public
      constructor Create(AParentTable: TMyTable); virtual;
      property ParentTable: TMyTable read FParentTable;
  end;

  { TFIDRefrence }

  TFIDRefrence = class(TFID)
    protected
      FRefrenceTable: TMyTable;
      function GetCaption(): string; override;
      function GetWidth(): integer; override;
    public
      property RefrenceTable: TMyTable read FRefrenceTable write FRefrenceTable;
      property Caption;
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFName }

  TFName = class(TMyField)
    protected
      FParentTable: TMyTable;
    public
      constructor Create(AParentTable: TMyTable); virtual;
  end;

  { TFTeacherName }

  TFTeacherName = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFSubjectName }

  TFSubjectName = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFGroupName }

  TFGroupName = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFClassRoomName }

  TFClassRoomName = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFWeekdayName }

  TFWeekdayName = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFPairNumber }

  TFPairNumber = class(TFName)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFGroupID }

  TFGroupID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFSubjectID }

  TFSubjectID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFTeacherID }

  TFTeacherID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFPairID }

  TFPairID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFWeekDayID }

  TFWeekDayID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TFClassRoomID }

  TFClassRoomID = class(TFIDRefrence)
    public
      constructor Create(AParentTable: TMyTable); override;
  end;

  { TTNameTable }

  TTNameTable = class(TMyTable)
    public
      constructor Create(); virtual;
  end;

  TMyTableArray = array of TMyTable;

  { TTRefrenceTable }

  TTRefrenceTable = class(TMyTable)
    protected
      FRefrences: TMyTableArray;
    public
      constructor Create(ARefrences: TMyTableArray); virtual;
      property Refrences: TMyTableArray read FRefrences;
      function GetSQLCode(): TStringList; override;
  end;

  TTRefrenceTableClass = class of TTRefrenceTable;

  { TTLessonsTable }

  TTLessonsTable = class(TTRefrenceTable)
    public
      constructor Create(ARefrences: TMyTableArray); override;
  end;

  { TTSubjectsTeachersTable }

  TTSubjectsTeachersTable = class(TTRefrenceTable)
    public
      constructor Create(ARefrences: TMyTableArray); override;
  end;

  { TTSubjectsGroupsTable }

  TTSubjectsGroupsTable = class(TTRefrenceTable)
    public
      constructor Create(ARefrences: TMyTableArray); override;
  end;

  { TTTeachersTable }

  TTTeachersTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  { TTSubjectsTable }

  TTSubjectsTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  { TTGroupsTable }

  TTGroupsTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  { TTPairsTable }

  TTPairsTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  { TTWeekDaysTable }

  TTWeekDaysTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  { TTClassRoomsTable }

  TTClassRoomsTable = class(TTNameTable)
    public
      constructor Create(); override;
  end;

  function AddTable(t: TMyTable): TMyTable;
  function AddTableFromClass(t: TTRefrenceTableClass;
    ARefrence1, ARefrence2: TMyTable): TMyTable;

var
  SQLTypes: array[0..TypeCount - 1] of String;
  Tables: array of TMyTable;
  TeachersTable, ClassRoomsTable, GroupsTable, SubjectsTable,
    WeekDayTable, PairTable: TMyTable;

implementation

{ TMyField }

function TMyField.GetWidth(): integer;
begin
  Result := FWidth;
end;

function TMyField.GetCaption: string;
begin
  Result := FCaption;
end;

{ TFClassRoomID }

constructor TFClassRoomID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.CLASS_ID';
end;

{ TFWeekDayID }

constructor TFWeekDayID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.WEEKDAY_ID';
end;

{ TFPairID }

constructor TFPairID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.PAIR_ID';
end;

{ TFTeacherID }

constructor TFTeacherID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.TEACHER_ID';
end;

{ TFSubjectID }

constructor TFSubjectID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.SUBJECT_ID';
end;

{ TFGroupID }

constructor TFGroupID.Create(AParentTable: TMyTable);
begin
  inherited Create(AParentTable);
  FName := FParentTable.Name + '.GROUP_ID';
end;

{ TFIDRefrence }

function TFIDRefrence.GetCaption: string;
begin
  Result := FRefrenceTable.NameField.Caption;
end;

function TFIDRefrence.GetWidth(): integer;
begin
  Result := RefrenceTable.NameField.Width;
end;

constructor TFIDRefrence.Create(AParentTable: TMyTable);
begin
  FParentTable := AParentTable;
  FCaption := 'ID';
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TTClassRoomsTable }

constructor TTClassRoomsTable.Create();
begin
  FName := 'CLASSROOMS';
  FCaption := 'Аудитории';
  inherited Create;
  AddField(TFClassRoomName.Create(Self));
end;

{ TTSubjectsGroupsTable }

constructor TTSubjectsGroupsTable.Create(ARefrences: TMyTableArray);
begin
  FName := 'GROUPS_SUBJECTS';
  FCaption := 'Соответствие Группы-Предметы';
  AddField(TFGroupID.Create(Self));
  AddField(TFSubjectID.Create(Self));
  inherited Create(ARefrences);
end;

{ TTSubjectsTeachersTable }

constructor TTSubjectsTeachersTable.Create(ARefrences: TMyTableArray);
begin
  FName := 'TEACHERS_SUBJECTS';
  FCaption := 'Соответствие Преподователи-Предметы';
  AddField(TFTeacherID.Create(Self));
  AddField(TFSubjectID.Create(Self));
  inherited Create(ARefrences);
end;

{ TTLessonsTable }

constructor TTLessonsTable.Create(ARefrences: TMyTableArray);
begin
  FName := 'LESSONS';
  FCaption := 'Занятия';
  AddField(TFPairID.Create(Self));
  AddField(TFWeekDayID.Create(Self));
  AddField(TFGroupID.Create(Self));
  AddField(TFSubjectID.Create(Self));
  AddField(TFClassRoomID.Create(Self));
  AddField(TFTeacherID.Create(Self));
  inherited Create(ARefrences);
end;

{ TTWeekDaysTable }

constructor TTWeekDaysTable.Create();
begin
  FName := 'WEEKDAY';
  FCaption := 'Дни недели';
  inherited Create;
  AddField(TFWeekdayName.Create(Self));
end;

{ TTPairsTable }

constructor TTPairsTable.Create;
begin
  FName := 'PAIR';
  FCaption := 'Номера пар';
  inherited Create;
  AddField(TFPairNumber.Create(Self));
end;

{ TTGroupsTable }

constructor TTGroupsTable.Create;
begin
  FName := 'GROUPS';
  FCaption := 'Группы';
  inherited Create;
  AddField(TFGroupName.Create(Self));
end;

{ TTSubjectsTable }

constructor TTSubjectsTable.Create();
begin
  FName := 'SUBJECTS';
  FCaption := 'Предметы';
  inherited Create;
  AddField(TFSubjectName.Create(Self));
end;

{ TTTeachersTable }

constructor TTTeachersTable.Create();
begin
  FName := 'TEACHERS';
  FCaption := 'Преподователи';
  inherited Create;
  AddField(TFTeacherName.Create(Self));
end;

{ TTRefrenceTable }

function TTRefrenceTable.GetSQLCode(): TStringList;
var
  i: integer;
  s: string;
begin
  Result := TStringList.Create();
  With Result do begin
    s := 'SELECT ';
    for i := 0 to High(FFields) do begin
      s += FRefrences[i].NameField.Name;
      if i <> High(FFields) then
        s += ', ';
    end;
    If Result <> nil then
      Add(s);
    i += 1;
    Add('FROM ' + Name);
  end;
  with Result do begin
    for i := 0 to High(FFields) do begin
      Add('INNER JOIN ' + FRefrences[i].Name);
      Add('ON ' + FRefrences[i].IDField.Name + ' = ' + FFields[i].Name);
    end;
  end;
end;

constructor TTRefrenceTable.Create(ARefrences: TMyTableArray);
var
  ref: TFIDRefrence;
  i: integer;
begin
  for i := 0 to high(ARefrences) do begin
    (FFields[i] as TFIDRefrence).RefrenceTable := ARefrences[i];
    SetLength(FRefrences, Length(FRefrences) + 1);
    FRefrences[i] := ARefrences[i];
  end;
end;

{ TTNameTable }

constructor TTNameTable.Create();
begin
  AddField(TFID.Create(Self));
end;

{ TFID }

constructor TFID.Create(AParentTable: TMyTable);
begin
  FParentTable := AParentTable;
  FCaption := 'ID';
  FName := FParentTable.Name + '.ID';
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TFSubjectName }

constructor TFSubjectName.Create(AParentTable: TMyTable);
begin
  FCaption := 'Предмет';
  inherited Create(AParentTable);
  FWidth := 240;
end;

{ TFPairNumber }

constructor TFPairNumber.Create(AParentTable: TMyTable);
begin
  FParentTable := AParentTable;
  FCaption := 'Пара';
  FName := FParentTable.Name + '.Num';
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TFWeekdayName }

constructor TFWeekdayName.Create(AParentTable: TMyTable);
begin
  FCaption := 'День недели';
  inherited Create(AParentTable);
  FWidth := 80;
end;

{ TFClassRoomName }

constructor TFClassRoomName.Create(AParentTable: TMyTable);
begin
  FCaption := 'Аудитория';
  inherited Create(AParentTable);
  FWidth := 80;
end;

{ TFGroupName }

constructor TFGroupName.Create(AParentTable: TMyTable);
begin
  FCaption := 'Группа';
  inherited Create(AParentTable);
  FWidth := 50;
end;

{ TMyTable }

function TMyTable.GetSQLCode: TStringList;
var
  s: string;
  i: integer;
begin
  Result := TStringList.Create();
  With Result do begin
    s := 'SELECT ';
    for i := 0 to High(FFields) do begin
      s += FFields[i].Name;
      if i <> High(FFields) then
        s += ', ';
    end;
    If Result <> nil then
      Add(s);
    i += 1;
    Add('FROM ' + Name);
  end;
end;

function TMyTable.GetIDField: TMyField;
begin
  If Length(FFields) <> 0 then Result := FFields[0]
  else Result := nil;
end;

function TMyTable.GetNameField: TMyField;
begin
  If Length(FFields) > 1 then Result := FFields[High(FFields)]
  else Result := nil;
end;

destructor TMyTable.Destroy;
var
  f: TMyField;
begin
  inherited Destroy;
  for f in FFields do
    f.Free;
end;

procedure TMyTable.AddField(ANewField: TMyField);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := ANewField;
end;

{ TFName }

constructor TFName.Create(AParentTable: TMyTable);
begin
  FParentTable := AParentTable;
  FName := FParentTable.Name + '.name';
  FFieldType := sqlInteger;
end;

{ TFTeacherName }

constructor TFTeacherName.Create(AParentTable: TMyTable);
begin
  FCaption := 'Преподователь';
  inherited Create(AParentTable);
  FWidth:= 200;
end;

{ Procedures }

function AddTable(t: TMyTable): TMyTable;
begin
  Result := t;
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := t;
end;

function AddTableFromClass(t: TTRefrenceTableClass; ARefrence1,
  ARefrence2: TMyTable): TMyTable;
var
  a: TMyTableArray;
begin
  SetLength(a, 2);
  a[0] := ARefrence1;
  a[1] := ARefrence2;
  Result := t.Create(a);
  SetLength(Tables, Length(Tables) + 1);
  Tables[High(Tables)] := Result;
end;

initialization
  SQLTypes[Integer(sqlInteger)] := 'INTEGER';
  SQLTypes[Integer(sqlVarChar)] := 'VARCHAR(50)';
  SQLTypes[Integer(sqlBoolean)] := 'BOOLEAN';
  SQLTypes[Integer(sqlFloat)] := 'FLOAT';
  PairTable := AddTable(TTPairsTable.Create());
  WeekDayTable := AddTable(TTWeekDaysTable .Create());
  GroupsTable := AddTable(TTGroupsTable.Create());
  SubjectsTable := AddTable(TTSubjectsTable.Create());
  ClassRoomsTable := AddTable(TTClassRoomsTable.Create());
  TeachersTable := AddTable(TTTeachersTable .Create());
  AddTable(TTLessonsTable.Create(Tables));
  AddTableFromClass(TTSubjectsGroupsTable, GroupsTable, SubjectsTable);
  AddTableFromClass(TTSubjectsTeachersTable, TeachersTable, SubjectsTable);
end.

