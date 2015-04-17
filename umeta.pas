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

type
  TSQLType = (sqlInteger, sqlVarChar, sqlBoolean, sqlFloat);

    { TMyField }

    TMyField = class(TPersistent)
    private
      FName, FCaption: String;
      FFieldType: TSQLType;
      FWidth: integer;
      function GetWidth(): integer; virtual;
      procedure Initializate(); virtual; abstract;
    public
      constructor Create();
      property Name: string read FName;
      property Caption: string read FCaption;
      property FieldType: TSQLType read FFieldType;
      property Width: integer read GetWidth;
  end;

  TMyFieldsArray = array of TMyField;
  TMyFieldClass = class of TMyField;

  { TMyTable }

  TMyTable = class(TPersistent)
    private
      FCaption, FName: String;
      FFields: TMyFieldsArray;
      procedure AddField(AField: TMyField);
      function GetMaxIndex(): integer;
      function GetWidth: integer; virtual;
      procedure Initializate(); virtual; abstract;
    public
      constructor Create();
      property Name: string read FName;
      property Caption: string read FCaption;
      property Fields: TMyFieldsArray read FFields;
      property MaxIndex: integer read GetMaxIndex;
      property Width: integer read GetWidth;
  end;

  TMyTableClass = class of TMyTable;
  TMyTableArray = array of TMyTable;

  { TMetaLibrary }

  TMetaLibrary = class
    private
      FMyTables: TMyTableArray;
      procedure AddTable(t: TMyTable);
    public
      constructor Create();
      function SearchTable(ASearchString: string): TMyTable;
      function SearchByClass(ATableClass: TMyTableClass):TMyTable;
      property Tables: TMyTableArray read FMyTables;
      destructor Destroy();
  end;

  { TFUnknow }

  TFUnknow = class(TMyField)
    private
      procedure Initializate(); override;
  end;

  { TFID }

  TFID = class(TFUnknow)
    private
      procedure Initializate; override;
  end;

  { TFIDRefrence }

  TFIDRefrence = class(TFID)
    private
      FRefrenceTableClass: TMyTableClass;
      FRefrenceTable: TMyTable;
      function GetRefrenceTable(): TMyTable;
      procedure MakeRefrence(ATable: TMyTable);
    public
      property RefrenceTable: TMyTable read GetRefrenceTable;
  end;

  { TFTeacherName }

  TFTeacherName = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFSubjectName }

  TFSubjectName = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFGroupName }

  TFGroupName = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFClassRoomName }

  TFClassRoomName = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFWeekdayName }

  TFWeekdayName = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFPairNumber }

  TFPairNumber = class(TFUnknow)
    private
      procedure Initializate(); override;
  end;

  { TFGroupID }

  TFGroupID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TFSubjectID }

  TFSubjectID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TFTeacherID }

  TFTeacherID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TFPairID }

  TFPairID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TFWeekDayID }

  TFWeekDayID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TFClassRoomID }

  TFClassRoomID = class(TFIDRefrence)
    private
      procedure Initializate(); override;
  end;

  { TTRefrenceTable }

  TTRefrenceTable = class(TMyTable)
    private
      function GetWidth(): integer; override;
    protected
      FRefrences: TMyTableArray;
      FRefrencesFields: TMyFieldsArray;
    public
      property Refrences: TMyTableArray read FRefrences;
  end;

  TTRefrenceTableClass = class of TTRefrenceTable;

  { TTLessonsTable }

  TTLessonsTable = class(TTRefrenceTable)
    private
      procedure Initializate(); override;
  end;

  { TTSubjectsTeachersTable }

  TTSubjectsTeachersTable = class(TTRefrenceTable)
    private
      procedure Initializate(); override;
  end;

  { TTSubjectsGroupsTable }

  TTSubjectsGroupsTable = class(TTRefrenceTable)
    private
      procedure Initializate(); override;
  end;

  { TTTeachersTable }

  TTTeachersTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

  { TTSubjectsTable }

  TTSubjectsTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

  { TTGroupsTable }

  TTGroupsTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

  { TTPairsTable }

  TTPairsTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

  { TTWeekDaysTable }

  TTWeekDaysTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

  { TTClassRoomsTable }

  TTClassRoomsTable = class(TMyTable)
    private
      procedure Initializate(); override;
  end;

var
  SQLTypes: array[0..TypeCount - 1] of String;
  MetaLibrary: TMetaLibrary;

implementation

{ TTRefrenceTable }

function TTRefrenceTable.GetWidth: integer;
var
  f: TMyField;
begin
  Result := 0;
  for f in FFields do
    Result += (f as TFIDRefrence).RefrenceTable.Fields[1].Width;
end;

{ TFIDRefrence }

function TFIDRefrence.GetRefrenceTable: TMyTable;
begin
  Result := MetaLibrary.SearchByClass(FRefrenceTableClass);
end;

procedure TFIDRefrence.MakeRefrence(ATable: TMyTable);
begin
  FRefrenceTable := ATable;
end;

{ TMetaLibrary }

procedure TMetaLibrary.AddTable(t: TMyTable);
begin
  SetLength(FMyTables, Length(FMyTables) + 1);
  FMyTables[High(FMyTables)] := t;
end;

constructor TMetaLibrary.Create;
begin
  AddTable(TTTeachersTable .Create());
  AddTable(TTSubjectsTable.Create());
  AddTable(TTPairsTable.Create());
  AddTable(TTWeekDaysTable .Create());
  AddTable(TTGroupsTable.Create());
  AddTable(TTClassRoomsTable.Create());
  AddTable(TTSubjectsTeachersTable.Create());
  AddTable(TTSubjectsGroupsTable.Create());
  AddTable(TTLessonsTable.Create());
end;

function TMetaLibrary.SearchTable(ASearchString: string): TMyTable;
var
  t: TMyTable;
begin
  for t in FMyTables do
    If t.Name = ASearchString then
      Exit(t);
  Result := nil;
end;

function TMetaLibrary.SearchByClass(ATableClass: TMyTableClass): TMyTable;
var
  t: TMyTable;
begin
  for t in FMyTables do
    if t is ATableClass then
      Exit(t);
end;

destructor TMetaLibrary.Destroy;
var
  t: TMyTable;
begin
  for t in FMyTables do
    t.Free;
end;

{ TMyField }

function TMyField.GetWidth(): integer;
begin
  Result := FWidth;
end;

constructor TMyField.Create;
begin
  Initializate();
end;

{ TFClassRoomID }

procedure TFClassRoomID.Initializate;
begin
  inherited Initializate();
  FName := 'CLASS_ID';
  FCaption := 'ID Аудитории';
  FRefrenceTableClass := TTClassRoomsTable;
end;

{ TFWeekDayID }

procedure TFWeekDayID.Initializate;
begin
  inherited Initializate();
  FName := 'WEEKDAY_ID';
  FCaption := 'ID дня недели';
  FRefrenceTableClass := TTWeekDaysTable;
end;

{ TFPairID }

procedure TFPairID.Initializate;
begin
  inherited Initializate();
  FName := 'PAIR_ID';
  FCaption := 'ID пары';
  FRefrenceTableClass := TTPairsTable;
end;

{ TFTeacherID }

procedure TFTeacherID.Initializate;
begin
  inherited Initializate();
  FName := 'TEACHER_ID';
  FCaption := 'ID преподователя';
  FRefrenceTableClass := TTTeachersTable;
end;

{ TFSubjectID }

procedure TFSubjectID.Initializate;
begin
  inherited Initializate;
  FName := 'SUBJECT_ID';
  FCaption := 'ID предмета';
  FRefrenceTableClass := TTSubjectsTable;
end;

{ TFGroupID }

procedure TFGroupID.Initializate;
begin
  inherited Initializate;
  FName := 'GROUP_ID';
  FCaption := 'ID группы';
  FRefrenceTableClass := TTGroupsTable;
end;

{ TTClassRoomsTable }

procedure TTClassRoomsTable.Initializate;
begin
  FName := 'CLASSROOMS';
  FCaption := 'Аудитории';
  AddField(TFID.Create());
  AddField(TFClassRoomName.Create());
end;

{ TTSubjectsGroupsTable }

procedure TTSubjectsGroupsTable.Initializate;
begin
  FName := 'GROUPS_SUBJECTS';
  FCaption := 'Соотношение группы-предметы';
  AddField(TFGroupID.Create());
  AddField(TFSubjectID.Create());
end;

{ TTSubjectsTeachersTable }

procedure TTSubjectsTeachersTable.Initializate;
begin
  FName := 'TEACHERS_SUBJECTS';
  FCaption := 'Соотношение преподователи-предметы';
  AddField(TFTeacherID.Create());
  AddField(TFSubjectID.Create());
end;

{ TTLessonsTable }

procedure TTLessonsTable.Initializate;
begin
  FName := 'LESSONS';
  FCaption := 'Занятия';
  AddField(TFPairID.Create());
  AddField(TFWeekDayID.Create());
  AddField(TFGroupID.Create());
  AddField(TFSubjectID.Create());
  AddField(TFClassRoomID.Create());
  AddField(TFTeacherID.Create());
end;

{ TTWeekDaysTable }

procedure TTWeekDaysTable.Initializate;
begin
  FName := 'WEEKDAY';
  FCaption := 'Дни недели';
  AddField(TFID.Create());
  AddField(TFWeekdayName.Create());
end;

{ TTPairsTable }

procedure TTPairsTable.Initializate;
begin
  FName := 'PAIR';
  FCaption := 'Пары';
  AddField(TFID.Create());
  AddField(TFPairNumber.Create());
end;

{ TTGroupsTable }

procedure TTGroupsTable.Initializate;
begin
  FName := 'GROUPS';
  FCaption := 'Группы';
  AddField(TFID.Create());
  AddField(TFGroupName.Create());
end;

{ TTSubjectsTable }

procedure TTSubjectsTable.Initializate;
begin
  FName := 'SUBJECTS';
  FCaption := 'Предметы';
  AddField(TFID.Create());
  AddField(TFSubjectName.Create());
end;

{ TTTeachersTable }

procedure TTTeachersTable.Initializate;
begin
  FName := 'TEACHERS';
  FCaption := 'Преподователи';
  AddField(TFID.Create());
  AddField(TFTeacherName.Create());
end;

{ TFID }

procedure TFID.Initializate;
begin
  FName := 'ID';
  FCaption := 'ID';
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TFSubjectName }

procedure TFSubjectName.Initializate();
begin
  inherited Initializate;
  FCaption := 'Название предмета';
  FWidth := 240;
end;

{ TFPairNumber }

procedure TFPairNumber.Initializate;
begin
  FCaption := 'Номер пары';
  FName := 'NUM';
  FWidth := 80;
  FFieldType := sqlInteger;
end;

{ TFWeekdayName }

procedure TFWeekdayName.Initializate;
begin
  inherited Initializate();
  FCaption := 'День недели';
  FWidth := 80;
end;

{ TFClassRoomName }

procedure TFClassRoomName.Initializate();
begin
  inherited Initializate();
  FCaption := 'Аудитория';
  FWidth := 80;
end;

{ TFGroupName }

procedure TFGroupName.Initializate;
begin
  inherited Initializate();
  FCaption := 'Код Группы';
  FWidth := 80;
end;

procedure TMyTable.AddField(AField: TMyField);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := AField;
end;

function TMyTable.GetMaxIndex(): integer;
begin
  Result := High(FFields);
end;

function TMyTable.GetWidth(): integer;
var
  f: TMyField;
begin
  Result := 0;
  for f in FFields do
    Result += f.Width;
end;

constructor TMyTable.Create;
begin
  Initializate();
end;

{ TFUnknow }

procedure TFUnknow.Initializate;
begin
  FName := 'NAME';
  FFieldType := sqlVarChar;
end;

{ TFTeacherName }

procedure TFTeacherName.Initializate;
begin
  inherited Initializate;
  FCaption := 'Преподователь';
  FWidth := 200;
end;

{ Procedures }

procedure InitializationMeta();
begin
  SQLTypes[Integer(sqlInteger)] := 'INTEGER';
  SQLTypes[Integer(sqlVarChar)] := 'VARCHAR(50)';
  SQLTypes[Integer(sqlBoolean)] := 'BOOLEAN';
  SQLTypes[Integer(sqlFloat)] := 'FLOAT';
end;

initialization
  InitializationMeta();
  MetaLibrary := TMetaLibrary.Create();

end.

