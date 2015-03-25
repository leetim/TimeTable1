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
  TablesCount = 9;
  FieldNameCount = 10;
  FieldCaptionCount = 14;
//   = ('INTEGER', 'VARCHAR(50)', 'BOOLEAN', 'FLOAT');

type
  TSQLType = (sqlInteger, sqlVarChar, sqlBoolean, sqlFloat);
  TTableNames = (tnTeachers, tnSubjects, tnPairs, tnWeekdays, tnGroups,
    tnClassRooms, tnTeachersSubjects, tnGroupsSubjects, tnLessons);
  TTableCaptions = (tcTeachers, tcSubjects, tcPairs, tcWeekdays, tcGroups,
    tcClassRooms, tcTeachersSubjects, tcGroupsSubjects, tcLessons);
  TFieldName = (fnUnknow, fnID, fnName, fnNumber, fnTeachersID, fnSubjectsID,
    fnPairsID, fnWeekdaysID, fnGroupsID, fnClassRoomsID);
  TFieldCaption = (fcUnknow, fcID, fcTeachers, fcSubjects, fcPairs, fcWeekdays,
    fcGroups, fcClassRooms, fcTeachersID, fcSubjectsID, fcPairsID, fcWeekdaysID,
    fcGroupsID, fcClassRoomsID);
  TLibraryField = (lfID, lfPairNumber, lfWeekdayName, lfGroupName, lfSubjectName,
    lfClassRoomName, lfTeacherName, lfPairID, lfWeekDayID, lfGroupID,
    lfSubjectID, lfClassRoomID, lfTeacherID );

    { TMyField }

    TMyField = class(TPersistent)
    private
      FName: TFieldName;
      FCaption: TFieldCaption;
      FFieldType: TSQLType;
      FWidth: integer;
      FParentName: TTableNames;
      function GetName(): string; virtual;
      function GetCaption(): string; virtual;
      function GetWidth(): integer; virtual;
      procedure Initializate(); virtual; abstract;
    public
      constructor Create();
      property Name: string read GetName;
      property Caption: string read GetCaption;
      property FieldType: TSQLType read FFieldType;
      property Width: integer read GetWidth;
  end;

  TMyFieldsArray = array of TMyField;
  TMyFieldClass = class of TMyField;

  { TMyTable }

  TMyTable = class(TPersistent)
    private
      FName: TTableNames;
      FCaption: TTableCaptions;
      FFields: TMyFieldsArray;
      procedure AddField(AField: TMyField);
      function GetMaxIndex(): integer;
      function GetName(): string; virtual;
      function GetCaption(): string; virtual;
      procedure Initializate(); virtual; abstract;
    public
      constructor Create();
      property Name: string read GetName;
      property Caption: string read GetCaption;
      property Fields: TMyFieldsArray read FFields;
      property MaxIndex: integer read GetMaxIndex;
      function GetSQLCode(): TStringList; virtual;
  end;

  TMyTableClass = class of TMyTable;
  TMyTableArray = array of TMyTable;

  { TMetaLibrary }

  TMetaLibrary = class
    private
      FMyTables: TMyTableArray;
      FMyFields: TMyFieldsArray;
      procedure AddTable(t: TMyTable);
      procedure AddField(f: TMyField);
    public
      constructor Create();
      function SearchField(ASearchString: string): TMyField;
      function SearchTable(ASearchString: string): TMyTable;
      property Tables: TMyTableArray read FMyTables;
      property Fields: TMyFieldsArray read FMyFields;
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
      FRefrenceTableName: TTableNames;
      FRefrenceTable: TMyTable;
      procedure MakeRefrence(ATable: TMyTable);
    public
      property RefrenceTable: TMyTable read FRefrenceTable;
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
    protected
      FRefrences: TMyTableArray;
    public
      property Refrences: TMyTableArray read FRefrences;
      function GetSQLCode(): TStringList; override;
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

  function AddTable(t: TMyTable): TMyTable;
  function AddField(f: TMyField): TMyField;

var
  SQLTypes: array[0..TypeCount - 1] of String;
  TableNames, TableCaptions: array[0..TablesCount - 1] of String;
  FieldNames: array[0..FieldNameCount - 1] of String;
  FieldCaption: array[0..FieldCaptionCount - 1] of String;
  MyTables: array of TMyTable;
  MyFields: array of TMyField;
  MetaLibrary: TMetaLibrary;

implementation

{ TFIDRefrence }

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

procedure TMetaLibrary.AddField(f: TMyField);
begin
  SetLength(FMyFields, Length(FMyFields) + 1);
  FMyFields[High(FMyFields)] := f;
end;

constructor TMetaLibrary.Create;
begin
  AddField(TFID.Create());
  AddField(TFPairNumber.Create());
  AddField(TFWeekdayName.Create());
  AddField(TFGroupName.Create());
  AddField(TFSubjectName.Create());
  AddField(TFClassRoomName.Create());
  AddField(TFTeacherName.Create());
  AddField(TFPairID.Create());
  AddField(TFWeekDayID.Create());
  AddField(TFGroupID.Create());
  AddField(TFSubjectID.Create());
  AddField(TFClassRoomID.Create());
  AddField(TFTeacherID.Create());

  AddTable(TTTeachersTable .Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfTeacherName)]);
  end;

  AddTable(TTSubjectsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfSubjectName)]);
  end;

  AddTable(TTPairsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfPairNumber)]);
  end;

  AddTable(TTWeekDaysTable .Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfWeekdayName)]);
  end;

  AddTable(TTGroupsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfGroupName)]);
  end;

  AddTable(TTClassRoomsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfID)]);
    AddField(FMyFields[Integer(lfClassRoomName)]);
  end;

  AddTable(TTSubjectsTeachersTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfTeacherID)]);
    AddField(FMyFields[Integer(lfSubjectID)]);
  end;

  AddTable(TTSubjectsGroupsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfGroupID)]);
    AddField(FMyFields[Integer(lfSubjectID)]);
  end;

  AddTable(TTLessonsTable.Create());
  With FMyTables[High(FMyTables)] do begin
    AddField(FMyFields[Integer(lfPairID)]);
    (FMyFields[Integer(lfPairID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnPairs)]);
    AddField(FMyFields[Integer(lfWeekDayID)]);
    (FMyFields[Integer(lfWeekDayID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnWeekdays)]);
    AddField(FMyFields[Integer(lfGroupID)]);
    (FMyFields[Integer(lfGroupID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnGroups)]);
    AddField(FMyFields[Integer(lfSubjectID)]);
    (FMyFields[Integer(lfSubjectID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnSubjects)]);
    AddField(FMyFields[Integer(lfClassRoomID)]);
    (FMyFields[Integer(lfClassRoomID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnClassRooms)]);
    AddField(FMyFields[Integer(lfTeacherID)]);
    (FMyFields[Integer(lfTeacherID)] as TFIDRefrence).
      MakeRefrence(FMyTables[Integer(tnTeachers)]);
  end;
end;

function TMetaLibrary.SearchField(ASearchString: string): TMyField;
var
  f: TMyField;
  i: integer;
  s: string;
begin
  Delete(ASearchString, Length(ASearchString), 1);
  for f in FMyFields do begin
    s := f.Name;
    If s = ASearchString then
      Exit(f);
  end;
  Result := nil;
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

destructor TMetaLibrary.Destroy;
var
  f: TMyField;
  t: TMyTable;
begin
  for f in FMyFields do
    f.Free;
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

function TMyField.GetName: string;
begin
  Result := FieldNames[Integer(FName)];
end;

function TMyField.GetCaption: string;
begin
  Result := FieldCaption[Integer(FCaption)];
end;

{ TFClassRoomID }

procedure TFClassRoomID.Initializate;
begin
  inherited Initializate();
  FName := fnClassRoomsID;
  FCaption := fcClassRoomsID;
  FRefrenceTableName := tnClassRooms;
end;

{ TFWeekDayID }

procedure TFWeekDayID.Initializate;
begin
  inherited Initializate();
  FName := fnWeekdaysID;
  FCaption := fcWeekdaysID;
  FRefrenceTableName := tnWeekdays;
end;

{ TFPairID }

procedure TFPairID.Initializate;
begin
  inherited Initializate();
  FName := fnPairsID;
  FCaption := fcPairsID;
  FRefrenceTableName := tnPairs;
end;

{ TFTeacherID }

procedure TFTeacherID.Initializate;
begin
  inherited Initializate();
  FName := fnTeachersID;
  FCaption := fcTeachersID;
  FRefrenceTableName := tnTeachers;
end;

{ TFSubjectID }

procedure TFSubjectID.Initializate;
begin
  inherited Initializate;
  FName := fnSubjectsID;
  FCaption := fcSubjectsID;
  FRefrenceTableName := tnSubjects;
end;

{ TFGroupID }

procedure TFGroupID.Initializate;
begin
  inherited Initializate;
  FName := fnGroupsID;
  FCaption := fcGroupsID;
  FRefrenceTableName := tnGroups;
  FParentName := tnGroupsSubjects;
end;

{ TTClassRoomsTable }

procedure TTClassRoomsTable.Initializate;
begin
  FName := tnClassRooms;
  FCaption := tcClassRooms;
end;

{ TTSubjectsGroupsTable }

procedure TTSubjectsGroupsTable.Initializate;
begin
  FName := tnGroupsSubjects;
  FCaption := tcGroupsSubjects;
end;

{ TTSubjectsTeachersTable }

procedure TTSubjectsTeachersTable.Initializate;
begin
  FName := tnTeachersSubjects;
  FCaption := tcTeachersSubjects;
end;

{ TTLessonsTable }

procedure TTLessonsTable.Initializate;
begin
  FName := tnLessons;
  FCaption := tcLessons;
end;

{ TTWeekDaysTable }

procedure TTWeekDaysTable.Initializate;
begin
  FName := tnWeekdays;
  FCaption := tcWeekdays;
end;

{ TTPairsTable }

procedure TTPairsTable.Initializate;
begin
  FName := tnPairs;
  FCaption := tcPairs;
end;

{ TTGroupsTable }

procedure TTGroupsTable.Initializate;
begin
  FName := tnGroups;
  FCaption := tcGroups;
end;

{ TTSubjectsTable }

procedure TTSubjectsTable.Initializate;
begin
  FName := tnSubjects;
  FCaption := tcSubjects;
end;

{ TTTeachersTable }

procedure TTTeachersTable.Initializate;
begin
  FName := tnTeachers;
  FCaption := tcTeachers;
end;

{ TTRefrenceTable }

function TTRefrenceTable.GetSQLCode(): TStringList;
var
  i: integer;
  s: string;
begin
  //Result := TStringList.Create();
  //With Result do begin
  //  s := 'SELECT ';
  //  for i := 0 to High(FFields) do begin
  //    s += FRefrences[i].NameField.Name;
  //    if i <> High(FFields) then
  //      s += ', ';
  //  end;
  //  If Result <> nil then
  //    Add(s);
  //  i += 1;
  //  Add('FROM ' + Name);
  //end;
  //with Result do begin
  //  for i := 0 to High(FFields) do begin
  //    Add('INNER JOIN ' + FRefrences[i].Name);
  //    Add('ON ' + FRefrences[i].IDField.Name + ' = ' + FFields[i].Name);
  //  end;
  //end;
end;

{ TFID }

procedure TFID.Initializate;
begin
  FName := fnID;
  FCaption := fcID;
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TFSubjectName }

procedure TFSubjectName.Initializate();
begin
  inherited Initializate;
  FParentName := tnSubjects;
  FCaption := fcSubjects;
  FWidth := 240;
end;

{ TFPairNumber }

procedure TFPairNumber.Initializate;
begin
  FCaption := fcPairs;
  FParentName := tnPairs;
  FName := fnNumber;
  FWidth := 40;
  FFieldType := sqlInteger;
end;

{ TFWeekdayName }

procedure TFWeekdayName.Initializate;
begin
  inherited Initializate();
  FParentName := tnWeekdays;
  FCaption := fcWeekdays;
  FWidth := 80;
end;

{ TFClassRoomName }

procedure TFClassRoomName.Initializate();
begin
  inherited Initializate();
  FParentName := tnClassRooms;
  FCaption := fcClassRooms;
  FWidth := 80;
end;

{ TFGroupName }

procedure TFGroupName.Initializate;
begin
  inherited Initializate();
  FParentName := tnGroups;
  FCaption := fcGroups;
  FWidth := 50;
end;

{ TMyTable }

function TMyTable.GetSQLCode: TStringList;
var
  s: string;
  i: integer;
begin
  //Result := TStringList.Create();
  //With Result do begin
  //  s := 'SELECT ';
  //  for i := 0 to High(FFields) do begin
  //    s += FFields[i].Name;
  //    if i <> High(FFields) then
  //      s += ', ';
  //  end;
  //  If Result <> nil then
  //    Add(s);
  //  i += 1;
  //  Add('FROM ' + Name);
  //end;
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

function TMyTable.GetName: string;
begin
  Result := TableNames[Integer(FName)];
end;

function TMyTable.GetCaption: string;
begin
  Result := TableCaptions[Integer(FCaption)];
end;

constructor TMyTable.Create;
begin
  Initializate();
end;

{ TFUnknow }

procedure TFUnknow.Initializate;
begin
  FName := fnName;
  FCaption := fcUnknow;
  FFieldType := sqlVarChar;
end;

{ TFTeacherName }

procedure TFTeacherName.Initializate;
begin
  inherited Initializate;
  FParentName := tnTeachers;
  FCaption := fcTeachers;
  FWidth := 200;
end;

{ Procedures }

function AddTable(t: TMyTable): TMyTable;
begin
  Result := t;
  SetLength(MyTables, Length(MyTables) + 1);
  MyTables[High(MyTables)] := t;
end;

function AddField(f: TMyField): TMyField;
begin
  Result := f;
  SetLength(MyFields, Length(MyFields) + 1);
  MyFields[High(MyFields)] := f;
end;

procedure InitializationMeta();
begin
  SQLTypes[Integer(sqlInteger)] := 'INTEGER';
  SQLTypes[Integer(sqlVarChar)] := 'VARCHAR(50)';
  SQLTypes[Integer(sqlBoolean)] := 'BOOLEAN';
  SQLTypes[Integer(sqlFloat)] := 'FLOAT';

  TableNames[Integer(tnClassRooms)] := 'CLASSROOMS ';
  TableNames[Integer(tnGroups)] := 'GROUPS ';
  TableNames[Integer(tnGroupsSubjects)] := 'GROUPS_SUBJECTS ';
  TableNames[Integer(tnLessons)] := 'LESSONS ';
  TableNames[Integer(tnPairs)] := 'PAIR ';
  TableNames[Integer(tnSubjects)] := 'SUBJECTS ';
  TableNames[Integer(tnTeachers)] := 'TEACHERS ';
  TableNames[Integer(tnTeachersSubjects)] := 'TEACHERS_SUBJECTS ';
  TableNames[Integer(tnWeekdays)] := 'WEEKDAY ';

  TableCaptions[Integer(tcClassRooms)] := 'Аудитории';
  TableCaptions[Integer(tcGroups)] := 'Группы';
  TableCaptions[Integer(tcGroupsSubjects)] := 'Соотношение Группы-Предметы';
  TableCaptions[Integer(tcLessons)] := 'Занятия';
  TableCaptions[Integer(tcPairs)] := 'Пары';
  TableCaptions[Integer(tcSubjects)] := 'Предметы';
  TableCaptions[Integer(tcTeachers)] := 'Преподователи';
  TableCaptions[Integer(tcTeachersSubjects)] := 'Соотношения Группы_Предметы';
  TableCaptions[Integer(tcWeekdays)] := 'Дни недели';

  FieldCaption[Integer(fcUnknow)] := 'Неизвестно';
  FieldCaption[Integer(fcID)] := 'ID';
  FieldCaption[Integer(fcTeachers)] := 'Преподователь';
  FieldCaption[Integer(fcSubjects)] := 'Предмет';
  FieldCaption[Integer(fcPairs)] := 'Пара';
  FieldCaption[Integer(fcWeekdays)] := 'День недели';
  FieldCaption[Integer(fcGroups)] := 'Группы';
  FieldCaption[Integer(fcClassRooms)] := 'Аудитория';
  FieldCaption[Integer(fcTeachersID)] := 'ID Преподователя';
  FieldCaption[Integer(fcSubjectsID)] := 'ID Предмета';
  FieldCaption[Integer(fcPairsID)] := 'ID Пары';
  FieldCaption[Integer(fcWeekdaysID)] := 'ID Дня недели';
  FieldCaption[Integer(fcGroupsID)] := 'ID Группы';
  FieldCaption[Integer(fcClassRoomsID)] := 'ID Аудитории';

  FieldNames[Integer(fnUnknow)] := 'UNKNOW ';
  FieldNames[Integer(fnID)] := 'ID ';
  FieldNames[Integer(fnNumber)] := 'NUM ';
  FieldNames[Integer(fnName)] := 'NAME ';
  FieldNames[Integer(fnClassRoomsID)] := 'CLASS_ID ';
  FieldNames[Integer(fnGroupsID)] := 'GROUP_ID ';
  FieldNames[Integer(fnPairsID)] := 'PAIR_ID ';
  FieldNames[Integer(fnSubjectsID)] := 'SUBJECT_ID ';
  FieldNames[Integer(fnTeachersID)] := 'TEACHER_ID ';
  FieldNames[Integer(fnWeekdaysID)] := 'WEEKDAY_ID ';
end;

initialization
  InitializationMeta();
  MetaLibrary := TMetaLibrary.Create();

end.

