unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, memds, db, FileUtil, SynMemo,
  SynHighlighterSQL, Forms, Controls, Graphics, Dialogs, DbCtrls, UTable, Menus,
  StdCtrls, UMeta, uTableManager, uConnectionForm, uconnection, DBGrids;

type

  { TTimeTableForm }

  TTimeTableForm = class(TForm)
    ExcecuteStatementButton: TButton;
    MainMenu: TMainMenu;
    FileItem: TMenuItem;
    AboutItem: TMenuItem;
    CodeMemo: TSynMemo;
    LogsMemo: TMemo;
    CommitItem: TMenuItem;
    TabelsItem: TMenuItem;
    SQLTransaction: TSQLTransaction;
    SynSQLSyn1: TSynSQLSyn;
    ExitItem: TMenuItem;
    procedure AboutItemClick(Sender: TObject);
    procedure CommitItemClick(Sender: TObject);
    procedure ExcecuteStatementButtonClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExceptionHandler(Sender : TObject; E : Exception);
  private
    SubTabelsItems: array of TMenuItem;
    FLog: string;
    { private declarations }
  public
    { public declarations }
  end;

var
  TimeTableForm: TTimeTableForm;


implementation

{$R *.lfm}

{ TTimeTableForm }

procedure TTimeTableForm.FormCreate(Sender: TObject);
var
  t: TTableManager;
begin
  Application.OnException := @ExceptionHandler;
  for t in TableManagers do begin
    SetLength(SubTabelsItems, Length(SubTabelsItems) + 1);
    SubTabelsItems[High(SubTabelsItems)] := t.GetMenuItem(Self);
    TabelsItem.Insert(High(SubTabelsItems), SubTabelsItems[High(SubTabelsItems)]);
  end;
  TransactionComponent := SQLTransaction;
end;

procedure TTimeTableForm.ExceptionHandler(Sender: TObject; E: Exception);
begin
  LogsMemo.Lines.Add(E.Message);
  If (TableForm <> nil) and TableForm.Visible then TableForm.Close;
end;

procedure TTimeTableForm.ExcecuteStatementButtonClick(Sender: TObject);
begin
  if TableForm = nil then begin
    Application.CreateForm(TTableForm, TableForm);
  end;
  with TableForm do begin
    ShowOnTop;
    SQLQuery.Active := False;
    SQLQuery.SQL.Clear;
    SQLQuery.SQL.AddStrings(CodeMemo.Lines);
    DBGrid.Options := DBGrid.Options + [dgAutoSizeColumns];
    SQLQuery.Active := True;
  end;
end;

procedure TTimeTableForm.AboutItemClick(Sender: TObject);
begin
  ShowMessage('Батраков Артем Б8103а');
end;

procedure TTimeTableForm.CommitItemClick(Sender: TObject);
begin
  TransactionComponent.Commit;
end;


procedure TTimeTableForm.ExitItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.

