unit UTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, memds, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, Menus, StdCtrls, DBGrids;

type

  { TTableForm }

  TTableForm = class(TForm)
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    procedure DBGridTitleClick(Column: TColumn);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TableForm: TTableForm;

implementation

{$R *.lfm}

{ TTableForm }


procedure TTableForm.DBGridTitleClick(Column: TColumn);
begin
  //ShowMessage(Column.Title.Caption);
end;

end.                                                                     
