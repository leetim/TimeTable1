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


end.                                                                     
