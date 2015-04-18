unit UTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, memds, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DbCtrls, Menus, StdCtrls, DBGrids, ExtCtrls, Buttons;

const
  Indent = 5;
  ScrollLength = 20;
  PanelHeight = 35;

type

  { TTableForm }

  TAddFilterEvent = procedure of Object;

  TTableForm = class(TForm)
    AddFilterButton: TButton;
    ApplyButton: TBitBtn;
    DBGrid: TDBGrid;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    FilterPanel: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure DBGridTitleClick(Column: TColumn);
    procedure DBNavigatorClick(Sender: TObject; Button: TDBNavButtonType);
    procedure FormCreate(Sender: TObject);
  private
  public
    FPanels: array of TPanel;
    FIndentPanel: integer;
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

procedure TTableForm.DBNavigatorClick(Sender: TObject; Button: TDBNavButtonType
  );
begin

end;

procedure TTableForm.FormCreate(Sender: TObject);
begin
  FIndentPanel := 0;
end;

end.                                                                     
