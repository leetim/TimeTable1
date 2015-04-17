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
    FilterPanel: TPanel;
    ScrollBar1: TScrollBar;
    SQLQuery: TSQLQuery;
    procedure DBGridTitleClick(Column: TColumn);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
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

procedure TTableForm.FormCreate(Sender: TObject);
begin
  FIndentPanel := 0;
end;

procedure TTableForm.ScrollBar1Change(Sender: TObject);
var
  p: TPanel;
  i: integer;
begin
  With ScrollBar1 do begin
    FIndentPanel := round(FilterPanel.Height * Position / PageSize);
    for i := 0 to High(FPanels) do begin
      //FPanels[i].Visible := False;
      FPanels[i].Top := Indent + i * (PanelHeight + Indent) - FIndentPanel;
      //FPanels[i].Visible := True;
    end;
  end;
end;

end.                                                                     
