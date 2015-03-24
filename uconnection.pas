unit uconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, db, FileUtil;

type

  { TConnectionModule }

  TConnectionModule = class(TDataModule)
    ConnectionComponent: TIBConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConnectionModule: TConnectionModule;

implementation

{$R *.lfm}

{ TConnectionModule }

procedure TConnectionModule.DataModuleCreate(Sender: TObject);
begin
  ConnectionModule.ConnectionComponent.Connected := True;
end;

end.

