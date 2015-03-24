program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMeta, UMain, UTable, uconnection;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TConnectionModule, ConnectionModule);
  Application.CreateForm(TTimeTableForm, TimeTableForm);
  TimeTableForm.Show;
  Application.Run;
end.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
