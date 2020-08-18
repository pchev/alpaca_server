program example_lcl;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, example_lcl_mainform, sysutils
  { you can add units after this };

{$R *.res}

begin
  {$ifdef USEHEAPTRC}
    {$ifdef mswindows}
      DeleteFile('C:\Temp\alpaca_example_heap.trc');
      SetHeapTraceOutput('C:\Temp\alpaca_example_heap.trc');
    {$else}
      DeleteFile('/tmp/alpaca_example_heap.trc');
      SetHeapTraceOutput('/tmp/alpaca_example_heap.trc');
    {$endif}
  {$endif}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

