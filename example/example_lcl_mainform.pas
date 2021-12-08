unit example_lcl_mainform;

{$mode objfpc}{$H+}

interface

uses  cu_alpacaserver, cu_alpacadevice, cu_example_protocol,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Discovery_Info: TLabel;
    Server_IP: TLabel;
    Server_Port: TLabel;
    Server_Info: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    AlpacaIPAddr, AlpacaIPPort, AlpacaDiscoveryPort : string;
    AlpacaServer : T_AlpacaServer;
    MyDriver: T_Alpaca_Example;
    procedure ShowError(var msg:string);
    procedure ShowMsg(var msg:string);
    procedure PortMsg(var msg:string);
    procedure DiscoveryPortMsg(var msg:string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  AlpacaIPAddr:='0.0.0.0';
  AlpacaIPPort:='22333';
  AlpacaDiscoveryPort:='32227';

  Server_IP.Caption:='Server IP: '+AlpacaIPAddr;
  Server_Port.Caption:='Server Port: '+AlpacaIPPort;
  Server_Info.Caption:='';
  Memo1.Clear;

  AlpacaServer:=T_AlpacaServer.Create(self);
  AlpacaServer.ServerName:='Example of Alpaca server';
  AlpacaServer.onShowError:=@ShowError;
  AlpacaServer.onShowMsg:=@ShowMsg;
  AlpacaServer.onPortMsg:=@PortMsg;
  AlpacaServer.onDiscoveryPortMsg:=@DiscoveryPortMsg;

  MyDriver:=T_Alpaca_Example.Create(nil);
  AlpacaServer.AddDevice(telescope,MyDriver);

  AlpacaServer.IPAddr:=AlpacaIPAddr;
  AlpacaServer.IPPort:=AlpacaIPPort;
  AlpacaServer.DiscoveryPort:=AlpacaDiscoveryPort;
  AlpacaServer.StartServer;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: integer;
begin
  MyDriver.SetConnected(False);
  AlpacaServer.StopServer;
  for i:=0 to 10 do begin
    sleep(200);
    Application.ProcessMessages;
  end;
end;

procedure TForm1.ShowError(var msg:string);
begin
  Memo1.Lines.Add('Error: '+msg);
end;

procedure TForm1.ShowMsg(var msg:string);
begin
  Memo1.Lines.Add(msg);
end;

procedure TForm1.PortMsg(var msg:string);
begin
  Server_Info.Caption:='Server running on port '+msg;
end;

procedure TForm1.DiscoveryPortMsg(var msg:string);
begin
  Discovery_Info.Caption:='Discovery running on port '+msg;
end;

end.

