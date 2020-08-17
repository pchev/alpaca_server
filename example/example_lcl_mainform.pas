unit example_lcl_mainform;

{$mode objfpc}{$H+}

interface

uses  cu_alpacaserver, cu_alpacadevice, cu_example_protocol,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Server_IP: TLabel;
    Server_Port: TLabel;
    Server_Info: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    AlpacaIPAddr, AlpacaIPPort : string;
    AlpacaServer : T_AlpacaServer;
    MyDriver: T_Alpaca_Example;
    procedure ShowError(var msg:string);
    procedure ShowMsg(var msg:string);
    procedure PortMsg(var msg:string);
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

  Server_IP.Caption:='Server IP: '+AlpacaIPAddr;
  Server_Port.Caption:='Server Port: '+AlpacaIPPort;
  Server_Info.Caption:='';
  Memo1.Clear;

  AlpacaServer:=T_AlpacaServer.Create(self);
  AlpacaServer.onShowError:=@ShowError;
  AlpacaServer.onShowMsg:=@ShowMsg;
  AlpacaServer.onPortMsg:=@PortMsg;

  MyDriver:=T_Alpaca_Example.Create(self);
  AlpacaServer.AddDevice(telescope,MyDriver);

  AlpacaServer.IPAddr:=AlpacaIPAddr;
  AlpacaServer.IPPort:=AlpacaIPPort;
  AlpacaServer.StartServer;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MyDriver.SetConnected(False);
  AlpacaServer.StopServer;
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

end.

