unit cu_tcpserver;


{
Copyright (C) 2020 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

{ TCP/IP Connexion, based on Synapse Echo demo }

{$MODE objfpc}{$H+}

interface

uses
  blcksock, synsock, synautil, cu_alpacadevice,
  SysUtils, Classes;

const
  Maxclient=100;
  msgFailed='Failed!';

type

  TTCPThrd = class(TThread)
  private
    FSock: TTCPBlockSocket;
    CSock: TSocket;
    FHttpRequest: string;
    FBody: string;
    FHttpResult: string;
    FConnectTime: double;
    FTerminate: TIntProc;
    FProcessGet: TGetCmd;
    FProcessPut: TPutCmd;
  public
    id: integer;
    abort, stoping: boolean;
    remoteip, remoteport: string;
    constructor Create(hsock: tSocket);
    procedure Execute; override;
    procedure SendData(str: string);
    procedure ProcessGet;
    procedure ProcessPut;
    property sock: TTCPBlockSocket read FSock;
    property ConnectTime: double read FConnectTime;
    property Terminated;
    property onTerminate: TIntProc read FTerminate write FTerminate;
    property onProcessGet: TGetCmd read FProcessGet write FProcessGet;
    property onProcessPut: TPutCmd read FProcessPut write FProcessPut;
  end;

  TTCPDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
    FShowError: TStringProc;
    FShowMsg: TStringProc;
    FShowSocket: TStringProc;
    FIPaddr, FIPport: string;
    FProcessGet: TGetCmd;
    FProcessPut: TPutCmd;
    procedure ShowError;
    procedure ThrdTerminate(var i: integer);
    function GetIPport: string;
  public
    stoping: boolean;
    TCPThrd: array [1..Maxclient] of TTCPThrd;
    ThrdActive: array [1..Maxclient] of boolean;
    constructor Create;
    procedure Execute; override;
    procedure ShowSocket;
    property IPaddr: string read FIPaddr write FIPaddr;
    property IPport: string read GetIPport write FIPport;
    property onShowError: TStringProc read FShowError write FShowError;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
    property onShowSocket: TStringProc read FShowSocket write FShowSocket;
    property onProcessGet: TGetCmd read FProcessGet write FProcessGet;
    property onProcessPut: TPutCmd read FProcessPut write FProcessPut;
  end;

implementation

{$ifdef darwin}
uses BaseUnix;       //  to catch SIGPIPE

var
  NewSigRec, OldSigRec: SigActionRec;
  res: integer;

{$endif}

procedure SplitCmdLineParams(const Params: string; ParamList: TStrings;
                             ReadBackslash: boolean = false);
// split spaces, quotes are parsed as single parameter
// if ReadBackslash=true then \" is replaced to " and not treated as quote
// #0 is always end
type
  TMode = (mNormal,mApostrophe,mQuote);
var
  p: Integer;
  Mode: TMode;
  Param: String;
begin
  p:=1;
  while p<=length(Params) do
  begin
    // skip whitespace
    while (p<=length(Params)) and (Params[p] in [' ',#9,#10,#13]) do inc(p);
    if (p>length(Params)) or (Params[p]=#0) then
      break;
    //writeln('SplitCmdLineParams After Space p=',p,'=[',Params[p],']');
    // read param
    Param:='';
    Mode:=mNormal;
    while p<=length(Params) do
    begin
      case Params[p] of
      #0:
        break;
      '\':
        begin
          inc(p);
          if ReadBackslash then
            begin
            // treat next character as normal character
            if (p>length(Params)) or (Params[p]=#0) then
              break;
            if ord(Params[p])<128 then
            begin
              Param+=Params[p];
              inc(p);
            end else begin
              // next character is already a normal character
            end;
          end else begin
            // treat backslash as normal character
            Param+='\';
          end;
        end;
      '''':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mApostrophe;
          mApostrophe:
            Mode:=mNormal;
          mQuote:
            Param+='''';
          end;
        end;
      '"':
        begin
          inc(p);
          case Mode of
          mNormal:
            Mode:=mQuote;
          mApostrophe:
            Param+='"';
          mQuote:
            Mode:=mNormal;
          end;
        end;
      ' ',#9,#10,#13:
        begin
          if Mode=mNormal then break;
          Param+=Params[p];
          inc(p);
        end;
      else
        Param+=Params[p];
        inc(p);
      end;
    end;
    //writeln('SplitCmdLineParams Param=#'+Param+'#');
    ParamList.Add(Param);
  end;
end;

constructor TTCPDaemon.Create;
var i: integer;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  for i:=1 to Maxclient do TCPThrd[i]:=nil;
end;

procedure TTCPDaemon.ShowError;
var
  msg: string;
begin
  msg := IntToStr(sock.lasterror) + ' ' + sock.GetErrorDesc(sock.lasterror);
  if assigned(FShowError) then FShowError(msg);
end;

function TTCPDaemon.GetIPport: string;
begin
  if sock=nil then
    result:=FIPport
  else begin
    sock.GetSins;
    result := IntToStr(sock.GetLocalSinPort);
  end;
end;

procedure TTCPDaemon.ShowSocket;
var
  locport: string;
begin
  sock.GetSins;
  locport := IntToStr(sock.GetLocalSinPort);
  if assigned(FShowSocket) then
    FShowSocket(locport);
end;

procedure TTCPDaemon.ThrdTerminate(var i: integer);
begin
  if (i>0) and (i<=Maxclient) then
     ThrdActive[i] := False;
end;

procedure TTCPDaemon.Execute;
var
  ClientSock: TSocket;
  i, n: integer;
begin
  //writetrace('start tcp deamon');
  stoping := False;
  for i := 1 to Maxclient do
    ThrdActive[i] := False;
  sock := TTCPBlockSocket.Create;
  //writetrace('blocksocked created');
  try
    with sock do
    begin
      //writetrace('create socket');
      CreateSocket;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      MaxLineLength := 1024;
      {$ifdef linux}
      //writetrace('setlinger');
      setLinger(True, 0);
      {$endif}
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('bind to '+fipaddr+' '+fipport);
      bind(FIPaddr, FIPport);
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('listen');
      listen;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      Synchronize(@ShowSocket);
      //writetrace('start main loop');
      repeat
        if stoping or terminated then
          break;
        if canread(500) and (not terminated) and (not stoping) then
        begin
          ClientSock := accept;
          if lastError = 0 then
          begin
            n := -1;
            for i := 1 to Maxclient do
              if (not ThrdActive[i]) or
                (TCPThrd[i] = nil) or (TCPThrd[i].Fsock = nil) or
                (TCPThrd[i].terminated) then
              begin
                n := i;
                break;
              end;
            if n > 0 then
            begin
              TCPThrd[n] := TTCPThrd.Create(ClientSock);
              TCPThrd[n].onTerminate := @ThrdTerminate;
              TCPThrd[n].onProcessGet := FProcessGet;
              TCPThrd[n].onProcessPut := FProcessPut;
              TCPThrd[n].id := n;
              ThrdActive[n] := True;
              TCPThrd[n].Start;
            end
            else
              with TTCPThrd.Create(ClientSock) do
              begin
                Fsock := TTCPBlockSocket.Create;
                Fsock.socket := CSock;
                Fsock.GetSins;
                Fsock.MaxLineLength := 1024;
                if not terminated then
                begin
                  if Fsock <> nil then begin
                   Fsock.SendString('HTTP/1.0 500' + CRLF);
                   Fsock.SendString('' + CRLF);
                   Fsock.SendString(msgFailed + ' Maximum connection reach!' + CRLF);
                  end;
                  Fsock.CloseSocket;
                  Fsock.Free;
                end;
                Free;
              end;
          end
          else if lasterror <> 0 then
            Synchronize(@ShowError);
        end;
      until False;
    end;
  finally
    //  Suspended:=true;
    Sock.CloseSocket;
    Sock.Free;
    //  terminate;
  end;
end;

constructor TTCPThrd.Create(Hsock: TSocket);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Csock := Hsock;
  abort := False;
  id:=-1;
end;

procedure TTCPThrd.Execute;
var
  req,hdr,body,method: string;
  args:Tstringlist;
begin
  try
    Fsock := TTCPBlockSocket.Create;
    args:=Tstringlist.Create;
    FConnectTime := now;
    stoping := False;
    try
      Fsock.socket := CSock;
      Fsock.GetSins;
      Fsock.MaxLineLength := 1024;
      remoteip := Fsock.GetRemoteSinIP;
      remoteport := IntToStr(Fsock.GetRemoteSinPort);
      with Fsock do
      begin
        repeat
          if stoping or terminated then
            break;
          req := RecvString(500);
          if lastError = 0 then
          begin
            hdr:='';
            repeat
              hdr:=hdr+crlf+RecvString(100);
            until LastError<>0;
            body:='';
            repeat
              body:=body+RecvPacket(100);
            until LastError<>0;
            SplitCmdLineParams(req,args);
            method:=uppercase(args[0]);
            if method='GET' then begin
               FHttpRequest:=args[1];
               Synchronize(@ProcessGet);
               SendString(FHttpResult + crlf);
               if lastError <> 0 then break;
               break;
            end
            else if method='PUT' then begin
               FHttpRequest:=args[1];
               FBody:=body;
               Synchronize(@ProcessPut);
               SendString(FHttpResult + crlf);
               if lastError <> 0 then break;
               break;
            end;
          end;
        until False;
      end;
    finally
      if assigned(FTerminate) then
        FTerminate(id);
      FSock.AbortSocket;
      Fsock.Free;
      args.Free;
    end;
  except
  end;
end;

procedure TTCPThrd.Senddata(str: string);
begin
  try
    if Fsock <> nil then
      with Fsock do
      begin
        if terminated then
          exit;
        SendString(str + CRLF);
        if LastError <> 0 then
          terminate;
      end;
  except
    terminate;
  end;
end;

procedure TTCPThrd.ProcessGet;
begin
  try
    FHttpResult:='';
    if Assigned(FProcessGet) then
      FHttpResult := FProcessGet(FHttpRequest);
  except
    FHttpResult := msgFailed;
  end;
end;

procedure TTCPThrd.ProcessPut;
begin
  try
    FHttpResult:='';
    if Assigned(FProcessPut) then
      FHttpResult := FProcessPut(FHttpRequest,FBody);
  except
    FHttpResult := msgFailed;
  end;
end;

initialization

 {$ifdef darwin}//  ignore SIGPIPE
 {$ifdef CPU32}
  with NewSigRec do
  begin
    integer(Sa_Handler) := SIG_IGN; // ignore signal
    Sa_Mask[0] := 0;
    Sa_Flags := 0;
  end;
  res := fpsigaction(SIGPIPE, @NewSigRec, @OldSigRec);
 {$endif}
 {$endif}
end.
