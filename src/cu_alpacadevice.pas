unit cu_alpacadevice;

{$mode objfpc}{$H+}
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

interface

uses
  Classes, SysUtils;

type

  TAlpacaDeviceType=(switch,safetymonitor,dome,camera,observingconditions,filterwheel,focuser,rotator,telescope);

  TAxisRate = class(TObject)
    public
      maximum,minimum: double;
  end;
  TAxisRates = array of TAxisRate;
  TTrackingRates = array of integer;

  TStringProc = procedure(var S: string) of object;
  TIntProc = procedure(var i: integer) of object;
  TGetCmd = function(cmd: string): string of object;
  TPutCmd = function(cmd,arg: string): string of object;


const
  ApiVersion='v1';
  AlpacaDeviceName: array[0..ord(high(TAlpacaDeviceType))] of string =
    ('switch','safetymonitor','dome','camera','observingconditions','filterwheel','focuser','rotator','telescope');
  NullCoord=-9999;
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  ERR_NOT_IMPLEMENTED = $400;
  ERR_INVALID_VALUE = $401;
  ERR_VALUE_NOT_SET = $402;
  ERR_NOT_CONNECTED = $407;
  ERR_INVALID_WHILE_PARKED = $408;
  ERR_INVALID_WHILE_SLAVED = $409;
  ERR_INVALID_OPERATION = $40B;
  ERR_ACTION_NOT_IMPLEMENTED = $40C;
  ERR_DRIVER_ERROR = $500;
  MSG_NOT_IMPLEMENTED = 'Property or method not implemented';
  MSG_INVALID_VALUE = 'Invalid value';
  MSG_VALUE_NOT_SET = 'Value not set';
  MSG_NOT_CONNECTED = 'Not connected';
  MSG_DRIVER_ERROR = 'Driver error';


type

  T_AlpacaDevice = class(TComponent)
    protected
      FConnected: boolean;
      FErrorNumber: integer;
      FErrorMessage: String;
      FPath,FSetupPath: string;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  FormatEmptyResp(ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatRawResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatStringResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatStringListResp(value:TStringList; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatBoolResp(value:boolean; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatIntResp(value:integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatIntArrayResp(value:array of integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatFloatResp(value:double; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatAxisRateResp(value:TAxisRates; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  DecodeRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
      function  DecodeSetupRequest(req: string; out method: string; var params: TStringlist):boolean;
      function  GetParamString(params: Tstringlist; key: string; out value: string):boolean;
      function  GetParamBool(params: Tstringlist; key: string; out value: boolean):boolean;
      function  GetParamFloat(params: Tstringlist; key: string; out value: double):boolean;
      function  GetParamInt(params: Tstringlist; key: string; out value: integer):boolean;
      function  ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string; virtual; abstract;
      function  ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string; virtual; abstract;
      function  ProcessSetup(req: string; out status: integer):string; virtual; abstract;
      function  GetGuid: string; virtual; abstract;
      // Common properties and methods.
      function  Action( actionName, actionParameters: string):string; virtual; abstract;
      procedure CommandBlind( command: string;  raw: boolean = false); virtual; abstract;
      function  CommandBool(command: string;  raw: boolean = false):boolean; virtual; abstract;
      function  CommandString(command: string;  raw: boolean = false):string; virtual; abstract;
      function  Connected:boolean; virtual; abstract;
      procedure SetConnected(value:boolean); virtual; abstract;
      function  Description:string; virtual; abstract;
      function  DriverInfo:string; virtual; abstract;
      function  DriverVersion:string; virtual; abstract;
      function  InterfaceVersion: integer; virtual; abstract;
      function  Name:string; virtual; abstract;
      function  SupportedActions:TStringList; virtual; abstract;
      property  Path: string read FPath write FPath;
      property  SetupPath: string read FSetupPath write FSetupPath;
  end;

procedure SplitRec(buf,sep:string; var arg: TStringList);


implementation

constructor T_AlpacaDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected:=False;
end;

destructor  T_AlpacaDevice.Destroy;
begin
  inherited Destroy;
end;

function T_AlpacaDevice.DecodeRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
var i,p: integer;
    buf:string;
begin
result:=false;
buf:=copy(req,1,length(FPath));
if copy(req,1,length(FPath))<>FPath then exit;
Delete(req,1,length(FPath));
p:=pos('?',req);
if p<=0 then begin
  method:=req;
  params.Clear;
  ClientID:=0;
  ClientTransactionID:=0;
end
else begin
  method:=copy(req,1,p-1);
  delete(req,1,p);
  params.Clear;
  ClientID:=0;
  ClientTransactionID:=0;
  SplitRec(req,'&',params);
  for i:=0 to params.Count-1 do begin
    if uppercase(copy(params[i],1,9))='CLIENTID=' then begin
      buf:=params[i];
      delete(buf,1,9);
      ClientID:=StrToIntDef(buf,0);
      params.Delete(i);
      break;
    end;
  end;
  for i:=0 to params.Count-1 do begin
    if uppercase(copy(params[i],1,20))='CLIENTTRANSACTIONID=' then begin
      buf:=params[i];
      delete(buf,1,20);
      ClientTransactionID:=StrToIntDef(buf,0);
      params.Delete(i);
      break;
    end;
  end;
end;
result:=true;
end;

function T_AlpacaDevice.DecodeSetupRequest(req: string; out method: string; var params: TStringlist):boolean;
var p: integer;
begin
result:=false;
if copy(req,1,length(FSetupPath))<>FSetupPath then exit;
Delete(req,1,length(FSetupPath));
p:=pos('?',req);
if p<=0 then begin
  method:=req;
  params.Clear;
end
else begin
  method:=copy(req,1,p-1);
  delete(req,1,p);
  params.Clear;
  SplitRec(req,'&',params);
end;
result:=true;
end;

function  T_AlpacaDevice.GetParamString(params: Tstringlist; key: string; out value: string):boolean;
var i: integer;
    buf: string;
begin
result:=false;
buf:=UpperCase(key)+'=';
for i:=0 to params.Count-1 do begin
  if uppercase(copy(params[i],1,length(buf)))=buf then begin
     value:=params[i];
     Delete(value,1,length(buf));
     result:=true;
     break;
  end;
end;
if not result then begin
  FErrorMessage:=MSG_VALUE_NOT_SET+' '+key;
  FErrorNumber:=ERR_VALUE_NOT_SET;
end;
end;

function  T_AlpacaDevice.GetParamBool(params: Tstringlist; key: string; out value: boolean):boolean;
var buf: string;
begin
result:=GetParamString(params,key,buf);
if result then begin
   buf:=UpperCase(buf);
   if buf='TRUE' then
     value:=true
   else if buf='FALSE' then
     value:=false
   else begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.GetParamFloat(params: Tstringlist; key: string; out value: double):boolean;
var buf: string;
begin
result:=GetParamString(params,key,buf);
if result then begin
   value:=StrToFloatDef(buf,NullCoord);
   if value=NullCoord then begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.GetParamInt(params: Tstringlist; key: string; out value: integer):boolean;
var buf: string;
    n: integer;
begin
result:=GetParamString(params,key,buf);
if result then begin
   val(buf,value,n);
   if n<>0 then begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.FormatEmptyResp(ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatStringResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +'"'+value+'",'
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatRawResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +value+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatStringListResp(value:TStringList; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    i: integer;
begin
  if value.Count=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to value.Count-1 do begin
      buf:=buf+'"'+value[i]+'",';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatBoolResp(value:boolean; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +BoolToStr(value,'true','false')+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatIntResp(value:integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +IntToStr(value)+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatIntArrayResp(value:array of integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    n,i: integer;
begin
  n:=length(value);
  if n=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to n-1 do begin
        buf:=buf+IntToStr(value[i])+',';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatFloatResp(value:double; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +FormatFloat('0.0000000000',value)+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatAxisRateResp(value:TAxisRates; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    n,i: integer;
begin
  n:=length(value);
  if n=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to n-1 do begin
        buf:=buf+'{"Maximum":'+FormatFloat('0.0000000000',value[i].maximum)+',';
        buf:=buf+'"Minimum":'+FormatFloat('0.0000000000',value[i].minimum)+'},';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

procedure SplitRec(buf,sep:string; var arg: TStringList);
var i,l:integer;
begin
arg.clear;
l:=length(sep);
while pos(sep,buf)<>0 do begin
 for i:=1 to length(buf) do begin
  if copy(buf,i,l) = sep then begin
      arg.add(copy(buf,1,i-1));
      delete(buf,1,i-1+l);
      break;
  end;
 end;
end;
arg.add(buf);
end;


end.

