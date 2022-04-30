unit cu_example_protocol;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
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

{
This unit implement the specific telescope protocol.
For the detail of the requirement for every function see:
https://www.ascom-standards.org/Help/Developer/html/T_ASCOM_DeviceInterface_ITelescopeV3.htm

Beware that many unimplemented result in this example do not respect the ASCOM protocol.
Be sure to test the driver using the ASCOM Conformance Checker Tool to validate the result.
}

interface

uses  cu_alpacatelescope, cu_alpacadevice, cu_serial, Classes, SysUtils;

type

  T_Alpaca_Example = class(T_AlpacaTelescope)
    protected
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  GetGuid: string; override;
      function  GetSetupPage: string; override;
      function  Action( actionName, actionParameters: string):string; override;
      procedure CommandBlind( command: string;  raw: boolean = false); override;
      function  CommandBool(command: string;  raw: boolean = false):boolean; override;
      function  CommandString(command: string;  raw: boolean = false):string; override;
      function  Connected:boolean; override;
      procedure SetConnected(value:boolean); override;
      function  Description:string; override;
      function  DriverInfo:string; override;
      function  DriverVersion:string; override;
      function  InterfaceVersion: integer; override;
      function  Name:string; override;
      function  SupportedActions:TStringList; override;
      function  alignmentmode: integer; override;
      function  altitude: double; override;
      function  aperturearea: double; override;
      function  aperturediameter: double; override;
      function  athome: boolean; override;
      function  atpark: boolean; override;
      function  azimuth: double; override;
      function  canfindhome: boolean; override;
      function  canpark: boolean; override;
      function  canunpark: boolean; override;
      function  canpulseguide: boolean; override;
      function  cansetdeclinationrate: boolean; override;
      function  cansetrightascensionrate: boolean; override;
      function  cansetguiderates: boolean; override;
      function  cansetpark: boolean; override;
      function  cansetpierside: boolean; override;
      function  cansettracking: boolean; override;
      function  canslew: boolean; override;
      function  canslewaltaz: boolean; override;
      function  canslewaltazasync: boolean; override;
      function  canslewasync: boolean; override;
      function  cansync: boolean; override;
      function  cansyncaltaz: boolean; override;
      function  declination: double; override;
      function  declinationrate: double; override;
      procedure setdeclinationrate(value: double); override;
      function  doesrefraction: boolean; override;
      procedure setdoesrefraction(value: boolean); override;
      function  equatorialsystem: integer; override;
      function  focallength: double; override;
      function  guideratedeclination: double; override;
      procedure setguideratedeclination(value: double); override;
      function  guideraterightascension: double; override;
      procedure setguideraterightascension(value: double); override;
      function  ispulseguiding: boolean; override;
      function  rightascension: double; override;
      function  rightascensionrate: double; override;
      procedure setrightascensionrate(value: double); override;
      function  sideofpier: integer; override;
      procedure setsideofpier(value: integer); override;
      function  siderealtime: double; override;
      function  siteelevation: double; override;
      procedure setsiteelevation(value: double); override;
      function  sitelatitude: double; override;
      procedure setsitelatitude(value: double); override;
      function  sitelongitude: double; override;
      procedure setsitelongitude(value: double); override;
      function  is_slewing: boolean; override;
      function  slewsettletime: integer; override;
      procedure setslewsettletime(value: integer; out ok: boolean); override;
      function  targetdeclination: double; override;
      procedure settargetdeclination(value: double; out ok: boolean); override;
      function  targetrightascension: double; override;
      procedure settargetrightascension(value: double; out ok: boolean); override;
      function  tracking: boolean; override;
      procedure settracking(value: boolean); override;
      function  utcdate: string; override;
      function  trackingrate: integer; override;
      procedure settrackingrate(value: integer); override;
      function  trackingrates: TTrackingRates; override;
      procedure setutcdate(value: string); override;
      procedure abortslew; override;
      function  axisrates(axis:integer): TAxisRates; override;
      function  canmoveaxis(axis:integer): boolean; override;
      function  destinationsideofpier(ra,dec: double):integer; override;
      procedure findhome; override;
      procedure moveaxis(axis:integer;rate:double); override;
      procedure park; override;
      procedure pulseguide(direction,duration: integer); override;
      procedure setpark; override;
      procedure slewtoaltaz(az,alt: double); override;
      procedure slewtoaltazasync(az,alt: double); override;
      procedure slewtocoordinates(ra,dec: double; out ok: boolean); override;
      procedure slewtocoordinatesasync(ra,dec: double; out ok: boolean); override;
      procedure slewtotarget; override;
      procedure slewtotargetasync; override;
      procedure synctoaltaz(az,alt: double); override;
      procedure synctocoordinates(ra,dec: double; out ok: boolean); override;
      procedure synctotarget; override;
      procedure unpark; override;
  end;

implementation

// Replace the following by the driver UniqueID
// On Linux this can be generated by the command uuidgen
// See the ASCOM Alpaca Management API configureddevices
// https://ascom-standards.org/api/?urls.primaryName=ASCOM%20Alpaca%20Management%20API#/Management%20Interface%20(JSON)/get_management_v1_configureddevices
const guid='example-driver-unique-id';

constructor T_Alpaca_Example.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_Alpaca_Example.Destroy;
begin
  inherited Destroy;
end;

function  T_Alpaca_Example.GetGuid: string;
begin
  result:=guid;
end;

function  T_Alpaca_Example.Action( actionName, actionParameters: string):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_Alpaca_Example.CommandBlind( command: string;  raw: boolean = false);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.CommandBool(command: string;  raw: boolean = false):boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.CommandString(command: string;  raw: boolean = false):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

function  T_Alpaca_Example.Connected:boolean;
begin
  result:=FConnected;
end;

procedure  T_Alpaca_Example.SetConnected(value:boolean);
begin
  // code to make connection
  FConnected:=value;
end;

function  T_Alpaca_Example.Description:string;
begin
  result:='Driver description.';
end;

function  T_Alpaca_Example.DriverInfo:string;
begin
  result:='Information about driver';
end;

function  T_Alpaca_Example.DriverVersion:string;
begin
  result:='Driver version';
end;

function  T_Alpaca_Example.InterfaceVersion: integer;
begin
  result:=3; // ITelescope version implementation
end;

function  T_Alpaca_Example.Name:string;
begin
  result:='Example driver';
end;

function  T_Alpaca_Example.SupportedActions:TStringList;
begin
  result:=TStringList.Create;
  result.Clear;
end;

function  T_Alpaca_Example.alignmentmode: integer;
begin
    result:=0;
    FErrorNumber:=ERR_NOT_IMPLEMENTED;
    FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.altitude: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.aperturearea: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_Alpaca_Example.aperturediameter: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_Alpaca_Example.athome: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.atpark: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.azimuth: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Example.canfindhome: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.canpark: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.canunpark: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.canpulseguide: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.cansetdeclinationrate: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.cansetguiderates: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.cansetpark: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.cansetpierside: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.cansetrightascensionrate: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.cansettracking: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.canslew: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.canslewaltaz: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.canslewaltazasync: boolean;
begin
  result:=false;
end;

function T_Alpaca_Example.canslewasync: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.cansync: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.cansyncaltaz: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Example.declination: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Example.declinationrate: double;
begin
  result:=0;
end;

procedure T_Alpaca_Example.setdeclinationrate(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.doesrefraction: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=true;
end;

procedure T_Alpaca_Example.setdoesrefraction(value: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.equatorialsystem: integer;
begin
  result:=0;
end;

function  T_Alpaca_Example.focallength: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Example.guideratedeclination: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Example.setguideratedeclination(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.guideraterightascension: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Example.setguideraterightascension(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.ispulseguiding: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=False;
end;

function  T_Alpaca_Example.rightascension: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Example.rightascensionrate: double;
begin
  result:=0;
end;

procedure T_Alpaca_Example.setrightascensionrate(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.sideofpier: integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Example.setsideofpier(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.siderealtime: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Example.siteelevation: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.setsiteelevation(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.sitelatitude: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.setsitelatitude(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.sitelongitude: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.setsitelongitude(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.is_slewing: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=False;
end;

function  T_Alpaca_Example.slewsettletime: integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Example.setslewsettletime(value: integer; out ok: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  ok:=true;
end;

function  T_Alpaca_Example.targetdeclination: double;
begin
  if TargetDEC>NullCoord then
    result:=TargetDEC
  else begin
    result:=0;
    FErrorNumber:=ERR_VALUE_NOT_SET;
    FErrorMessage:=MSG_VALUE_NOT_SET;
  end;
end;

procedure T_Alpaca_Example.settargetdeclination(value: double; out ok: boolean);
begin
  if (value>=-90)and(value<=90) then begin
     TargetDEC:=value;
     ok:=true;
  end
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' dec='+ FormatFloat('0.000',value);
    ok:=false;
  end;
end;

function  T_Alpaca_Example.targetrightascension: double;
begin
  if TargetDEC>NullCoord then
    result:=TargetRA
  else begin
    result:=0;
    FErrorNumber:=ERR_VALUE_NOT_SET;
    FErrorMessage:=MSG_VALUE_NOT_SET;
  end;
end;

procedure T_Alpaca_Example.settargetrightascension(value: double; out ok: boolean);
begin
  if (value>=0)and(value<=24) then begin
     TargetRA:=value;
     ok:=true;
  end
  else begin
    FErrorNumber:=ERR_INVALID_VALUE;
    FErrorMessage:=MSG_INVALID_VALUE +' ra='+ FormatFloat('0.000',value);
    ok:=false;
  end;
end;

function  T_Alpaca_Example.tracking: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

procedure T_Alpaca_Example.settracking(value: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.trackingrate: integer;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.settrackingrate(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.trackingrates: TTrackingRates;
begin
  result:=nil;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.utcdate: string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_Alpaca_Example.setutcdate(value: string);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.abortslew;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.axisrates(axis:integer): TAxisRates;
begin
  result:=nil;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Example.canmoveaxis(axis:integer): boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Example.destinationsideofpier(ra,dec: double):integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Example.findhome;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.moveaxis(axis:integer;rate:double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.park;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.pulseguide(direction,duration: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.setpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.slewtoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.slewtoaltazasync(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.slewtocoordinates(ra,dec: double; out ok: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  ok:=true;
end;

procedure T_Alpaca_Example.slewtocoordinatesasync(ra,dec: double; out ok: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  ok:=true;
end;

procedure T_Alpaca_Example.slewtotarget;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.slewtotargetasync;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.synctoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.synctocoordinates(ra,dec: double; out ok: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  ok:=true;
end;

procedure T_Alpaca_Example.synctotarget;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Example.unpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function   T_Alpaca_Example.GetSetupPage: string;
begin
  result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
       '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
       '<title>Example driver</title></head><body text>'+
       '<H1>Driver Setup</H1><br/>'+
       'Give here setup option or information.<br/><br/>'+
       '</body></html>';
end;

end.

