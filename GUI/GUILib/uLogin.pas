unit uLogin;

interface

uses
  Controls, Variants, Forms, ComCtrls, Windows, Classes, Graphics,
  ButtonGroup, ASCII,
  uGlobalDefs, uConnection,
  uGT, uMessageHub, udbTree, uMirrorDB, uFidsTags, SysUtils,
  uCommon, Dialogs, Math, uTTRules, ColorButton;

type
  cLogin = class
  public
    // Variables
    Username: string;

    procedure Connect(event: apConnectionEventReader);
    function GetUsername: String;
    function Check(Username: string; password: string): Boolean;
    function GetJobName(): String;
  end;

implementation

const
  BASE_TAG_USERS = 'Users|';
  PASSWORD_TAG = '|PassWord';

procedure cLogin.Connect(event: apConnectionEventReader);
begin
  uConnection.Connect(event);
end;

function cLogin.GetJobName(): String;
begin
  // get the jobname (airport name)
  Result := DB().GetNode(fidsJobNamePath).Content;
end;

function cLogin.Check(Username: string; password: string): Boolean;
begin

  // Check for the user trying to login with
  if Assigned(DB().GetNode(BASE_TAG_USERS + Username)) then
  begin
    if (DB().GetNode(BASE_TAG_USERS + Username + PASSWORD_TAG)
      .Content = password) then
    begin
      Result := True;
      Username := Username;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;

end;

function cLogin.GetUsername(): String;
begin

  if DEBUG_MODE = 1 then
  begin
    Result := 'FIDS2';
  end
  else
  begin
    Result := Username;
  end;

end;

end.
