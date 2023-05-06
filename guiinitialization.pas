unit GUIInitialization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs,
  CastleControl, CastleControls, CastleUIControls,
  CastleShapes, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, X3DNodes, X3DFields,
  CastleImages, CastleGLImages, CastleApplicationProperties,
  CastleLog, CastleTimeUtils, CastleKeysMouse, CastleSteam,
  MainGameUnit;

type
  { TCastleForm }

  TCastleForm = class(TForm)
    Window: TCastleControl;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WindowClose(Sender: TObject);
    procedure WindowOpen(Sender: TObject);
  end;

var
  CastleForm: TCastleForm;
const
  { Here we are using AppID of SteamWorks game example - SpaceWar
    see https://partner.steamgames.com/doc/sdk/api/example
    Note that using this example will add this game to your Steam library }
  AppId = UInt32(2275430);

implementation
{$R *.lfm}

procedure TCastleForm.FormCreate(Sender: TObject);
begin
  InitializeLog;
  InitSteam(AppId);
  WriteLnLog('FormCreate : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  AppTime := CastleGetTickCount64;
  PrepDone := False;
  Caption := 'IsoTest GUI';
end;

procedure TCastleForm.FormDestroy(Sender: TObject);
begin
  WriteLnLog('FormDestroy : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleForm.WindowOpen(Sender: TObject);
begin
  CastleApp := TCastleApp.Create(Window);
  Window.Container.View := CastleApp;
  Window.Container.UIScaling := usNone;
end;

procedure TCastleForm.WindowClose(Sender: TObject);
begin
  WriteLnLog('WindowClose : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

end.

