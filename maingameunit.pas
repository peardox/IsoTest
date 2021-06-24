unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, CastleUIState,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleTriangles, CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DNodes, X3DFields, X3DTIme,
  CastleImages, CastleGLImages,
  CastleTextureImages, CastleCompositeImage,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TUIStateHelper }
  TUIStateHelper = class helper for TUIState
  public
    procedure CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil; const BottomUp: Boolean = True);
    procedure CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
  end;

  { TCastleSceneHelper }
  TCastleSceneHelper = class helper for TCastleScene
  public
     procedure Normalize;
  end;

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TUIState
    function  Motion(const Event: TInputMotion): Boolean; override; // TUIState
    function  Press(const Event: TInputPressRelease): Boolean; override; // TUIState
    function  Release(const Event: TInputPressRelease): Boolean; override; // TUIState
  private
    Viewport: TCastleViewport;
    VPBackImage: TCastleImageControl;
    Scene: TCastleScene;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    BigRedButton: TCastleButton;
    AnotherRedButton: TCastleButton;
    DieOnResize: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BlowUpResize(Sender: TObject);
    procedure BlowUpWorld(Sender: TObject);
    destructor Destroy; override;
    procedure BootStrap;
    procedure Start; override; // TUIState
    procedure Stop; override; // TUIState
    procedure LoadViewport;
    procedure LoadScene(filename: String);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
  end;

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;

var
  AppTime: Int64;
  PrepDone: Boolean;
  RenderReady: Boolean;
  CastleApp: TCastleApp;

implementation
{$ifdef cgeapp}
uses AppInitialization;
{$else}
uses GUIInitialization;
{$endif}
constructor TCastleApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCastleApp.Destroy;
begin
  inherited;
end;

{ TCastleSceneHelper }

{ Normalize - Center the model in a 1x1x1 cube }
procedure TCastleSceneHelper.Normalize;
begin
  if not(RootNode = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize);
            Translation := -Center;
          end;
      end;
    end;
end;

{ TCastleApp }

procedure TCastleApp.BootStrap;
var
  ProcTimer: Int64;
begin
  ProcTimer := CastleGetTickCount64;
  LoadScene('castle-data:/up.glb');
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadScene) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
end;

procedure TCastleApp.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
var
  Spherical: TVector3;
begin
  Spherical := -ADirection.Normalize;
  Spherical := Spherical * ARadius;
  Viewport.Camera.Up := Vector3(0, 1, 0);
  Viewport.Camera.Direction := ADirection;
  Viewport.Camera.Position  := Spherical;
end;

procedure TCastleApp.LoadViewport;
begin
  VPBackImage := TCastleImageControl.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  VPBackImage.OwnsImage := True;
  InsertFront(VPBackImage);

  Viewport := TCastleViewport.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  Viewport.FullSize := False;
  Viewport.AutoCamera := False;
  Viewport.Setup2D;
  Viewport.Transparent := True;
  Viewport.NavigationType := ntNone;
  Viewport.AssignDefaultCamera;
  Viewport.Width := StateContainer.Width;
  Viewport.Height := StateContainer.Height;
  Viewport.Camera.Orthographic.Width := 2;
  Viewport.Camera.Orthographic.Height := 2;
  Viewport.Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Viewport.Camera.Orthographic.Scale := 1;
  Viewport.Camera.ProjectionType := ptOrthographic;

  InsertFront(Viewport);

  CreateButton(BigRedButton, 'Blow up world', 0, @BlowUpWorld);
  CreateButton(AnotherRedButton, 'Blow up on ReSize', 0, @BlowUpResize, False);

  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);

  ViewFromRadius(2, Vector3(-1, -1, -1));
end;

procedure TCastleApp.LoadScene(filename: String);
begin
  try
    Scene := TCastleScene.Create(Application);
    Scene.Spatial := [ssDynamicCollisions, ssRendering];
    Scene.Load(filename);
    Scene.Normalize;
    Scene.PrepareResources([prSpatial, prRenderSelf, prRenderClones, prScreenEffects],
        True,
        Viewport.PrepareParams);
    Viewport.Items.Add(Scene);
    Viewport.Items.MainScene := Scene;
  except
    on E : Exception do
      begin
        WriteLnLog('Oops #1' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCastleApp.Start;
begin
  inherited;
  LogTextureCache := True;
  WriteLnLog('Start : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
  Scene := nil;
  LoadViewport;
  PrepDone := True;
end;

procedure TCastleApp.Stop;
begin
  inherited;
  WriteLnLog('Stop : ' + FormatFloat('####0.000', (CastleGetTickCount64 - AppTime) / 1000) + ' : ');
end;

procedure TCastleApp.BeforeRender;
begin
  inherited;
  LabelFPS.Caption := 'FPS = ' + FormatFloat('####0.00', Container.Fps.RealFps);
  LabelRender.Caption := 'Render = ' + FormatFloat('####0.00', Container.Fps.OnlyRenderFps);

end;

procedure TCastleApp.Render;
begin
  inherited;

  if PrepDone and GLInitialized and RenderReady then
    begin
      PrepDone := False;
      BootStrap;
    end;
  RenderReady := True;
end;

procedure TCastleApp.Resize;
begin
  inherited;
  if DieOnResize then
    BlowUpWorld(nil);
end;

procedure TCastleApp.BlowUpResize(Sender: TObject);
begin
  DieOnResize := True;
end;

procedure TCastleApp.BlowUpWorld(Sender: TObject);
begin
  VPBackImage.Image := MakeTransparentLayerGrid(64, 64, Trunc(Viewport.Width), Trunc(Viewport.Height), 8);
  VPBackImage.Left := Viewport.Left;
  VPBackImage.Bottom := Viewport.Bottom;
  VPBackImage.Width := Viewport.Width;
  VPBackImage.Height := Viewport.Height;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TCastleApp.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

function TCastleApp.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
end;

procedure TUIStateHelper.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil; const BottomUp: Boolean = True);
begin
  objButton := TCastleButton.Create(Self);
  objButton.Caption := ButtonText;
  objButton.Anchor(hpMiddle, 10);
  if BottomUp then
    objButton.Anchor(vpBottom, 10 + (Line * 35))
  else
    objButton.Anchor(vpTop, -(10 + (Line * 35)));
  objButton.onClick := ButtonCode;
  InsertFront(objButton);
end;

procedure TUIStateHelper.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
begin
  objLabel := TCastleLabel.Create(Self);
  objLabel.Padding := 5;
  objLabel.Color := White;
  objLabel.Frame := True;
  objLabel.FrameColor := Black;
  objLabel.Anchor(hpLeft, 10);
  if RightAlign then
    objLabel.Anchor(hpRight, -10)
  else
    objLabel.Anchor(hpLeft, 10);
  if BottomUp then
    objLabel.Anchor(vpBottom, 10 + (Line * 35))
  else
    objLabel.Anchor(vpTop, -(10 + (Line * 35)));
  InsertFront(objLabel);
end;

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;
var
  img: TCastleImage;
  XPos: Cardinal;
  YPos: Cardinal;
  XGrid: Single;
  YGrid: Single;
  Skip: Integer;
  LightGrey: TCastleColor;
begin
  img := TRGBImage.Create(AViewWidth, AViewHeight);
  LightGrey := Red; // HexToColor('B2B2B2');
  img.Clear(HexToColor('838383'));
  XGrid := AViewWidth / ASpriteWidth;
  YGrid := AViewHeight / ASpriteHeight;

//  WriteLnLog('BlobVars : ' + IntToStr(GridSize) + ' - ' + FloatToStr(XGrid) + ' - ' + FloatToStr(YGrid) + LineEnding);
  for YPos := 0 to (ASpriteHeight div GridSize) - 1 do
    begin
      Skip := YPos Mod 2;
      for XPos := 0 to (ASpriteWidth div GridSize) - 1 do
        begin
          if (((Skip + XPos) Mod 2) = 0) then
            begin
              img.FillRectangle(Trunc(XPos * GridSize * XGrid), Trunc(YPos * GridSize * YGrid),
                Trunc(((XPos + 1) * GridSize * XGrid)) - 1, Trunc(((YPos + 1) * GridSize * YGrid)) - 1,
                LightGrey);
{
              WriteLnLog('Red Blob : ' + IntToStr(Skip + XPos) + ' = ' +
                IntToStr(Trunc(XPos * GridSize * XGrid)) + ', ' +
                IntToStr(Trunc(YPos * GridSize * YGrid)) + ' - ' +
                IntToStr(Trunc(((XPos + 1) * GridSize * XGrid)) - 1) + ', ' +
                IntToStr(Trunc(((YPos + 1) * GridSize * YGrid)) - 1));
}
            end
          else
          begin
            img.FillRectangle(Trunc(XPos * GridSize * XGrid), Trunc(YPos * GridSize * YGrid),
              Trunc(((XPos + 1) * GridSize * XGrid)) - 1, Trunc(((YPos + 1) * GridSize * YGrid)) - 1,
              Blue);
{
            WriteLnLog('Blue Blob : ' + IntToStr(Skip + XPos) + ' = ' +
              IntToStr(Trunc(XPos * GridSize * XGrid)) + ', ' +
              IntToStr(Trunc(YPos * GridSize * YGrid)) + ' - ' +
              IntToStr(Trunc(((XPos + 1) * GridSize * XGrid)) - 1) + ', ' +
              IntToStr(Trunc(((YPos + 1) * GridSize * YGrid)) - 1));
}
           end;
        end;
    end;
//  SaveImage(img, 'testgrid.png');

  Result := img;
end;



end.

