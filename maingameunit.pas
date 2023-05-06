unit MainGameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  {$ifndef cgeapp}
  Forms, Controls, Graphics, Dialogs, CastleControl,
  {$else}
  CastleWindow,
  {$endif}
  CastleControls, CastleColors, CastleUIControls,
  CastleShapes, CastleVectors,
  CastleSceneCore, CastleScene, CastleTransform,
  CastleViewport, CastleCameras, CastleProjection,
  X3DNodes, X3DFields,
  CastleImages, CastleGLImages,
  CastleTextureImages,
  CastleApplicationProperties, CastleLog, CastleTimeUtils, CastleKeysMouse;

type
  { TCastleViewHelper }
  TCastleViewHelper = class helper for TCastleView
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

  TCastleApp = class(TCastleView)
    procedure BeforeRender; override; // TCastleUserInterface
    procedure Render; override; // TCastleUserInterface
    procedure Resize; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override; // TCastleView
    function  Motion(const Event: TInputMotion): Boolean; override; // TCastleView
    function  Press(const Event: TInputPressRelease): Boolean; override; // TCastleView
    function  Release(const Event: TInputPressRelease): Boolean; override; // TCastleView
  private
    Viewport: TCastleViewport;
    Camera: TCastleCamera;
    VPBackImage: TCastleImageControl;
    Scene: TCastleScene;
    LabelBBMin: TCastleLabel;
    LabelBBMax: TCastleLabel;
    LabelCam: TCastleLabel;
    LabelFPS: TCastleLabel;
    LabelRender: TCastleLabel;
    BigRedButton: TCastleButton;
    keyShift: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure BlowUpWorld(Sender: TObject);
    destructor Destroy; override;
    procedure BootStrap;
    procedure Start; override; // TCastleView
    procedure Stop; override; // TCastleView
    procedure LoadViewport;
    function CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
    procedure LoadScene(filename: String);
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
  end;

function MakeTransparentLayerGrid(const ASpriteWidth: Cardinal; const ASpriteHeight: Cardinal; const AViewWidth: Cardinal; const AViewHeight: Cardinal; const GridSize: Cardinal = 8): TCastleImage;

var
  AppTime: Int64;
  PrepDone: Boolean;
  RenderReady: Boolean;
  CastleApp: TCastleApp;
  DbgSingle: Single;

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
var
  BBMax: Single;
begin
  if not(RootNode = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Center := Vector3(Min(LocalBoundingBox.Data[0].X, LocalBoundingBox.Data[1].X) + (LocalBoundingBox.SizeX / 2),
                              Min(LocalBoundingBox.Data[0].Y, LocalBoundingBox.Data[1].Y) + (LocalBoundingBox.SizeY / 2),
                              Min(LocalBoundingBox.Data[0].Z, LocalBoundingBox.Data[1].Z) + (LocalBoundingBox.SizeZ / 2));
            BBMax := LocalBoundingBox.MaxSize;
            Scale := Vector3(1 / BBMax,
                             1 / BBMax,
                             1 / BBMax);
            DbgSingle := Scale.X;
            Translation := -Center;
          end;
      end;
    end;
end;

{ TCastleApp }

function TCastleApp.CreateDirectionalLight(LightPos: TVector3): TCastleDirectionalLight;
var
  Light: TCastleDirectionalLight;
begin
  Light := TCastleDirectionalLight.Create(Self);

  Light.Direction := LightPos;
  Light.Color := Vector3(1, 1, 1);
  Light.Intensity := 1;

  Result := Light;
end;

procedure TCastleApp.BootStrap;
var
  ProcTimer: Int64;
begin
  DbgSingle := 0;
  ProcTimer := CastleGetTickCount64;
  LoadScene('castle-data:/up.glb');
//  LoadScene('castle-data:/spider.gltf');
//  LoadScene('castle-data:/boy_character.glb');
//  LoadScene('C:\\work\\CrazyRabbit\\cr01.gltf');
  ProcTimer := CastleGetTickCount64 - ProcTimer;
  WriteLnLog('ProcTimer (LoadScene) = ' + FormatFloat('####0.000', ProcTimer / 1000) + ' seconds');
  Resize;
end;

procedure TCastleApp.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
var
  Spherical: TVector3;
begin
  Spherical := -ADirection.Normalize;
  Spherical := Spherical * ARadius;
  Camera.Up := Vector3(0, 1, 0);
  Camera.Direction := ADirection;
  Camera.Translation  := Spherical;
end;

procedure TCastleApp.LoadViewport;
var
  Light: TCastleDirectionalLight;
begin
  VPBackImage := TCastleImageControl.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  VPBackImage.OwnsImage := True;
  InsertFront(VPBackImage);

  Viewport := TCastleViewport.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  Viewport.FullSize := False;
  Viewport.Setup2D;
  Viewport.Transparent := True;
  Viewport.Width := Container.Width;
  Viewport.Height := Container.Height;

  Camera := TCastleCamera.Create({$ifndef cgeapp}CastleForm.{$endif}Window);
  Camera.Orthographic.Width := 1;
  Camera.Orthographic.Height := 1;
  Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  Camera.ProjectionType := ptOrthographic;

  Light := CreateDirectionalLight(Viewport.Camera.Translation);
  Camera.Add(Light);

  Viewport.Items.Add(Camera);
  Viewport.Camera := Camera;

  InsertFront(Viewport);
//  ViewFromRadius(2, Vector3(0, -1, 0));

  CreateButton(BigRedButton, 'Blow up world', 0, @BlowUpWorld);

  CreateLabel(LabelBBMin, 4);
  CreateLabel(LabelBBMax, 3);
  CreateLabel(LabelCam, 2);
  CreateLabel(LabelFPS, 1);
  CreateLabel(LabelRender, 0);

end;

procedure TCastleApp.LoadScene(filename: String);
begin
  try
    Scene := TCastleScene.Create(Application);
    Scene.PreciseCollisions := False;
    Scene.Load(filename);
    Scene.Normalize;
    Scene.CastGlobalLights := True;

    Viewport.PrepareResources(Scene);
    Viewport.Items.Add(Scene);
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
  if(Assigned(Scene)) then
    begin
      if keyShift then
        begin
          LabelBBMin.Caption := 'Min = ' + ' (' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[0].X) +
                                ', ' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[0].Y) +
                                ', ' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[0].Z) +
                                ')';
          LabelBBMax.Caption := 'Max = ' + ' (' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[1].X) +
                                ',' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[1].Y) +
                                ', ' +
                                FormatFloat('##0.0000', Scene.LocalBoundingBox.Data[1].Z) +
                                ')';
        end
    else
      begin
        LabelBBMin.Caption := 'Min = ' + ' (' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[0].X) +
                              ', ' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[0].Y) +
                              ', ' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[0].Z) +
                              ')';
        LabelBBMax.Caption := 'Max = ' + ' (' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[1].X) +
                              ',' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[1].Y) +
                              ', ' +
                              FormatFloat('##0.0000', Scene.BoundingBox.Data[1].Z) +
                              ')';
      end;
      LabelCam.Caption := 'Cam = ' + FormatFloat('####0.00', Camera.Orthographic.Width) +
                          ' x ' +
                          FormatFloat('####0.00', Camera.Orthographic.Height) +
                          ' (' +
                          FormatFloat('####0.00', Camera.Orthographic.Origin.X) +
                          ' x ' +
                          FormatFloat('####0.00', Camera.Orthographic.Origin.Y) +
                          ')'
                          + ', BBMax = ' + FormatFloat('#0.000000', DbgSingle)
                          ;

    end;
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
  Viewport.Width := Container.Width;
  Viewport.Height := Container.Height;
  Camera.Orthographic.Width := 1;
  Camera.Orthographic.Height := 1;
  Camera.Orthographic.Origin := Vector2(0.5, 0.5);
  ViewFromRadius(2, Vector3(0, 0, -1));

end;

procedure TCastleApp.BlowUpWorld(Sender: TObject);
begin
  {
  VPBackImage.Image := MakeTransparentLayerGrid(64, 64, Trunc(Viewport.Width), Trunc(Viewport.Height), 8);
  VPBackImage.Translation := Viewport.Translation;
  VPBackImage.Width := Viewport.Width;
  VPBackImage.Height := Viewport.Height;
  }
  keyShift := not keyShift;
  if(Assigned(Scene)) then Scene.Normalize;

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

procedure TCastleViewHelper.CreateButton(var objButton: TCastleButton; const ButtonText: String; const Line: Integer; const ButtonCode: TNotifyEvent = nil; const BottomUp: Boolean = True);
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

procedure TCastleViewHelper.CreateLabel(var objLabel: TCastleLabel; const Line: Integer; const BottomUp: Boolean = True; RightAlign: Boolean = False);
begin
  objLabel := TCastleLabel.Create(Self);
  objLabel.Padding := 5;
  objLabel.Color := White;
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

