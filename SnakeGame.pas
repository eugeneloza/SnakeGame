{ MIT License

  Copyright (c) 2016-2020 Yevhen Loza

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

program SnakeGame;

{to hide DOS window in Windows use $AppType GUI compiler directive}
//{$IFDEF windows}{$AppType GUI}{$ENDIF}

{I highly recommend preforming range-checking during development}
{$R+}{$Q+}

uses
   SysUtils, Math, CastleWindow, CastleKeysMouse, CastleGLImages, CastleGLUtils,
   CastleVectors, CastleControls, CastleLog,
   SnakeUnit;

{ scale of the source sprites and their scaled version size at the screen }
const
  SourceScale = 16;
  DestinationScale = SourceScale * 2;

var { basic CastleWindow }
    Window: TCastleWindowBase;
    { All 16 sprites in one image }
    SnakeImage, SnakeFlipImage: TDrawableImage;
    {create some cheap animation effect}
    FlipImage: boolean;

{ this procedure is called each render }
Procedure WindowRender(Container: TUIContainer);
var
  i, ix, iy: Integer;
  CurrentSnakeImage: TDrawableImage;
begin
  //draw grassland
  for ix := 0 to MaxX do
    for iy := 0 to MaxY do
      SnakeFlipImage.Draw(ix * DestinationScale, iy * DestinationScale,
        DestinationScale, DestinationScale,
        3 * SourceScale, 0, SourceScale, SourceScale);
  {we use Draw(screenx, screeny, screenwidth, screenheight, sourcex, sourcey, soucrewidth, sourceheight)
   version of TDrawableImage.Draw procedure which allows working with spritesheets}

  //show game Score
  If not GameOver then
  begin
    if Score <= BestScore then
      UIFont.Print(0, 0, Vector4(0.4, 0.3, 0.1, 1), 'Score: ' + IntToStr(Score) + ' best: ' + IntToStr(BestScore))
    else
      UIFont.Print(0, 0, Vector4(1, 0.8, 0, 1), 'Score: ' + IntToStr(Score) + '(best!)');
  end;
  {We use UIFont defined in CastleControls unit as a "basic" font}

  {Show music CC-BY-SA credit :)}
  UIFont.Print(0, Window.Height - 18, Vector4(0, 0.6, 0.2, 1), LicenseString);

  //draw Rabbit
  SnakeImage.Draw(Rabbit.x * DestinationScale, Rabbit.y * DestinationScale,
    DestinationScale, DestinationScale,
    2 * SourceScale, 0, SourceScale, SourceScale);

  //draw Snake
  {flipping image once per 300ms gives some cheap animation to the Snake.}
  if FlipImage then
    CurrentSnakeImage := SnakeImage
  else
    CurrentSnakeImage := SnakeFlipImage;
  for i := 0 to Snake.Tail.Count - 1 do
    with Snake.Tail[i] do
      case TailKind of
        tkHead    : CurrentSnakeImage.Draw(x * DestinationScale, y * DestinationScale, DestinationScale, DestinationScale,
                       Direction * SourceScale, 3 * SourceScale, SourceScale, SourceScale);
        tkTail    : CurrentSnakeImage.Draw(x * DestinationScale, y * DestinationScale, DestinationScale, DestinationScale,
                       Direction * SourceScale, 2 * SourceScale, SourceScale, SourceScale);
        tkTurn    : CurrentSnakeImage.Draw(x * DestinationScale, y * DestinationScale, DestinationScale, DestinationScale,
                       Direction * SourceScale, 1 * SourceScale, SourceScale, SourceScale);
        tkStraight: CurrentSnakeImage.Draw(x * DestinationScale, y * DestinationScale, DestinationScale, DestinationScale,
                       Direction * SourceScale, 0, SourceScale, SourceScale);
      end;

  //give a endgame message
  if GameOver then
  begin
    if Score <= BestScore then
    begin
      UIFont.Print(Window.Width div 2 - 80, Window.Height div 2 + 15, Vector4(0.2, 0.1, 0, 1), 'GAME OVER!');
      UIFont.Print(Window.Width div 2 - 80, Window.Height div 2 - 15, Vector4(0.2, 0.1, 0, 1), 'Your Score was: ' + IntToStr(Score));
    end else
    begin
      UIFont.Print(Window.Width div 2 - 80, Window.Height div 2 + 15, Vector4(1, 1, 0, 1), 'GAME OVER!');
      UIFont.Print(Window.Width div 2 - 80, Window.Height div 2 - 15, Vector4(1, 1, 0, 1), 'BEST Score: ' + IntToStr(Score));
    end
  end;
end;

{this procedure is called very 300 miliseconds}
procedure DoTimer;
begin
 if not GameOver then
 begin
   Snake.Move;
   FlipImage := not FlipImage;
 end;
end;

{this procedure handles mouse and key presses}
procedure KeyPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  dx, dy: Integer;
begin
  if not GameOver then
  begin
    if Event.EventType = itKey then
      {this event is a keyboard event. Detect which button has been pressed}
      case Event.Key of
        keyUp: Snake.SetDirection(0, 1);
        keyDown: Snake.SetDirection(0, -1);
        keyLeft: Snake.SetDirection(-1, 0);
        keyRight: Snake.SetDirection(1, 0);
        keyM: ToggleMusic;
      end
    else
    if Event.EventType = itMouseButton then
    begin
      {this event is a mouse button or touch event.
       get click/touch coordinates.
       event.Position[0] is x and event.Position[1] is y}
      dx := Round(Event.Position[0] - (Snake.x + 0.5) * DestinationScale);
      dy := Round(Event.Position[1] - (Snake.y + 0.5) * DestinationScale);
      {and set the direction accordingly}
      if Abs(dx) > Abs(dy) then
      begin
        if not Snake.SetDirection(Sign(dx), 0) then
          Snake.SetDirection(0, Sign(dy));
      end else
      begin
        if not Snake.SetDirection(0, Sign(dy)) then
          Snake.SetDirection(Sign(dx), 0);
      end;
    end;
  end else
    NewGame;
end;

begin
  {initialize log. Without parameters it goes directly into console on Linux
   or DOS window in Windows, see documentation for more details.}
  InitializeLog;
  {write something into Log}
  WriteLnLog('Hello','World!');

  {create window}
  Window := TCastleWindowBase.Create(Application);
  {initialize random sequence}
  Randomize;

  {map size is 16x16}
  MaxX := 15;
  MaxY := 15;

  {set the appropriate window size}
  Window.Width := (MaxX + 1) * DestinationScale;
  Window.Height := (MaxY + 1) * DestinationScale;

  {load spritesheet / no nice scaling because it's pixelart :)}
  {ApplicationData points to content "data" in a cross-platform way, so that it
   is correct in Windows, Linux, Android and any other OS}
  SnakeImage := TDrawableImage.Create('castle-data:/Snake.png', false);
  SnakeFlipImage := TDrawableImage.Create('castle-data:/SnakeFlip.png', false);
  FlipImage := true;

  {create Snake}
  Snake := TSnake.Create(Application);
  {create Rabbit}
  Rabbit := TRabbit.Create(Application);

  {set up window events callbacks}
  Window.OnRender := @WindowRender;
  Window.OnPress := @KeyPress;
  Window.ResizeAllowed := raNotAllowed;

  {set up application timer}
  {this event will fire once every TimerMilisec (i.e. 300 msec)
   It's not very accurate but hardly accuracy is significant}
  Application.TimerMilisec := 300;
  Application.OnTimer := @DoTimer;

  {Read High Score from a file}
  ReadHighScore;

  {Load music and sound}
  LoadMusic;

  {start a new game}
  Score := 0;
  NewGame;

  {and finally open the window and start the game}
  Window.OpenAndRun;

  WriteHighScore;

  {don't forget to free everything that is not freed automatically}
  FreeAndNil(SnakeImage);
  FreeAndNil(SnakeFlipImage);
end.

