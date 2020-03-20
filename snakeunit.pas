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

{ Note that with toroid field the snake tail is buggy when it's the last
  part of the snake before leaving the border }
//{$define ToroidField}

unit SnakeUnit;

{$mode objfpc}{$H+}

interface

Uses
  Classes, fgl,
  CastleSoundEngine, CastleConfig;

Type
  {creates and stores Rabbit coordinates}
  TRabbit = class(TComponent)
  public
    x, y: Integer;
    {set new Random coordinates}
    procedure ResetRabbit;
  end;

{different kinds of Tail - referring to different sprites}
type TTailKind = (tkHead, tkStraight, tkTurn, tkTail);

type
  {represents a Tail segment}
  TTail = class
    x, y: Integer;
    {actually it's just a number of the sprite in the spritesheet,
     TailKind determines the row and Direction determines the column}
    TailKind: TTailKind;
    Direction: Integer;
  end;

{list of Tail segments}
type TSnakeTail = specialize TFPGObjectList<TTail>;

type
  {this is our basic class where the game takes place.}
  TSnake = class(TComponent)
  public
    {current coordinates of the Snake head}
    x, y: Integer;
    {current direction of the Snake}
    dx, dy: Integer;
    {"next" direction of the Snake / set by keyboard}
    next_dx, next_dy: Integer;
    {this is all the Snake body from its head (last element) to the Tail end (zero element) }
    Tail: TSnakeTail;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {resets Snake to initial values}
    procedure ResetSnake;
    {hanldes keyboard/mouse input and sets next_dx and next_dy,
     returns true if this direction is possible}
    function SetDirection(ddx, ddy: Integer): Boolean;
    {makes a step towards dx,dy}
    procedure Move;
    {detects collision with the Snake.}
    function DetectCollision(tx,ty: Integer): Boolean;
  private
    {adds a head segment of the Snake}
    procedure AddHead;
    {creates the Snake body}
    procedure FixTail;
  end;

var MaxX,MaxY: Integer;
    Snake: TSnake;
    Rabbit: TRabbit;
    GameOver: Boolean;
    Score, BestScore: Integer;
    {sounds and music to be used in game}
    EatSound, EndSound, Music: TSoundBuffer;
    PlaySound: Boolean; //sound on/off
    LicenseString: String;

{read high Score from a file.}
procedure ReadHighScore;
{this procedure starts a new game}
procedure NewGame;
{prepare music and sound}
procedure LoadMusic;
{start and stop the music}
procedure ToggleMusic;
{Write High Score to a file}
procedure WriteHighScore;

implementation
uses
  SysUtils, CastleVectors, CastleDownload;

procedure TRabbit.ResetRabbit;
begin
  repeat
    x := Random(MaxX);
    y := Random(MaxY);
  until (Snake.DetectCollision(x, y) = false) or (Snake.Tail.Count = (MaxX + 1) * (MaxY + 1));   //to avoid plaicing Rabbit inside the Snake
end;

{get sprite number for straight direction}
function GetDirection(dx, dy: Integer): Integer;
begin
  if dy > 0 then
    Result := 0
  else
  if dx > 0 then
    Result := 1
  else
  if dy < 0 then
    Result := 2
  else
    Result := 3;
end;

{get sprite number for turn-around direction}
function GetRotation(dx, dy: Integer): Integer;
begin
  if dx > 0 then
  begin
    if dy > 0 then
      Result := 0
    else
      Result := 1;
  end else
  begin
    if dy > 0 then
      Result := 3
    else
      Result := 2;
  end;
end;

constructor TSnake.Create(AOwner: TComponent);
begin
  inherited;
  Tail := TSnakeTail.Create(true);
end;

destructor TSnake.Destroy;
begin
  Tail.Clear;
  FreeAndNil(Tail);
  inherited;
end;

procedure TSnake.AddHead; inline;
var
  TmpTail: TTail;
begin
  TmpTail := TTail.Create;
  TmpTail.x := x;
  TmpTail.y := y;
  TmpTail.TailKind := tkHead;
  TmpTail.Direction := GetDirection(dx, dy);
  Tail.Add(tmpTail);
end;

procedure TSnake.ResetSnake;
var
  TmpTail: TTail;
begin
  {set it to the center of the map facing up}
  x := MaxX div 2;
  y := MaxY div 2;
  dx := 0;
  dy := 1;
  next_dx := dx;
  next_dy := dy;

  Tail.Clear;   //clear Tail

  {add Tail end one space back}
  TmpTail := TTail.Create;
  TmpTail.x := x - dx;
  TmpTail.y := y - dy;
  TmpTail.TailKind := tkTail;
  TmpTail.Direction := GetDirection(dx, dy);
  Tail.Add(tmpTail);

  {add head}
  AddHead;
end;

procedure TSnake.Move;
begin
  {$ifdef ToroidField}
  { warp snake at the borders of the field aka toroid }
  if (x + next_dx < 0) then
    x := MaxX + 1
  else
  if (x + next_dx > MaxX) then
    x := -1;
  if (y + next_dy < 0) then
    y := MaxY + 1
  else
  if (y + next_dy > MaxY) then
    y := -1;
  {$else}
  { detect collision with screen borders and deflect the Snake randomly }
  if (x + next_dx < 0) or (x + next_dx > MaxX) then
  begin
    next_dx := 0;
    if Random(2) = 0 then
      next_dy := 1
    else
      next_dy := -1;
    if (y + next_dy < 0) or (y + next_dy > MaxY) then
      next_dy := - next_dy;
  end;
  if (y + next_dy < 0) or (y + next_dy > MaxY) then
  begin
    next_dy := 0;
    if Random(2) = 0 then
      next_dx := 1
    else
      next_dx := -1;
    if (x + next_dx < 0) or (x + next_dx > MaxX) then
      next_dx := -next_dx;
  end;
  {$endif}

  {assign new dx,dy}
  dx := next_dx;
  dy := next_dy;
  {make a step}
  x += dx;
  y += dy;

  {detect if Snake collides itself}
  if DetectCollision(x, y) then
  begin
    GameOver := true;
    {play defeat sound}
    if PlaySound then
      SoundEngine.PlaySound(EndSound, false, false, 0, 1, 0, 1, TVector3.Zero)
  end;

  {detect if Snake has eaten a Rabbit}
  if (x <> Rabbit.x) or (y <> Rabbit.y) then
  begin
    //cutting the Tail automatially moves the body one segment back
    Snake.Tail.Remove(Snake.Tail[0]);
    //add head as the last element
    AddHead;
    //and replace the first element for a Tail end
    Tail[0].TailKind := tkTail;
    Tail[0].Direction := GetDirection(Snake.Tail[1].x - Snake.Tail[0].x, Snake.Tail[1].y - Snake.Tail[0].y);
    //fix second element
    FixTail;
  end else
  begin
    {Snake has eaten a Rabbit}
    Inc(Score);
    {we don't cut off it's Tail in this case which increases Snake length by 1}
    AddHead;
    FixTail;
    //put a new Rabbit
    Rabbit.ResetRabbit;
    {play eating sound}
    if PlaySound then
      SoundEngine.PlaySound(EatSound, false, false, 0, 1, 0, 1, TVector3.Zero)
  end;
end;

procedure TSnake.FixTail; inline;
var
  ddx, ddy: Integer;
begin
  if Tail.Count > 2 then
  begin
    {get type of Tail junction based on ddx and ddy difference between current
     tile (second from the head, which actually replaces previous head).}
    ddx := Tail[Tail.Count - 2].x - Tail[Tail.Count - 3].x;
    if ddx = 0 then
      ddx := Tail[Tail.Count - 2].x - Tail[Tail.Count - 1].x;
    ddy := Tail[Tail.Count - 2].y - Tail[Tail.Count - 3].y;
    if ddy = 0 then
      ddy := Tail[Tail.Count - 2].y - Tail[Tail.Count - 1].y;

    {based on ddx and ddy determine the straight or turn of the Tail
     and the corresponding sprite to use}
    if ddx = 0 then
    begin
      Tail[Tail.Count - 2].TailKind := tkStraight;
      Tail[Tail.Count - 2].Direction := 0;
    end else
    if ddy = 0 then
    begin
      Tail[Tail.Count - 2].TailKind := tkStraight;
      Tail[Tail.Count - 2].Direction := 1;
    end else
    begin
      Tail[Tail.Count - 2].TailKind := tkTurn;
      Tail[Tail.Count - 2].Direction := GetRotation(-ddx, -ddy);
    end;
  end;
end;

function TSnake.SetDirection(ddx, ddy: Integer): Boolean;
begin
  {respond to kepyress and set dx,dy, which will be assigned on next Snake move}
  Result := false;
  //Snake can actually only turn, so direction 0 deg and 180 deg should are impossible
  //one of dx,dy is zero, so we can do it by a simple check
  if (dx = -ddx) and (dy = -ddy) then
    Exit;
  if (dx = ddx) and (dy = ddy) then
    Exit;

  Result := true;
  next_dx := ddx;
  next_dy := ddy;
end;

function TSnake.DetectCollision(tx,ty: Integer): Boolean;
var
  i: Integer;
begin
  Result := false;
  {detects if point (tx,ty) collides with any segment of the Snake Tail}
  //we don't check Snake Tail end
  for i := 1 to Tail.Count - 1 do
    if (Tail[i].x = tx) and (Tail[i].y = ty) then
    begin
      Result := true;
      Break;
    end;
end;

{read a text file for license and read user config for highScore}
procedure ReadHighScore;
var
  FileStream: TStream;
  MyStrings: TStringList;
begin
 //create the stream to "download"
 {ApplicationData files should be treated as read-only in most cases}
 FileStream := Download('castle-data:/license.txt');
 //create and load TStringList from the stream
 MyStrings := TStringList.Create;
 MyStrings.LoadFromStream(FileStream);
 LicenseString := MyStrings[0];
 //clean up everything
 FreeAndNil(FileStream);
 FreeAndNil(MyStrings);

 // read user configuration and looks for string "best_Score"
 UserConfig.Load;
 BestScore := UserConfig.GetValue('best_Score', Round(2 * Sqrt((MaxX + 1)*(MaxY + 1))));
end;

procedure WriteHighScore;
begin
  UserConfig.SetValue('best_Score', BestScore);
  UserConfig.Save;
end;

{start a new game}
procedure NewGame;
begin
  {set high Score}
  if Score > BestScore then
  begin
    BestScore := Score;
    {Save high Score to a file}
    WriteHighScore;
  end;
  {reset Snake and Rabbit}
  Snake.ResetSnake;
  Rabbit.ResetRabbit;
  {game is not over yet}
  GameOver := false;
  {current Score is zero}
  Score := 0;
end;

var CurrentMusic: TSound;

procedure LoadMusic;
begin
  PlaySound := true;
  {load the sounds and music}
  EatSound := SoundEngine.LoadBuffer('castle-data:/EatSound_CC0_by_EugeneLoza.ogg');
  EndSound := SoundEngine.LoadBuffer('castle-data:/DieSound_CC0_by_EugeneLoza.ogg');
  Music := SoundEngine.LoadBuffer('castle-data:/GreenForest_CC-BY-SA_by_Metaruka.ogg');
  {initialize Sound Engine}
  SoundEngine.ParseParameters;
  //SoundEngine.MinAllocatedSources := 1;
  {and start the music looping}
  CurrentMusic := SoundEngine.PlaySound(Music, false, true, 10, 1, 0, 1, TVector3.Zero);
end;

{start and stop the music}
procedure ToggleMusic;
begin
  if PlaySound then
  begin
    PlaySound := false;
    CurrentMusic.Gain:= 0;
  end else
  begin
    PlaySound := true;
    CurrentMusic.Gain:= 1;
  end;
end;

end.

