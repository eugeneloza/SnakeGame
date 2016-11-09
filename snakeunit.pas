unit SnakeUnit;

{$mode objfpc}{$H+}

interface

Uses Classes, fgl,
  CastleSoundEngine,
  CastleConfig;

Type
  {creates and stores rabbit coordinates}
  Trabbit = class(TComponent)
  public
    x,y: integer;
    {set new random coordinates}
    procedure ResetRabbit;
  end;

{different kinds of tail - referring to different sprites}
type TTailKind = (tkHead,tkStraight,tkTurn,tkTail);

type
  {represents a tail segment}
  TTail = class
    x,y: integer;
    {actually it's just a number of the sprite in the spritesheet,
     TailKind determines the row and Direction determines the column}
    TailKind: TTailKind;
    Direction: integer;
  end;

{list of tail segments}
type TSnakeTail = specialize TFPGObjectList<TTail>;

type
  {this is our basic class where the game takes place.}
  TSnake = class(TComponent)
  public
    {current coordinates of the snake head}
    x,y: integer;
    {current direction of the snake}
    dx,dy: integer;
    {"next" direction of the snake / set by keyboard}
    next_dx,next_dy: integer;
    {this is all the snake body from its head (last element) to the tail end (zero element) }
    Tail: TSnakeTail;
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    {resets snake to initial values}
    procedure ResetSnake;
    {hanldes keyboard/mouse input and sets next_dx and next_dy,
     returns true if this direction is possible}
    function setDirection(ddx,ddy:integer): boolean;
    {makes a step towards dx,dy}
    procedure move;
    {detects collision with the snake.}
    function detectCollision(tx,ty: integer): boolean;
  private
    {adds a head segment of the snake}
    procedure addHead;
    {creates the snake body}
    procedure FixTail;
  end;

var maxx,maxy: integer;
    Snake: TSnake;
    Rabbit: TRabbit;
    GameOver: boolean;
    score, bestscore: integer;
    {sounds and music to be used in game}
    EatSound,EndSound,music: TSoundBuffer;
    PlaySound: boolean; //sound on/off
    license_string: string;

{read high score from a file.}
procedure ReadHighScore;
{this procedure starts a new game}
procedure NewGame;
{prepare music and sound}
procedure LoadMusic;
{start and stop the music}
procedure ToggleMusic;
{Write High score to a file}
procedure WriteHighScore;

implementation

uses SysUtils, castleVectors, castleFilesUtils, CastleDownload;

procedure TRabbit.ResetRabbit;
begin
  repeat
    x := random(maxx);
    y := random(maxy);
  until (snake.detectCollision(x,y)=false) or (snake.tail.count=(maxx+1)*(maxy+1));   //to avoid plaicing rabbit inside the snake
end;

{get sprite number for straight direction}
function getDirection(dx,dy: integer): integer;
begin
  if dy>0 then result := 0 else
  if dx>0 then result := 1 else
  if dy<0 then result := 2 else
               result := 3;
end;

{get sprite number for turn-around direction}
function getRotation(dx,dy: integer): integer;
begin
  if dx>0 then begin
    if dy>0 then result := 0
            else result := 1
  end else begin
    if dy>0 then result := 3
            else result := 2
  end;
end;

constructor TSnake.Create(AOwner:TComponent);
begin
  inherited create(AOwner);
  Tail := TSnakeTail.Create(true);
end;

destructor TSnake.destroy;
begin
  Tail.clear;
  freeandnil(Tail);
  inherited;
end;

procedure TSnake.AddHead; inline;
var TmpTail: TTail;
begin
  tmpTail := TTail.create;
  tmpTail.x := x;
  tmpTail.y := y;
  tmpTail.TailKind := tkHead;
  tmpTail.Direction := getDirection(dx,dy);
  Tail.add(tmpTail);
end;

procedure TSnake.ResetSnake;
var TmpTail: TTail;
begin
  {set it to the center of the map facing up}
  x := maxx div 2;
  y := maxy div 2;
  dx := 0;
  dy := 1;
  next_dx := dx;
  next_dy := dy;

  Tail.Clear;   //clear tail

  {add tail end one space back}
  tmpTail := TTail.create;
  tmpTail.x := x-dx;
  tmpTail.y := y-dy;
  tmpTail.TailKind := tkTail;
  tmpTail.Direction := getDirection(dx,dy);
  Tail.add(tmpTail);

  {add head}
  AddHead;
end;

procedure TSnake.move;
begin
  {detect collision with screen borders and deflect the snake randomly}
  if (x+next_dx<0) or (x+next_dx>maxx) then begin
    next_dx := 0;
    if random(2)=0 then next_dy := 1 else next_dy := -1;
    if (y+next_dy<0) or (y+next_dy>maxy) then next_dy := - next_dy
  end;
  if (y+next_dy<0) or (y+next_dy>maxy) then begin
    next_dy := 0;
    if random(2)=0 then next_dx := 1 else next_dx := -1;
    if (x+next_dx<0) or (x+next_dx>maxx) then  next_dx := -next_dx;
  end;

  {assign new dx,dy}
  dx := next_dx;
  dy := next_dy;
  {make a step}
  x += dx;
  y += dy;

  {detect if snake collides itself}
  if detectCollision(x,y) then begin
    GameOver := true;
    {play defeat sound}
    if PlaySound then
      SoundEngine.PlaySound(EndSound, false, false, 0, 1, 0, 1, ZeroVector3Single)
  end;

  {detect if snake has eaten a rabbit}
  if (x<>rabbit.x) or (y<>rabbit.y) then begin
    //cutting the tail automatially moves the body one segment back
    Snake.Tail.Remove(Snake.Tail[0]);
    //add head as the last element
    AddHead;
    //and replace the first element for a tail end
    Tail[0].TailKind := tkTail;
    Tail[0].Direction := GetDirection(Snake.Tail[1].x-Snake.Tail[0].x,Snake.Tail[1].y-Snake.Tail[0].y);
    //fix second element
    fixTail;
  end else begin
    {snake has eaten a rabbit}
    inc(score);
    {we don't cut off it's tail in this case which increases snake length by 1}
    AddHead;
    fixTail;
    //put a new rabbit
    Rabbit.ResetRabbit;
    {play eating sound}
    if PlaySound then
      SoundEngine.PlaySound(EatSound, false, false, 0, 1, 0, 1, ZeroVector3Single)
  end;
end;

procedure TSnake.FixTail; inline;
var ddx,ddy: integer;
begin
  if Tail.count>2 then begin
    {get type of tail junction based on ddx and ddy difference between current
     tile (second from the head, which actually replaces previous head).}
    ddx := tail[Tail.count-2].x-tail[Tail.count-3].x;
    if ddx=0 then ddx := tail[Tail.count-2].x-tail[Tail.count-1].x;
    ddy := tail[Tail.count-2].y-tail[Tail.count-3].y;
     if ddy=0 then ddy := tail[Tail.count-2].y-tail[Tail.count-1].y;

    {based on ddx and ddy determine the straight or turn of the tail
     and the corresponding sprite to use}
    if ddx=0 then begin
      tail[Tail.count-2].TailKind := tkStraight;
      tail[Tail.count-2].Direction := 0;
    end else if ddy=0 then begin
      tail[Tail.count-2].TailKind := tkStraight;
      tail[Tail.count-2].Direction := 1;
    end else begin
      tail[Tail.count-2].TailKind := tkTurn;
      tail[Tail.count-2].Direction := getRotation(-ddx,-ddy);
    end;
  end;
end;

function TSnake.setDirection(ddx,ddy: integer): boolean;
begin
  {respond to kepyress and set dx,dy, which will be assigned on next snake move}
  result := false;
  //snake can actually only turn, so direction 0 deg and 180 deg should are impossible
  //one of dx,dy is zero, so we can do it by a simple check
  if (dx = -ddx) and (dy = -ddy) then exit;
  if (dx = ddx) and (dy = ddy) then exit;

  result := true;
  next_dx := ddx;
  next_dy := ddy;
end;

function TSnake.detectCollision(tx,ty: integer): boolean;
var i: integer;
begin
  result := false;
  {detects if point (tx,ty) collides with any segment of the snake tail}
  //we don't check snake tail end
  for i := 1 to tail.count-1 do
    if (tail[i].x = tx) and (tail[i].y = ty) then begin
      result := true;
      break;
    end;
end;

{read a text file for license and read user config for highscore}
procedure ReadHighScore;
var FileStream: TStream;
    MyStrings: TStringList;
begin
 //create the stream to "download"
 {ApplicationData files should be treated as read-only in most cases}
 FileStream := Download(ApplicationData('license.txt'));
 //create and load TStringList from the stream
 MyStrings := TStringList.create;
 MyStrings.LoadFromStream(FileStream);
 license_string := MyStrings[0];
 //clean up everything
 freeAndNil(FileStream);
 freeAndNil(MyStrings);

 // read user configuration and looks for string "best_score"
 UserConfig.Load;
 BestScore := UserConfig.GetValue('best_score', round(2*sqrt((maxx+1)*(maxy+1))));
end;

procedure WriteHighScore;
begin
  UserConfig.SetValue('best_score', BestScore);
  UserConfig.Save;
end;

{start a new game}
procedure NewGame;
begin
  {set high score}
  if score > bestScore then begin
    bestScore := score;
    {Save high score to a file}
    WriteHighScore;
  end;
  {reset snake and rabbit}
  Snake.ResetSnake;
  Rabbit.ResetRabbit;
  {game is not over yet}
  GameOver := false;
  {current score is zero}
  score := 0;
end;

var CurrentMusic: TSound;

procedure LoadMusic;
begin
  playSound := true;
  {load the sounds and music}
  EatSound := SoundEngine.LoadBuffer(ApplicationData('EatSound_CC0_by_EugeneLoza.ogg'));
  EndSound := SoundEngine.LoadBuffer(ApplicationData('DieSound_CC0_by_EugeneLoza.ogg'));
  Music    := SoundEngine.LoadBuffer(ApplicationData('GreenForest_CC-BY-SA_by_Metaruka.ogg'));
  {initialize Sound Engine}
  SoundEngine.ParseParameters;
  //SoundEngine.MinAllocatedSources := 1;
  {and start the music looping}
  CurrentMusic := SoundEngine.PlaySound(music, false, true, 10, 1, 0, 1, ZeroVector3Single);
end;

{start and stop the music}
procedure ToggleMusic;
begin
  if PlaySound then begin
    PlaySound := false;
    CurrentMusic.Gain:= 0
  end else begin
    PlaySound := true;
    CurrentMusic.Gain:= 1
  end;
end;

end.

