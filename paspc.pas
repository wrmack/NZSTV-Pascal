// From https://yingtongli.me/blog/2021/07/08/nzmeek.html

uses Crt, Sysutils;

{"Compiler-supplied" types for nzmeek}
type integer = int32;
type integer1 = int8;
type integer2 = int16;

{"Compiler-supplied" CRT routines}
var ColPaper, ColInk: byte;

const lightgrey = LightGray;

procedure clrvideo;
begin
	ClrScr
end;

procedure getkeyboard(var a: char; var b: byte);
begin
	a := ReadKey
end;

procedure initscreen;
begin
	TextBackground(Black);
	TextColor(LightGray);
	ClrScr
end;

procedure ink(a: byte);
begin
	ColInk := a;
	TextColor(a)
end;

procedure paper(a: byte);
begin
	ColPaper := a;
	TextBackground(a)
end;

procedure putchattr(a: char; b, c, d: byte);
	var x, y: byte;
begin
	x := WhereX();
	y := WhereY();
	TextBackground(b);
	TextColor(c);
	Write(a);
	GotoXY(x, y);
	TextBackground(ColPaper);
	TextColor(ColInk)
end;

procedure soundoff;
begin
	NoSound
end;

{Other "Compiler-supplied" routines}
procedure close(var a: text; b: boolean);
begin
	System.Close(a)
end;

procedure exitprog(a: longint);
begin
	Halt(a)
end;

function get(var a: text): char;
	var c: char;
begin
	Read(a, c);
	get := c
end;

function memavail: integer;
begin
	memavail := 999999
end;

procedure ramfile(var a: text);
begin
	Assign(a, GetTempFileName)
end;

{Peek function instead of dereference}
function peek(var a: text): char;
	var c: char;
begin
	Read(a, c);
	Dec(TextRec(a).bufpos);
	peek := c
end;

{FIXME: Stub implementation not necessary for nzmeek to run}
function fstat(a:string): boolean; 
    begin fstat:=false; 
    end;
procedure time(var a,b,c,d:integer); begin end;