{ See Jupyter notebook: Electoral Systems/!NZMeekSTV-Hill.ipynb & ProgrammingLanguages/Pascal/pascal_notes.ipynb

  From: (gdb)info functions

  All procedures and functions        
        procedure ADJUST(LONGINT, LONGINT);
        procedure ADJUSTDATA;
        procedure ANYKEY(BYTE);
        function  ASK(CANDIDATES) : CANDIDATES;
        procedure CLOSE(TEXT, BOOLEAN);
        procedure CLRVIDEO;
        function  CODECAN(BYTE) : CHAR;
        procedure CONSTRAINTQUERY;
        procedure COUNTVOTES;
        function  DECODECAN(CHAR) : BYTE;
        function  DECODENUM(CHAR) : BYTE;
        procedure DELAY(WORD);
        procedure DIVIDE(LONGINT, LONGINT, LONGINT, LONGINT, LONGINT);
        procedure ELECT(CANDIDATES);
        procedure EXCLUDE(CANDIDATES, BOOLEAN);
        procedure EXITPROG(LONGINT);
        procedure FINDQUOTA;
        function  FSTAT(SHORTSTRING) : BOOLEAN;
        function  GET(TEXT) : CHAR;
        procedure GETKEYBOARD(CHAR, BYTE);
        function  INCAND : BYTE;
        procedure INITSCREEN;
        procedure INK(BYTE);
        function  INNUM : BYTE;
        function  INVAL : LONGINT;
        function  LOWESTCAND : CANDIDATES;
        function  MEMAVAIL : LONGINT;
        procedure MULTIPLY(LONGINT, LONGINT, LONGINT, LONGINT, LONGINT);
        procedure NEWLINE(BYTE);
        procedure OEIOUTPUT;
        procedure OUTTWICE(BOOLEAN, SHORTSTRING, BOOLEAN);
        procedure PAINTSCREEN(BYTE);
        procedure PAPER(BYTE);
        function  PEEK(TEXT) : CHAR;
        procedure PRINTOUT(BYTE);
        procedure PUTCHATTR(CHAR, BYTE, BYTE, BYTE);
        procedure PWLZ(LONGINT);
        procedure RAMFILE(TEXT);
        function  READINT(BYTE) : LONGINT;
        function  READKEY : BYTE;
        procedure READNAME(SHORTSTRING, BYTE);
        function  REED : LONGINT;
        procedure SETTIEBREAKS;
        procedure SHOW(CANDIDATES, BOOLEAN, BYTE, BYTE);
        procedure SHOWELECT;
        procedure SHOWNUMBERS;
        procedure SHUTDOWN(TEXT);
         procedure SIGNAL(BYTE);
        procedure SOUNDOFF;
        function  TIM : LONGINT;
        procedure TIME(LONGINT, LONGINT, LONGINT, LONGINT);
        procedure UPDATE;
        procedure main;
        static procedure HEAD(parentfp_void_pointer, BYTE, BYTE, BYTE);
        static procedure MESSAGE(parentfp_void_pointer);
        static procedure MULDIV(parentfp_void_pointer);
        static function  RANDOM(parentfp_void_pointer) : WORD;
        static procedure UP(parentfp_void_pointer, BYTE);
        static procedure WRT(parentfp_void_pointer, TEXT, LONGINT, LONGINT, SHORTINT, SHORTINT);               
}



Program nzmeek(datafile,outt,tmpp,rslt, confile,oei,ramm,rann);
{$modeswitch ISOPROGRAMPARAS} {WM Added}

{Count the votes using Meek style STV rules. To make sure 
that every computer will give the same results, there are 
no "real" variables or operations in this program. 

Instead numbers that would be "real" are represented by a 
pair of integers, or (if necessarily fractional) by a single 
integer. An integer pair represents the part before the 
decimal point in the first, and a 9-figure part after the 
point in the second. Thus, for example, 346 in the first 
and 704000000 in the second means 346.704. An integer 
representing a fraction similarly has 9 digits, so that, 
for example, 496000000 means 0.496. As an extension, a 
fraction can be 1000000000 meaning 1.0. If using Algol 68 
it would be worth declaring these as types, but Pascal 
does not allow operations on new types to be defined, so 
it is not worth it. Wherever names such as max1 and max2 
are used, it is implied that they are a pair in the 
above sense}

{$i paspc}
{Although in the form of a comment, this is an instruction 
to include extra compiler-supplied features}

Const maxcandidates = 50;
  namelength = 20;
  titlength = 40;

Type candidates = 1..maxcandidates;
  candrange = 0..maxcandidates;
  boolvector = array[candidates] Of boolean;
  nametype = string[namelength];
  titname = string[titlength];

Var mincand,numcandidates: candidates;
  numguarded,numwithdrawn: candrange;
  nontrans1,nontrans2,quota1,quota2,total,temp1,temp2,surplus1,surplus2,
  minvotes1,minvotes2,nextmin1,nextmin2,nn,count,numinv,filesize,randomcount,aheadcount:integer;
  finish,randomused,aheadused,coloured,incomplete,someone,continue,okmm,constrained, gdlast,pseudoquota,changed: boolean;
  guarded, ignore: boolvector;
  cand,numseats,numelected,numexcluded,electindex,numzero,
  colgrn,colred,colcyn,colyel,colwht,randomethod,fracdigits, rules,
  stopp,conmarker,numb,rw: byte;{note 1}
  table, iteration,countno: integer2;{note 2}
  datafile,outt,tmpp,rslt,confile,oei,ramm,rann:text;
  title, str1: titname;
  banner: string[78];{note 3}
  reply,str2: string[80];
  cw: char;
  ahead, electarray: array[candidates] Of byte;
  deemexcl: array[candidates] Of integer1;{note 4}
  tiebreak: array[candidates] Of word;{note 5}
  votes1,votes2,keep: array[candidates] Of integer;
  status: array[candidates] Of (hopeful,elected,excluded);
  name,shortname: array[candidates] Of nametype;
  screentype,nextxleft,nextyleft, nextxright,nextyright,vertdivi, hordivi,
  papercolour,counter: byte;
  screen: array[1..25,1..80] Of char;{note 6}

Procedure paintscreen(top:byte);{note 7}
  {Print the screen to contain the displayed results as they occur}

  Var row,col,numdumps,xl,xr,xm,len, paper, ink: byte;

  Procedure head(xx,len,yy:byte);
    Var head0: string[8];
      col: byte;

    Begin
      If len=7 Then head0 := 'Elected'
      Else head0 := 'Excluded';
      For col:=1 To len Do
        Begin
          screen[yy,xx] := head0[col];
          xx := xx+1
        End
    End{head};

  Begin{paintscreen}
    If top=0 Then

      {Prepare whole screen}
      Begin
        clrscr;
        For row:=1 To 25 Do
          For col:=1 To 80 Do
            screen[row,col] := ' ';

        len := length(title);
        xl := 40-len Div 2;

        For col:=1 To len Do
          Begin
            screen[1,xl] := title[col];
            xl := xl+1
          End;

        {screentype=
        1 if 1 column needed for elected names, 1 for excluded names 
        2 if 1 column for elected names, 2 for excluded names 
        3 if 2 columns for elected names, 1 for excluded names 
        4 means less than 1 for elected, 2+ for excluded 
        5 means 2+ for elected, less than 1 for excluded. 

        vertdivi is the vertical dividing point between elected and excluded. 
        hordivi is the horizontal dividing point if screentype is 4 or 5}

        numdumps := numcandidates-numwithdrawn-numseats;

        If (numseats<21)And(numdumps<21)Then
          Begin
            screentype := 1;
            vertdivi := 40;
            hordivi := 0
          End

        Else If (numseats<21)And(numdumps<41 )Then
          Begin
            screentype := 2;
            vertdivi := 29;
            hordivi := 0
          End

        Else If (numseats<41 )And(numdumps<21)Then
          Begin
            screentype := 3;
            vertdivi := 53;
            hordivi := 0
          End

        Else If numseats<21 Then
          Begin
            screentype := 4;
            vertdivi := 29;
            hordivi := 62-numdumps
          End

        Else If numdumps<21 Then
          Begin
            screentype := 5;
            vertdivi := 53;
            hordivi := 22-numdumps
          End

        Else
          Begin
            screentype := 6;
            vertdivi := 53;
            hordivi := numseats-14
          End;

        If screentype=1 Then
          Begin
            xl := 9;
            xm := 0;
            xr := 49
          End

        Else
          Begin
            xl := 8;
            xm := 32;
            xr := 56;
          End;

        {Put the "Elected" and "Excluded" headings in place}
        head(xl,7,3);
        If (screentype=3)Or(screentype>4)Then head(xm,7,3);
        
        If screentype=5 Then head(xr,7,3)
        Else head(xr,8,3);
        
        If (screentype=2)Or(screentype=4)Then head(xm,8,3);
        
        If screentype=4 Then head(xl,8,hordivi+1)
        Else
          If screentype=5 Then head(xr,8,hordivi+1)
        Else
          If screentype=6 Then head(xm,8,hordivi+1);

        {Monochrome screen uses | to divide left screen from 
        right screen instead of green and red colours}

        If Not coloured Then
          Begin
            If hordivi=0 Then
              Begin
                For row:=3 To 25 Do
                  screen[row,vertdivi] := '|'
              End
            Else
              Begin
                If screentype=4 Then
                  Begin
                    For col:=1 To vertdivi Do
                      screen[hordivi,col] := '-';
                    For row:=3 To hordivi-1 Do
                      screen[row,vertdivi] := '|'
                  End

                Else If screentype=6 Then
                  Begin
                    For col:=29 To 53 Do
                      screen[hordivi,col] := '-';

                    For row:=3 To hordivi-1 Do
                      screen[row,53] := '|';

                    For row:=hordivi+1 To 25 Do
                      screen[row,29] := '|'
                  End

                Else
                  Begin
                    For col:=vertdivi To 80 Do
                      screen[hordivi,col] := '-';
                    For row:=hordivi+1 To 25 Do
                      screen[row,vertdivi] := '|'
                  End
              End
          End;

        {Do title section}
        For row:=1 To 2 Do
          For col:=1 To 80 Do
            Begin
              gotoxy(col,row);
              putchattr(screen[row,col],colcyn,colyel,1)
            End
      End;

    {top=0 means all lines to be done, otherwise only from top 
    downwards}

    For row:=3 To 25 Do
      If (top=0)Or(row>=top)Then
        For col:=1 To 80 Do
          Begin
            If col<=vertdivi Then paper := colgrn
            Else paper := colred;
            If row=3 Then ink := colyel
            Else ink := colwht;
            If hordivi>0 Then
              Begin
                If (screentype=4)And(row>=hordivi)Then
                  Begin
                    paper := colred;
                    If (col<vertdivi)And(row=hordivi+1 )Then ink := colyel
                  End

                Else If screentype=5 Then
                  Begin
                    If row<hordivi Then paper := colgrn;
                    If (col>vertdivi)And(row=hordivi+1 )Then ink := colyel
                  End

                Else If screentype=6 Then
                  Begin
                    If (row>hordivi)And(col>29)And(col<54)Then paper := colred;
                    If (row=hordivi+1 )And(col>29)And(col<54)Then ink := colyel;
                  End
              End;

            gotoxy(col,row);
            putchattr(screen[row,col], paper, ink, 1)
          End;

    {Set up initial screen coordinates for elected (left) and 
    excluded (right) candidate names}

    If top=0 Then
      Begin
        nextyleft := 5;
        If hordivi=0 Then nextyright := 5
        Else nextyright := hordivi+3;
        If screentype=1 Then
          Begin
            nextxleft := 9;
            nextxright := 49
          End

        Else
          Begin
            nextxleft := 8;
            If (screentype=2)Or(screentype=6)Then nextxright := 32
            Else
              If screentype=4 Then nextxright := 8
            Else nextxright := 56
          End
      End
  End{paintscreen};


Procedure show(cand:candidates;elected:boolean;Var nextx,nexty:byte);
  {Print on screen the name of the candidate just elected 
  or excluded, in position specified by nextx and nexty which 
  are updated. The same information is put into the screen 
  array, so that the screen can be rewritten later if necessary}

  Var paper,k: byte;

  Begin
    gotoxy(nextx,nexty);{note 8}
    If elected Then paper := colgrn
    Else paper := colred;

    For k:=1 To 20 Do
      Begin
        screen[nexty,nextx+k-1] := name[cand,k];
        putchattr(name[cand,k],paper,colwht,1);{note 9}
        gotoxy(nextx+k,nexty)
      End;

    nexty := nexty+1;

    If nexty=25 Then
      Begin
        If screentype=1 Then nextx := nextx+40
        Else nextx := nextx+24;
        nexty := 5
      End

  End{show};


{For output to screen, the standard write and writeln procedures will 
serve, though I do not much like them. For input from keyboard I
must use my own routines instead of read and readln, so as (1) to 
catch escape key when necessary and (2) if the user makes an error 
to catch it myself and not leave them to the mercy of compiler or 
operating-system error messages}
{note 10}


Function readkey: byte;
  {Reads one character from the keyboard and returns an integer to represent it. 
  Returns 0..9 for digits 0..9, 
  12 for enter 
  15 for delete or back-delete 
  20 for space 
  99 for everything else. 
  If however the character is Esc, a jump is taken out of the program}

  Var hh: char;
    c,h: byte;

  Begin
    getkeyboard(hh,c);
    h := ord(hh);{note 11}

    If h=0 Then
      Begin
        If c=83 Then readkey := 15
        Else readkey := 99
      End

    Else
      Case h Of 
        48..57: readkey := h-48;
        13: readkey := 12;
        8: readkey := 15;
        27: exitprog(1);
        32: readkey := 20;
        otherwise readkey := 99
      End
  End{readkey};


Function readint(len:byte): integer;
  {Reads, from the keyboard, a non-negative integer in decimal 
  notation of not more than len characters, where O<len<6, 
  terminated with enter. If more than len digits are entered, 
  the last digit is overwritten. The cursor is switched on 
  during the reading but switched off at exit}

  Var y,j,k,xx,yy: byte;
    w: integer;
    val: array[1..6] Of byte;

  Begin
    If len<1 Then len := 1
    Else
      If len>5 Then len := 5;
    cursoron;{note 12}
    xx := wherex;
    yy := wherey;{note 13}
    For j:=1 To len+1 Do
      val[j] := 10;
    For k:=1 To len Do
      write(' ');
    gotoxy(xx,yy);
    j := 1;

    Repeat
      y := readkey;
      If y<10{a digit 0..9} Then
        Begin
          val[j] := y;
          If j<len Then j := j+1;
        End

      Else If y=15{delete} Then
        Begin
          If (j>1 )And((j<len)Or(val[len]=10))Then j := j-1;
          val[j] := 10
        End;

      gotoxy(xx,yy);

      For k:=1 To len Do
        If val[k]=10 Then write(' ')
        Else write(chr(val[k]+48));
      
      gotoxy(xx+j-1,yy)

    Until (y=12)And(val[1]<10);

    {i.e. until enter with at least one digit before it}
    cursoroff;{note 12}

    j := 1;
    w := 0;

    While val[j]<10 Do
      Begin
        w := 10*w+val[j];
        j := j+1
      End;

    readint := w

  End{readint};


Procedure newline(k:byte);

  Var j: byte;

  Begin
    For j:=1 To k Do
      writeln
  End{newline};


Procedure anykey(m:byte);
  {Prints the message ‘Press any key to continue’ on line 
  m of screen. In actual use m is always 24 or 25 anda 
  shorter banner is wanted if on line 24 because it is 
  then within a "window". Waits until a key is pressed 
  before returning}

  Var c: char;
    y: byte;

  Begin
    gotoxy(26-m,m);
    putchattr(' ', lightgrey,black,2*m+30);
    gotoxy(22,m);
    write('Press any key to continue'); 
          getkeyboard(c,y);

    gotoxy(22,m);
    write(' ':10,'Wait ...',' ':10) 
  End{anykey};


Function tim:integer;

  Var m,n,s,t:integer;

  Begin
    time(m,n,s,t);
    tim := ((60*m+n)*60+s)*100+t{note 14}
  End{tim};

Procedure delay(centisecs:word);
  {Causes the computer to wait centisecs hundredths of 
  a second before proceeding. It is a little more 
  complicated than would otherwise be necessary to 
  allow for the possibility that midnight might occur 
  during the wait}

  Var j,q: integer;

  Begin
    j := tim;
    q := (j+centisecs)Mod 8640000;
    While (j>q)And(j>8630000) Do
      j := tim;

    While j<q Do
      j := tim
  End{delay};


Procedure signal(k:byte);
  {If k=O sounds a low note. If k=1 or k=2 sounds a higher note, 
  once or twice respectively. If k>2 sounds k times alternating 
  between two notes a perfect fifth apart. All notes, and gaps 
  between notes, last for 2/25 of a second}

  Var j: byte;

  Begin
    If k=0 Then
      Begin
        sound(550);
        delay(8);{note 15}
        soundoff
      End

    Else
      For j:=1 To k Do
        Begin
          If (k=2)Or odd(j) Then sound(880)
          Else sound(1320);
          delay(8);
          soundoff;
          delay(8)
        End
  End{signal};


Procedure shutdown(Var x:text);
  {Closes file x after writing U as an extra line on the end. This 
  is necessary if an operating system stores files without their 
  final CRLF. The U is never actually used, but the previous line 
  with final CRLF is safe}
  { WM  U with circumflex is chr(219) }

  Begin
    writeln(x,chr(219));
    close(x,true){note 16}
  End{shutdown};


Function codecan(n:byte): char;
  {Turns a candidate number into the equivalent character}
  Begin
    If n>77 Then codecan := chr(n+50)
    Else codecan := chr(n+48)
  End{codecan};


Function decodecan(m:char): byte;
  {Turns a character into the equivalent candidate number}
  Begin
    If m>'~' Then decodecan := ord(m)-50
    Else decodecan := ord(m)-48
  End{decodecan};


Function decodenum(m:char): byte;
  {Turns a character into the equivalent number of candidates 
  or number of seats}

  Begin
    If m>'~' Then decodenum := ord(m)-66
    Else
      If m>'?' Then decodenum := ord(m)-64
    Else decodenum := ord(m)+156
  End{decodenum};


Procedure adjust(Var a1,a2:integer);
  {a1 and a2 are a pair representing a real. If a2 is out of 
  range, they are adjusted to bring it back into range. The 
  result should never be negative but, as a precaution in case 
  of rounding error, if it is, the value is reset to 0.0}

  Begin
    While a2<0 Do
      Begin
        a1 := a1-1;
        a2 := a2+1000000000;

        If a1<0 Then
          Begin
            a1 := 0;
            a2 := 0
          End
      End;

    While a2>999999999 Do
      Begin
        a1 := a1+1;
        a2 := a2-1000000000
      End

  End{adjust};


Procedure update;{note 17}
  {Update the ahead array to show who was ahead at first difference. 
  The numbers assigned are 0, 2, 4, etc., the larger being ahead 
  of the smaller. If equal at all stages so far the average number 
  for the tied group is used. The steps of 2 make this always an 
  integer}

  Var cand: candidates;
    max,val,number: byte;
    list: array[candidates] Of byte;

  Procedure up(number:byte);
    {number gives the number of candidates in a tied ranking, 
    to be separated if possible.}

    Var high: integer1;
      low,n,k: byte;
      max1,max2: integer;
      continue: boolean;
      arra1 ,arra2: array[candidates] Of integer;

    Begin
      {list contains the candidate numbers of those concerned. 
      high and low are found as highest and lowest values in 
      the group if they can all be separated}

      high := ahead[list[1]]+number-1 ;
      low := high-2*number+2;
      For n:=1 To number Do
        Begin
          arra1[n] := votes1[list[n]];
          arra2[n] := votes2[list[n]]
        End;

      continue := true;

      While continue Do
        Begin

          {Find highest value of votes among those concerned and 
          set k to number of candidates with that highest value}
          max1 := -1;
          max2 := 0;

          For n:=1 To number Do
            If (arra1[n]>max1 )Or(arra1[n]=max1 )And(arra2[n]>max2)Then
              Begin
                k := 1;
                max1 := arra1[n];
                max2 := arra2[n]
              End

            Else If (arra1[n]=max1 )And(arra2[n]=max2)Then k := k+1;

          {Set ahead to new sole value or average value and reset 
          arra1 and arra2 arrays so that these cannot be highest again}
          For n:=1 To number Do
            If (arra1[n]=max1 )And(arra2[n]=max2)Then
              Begin
                ahead[list[n]] := high-k+1 ;
                arra1[n] := -2;
                arra2[n] := 0
              End;

          high := high-2*k;
          continue := high>=low
        End

    End{up};

  Begin{update}
    incomplete := false;
    max := 2*numcandidates-2;
    val := 0;

    While val<=max Do
      Begin
        number := 0;

        {Find number of candidates whose present ahead is val}
        For cand:=1 To numcandidates Do
          If ahead[cand]=val Then
            Begin
              number := number+1;
              list[number] := cand
            End;

        {If there is more than 1 of them, see if they now differ 
        in number of votes to allow separation. If no number 
        exceeds 1, incomplete will be false on exit and update 
        will not be entered again}

        If number>1 Then
          Begin
            up(number);
            incomplete := true;
            val := val+number
          End;

        val := val+1

      End

  End{update};


Function incand: byte;{note 18}
  {Reads a candidate number from the coded data and 
  converts it to the equivalent integer}

  Var ch: char;

  Begin
    Repeat
      read(ramm,ch)
    Until (ch>='0');

    incand := decodecan(ch)
  End{incand};


Function innum: byte;
  {Reads a number of candidates or seats from the 
  coded data and converts it to the equivalent integer}

  Var ch: char;

  Begin
    Repeat
      read(datafile,ch)
    Until ch>'';
    innum := decodenum(ch)
  End{innum};


Function inval: integer;{note 19}
  {Reads a vote value from the coded data and 
  converts it to the equivalent integer}

  Var i: integer;
    ch: char;

  Begin
    Repeat
      read(ramm,ch)
    Until ch>='$';

    {A vote value is three characters unless the first two 
    would be 00 when $ replaces them}

    If ch='$' Then
      Begin
        Repeat
          read(ramm,ch)
        Until ch>='0';

        i := ord(ch)-48
      End

    Else
      Begin
        {A three character value meaning three digits in 
        the scale of 50}

        If ch='0' Then i := 0
        Else i := 2500*(ord(ch)-48);
        Repeat
          read(ramm,ch)
        Until ch>='0';

        i := i+50*(ord(ch)-48);

        Repeat
          read(ramm,ch)
        Until ch>='0';

        i := i+ord(ch)-48
      End;

    inval := i
  End{inval};


Procedure multiply(Var c1,c2:integer;a1,a2,b2:integer);
  {Multiply the pair a1, a2 by the fraction b2, putting 
  the result into the pair c1,c2. The operation is split 
  into parts to ensure no overflow. The result is rounded 
  up if not exact}

  Var c,d,e,a11,a12,a13,a21,a22,a23,b21,b22,b23: integer;

  Begin
    If (a1=1 )And(a2=0)Then
      Begin
        c1 := 0;
        c2 := b2
      End

    Else If b2=1000000000 Then
      Begin
        c1 := a1;
        c2 := a2
      End

    Else If (a1=0)And(a2=0)Or(b2=0)Then
      Begin
        c1 := 0;
        c2 := 0
      End
    Else
      Begin
        {Split each of a1, a2 and b2 into three parts}

        a11 := a1 Div 1000;
        a13 := a1 Mod 1000;

        a12 := a11 Mod 1000;
        a11 := a11 Div 1000;

        a21 := a2 Div 1000;
        a23 := a2 Mod 1000;

        a22 := a21 Mod 1000;
        a21 := a21 Div 1000;

        b21 := b2 Div 1000;
        b23 := b2 Mod 1000;

        b22 := b21 Mod 1000;
        b21 := b21 Div 1000;

        {Multiply the appropriate parts but avoid multiplying by 
        parts that are usually zero (if they are)}

        If a11=0 Then
          Begin
            If a12=0 Then d := 1000*a13*b21+a21*b21+a13*b22
            Else d := 1000*(a13*b21+a12*b22)+a21*b21+a13*b22+a12*b23
          End

        Else d := 1000*(a13*b21+a12*b22+a11*b23)+a21*b21+a13*b22+a12*b23;


        If d>999999 Then
          Begin
            c := d Div 1000000;
            d := d Mod 1000000
          End

        Else c := 0;

        e := a23*b23;

        If e Mod 1000>0 Then e := e Div 1000+1
        Else e := e Div 1000;

        e := e+a23*b22+a22*b23+1000*((a23*b21)Mod 1000+(a22*b22)Mod 1000+(a21*b23)Mod 1000);

        If e Mod 1000000>0 Then e := e Div 1000000+1
        Else e := e Div 1000000;


        d := 1000*d+a22*b21+a21*b22+e+a13*b23+(a23*b21)Div 1000+(a22*b22)Div 1000+(a21*b23)Div 1000;
        If d>999999999 Then c := c+1;

        c2 := d Mod 1000000000;

        if a11 = 0 Then
          Begin
            If a12=0 Then c1 := c
            Else c1 := c+a12*b21
          End

          Else c1 := c+1000*a11*b21+a12*b21+a11*b22
      End
  End{multiply};


Procedure divide(Var c2:integer;a1,a2,b1,b2:integer);{note 20}
  {Divide the pair a1, a2 by the pair b1, b2 putting the 
  result into the fraction c2. It is known that the 
  result must always be less than 1.0. a and b are 
  changed within the procedure but, having been called 
  by value, this does no harm. The result is rounded 
  up if not exact}

  Var n: integer;
    m: byte;

  Procedure muldiv;{note 21}
    {Multiply a by 10 and divide n by 10}

  Begin
    n := n Div 10;
    If n>0 Then
      Begin
        a1 := 10*a1+a2 Div 100000000;
        a2 := 10*(a2 Mod 100000000)
      End

  End{muldiv};

  Begin{divide}
    c2 := 0;
    If (a1>0)Or(a2>0)Then
      Begin
        n := 1000000000;

        {Standardise the numbers with n keeping the score}
        While (b1>a1 )Or(b1=a1 )And(b2>a2) Do
          muldiv;

        While n>0 Do
          Begin
            m := 0;

            {See how many times b can be subtracted from a. Answer in m}
            While (b1<a1 )Or(b1=a1 )And(b2<=a2) Do
              Begin
                a1 := a1-b1;
                a2 := a2-b2;

                adjust(a1,a2);
                m := m+1
              End;

            c2 := c2+m*n;

            {Move along one place and try again for next digit}
            muldiv
          End;

        {round up if not exact}
        If (a1>0)Or(a2>0)Then c2 := c2+1
      End
  End{divide};


Procedure findquota;
  {Calculate quota as active votes over seats + 1}

  Var x1,x2: integer;
    nn: byte;

  Begin
    If Not pseudoquota Then
      Begin
        x1 := total-nontrans1;
        x2 := -nontrans2;
        adjust(x1,x2);
        nn := numseats+1;
        quota1 := x1 Div nn;
        x1 := 100000*(x1 Mod nn)+x2 Div 10000;

        {+1 at the end increases the result by one billionth of a 
        vote above the exact quota. This saves trouble later and 
        (unlike Droop's original +1 whole vote) it is very hard 
        to believe that it can possibly do any harm}

        quota2 := 10000*(x1 Div nn)+(10000*(x1 Mod nn)+x2 Mod 10000)Div nn+1;
        adjust(quota1 ,quota2);
        If (rules=7{nz})And(numseats=1 )Then
          Begin
            If quota2>1 Then quota1 := quota1+1;
            quota2 := 0
          End
      End
  End{findquota};


Procedure outtwice(line:boolean;ss:String;guarded:boolean);{note 22}

  Begin
    If line Then writeln(outt);
    If line Then writeln(oei);
    writeln(outt,ss);
    writeln(oei,ss);
    gdlast := guarded
  End{outtwice};


Procedure printout(index:byte);
  {Print on the output channel for serial output the current 
  situation. This is used only when there is an event to 
  report - not at every iteration. index gives the nature 
  of the event requiring printout. See the messages printed 
  at the end of this procedure for details}

  Var arg1,arg2: integer;
    j: byte;
    cand: candidates;


  Procedure wrt(Var channel:text;part1,part2:integer;dig1,dig2:integer1);
    {Print on channel file the number represented by part as 
    part1 before the point, part2 after the point, in total 
    width of dig1 (or more if needed) with dig2 figures 
    after the point. If a fractional value is to be printed 
    it is held in part1 with part2 set negative, and is then 
    printd as 100 times its true value (followed by % if part2 = -1)}

    Var j: byte;
      k,pc: integer;

    Begin
      dig1 := dig1-dig2-1;
      If dig1<1 Then dig1 := 1;
      pc := part2;
      If pc<0 Then
        Begin
          part2 := 100*(part1 Mod 10000000);
          part1 := part1 Div 10000000
        End;
      k := 50000000;

      For j:=1 To dig2-1 Do
        k := k Div 10;

      part2 := part2+k;
      adjust(part1,part2);
      write(channel,part1:dig1,'.');

      For j:=1 To dig2 Do
        Begin
          write(channel,part2 Div 100000000:1);
          part2 := 10*(part2 Mod 100000000)
        End;

      If pc=-1 Then write(channel,'%')

    End{wrt};


  Begin{printout}
    {if no count has been made since printout last used, then 
    there is no point in repeating it}

    If iteration=countno Then writeln(outt)
    Else
      Begin
        countno := iteration;
        table := table+1;
        writeln(outt);

        {If index=5 or index=8 all seats are filled and printing of details 
        beyond that message is not required}
        If (index<>5)And(index<>8)Then
          Begin
            writeln(outt);
            writeln(outt);
            writeln(outt,' ':20,title);
            writeln(outt);
            write(outt,' Table: ',table:1,' Count: ',iteration:1);

            {Calculate j as the number of spaces required before 
            printing quota to align it with votes}

            If Not pseudoquota Then
              Begin
                If quota1<10 Then j := 1
                Else If quota1<100 Then j := 2
                Else If quota1<1000 Then j := 3
                Else If quota1<10000 Then j := 4
                Else j := 5;

                j := j+fracdigits;

                If table<10 Then j := 17-j
                Else If table<100 Then j := 16-j
                Else j := 15-j;
                
                If iteration>99 Then j:=j-2
                Else If iteration>9 Then j := j-1;

                If (rules=7{nz})And(numseats=1) Then
                  write(outt,' ':j,'Absolute majority: ')
                Else write(outt,' ':j,'Quota: ');

                wrt(outt,quota1 ,quota2, 1,fracdigits)
              End;

            writeln(outt);
            writeln(outt);
            write(outt,'Candidate',' ':12,'Keep     Transfer   Votes');
            writeln(outt);
            writeln(outt);

            j := 1;

            For cand:=1 To numcandidates Do
              If Not ignore[cand] Then
                Begin
                  write(outt, name[cand]);
                  wrt(outt,keep[cand],-1,6,1);
                  wrt(outt, 1000000000-keep[cand],-1,8,1);

                  {Allow for possibility of slight rounding error by 
                  setting arg to quota instead of to votes if 
                  necessary and justified}

                  If Not pseudoquota And (status[cand]=elected) And ((votes1 [cand] < quota1)
                    Or (votes1[cand]=quota1 ) And (votes2[cand]<quota2))Then
                    Begin
                      arg1 := quota1;
                      arg2 := quota2
                    End
                  Else
                    Begin
                      arg1 := votes1[cand];
                      arg2 := votes2[cand]
                    End;

                  wrt(outt,arg1,arg2,10,fracdigits);

                  If status[cand]=elected Then write(outt,' >>> Elected')
                  Else If status[cand]=excluded Then
                    Begin
                      write(outt,' Excluded');
                      ignore[cand] := true
                    End;

                  writeln(outt);

                  {Put in blank line every 5 to improve readability}
                  If (numcandidates>9)And(j Mod 5=0)And
                    (cand<>numcandidates)Then writeln(outt);
                  j := j+1
                End;

            writeln(outt);
            write(outt,'Non-transferable');
            wrt(outt,nontrans1,nontrans2,30,fracdigits);
            writeln(outt);
            writeln(outt);
            write(outt, 'Total');
            wrt(outt,total,0,41 ,fracdigits );
            writeln(outt);
            writeln(outt);
            writeln(outt);

            {Also write information for result sheet output}
            write(tmpp,iteration:1,' ');

            If pseudoquota Then wrt(tmpp,0,0,1,9)
            Else
              wrt(tmpp,quota1 ,quota2, 1,9);

            writeln(tmpp);

            For cand:=1 To numcandidates Do
              Begin
                wrt(tmpp,votes1 [cand], votes2[cand],1,9);
                write(tmpp,' ');
                wrt(tmpp,keep[cand],-2,1,7);
                writeln(tmpp)
              End;

            wrt(tmpp,nontrans1 ,nontrans2,1 ,9);
            writeln(tmpp)
          End
      End;

    If numseats>1 Then
      Begin
        writeln(oei);
        If index=2 Then
          Begin
            writeln(oei,'As lowest difference exceeds total surplus, lowest candidate');
            write(outt,'Lowest candidate ');
            outtwice(false,'cannot overtake, so can safely be excluded.', false)
          End

        Else If index=3 Then
          Begin
            write(outt,'No surplus remains');
            write(oei,'Remaining surplus < 0.0001');
            outtwice(false,', so lowest candidate must be excluded.', false)
          End

        Else If index=4 Then
          Begin
            outtwice(false,'All remaining candidates can be elected, for',false);
            outtwice(false, 'their number is no greater than the seats available.', false)
          End

        Else If index=5 Then outtwice(false,'because all seats are now filled.',false)
        Else If index=6 Then outtwice(false,'Exclude all with zero votes.', false)
        Else If index=7 Then
          Begin
            outtwice(false,'Exclude those with zero votes, but retaining', false);
            outtwice(false,'enough candidates to fill all seats.', false)
          End
        Else If index=8 Then
          outtwice(false,'Elect all guarded candidates and exclude others.', false) 
        Else If index=9 Then
          Begin
            Write(outt,'No surplus remains');
            write(oei,'Remaining surplus negligible');
            outtwice(false,', So lowest candidate must be excluded.', false)
          End
    End
  End{printout};


Procedure exclude(cand:candidates;runnerup:boolean);
  {Exclude cand}

  Begin

    {Announce exclusion on screen}
    show(cand,false,nextxright,nextyright);

    {And on output files for serial output}
    str2 := concat('>>>>> ',shortname[cand],' excluded');

    If aheadused Then str2 := concat(str2,' (using "ahead at first difference" rule)')
    Else If randomused Then str2 := concat(str2,' (using random choice)');
    
    If length(str2)<72 Then str2 := concat(str2,' <<<<<');

    outtwice(true,str2,false);

    {And for result sheet output}
    writeln(tmpp,'-2 ',cand:1);
    deemexcl[cand] := -table;
    status[cand] := excluded;

    If Not runnerup Then keep[cand] := 0;
    numexcluded := numexcluded+1 ;

    If randomused Then
      Begin
        writeln(tmpp,'"2',iteration:1);
        randomused := false
      End;

    If aheadused Then
      Begin
        writeln(tmpp,'"7', iteration: 1);
        aheadused := false
      End

  End{exclude};


Procedure elect(cand:candidates);
  {Set cand as elected and store the fact in electarray}

  Begin
    If numelected<numseats Then
      Begin
        status[cand] := elected;
        numelected := numelected+1;

        If guarded[cand] Then
          Begin
            guarded[cand] := false;
            numguarded := numguarded-1
          End;
        electindex := electindex+1;
        electarray[electindex] := cand
      End
  End{elect};


Procedure showelect;
  {Take candidates in electarray and announce their election 
  in order of their numbers of votes}

  Var count,candref,maxcandref: byte;

  Begin

    {Find candidate with most votes}
    For count:=1 To electindex Do
      Begin
        maxcandref := 1;

        While electarray[maxcandref]=0 Do
          maxcandref := maxcandref+1;
        For candref:=maxcandref+1 To electindex Do
          Begin
            If electarray[candref]>0 Then
              If (votes1[electarray[candref]]>votes1[electarray[maxcandref]])Or
                (votes1[electarray[candref]]=votes1[electarray[maxcandref]])And
                (votes2[electarray[candref]]>votes2[electarray[maxcandref]])
                Then maxcandref := candref
          End;

        {Announce on screen}
        show(electarray[maxcandref], true ,nextxleft,nextyleft);

        {And on output files for serial output}
        outtwice(true,concat('>>>>> ',shortname[electarray[maxcandref]],' elected <<<<<'), false);

        {And for result sheet output}
        deemexcl[electarray[maxcandref]] := table;
        writeln(tmpp,'-1 ',electarray[maxcandref]:1 );

        {Stop the same one from being found again}
        electarray[maxcandref] := 0

      End;

    {Restart the electarray table}
    electindex := 0

  End{showelect};


Function ask(eg:candidates): candidates;
  {If a random choice is necessary for whom to exclude and 
  randomethod is interactive, ask the user to choose. 
  eg is an example candidate of those involved}

  Var count: byte;
    cand,row,top: integer;
    ok: boolean;
    poss: boolvector;

  Begin
    count := 0;
    For cand:=1 To numcandidates Do
      Begin
        {poss array is set to say whether cand is a possible one 
        to be asked about. count is found as the number of possibles}
        poss[cand] := (status[cand]<>excluded)And Not guarded[cand]
                      And(votes1[cand]=votes1[eg])And(votes2[cand]=votes2[eg]);
        If poss[cand] Then count := count+1
      End;

    {If there is only 1 possible, there is no need to ask after all}
    If count<2 Then ask := eg
    Else
      Begin
        {The asking will overwrite part of the result screen. 
        Calculate where the top of the window must be}
        top := 22-(count+2)Div 3;
        ink(black);
        If coloured Then
          Begin
            paper(cyan);
            papercolour := cyan
          End
        Else
          Begin
            paper(lightgrey);
            papercolour := lightgrey
          End;

        {Form the window, but not using compiler-supplied window method}
        For row:=top To 25 Do
          Begin
            gotoxy(1,row);
            putchattr(' ',papercolour,black,80)
          End;

        gotoxy(3,top);

        writeln('A random choice is necessary for whom to exclude.');
        write(' The relevant candidates are:');

        count := 0;
        writeln;

        For cand:=1 To numcandidates Do
          If poss[cand] Then
            Begin
              {Print candidate numbers as well as names as user selection 
              is made by number. Print 3 candidates per line}
              write('',cand:2,' ',name[cand]);
              count := count+1;

              If count Mod 3=0 Then writeln

            End;

        ok := true;

        Repeat

          {Signal to user that a decision is wanted}
          If ok Then signal(3)
          Else signal(6);
          gotoxy(5,24);

          write('Please choose one at random and give the candidate number: ');

          cand := readint(2);
          ok := cand<=numcandidates;

          If ok Then ok := poss[cand]

          {Refuse impossible candidates}

        Until ok;

        {Restore screen}
        paintscreen(top);

        ask := cand
      End
  End{ask};


Procedure adjustdata;

  Var j,nm,num: byte;
    ch: char;
    str1: string[10];
    ended: boolean;

  Begin
    num := numcandidates-numexcluded;
    reset(ramm);
    rewrite(rann);
    read(ramm,ch);
    write(rann,ch);
    ended := false;

    While Not ended Do
      Begin
        {copy value of vote from ramm to rann}
        If ch<>'$' Then
          Begin
            read(ramm,ch);
            write(rann,ch)
          End

        Else If peek(ramm) = '0' Then ended := true; {WM yingtongli}

        If Not ended Then
          Begin
            read(ramm,ch);
            write(rann,ch);

            {copy vote with relevant adjustments}
            nm := 0;
            read(ramm,ch);

            While ch<>'0' Do
              Begin
                If status[decodecan(ch)]<>excluded Then
                  Begin
                    nm := nm+1;

                    {If nm=num it means that all remaining candidates are 
                    mentioned so last one should not be output}
                    If nm<num Then write(rann,ch)
                  End;

                read(ramm,ch)
              End;

            read(ramm,ch);
            write(rann,'0',ch)
          End

        Else write(rann,'0')
      End;

    If Not okmm Then
      Begin
        close(rann,true);
        assign(rann,'votes.rnn')
      End;

    reset(rann);

    If Not okmm Then
      Begin
        close(ramm,true);
        assign(ramm,'votes.rmm')
      End;

    rewrite(ramm);

    {Modified data in temporary file rann. Copy it over to ramm which 
    is the expected name outside this routine}
    While Not eof(rann) Do
      Begin
        read(rann,ch);
        write(ramm,ch)
      End
  End{adjustdata};


Procedure countvotes;{note 23}

  Var cand,mm,marker: byte;
    ended: boolean;
    count,value1 ,value2,tot1 ,tot2: integer;

  Begin
    iteration := iteration+1;
    reset(ramm);
    nontrans1 := 0;
    nontrans2 := 0;

    For cand:=1 To numcandidates Do
      Begin
        votes1[cand] := 0;
        votes2[cand] := 0
      End;

    count := inval;

    While count>0 Do                                                           {WM count is number votes at start of each row, so do each row of votes}
      Begin
        value1 := 1;
        value2 := 0;
        cand := incand;                                                        {WM reads candidate number from the coded data }
        ended := false;                                                        {WM ended is a flag for whether the vote can be transferred further}
        While cand>0 Do                                                        {WM cand is candidate number in the row. Each row ends with 0.}
          Begin
            If Not ended Then                                                  {WM if is ended then goes to line 1611 and reads next candidate}
              Begin
                If Keep[cand]>0 Then                                           {WM keep value of 0 means excluded}
                  Begin
                    ended := status[cand]=hopeful;                             {WM ended is true if candidate is hopeful; the whole vote goes to the candidate}

                    If ended Then
                      Begin
                        votes1[cand] := votes1[cand]+count*value1;             {WM increase the votes of this candidate by the value of this vote multiplied by the number of ballots on this data line}
                        multiply(temp1,temp2,count,0,value2);                  {WM multiply pair count,0 by value2 and put result in temp1, temp2}   
                        votes1[cand] := votes1[cand]+temp1;
                        votes2[cand] := votes2[cand]+temp2;
                        adjust(votes1[cand], votes2[cand]);                    {WM a1 and a2 are a pair representing a real. If a2 is out of range, they are adjusted to bring it back into range.}
                        value1 := 0;
                        value2 := 0
                      End

                    Else
                      Begin
                        multiply(temp1,temp2,value1,value2,keep[cand]);        {WM multiply current value of vote by this candidate's keep value and store in temp1, temp2}
                        value1 := value1-temp1;                                {WM reduce the value of the vote by the value stored for this candidate}
                        value2 := value2-temp2;
                        adjust(value1 ,value2);
                        votes1[cand] := votes1[cand]+count*temp1;              {WM increase the votes of this candidate by the amount stored, multiplied by the number of ballots on this data line}
                        multiply(temp1,temp2,count,0,temp2);                   {WM Don't understand these lines. See procedure multiply}
                        votes1[cand] := votes1[cand]+temp1;
                        votes2[cand] := votes2[cand]+temp2;
                        adjust(votes1[cand],votes2[cand])
                      End
                  End;

                If (value1=0)And(value2=0)Then ended := true                    {WM if value is 0, vote cannot be transferred further}
              End;
            cand := incand                                                      {WM read next candidate number from the coded data}
          End;

        nontrans1 := nontrans1+count*value1;                                    {WM if value if vote is positive at this point, it cannot be transferred further so increase non-transferable votes}
        multiply(temp1,temp2,count,0,value2);
        nontrans1 := nontrans1+temp1;
        nontrans2 := nontrans2+temp2;
        adjust(nontrans1,nontrans2);
        count := inval
      End;

    For cand:=1 To numcandidates Do
      tiebreak[cand] := 10000-tiebreak[cand] {note 24}

  End{countvotes};


Function lowestcand: candidates;
  {Find the continuing (non-guarded) candidate with fewest votes}

  Var cand,candy: candidates;

  Begin
    candy := 1;

    While (status[candy]<>hopeful)Or guarded[candy] Do
      candy := candy+1;

    For cand:=candy+1 To numcandidates Do
      If (status[cand]=hopeful)And Not guarded[cand] Then
        Begin
          If (votes1[cand]<votes1[candy])Or(votes1[cand]=votes1[candy])And
            (votes2[cand]<votes2[candy])Then

            Begin
            {New lowest so far}

              candy := cand;
              randomused := false;
              aheadused := false
            End

          Else
            If (votes1[cand]=votes1[candy])And(votes2[cand]=votes2[candy])Then
              Begin
                If ahead[cand]<>ahead[candy] Then
                  Begin
                    {Equal lowest on votes but lowest on ahead criterion}
                    If ahead[cand]<ahead[candy] Then candy := cand;
                    randomused := false;
                    aheadused := true
                  End

                Else
                  Begin

                  {Equal lowest on votes and on ahead. Break tie 
                  with random choice}

                    If tiebreak[cand]<tiebreak[candy] Then candy := cand;
                    randomused := true;
                    aheadused := false
                  End
              End
        End;

    If aheadused Then aheadcount := aheadcount+1;

    {If a random choice was in the end necessary then, if 
    randomethod is interactive, ask the user to make the 
    necessary selection with candy as an example of 
    those relevant}

    If randomused Then
      Begin
        randomcount := randomcount+1;
        If randomethod=3 Then candy := ask(candy)
      End;

    lowestcand := candy
  End{lowestcand};


Procedure settiebreaks;{note 25}
  {Give each candidate a four digit random number in case it 
  is needed as a tie breaker. Ensure that no two candidates 
  get the same number}

  Var seed1,seed2,seed3: integer2;
    cand,candy: byte;
    ok: boolean;

  Function random: word;
    {Returns a pseudo-random four-digit integer, rectangularly 
    distributed between 0001 and 9999. Based on Wichmann and 
    Hill, Algorithm AS 183, Appl. Statist. (1982) 31, 188-190. 
    At first entry sets seeds}

    Var h,m,s,u: integer;

    Begin
      If seed1=0 Then
        Begin

        {seeds need setting. Use clock reading unless user has 
        asked for fixed method}

          If randomethod=1 Then
            Begin
              h := 5;
              m := 0;
              s := 0;
              u := 0
            End

          Else time(h,m,s,u);{note 14}

          seed1 := numcandidates+h+s;
          seed2 := numseats+m+u;
          seed3 := (total+10000*(total Mod 10)) Mod 30323

        End

      Else h := 1;

      For m:=1 to h Do
        Begin
          seed1 := (171*seed1)Mod 30269;
          seed2 := (172*seed2)Mod 30307;
          seed3 := (170*seed3)Mod 30323
        End;

      random := ((seed1*10000)Div 30269+(seed2*10000)Div 30307+
                (seed3*10000)Div 30323)Mod 10000
    End{random};

  Begin{settiebreaks}
    seed1 := 0;

    For cand:=1 To numcandidates Do
      Repeat
        tiebreak[cand] := random;
        ok := true;
        For candy:=1 To cand-1 Do
          If tiebreak[cand]=tiebreak[candy] Then ok := false
        {Keep trying until number is unique}
      Until ok
  End{settiebreaks};


Procedure readname(Var tag:String;maxlen:byte);
  {Read candidate's name or title from data file and return it in tag. 
  If a candidate's name also set shortname with no trailing spaces}

  Var sub: byte;
    ch: char;

  Begin
    tag := '';

    {Find opening "}

    Repeat
      read(datafile,ch)
    Until ch='"';

    sub := 0;
    read(datafile,ch);

    {Read characters until closing " and add them to tag up 
    to a maximum length of maxlen}

    While ch<>'"' Do
      Begin
        If sub<maxlen Then
          Begin
            sub := sub+1 ;
            tag := concat(tag,ch)
          End;

        read(datafile,ch)
      End;

    If maxlen<titlength Then
      Begin

        {This is a candidate's name, not the title of election}
        shortname[cand] := name[cand];

        {Make up to maxlen characters with trailing spaces}
        While sub<maxlen Do
          Begin
            sub := sub+1;
            tag := concat(tag,' ')
          End
      End
  End{readname};


Procedure pwlz(k:integer);
  {pwlz means Print With Leading Zeroes, on oei output channel}

  Var n: integer;
    m: byte;

  Begin
    write(oei,'.');
    n := 100000000;

    Repeat
      m := k Div n;
      write(oei,m:1);
      k := k Mod n;
      n := n Div 10
    Until n=0
  End{pwilz};


Procedure shownumbers;
  {Shows numbers at bottom of screen to let user know that something 
  is happening. Puts information to oei output channel too}

  Var wx,wy: byte;

  Begin
    wx := wherex;
    wy := wherey;{note 13}

    If coloured Then
      Begin
        paper(cyan);
        ink(black)
      End

    Else
      Begin
        paper(black);
        ink(lightgrey)
      End;

    gotoxy(1,25);
    clreol;{note 26}
    gotoxy(5,25);
    write(surplus1+surplus2/1000000000.0:16:6);
    gotoxy(35,25);
    write(temp1+temp2/1000000000.0:16:6);
    gotoxy(wx,wy);

    If numseats>1 Then
      Begin
        writeln(oei);
        write(oei,'Total surplus = ',surplus1:1);
        pwlz(surplus2);
        write(oei,' ':5,'Lowest difference = ',temp1:1);
        pwlz(temp2);
        writeln(oei)
      End
  End{shownumbers};


Function reed: integer;
  {Finds next number on datafile}

  Var r: word;
    c: char;

  Begin
    Repeat
      read(datafile,c)
    Until (c>='0')And(c<='9');

    r := ord(c)-48;

    While (peek(datafile) >= '0')And(peek(datafile)<='9') Do  {WM from yingtongli}
      Begin
        read(datafile,c);
        r := 10*r+ord(c)-48                                    {WM eg 23 = 10*2 + 3}
      End;

    reed := r
  End{reed};


Procedure oeioutput;

  Var arg1,arg2: integer;
    cand: candidates;

  Begin
    If iteration>1 Then writeln(oei);
    writeln(oei);
    writeln(oei,' ':20,title);
    writeln(oei);
    write(oei, 'Iteration: ',iteration:1,' ');
    
    If Not pseudoquota Then
      Begin
        If (rules=7{nz})And(numseats=1) Then write(oei,'Absolute majority')
        Else write(oei,'Quota');

        write(oei,': ',quota1:1);
        pwlz(quota2)
      End;

    writeln(oei);
    writeln(oei);
    write(oei,'Candidate',' ':12,' Keep',' ':11,' Votes',' ':10,'Ahead');
    If randomethod<3 Then write(oei,' Random');
    writeln(oei);
    writeln(oei);

    For cand:=1 To numcandidates Do
      Begin
        write(oei, name[cand]);

        If keep[cand]=1000000000 Then write(oei,' 1.000000000')
        Else
          Begin
            write(oei,' 0');
            pwlz(keep[cand])
          End;

        write(oei, votes1[cand]:8);
        pwlz(votes2[cand]);
        write(oei,ahead[cand]:7);

        If randomethod<3 Then write(oei,tiebreak[cand]:8);
        writeln(oei);

        If (numcandidates>9)And(cand Mod 5=0)And
          (cand<>numcandidates)Then writeln(oei)
      End;

    writeln(oei);
    write(oei,'Non-transferable',nontrans1:25);
    pwlz(nontrans2);
    writeln(oei);
    writeln(oei);
    writeln(oei,' Total', total:36,'.000000000')
  End{oeioutput};


Procedure constraintquery;
  {Asks whether any constraints exist. The procedure is 
  entered anyway but, if not applying, only to set the 
  necessary markers to show no constraints}

  Var candy: integer1;
    answered: boolean;

  Procedure message;
  {Message and finish if pointless to continue}

    Begin
      clrscr;
      newline(3);

      If numguarded=numseats Then
        Begin
          writeln(' No election necessary. All guarded candidates');
          writeln(' are elected. All others are excluded.');
          outtwice(true,' No election necessary. All guarded candidates', false);
          outtwice(true,' are elected. All others are excluded. ',false)
        End

      Else
        Begin
          writeln(' Impossible - number of guarded candidates');
          writeln(' exceeds number of seats.');
          outtwice(true,' Impossible - number of guarded candidates', false);
          outtwice(true,' exceeds number of seats.',false)
        End;

      shutdown(outt);
      shutdown(oei);
      shutdown(rslt);
      exitprog(2)
    End{message};

  Begin{constraintquery}
    constrained := false;

    {A guarded candidate is constrained to be elected eventually}
    numguarded := 0;

    For candy:=1 To numcandidates Do
      guarded[candy] := false;
    If fstat('votes.con')Then
      Begin
        assign(confile,'votes.con');
        reset(confile);
        read(confile,candy);
        read(confile,candy);
        read(confile,candy);

        While candy>0 Do
          Begin
            guarded[candy] := true;
            numguarded := numguarded+1;
            writeln(tmpp,'"4',name[candy],'"');
            outtwice(Not gdlast,concat(shortname[candy],' is guarded.'),true);
            read(confile,candy)
          End;

        {message prints message and terminates program}
        If numguarded>=numseats Then message
      End

  End{constraintquery};

Begin{main program}
  numinv := 0;
  randomcount := 0;
  aheadcount := 0;
  pseudoquota := false;
  gdlast := false;
  conmarker := 0;
  initscreen;cursoroff;{note 27}                                               {WM Can comment out while debugging}
  clrvideo;{note 28}                                                           {WM Can comment out while debugging}
  coloured := false;{note 29}

  If coloured Then
    Begin
      paper(blue);
      ink(yellow);

      For rw:=1 To 25 Do
        Begin
          gotoxy(1,rw);
          putchattr(' ',blue, yellow,80)
        End
    End;

  gotoxy(1,3); {WM}

  write(' Wait ...');

  If coloured Then
    Begin
      colgrn := green;
      colred := red;
      colcyn := cyan;
      colyel := yellow;
      colwht := white
    End

  Else
    Begin
      colgrn := black;
      colred := black;
      colcyn := black;
      colyel := lightgrey;
      colwht := lightgrey
    End;

  randomethod := 1;
  rules := 7;{note 30}
  banner := 'New Zealand demonstration';{note 31}
  
  {WM erase file size check as per https://yingtongli.me/blog/2021/07/08/nzmeek.html }
  // command('dir votes.dat >votes.tep',nn);{note 32}
  // assign(tmpp,'votes.tep');
  // reset(tmpp);
  // Repeat
  //   readln(tmpp,str1)
  // Until (pos('VOTES',str1 )>0)And(pos('DAT',str1)>0);
  // filesize := 0;
  // nn := 1;
  // While (str1[nn]<'0')Or(str1[nn]>'9') Do
  //   nn := nn+1;
  // Repeat
  //   filesize := 10*filesize+ord(str1[nn])-48;
  //   nn := nn+1
  // Until str1[nn]=' ';
  // erase(tmpp);
  filesize:=0;

  assign(outt,'votes.res');                                                    {WM Information in normal form - refer Hill }
  rewrite(outt);

  assign(oei,'votes.oei');                                                     {WM Information in extended form }
  rewrite(oei);
  
  assign(rslt,'votes.rlt');                                                    {WM 'This is not intended for reading by people but as the necessary information }
  rewrite(rslt);                                                               { to allow a computer to read it to produce other forms of output.'}

  {All candidates have initial Keep value of 1.0}
  For cand:=1 To maxcandidates Do
    keep[cand] := 1000000000;

  {Form a ramfile ramm, provided that would not be too big}
  okmm := filesize<0.9*memavail;{note 33}
  If okmm Then ramfile(ramm)
  Else assign(ramm,'votes.rmm');
  rewrite(ramm);

  assign(datafile,'Sydney.blt');
  reset(datafile);

  assign(tmpp, 'votes.tip');
  rewrite(tmpp);

  {Pass over any initial spaces, tabs or newlines}
  While (peek(datafile)=chr(32))Or(peek(datafile)=chr(9)) Do                   {WM yingtongli}
    get(datafile);

  If (peek(datafile)>='0')And(peek(datafile)<='9')Then                         {WM yingtongli}
    Begin
      {Non-coded data on votes.dat}
      
      read(datafile, numcandidates,numseats);

      {Pass over any spaces, tabs or newlines}
      While (peek(datafile)=chr(32))Or(peek(datafile)=chr(9)) Do               {WM yingtongli}
        get(datafile);

      {Look for withdrawals}
      read(datafile, temp1);
      If temp1<0 Then                                                          {WM a negative integer means withdrawal of that candidate }
        Repeat
          keep[-temp1] := 0;
          read(datafile,temp1)
        Until temp1>0;

      {Code data and put it into ramm}                                         {WM ramm is a ram file }

      While temp1>0 Do
        Begin
          If temp1<50 Then write(ramm,'$',chr(temp1+48))                       {WM chr(48) is 0. }
          Else write(ramm,chr(temp1 Div 2500+48),                              {WM div is integer division. See function inval for further comments} 
            chr((temp1 Mod 2500)div 50+48),chr(temp1 Mod 50+48));
          
          Repeat
            temp2 := reed;                                                     {WM reed is a function to find next number in datafile}
            write(ramm,codecan(temp2))
          Until temp2=0;

          read(datafile ,temp1 )
        End;
      write(ramm,'$0')
    End

  Else
    Begin
      {Coded data on votes.dat}

      {Pass over any spaces, tabs or newlines}
      While (peek(datafile)=chr(32))Or(peek(datafile)=chr(9)) Do               {WM yingtongli}
        get(datafile);

      numcandidates := innum;
      numseats := innum;

      {Pass over any spaces, tabs or newlines}
      While (peek(datafile)=chr(32))Or(peek(datafile)=chr(9)) Do               {WM yingtongli}
        get(datafile);

      {Look for withdrawals}
      If peek(datafile)='-' Then  {WM yingtongli}
        Repeat
          get(datafile);
          read(datafile,cw);
          keep[decodecan(cw)] := 0
        Until peek(datafile)<>'-';  {WM yingtongli}

      {Copy direct to ramm, but omitting any spaces or newlines}
      stopp := 0;
      Repeat
        read(datafile,cw);
        If cw='$' Then stopp := 1
        Else
          If (stopp=1)And(cw='0')Then stopp := 2
        Else stopp := 0;
        If cw<>'' Then write(ramm,cw)
      Until stopp=2
    End;

  If Not okmm Then
    Begin
      close(ramm,true);
      assign(ramm,'votes.rmm')
    End;

  reset(ramm);
  outtwice(false, banner,false);                                               {WM outtwice writes to outt (votes.res) and oei (votes.oei)}
  reply := 'Version 6.7.7 - NZ rules';{note 34}
  outtwice(false,reply,false);
  writeln(rslt, banner);
  writeln(rslt,reply);
  writeln(outt,' Number of candidates = ',numcandidates:1);
  writeln(outt,' Number of seats = ',numseats:1);

  If fstat('votes.con')Then{note 35}
    outtwice(true,'Note that this election was subject to constraints.', false);

  total := 0;
  table := 0;
  iteration := 0;
  countno := 0;
  numelected := 0;
  numexcluded := 0;
  randomused := false;
  aheadused := false;
  electindex := 0;
  incomplete := true;
  numwithdrawn := 0;

  For cand:=1 To numcandidates Do
    Begin
      readname(name[cand],namelength);
      status[cand] := hopeful;
      ignore[cand] := false;

      {Exclude any withdrawn candidate}
      If keep[cand]=0 Then
        Begin
          status[cand] := excluded;
          numexcluded := numexcluded+1;
          numwithdrawn := numwithdrawn+1;
          writeln(tmpp,'"1',name[cand]);
          outtwice(false,concat('Withdrawn before count - ',name[cand]),false)
        End;
      deemexcl[cand] := 0;
      ahead[cand] := numcandidates-1
    End;

  readname(title,titlength);
  close(datafile,true);

  If numinv>0 Then
    Begin
      If rules=1 Then write(outt,' ');
      write(outt,'Number of in');
      
      If rules=1 Then write(outt,'valid')
      Else write(outt,'formal');

      writeln(outt,' votes = ',numinv:1)
    End;

  {Read value of first vote}                                                   {WM count total votes}
  count := inval;
  While count>0 Do                                                             {WM count is the integer at start of row}
    Begin
      total := total+count;                                                    {WM total holds total votes}
      Repeat
        cand := incand                                                         {WM cand is candidate}
      Until cand<1;                                                            {WM cand is 0 at end of each row}

      {Read next vote value}
      count := inval
    End;

  settiebreaks;
  constraintquery; {??????}

  {Find number of digits to show in fractional parts in output}
  fracdigits := 4;
  If total>999 Then fracdigits := fracdigits-1;
  If total>99 Then fracdigits := fracdigits-1;
  If total>9 Then fracdigits := fracdigits-1;

  {Initiate output for result sheet}
  writeln(tmpp,title);
  writeln(tmpp,numcandidates:1,' ',numseats:1,' ',numinv:1,' ',total:1);
  For cand:=1 To numcandidates Do
    writeln(tmpp,shortname[cand]);
  
  findquota;
  cursoroff;

  If coloured Then
    Begin
      paper(blue);
      ink(yellow)
    End;

  clrscr;
  newline(4);
  writeln(' ':10,'The numbers that will appear at the bottom Of the screen');
  writeln(' ':10,'during the count are of no importance, except to indicate');
  writeln(' ':10,'that calculations are continuing. As long as they are'); 
  writeln(' ':10,'changing, there is no need to kick the computer even if');
  writeln(' ':10,'the next decision seems to be taking a long time.'); 

  If Not okmm Then
    Begin
      writeln;
      writeln(' ':10,'As this is such a large election, the first such numbers'); 
      writeln(' ':10,'may not appear for some time, and they may change only');
      writeln(' ':10,'infrequently. Don"t kick the computer anyway.')
    End;

  anykey(25);
  paintscreen(0);

  Repeat
    If numcandidates-numexcluded<=numseats Then
      Begin
        {All remaining candidates can be elected, for 
        their number is no greater than the seats available}
        printout(4);
        someone := false;

        For cand:=1 To numcandidates Do
          If status[cand]=hopeful Then
            Begin
              elect(cand);
              someone := true
            End;

        If someone Then showelect;
        finish := true
      End

    Else If numelected+numguarded=numseats Then
      Begin

        {All guarded candidates can be elected, for 
        their number is no greater than the seats available. 
        All others must be excluded}

        printout(8);
        someone := false;

        For cand:=1 To numcandidates Do
          If guarded[cand] Then
            Begin
              elect(cand);
              someone := true
            End

          Else If status[cand]=hopeful Then exclude(cand, false);

        If someone Then showelect;
        finish := true
      End

    Else
      Begin
        countvotes;

        {Update the ahead array unless already complete}
        If incomplete Then update;
        
        findquota;
        oeioutput;
        surplus1 := 0;
        surplus2 := 0;
        someone := false;
        numzero := 0;
        numb := 0;

        For cand:=1 To numcandidates Do
          If (status[cand]=hopeful)And Not guarded[cand] Then                  {WM sets value for numb - number of hopeful with more than the quota ??}
            Begin
              If (votes1[cand]>quota1 )Or(votes1[cand]=quota1)And              {WM AND has higher precedence than OR}
                (votes2[cand]>=quota2)Then numb := numb+1                      {WM quota1 is integer part and quota2 is decimal part of quota}
            End;                                                               {WM votes1 and votes2 ditto}

        If numelected+numguarded+numb>numseats Then{note 36}
          Begin
            If numguarded>0 Then
              Begin
                quota1 := total+1;
                quota2 := 0;

                outtwice(true,'Because of guarded candidates, too many have', false);
                outtwice(false,'become electable. Special action follows.',false);
                pseudoquota := true
              End

            Else quota2 := 1
          End;

        minvotes1 := quota1;
        minvotes2 := quota2;
        nextmin1 := quota1 ;
        nextmin2 := quota2;

        {Find mincand as continuing non-guarded candidate with minimum 
        votes, numzero as number with zero votes, minvotes as minimum number of votes, 
        nextmin as the number of votes of the lowest candidate 
        other than mincand. (NB nextmin may equal minvotes)}

        For cand:=1 To numcandidates Do
          Begin
            If (status[cand]=hopeful)And Not guarded[cand] And
              (votes1[cand]=0)And(votes2[cand]=0)Then
              Begin
                numzero := numzero+ 1 ;
                mincand := cand
              End

            Else
              If (status[cand]=hopeful)And Not guarded[cand] And
                ((votes1[cand]<minvotes1 )Or(votes1[cand]=minvotes1)
                And(votes2[cand]<minvotes2))Then
                Begin
                  nextmin1 := minvotes1 ;
                  nextmin2 := minvotes2;
                  minvotes1 := votes1[cand];
                  minvotes2 := votes2[cand];
                  If numzero=0 Then mincand := cand
                End

            Else
              If (status[cand]=hopeful)And Not guarded[cand] And
                ((votes1[cand]<nextmin1 )Or(votes1[cand]=nextmin1)
                And(votes2[cand]<nextmin2))Then
                Begin
                  nextmin1 := votes1[cand];
                  nextmin2 := votes2[cand]
                End

            Else
              If (votes1[cand]>quota1 )Or(votes1[cand]=quota1)And
                (votes2[cand]>=quota2)Then
                Begin

                  {Find the total surplus}
                  surplus1 := surplus1+votes1[cand]-quota1;
                  surplus2 := surplus2+votes2[cand]-quota2;
                  adjust(surplus1,surplus2);

                  If status[cand]=hopeful Then
                    Begin
                      elect(cand);
                      someone := true
                    End
                End
          End;

        If someone Then
          Begin
            {printout with parameter 1 has no special message}
            printout(1);
            showelect
          End;

        If numelected<numseats Then
          Begin
            finish := false;
            If numzero<2 Then
              Begin
                {Find gap between lowest and next lowest}
                If numzero=1 Then
                  Begin
                    temp1 := minvotes1;
                    temp2 := minvotes2
                  End

                Else
                  Begin
                    temp1 := nextmin1-minvotes1;
                    temp2 := nextmin2-minvotes2;
                    adjust(temp1,temp2)
                  End;

                // shownumbers;                                                            {WM Creates rubbish numbers in terminal }

                If (temp1>surplus1 )Or(temp1=surplus1)And(temp2>surplus2)Then
                  Begin
                    {Lowest candidate cannot overtake, so can safely be excluded}
                    printout(2);
                    exclude(mincand, false)
                  End

                Else If (surplus1=0)And(surplus2<100000)Then
                  Begin
                    {No surplus remains, so lowest candidate must be excluded}
                    printout(3);
                    exclude(lowestcand, false)
                  End
              End

            Else
              Begin
                temp1 := 0;
                temp2 := 0;
                shownumbers;

                If (surplus1=0)And(surplus2<100000)Then
                  Begin
                    If numcandidates-numexcluded-numzero>=numseats Then
                      Begin
                        {Exclude all with zero votes}
                        printout(6);

                        For cand:=1 To numcandidates Do
                          If (status[cand]=hopeful)And Not guarded[cand] And
                            (votes1[cand]=0)And(votes2[cand]=0)Then exclude(cand, false)
                      End

                    Else
                      Begin
                        {Exclude those with zero votes, but retaining 
                        enough candidates to fill all seats}
                        printout(7);

                        For cand:=1 To numcandidates-numexcluded-numseats Do
                          exclude(lowestcand, false)
                      End
                  End
              End
          End
        Else
          {numelected=numseats}
          finish := true
      End;

    If Not finish Then
      Begin
        changed := false;

        For cand:=1 To numcandidates Do
          If status[cand]=excluded Then
            Begin
              If (votes1[cand]>0)Or(votes2[cand]>0)Then changed := true
            End

          Else If (votes1[cand]>quota1 )Or(votes1[cand]=quota1 )And
                  (votes2[cand]>quota2)Then
            Begin
              {Find new keep value as present keep value times quota/votes}
              multiply(temp1,temp2,quota1 ,quota2,keep[cand]);
              divide(temp2,temp1 ,temp2,votes1[cand],votes2[cand]);
              If keep[cand]<>temp2 Then changed := true;
              If temp2>999999999 Then keep[cand] := 1000000000
              Else
                keep[cand] := temp2
            End;

        If Not changed Then
          {Special case. Lowest candidate must be excluded}
          Begin
            printout(9);
            exclude(lowestcand, false)
          End
      End

  Until finish;

  someone := false;

  For cand:=1 To numcandidates Do
    If status[cand]=hopeful Then someone := true;

  If someone Then
    Begin
      {Exclude remainder because all seats are now filled}
      For cand:=1 To numcandidates Do
        If status[cand]=hopeful Then exclude(cand, true);
      printout(5)
    End;

  writeln(outt);
  writeln(outt);

  writeln(outt,'Election complete');
  shutdown(outt);
  shutdown(oei);

  {Terminator for result sheet data}
  writeln(tmpp,'-3');
  close(tmpp, true);

  assign(tmpp, 'votes.tip');
  reset(tmpp);

  {Form output for result sheet}
  writeln(rslt,rules+100:1);
  For cand:=1 To numcandidates Do
    writeln(rslt,cand:1,' ',deemexcl[cand]:1,' ',keep[cand]:1);

  writeln(rslt,'0 0 0'); 

  {Copy from temporary file}
  For counter:=1 To numcandidates+2 Do
    Begin
      readln(tmpp,reply);
      While reply[1]='"' Do
        readln(tmpp, reply);
      writeln(rslt,reply)
    End;

  readln(tmpp,reply);

  continue := true;

  While ((reply[1]='"')Or(reply[1]='-'))And continue Do
    Begin
      If reply[1]='-' Then
        Begin
          writeln(rslt,reply);
          If reply[2]='3' Then continue := false;
          If continue Then readln(tmpp,reply)
        End

      Else readln(tmpp,reply)
    End;

  {Some of the order of items needs changing from tmpp to rst. 
  This change is effected by putting some items into ramm 
  temporarily}

  If Not okmm Then
    Begin
      erase(ramm);
      ramfile(ramm)
    End;

  Repeat
    rewrite(ramm);
    For counter:=1 To numcandidates+2 Do
      Begin
        writeln(ramm,reply);
        readln(tmpp,reply);
        While reply[1]='"' Do
          readln(tmpp, reply)
      End;

    Repeat
      writeln(rslt,reply);
      readln(tmpp,reply);

      While reply[1]='"' Do
        readln(tmpp, reply);

      finish := (reply[1]='-')And(reply[2]='3')
    Until finish Or(reply[1]<>'-');

    reset(ramm);

    For counter:=1 To numcandidates+2 Do
      Begin
        readln(ramm,str2);
        writeln(rslt,str2)
      End;

    If finish Then writeln(rslt,'-3')

  Until finish;

  reset(tmpp);

  {Copy footnote indicators to result file}
  finish := false;
  While Not finish Do
    If eof(tmpp)Then finish := true
    Else
      If peek(tmpp)=chr(219) Then finish := true                        {WM yingtongli}
    Else
      Begin
        readln(tmpp,reply);
        If reply[1]='"' Then writeln(rslt,reply)                        {WM For given votes.dat the footnote indicator is "1Basil }
      End;

  shutdown(rslt);
  close(tmpp,true);
  erase(tmpp);
  close(ramm,true);
  erase(ramm);
  anykey(25);
  cursoron;
  initscreen{note 37}

End.
