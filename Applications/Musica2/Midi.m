(* :Title: Midi *)

(* :Summary: Functions for Midi *)

(* :Author: Bo C. Herlin *)

(* :Licence: GPL

Copyright (C) 2004  Bo C. Herlin

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

(* :Contact: bo@gcab.net *)

(* :Context: Musica2`Midi` *)

(* :History:
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-13  bch :  added Track- and Event-object's
  2004-09-12  bch :  using Common.m, MidiGetDuration(s) is now GetDuration(s), MidiGetInfo is now GetInfo
  2004-09-11  bch :  added use of Note.m's Chord- and Melody-types
                     added use of Util.m's ParOfSeqToSeqOfPar and SeqOfParToParOfSeq
  2004-09-??  bch :  changed all Melodys to Melodies, ok?
  2004-09-04  bch :  added MidiQ
  2004-09-02  bch :  renamed MidiData*Value* to Data*Value* and moved them to Utils.m
                     renamed MidiTie* to DataTie* and moved them to Utils.m
                     renamed MidiVoice to MidiMelody
  2004-09-01  bch :  added MidiPitchCenter
  2004-08-27  bch :  simplified Format so that ?? wont get messy, need help here...
                     added message todo
  2004-08-26  bch :  added some help/usage-text
  2004-08-23  bch :  added MidiFixTime, MidiSet(Pitch|Time)
                     bugfix in going from shape file to voice
                     added MidiGetPitchRange(s)
  2004-08-22  bch :  added Midi(Pitch|Time)(Bend|Flip|Shift)
  2004-08-17  bch :  added MidiSetTPQ
  2004-08-17  bch :  added Midi(Add|Get|Rem|Set)(Chords|Voices)
                     corrected trackhandling when changing from shape MidiVoice to MidiFile
                     added MidiOfSilence and MidiPitchShift
  2004-08-15  bch :  dropped the voice-nr from shape voice and chord
                     renamed MidiAddEOT to MidiFixEOT and MidiNormalizeNoteOff to MidiFixNoteOff
  2004-08-14  bch :  yet another rest/tie scheme, now with MidiDataAnyValue and MidiDataNoValue, MidiRest* is removed
                     added MidiMilliSec as an integer MidiTimeUnit
                     added MidiPar and MidiSeq
                     changed MidiEmpty to have one empty track rather than no track at all
  2004-08-13  bch :  added Midi(Add|Get|Rem|Set)(Notes|QPM) and MidiEmpty
                     renamed MidiEqualizeEOT to MidiAddEOT
  2004-08-10  bch :  removed MidiAddEOT, now incorporated into MidiEqualizeEOT
  2004-08-10  bch :  added MidiRest* and MidiTie*
  2004-08-09  bch :  used Reap/Sow and AppendTo as a speed boost, changed param to MidiVoiceReleaseTimeFunction
  2004-08-08  bch :  added this whole "state"-thing, all the MidiSetXxx called by a MidiSetStateLow will have its code moved to its caller and then be deleted
  2004-08-06  bch :  extended MidiVoiceReleaseTimeFunction to also take pitch and velocity
                     added some help/usage-text
  2004-08-04  bch :  first release
  2004-07-28  bch :  created
*)

(* :Keywords: music, midi *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Midi`",
  {
    "Utilities`BinaryFiles`",
    "Musica2`Common`",
    "Musica2`Note`",
    "Musica2`Utils`"
    }
]

Unprotect[
  ];

Event::usage = ""
EventData::usage = ""
EventQ::usage = ""
EventTime::usage = ""
EventType::usage = ""
EventTypeEOT::usage = ""
EventTypeMeta::usage = ""
EventTypeNoteOff::usage = ""
EventTypeNoteOn::usage = ""
EventTypeSysX0::usage = ""
EventTypeSysX7::usage = ""
EventTypeTempo::usage = ""
FileFormat::usage = ""
Midi::usage = ""
MidiQ::usage = ""
MilliSecond::usage = ""
QPM::usage = ""
SecondToTickFunction::usage = ""
Tick::usage = ""
TickToSecondFunction::usage = ""
TimeUnit::usage = ""
TPQ::usage = ""
Track::usage = ""
TrackQ::usage = ""

Begin["`Private`"]

seti[i_,n_,v_] := If[(n/.i)===n,Append[i,n->v],i/.(n->_)->(n->v)]

(*****************)

DataQ[Event] = MatchQ[#, {_, {_,_}}]&
DefineSub[Event];

DefineSup[Midi,Track];
Options[Midi] = {QPM -> 120,FileFormat -> 1,TimeUnit -> Tick,TPQ -> 960}

DefineSup[Track,Event];

(*****************)

Convert[m_?MidiQ, Tick, Second] :=
  Module[{f=TickToSecondFunction[m]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence@@Info[#]]&/@#)&/@Track[m],
      Sequence @@ seti[Info[m],TimeUnit,Second]
      ]
    ]

Convert[m_?MidiQ, Second, Tick] :=
  Module[{f=SecondToTickFunction[m]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence@@Info[#]]&/@#)&/@Track[m],
      Sequence @@ seti[Info[m],TimeUnit,Tick]
      ]
    ]

Convert[m_?MidiQ, Second, MilliSecond] :=
  Midi[
    (Event[{EventTime[#]*1000,{EventType[#],EventData[#]}},Sequence@@Info[#]]&/@#)&/@Track[m],
    Sequence @@ seti[Info[m],TimeUnit,MilliSecond]
    ]

Convert[m_?MidiQ, MilliSecond, Second] :=
  Midi[
    (Event[{EventTime[#]/1000,{EventType[#],EventData[#]}},Sequence@@Info[#]]&/@#)&/@Track[m],
    Sequence @@ seti[Info[m],TimeUnit,Second]
    ]

Convert[m_?MidiQ, Tick, MilliSecond] := Convert[Convert[m, Tick, Second], Second, MilliSecond]

Convert[m_?MidiQ, MilliSecond, Tick] := Convert[Convert[m, MilliSecond, Second], Second, Tick]

Midi /: Counterpoint[x_Midi,rtf_:((0)&)] := Counterpoint[Select[Flatten[Melody[Counterpoint[#]]& /@ x],(0<Length[#])&]] /; MidiQ[x]

Track /: Counterpoint[x_Track,rtf_:((0)&)] :=
  Module[{on,off,n},
    (* get all note-on's as {{ch,p,time,v}...} *)
    on = {EventData[#][[1]],EventData[#][[2]],EventTime[#],EventData[#][[3]]}& /@ Select[x,MatchQ[Data[#],{_,{EventTypeNoteOn,{_,_,_}}}]&];
    (* get all note-on's as {{ch,p,time,v}...} *)
    off = {EventData[#][[1]],EventData[#][[2]],EventTime[#],EventData[#][[3]]}& /@ Select[x,MatchQ[Data[#],{_,{EventTypeNoteOff,{_,_,_}}}]&];
    (* sort them *)
    on = Sort[on];
    off = Sort[off];
    (* merge them to {{ch, {p, v}, {on, off}}...} *)
    n = MapThread[{#1[[1]], {#1[[2]],#1[[4]]}, {#1[[3]], #2[[3]]}} &, {on, off}];
    (* group by ch to {{{ch1,{p, v}, {on, off}}...}, {{ch2,{p, v}, {on, off}}...}...} *)
    n = Split[n, (#1[[1]] === #2[[1]]) &];
    (* then reverse each event to get {{{{on, off}, {p, v}, ch}...}...} *)
    n = (Reverse /@ #) & /@ n;
    (* make monophonic melodies, set to {{{{{on, off}, {p, v}, ch}...}...}...} *)
    n =
      Function[trni, (* trni is {{{on, off}, {p, v}, ch}...} *)
        Reap[
          Module[{tim={{0,1}}},(* tim is {{off,voice}...} *)
            Scan[
              Module[{pos,rt,v}, (* # is {{on, off}, {p, v}, ch} *)
                pos=Cases[tim,{e$_,_}/;(e$<=#[[1,1]])];
                If[0<Length[pos],
                  v=Sort[pos][[-1,2]],
                  AppendTo[tim,{0,v=(Length[tim]+1)}]
                  ];
                (* add a rest ? *)
                If[tim[[v,1]]!=#[[1,1]],
                  (* add a tuple, not just an atom, so that Transpose works when trying to get pitch *)
                  Sow[{{tim[[v,1]],#[[1,1]]},{DataNoValue,DataNoValue},#[[3]]},v]
                  ];
                (* calculate release-time *)
                rt = rtf[{#[[1]],Prepend[#[[2]],#[[3]]]}];
                (* update tim *)
                tim[[v]]={#[[1,2]]+rt,v};
                (* add the note *)
                Sow[#,v];
                ]&,
              Sort[trni]
              ];
            ];
          ][[2]]
        ] /@ n;
    (* set to {{{{on, off}, {p, v}, ch}...}...} *)
    n = Flatten[n, 1];
    (* set to {{ch, {{({p, v}|{DataNoValue, DataNoValue}), tick}...}}...} *)
    n = {#[[1, 3]], Flatten[{{{#[[2, 1]], #[[2, 2]]}, #[[1, 1]]}, {{DataNoValue, DataNoValue}, #[[1, 2]]}} & /@ #, 1]} & /@ n;
    (* add a preceding rest if necesary, is it ever? *)
    n = {#[[1]], If[#[[2, 1, 2]] == 0, #[[2]], Prepend[#[[2]], {{DataNoValue, DataNoValue}, 0}]]} & /@ n;
    (* add a trailing dummy-rest *)
    n = {#[[1]], Append[#[[2]], {{DataNoValue, DataNoValue}, que}]} & /@ n;
    (* transpose to {{ch, {{{p, v}...}, {tick...}}}...} *)
    n = {#[[1]], Transpose[#[[2]]]} & /@ n;
    (* switch to durations and chop off the dummy-rest {{ch, {{duration...}, {data...}}}...} *)
    n = {#[[1]], {ValuesToDeltas[#[[2, 2]]], Drop[#[[2, 1]], -1]}} & /@ n;

    (* transpose to {{ch, {{duration, data}...}}...} *)
    n = {#[[1]], Transpose[#[[2]]]} & /@ n;
    (* get all with duration != 0 *)
    n = {#[[1]], Select[#[[2]], (#[[1]] != 0) &]} & /@ n;
    (* get all nonempty voices *)
    n = Select[n, (#[[2]] =!= {}) &];

    Counterpoint[Melody[#[[2]],MidiChannel->#[[1]]]& /@ n]
    ] /; TrackQ[x]

Midi /: Duration[x_Midi] := Max[Duration /@ x] /; MidiQ[x]
Track /: Duration[x_Track] := Max[EventTime /@ x] /; TrackQ[x]

Event /: EventData[x_Event] := Data[x][[2,2]] /; EventQ[x]
Event /: EventTime[x_Event] := Data[x][[1]] /; EventQ[x]
Event /: EventType[x_Event] := Data[x][[2,1]] /; EventQ[x]

EventTypeEOT = {EventTypeMeta,16^^2F};
EventTypeMeta = 16^^FF;
EventTypeNoteOff = 0;
EventTypeNoteOn = 1;
EventTypeSysX0 = 16^^F0;
EventTypeSysX7 = 16^^F7;
EventTypeTempo = {EventTypeMeta,16^^51};

EOT = {EventTypeEOT,{}};

Midi /: Export[fn_String,mx_Midi] :=
  Module[{m=Midi[mx,TimeUnit->Tick],f=Null},
    (*m = MidiFixEOT[m,True];*)
    (* det the data and change timing to delta *)
    t = (Transpose[{ValuesToDeltas[Prepend[#[[1]],0]],#[[2]]}]&[Transpose[Sort[Data/@#]]])&/@m;
    f = OpenWriteBinary[fn];
    Catch[
      WriteString[f,"MThd"];
      WriteInt[f,4,6];
      WriteInt[f,2,FileFormat /. Info[m] /. Options[Midi]];
      WriteInt[f,2,Length[t]];
      WriteInt[f,2,TPQ /. Info[m] /. Options[Midi]];
      WriteTrack[f,#]&/@t;
      Flush[f];
      Close[f];
      ,
      _,
      (Print["failure ",#1];Close[f];#1)&
      ];
    m
    ]

Midi /: Import[fn_String,Midi]:=
  Module[
    {
      f=Null,
      h={TimeUnit->Tick},
      t={},
      n
      },
    f=OpenReadBinary[fn];(*,DOSTextFormat->False];*)
    Catch[
      If[StringJoin@@ReadList[f,Character,4]!="MThd",error];
      If[ReadInt[f,4]!=6,error];
      AppendTo[h,FileFormat->ReadInt[f,2]];
      n=ReadInt[f,2];
      AppendTo[h,TPQ->ReadInt[f,2]];
      Do[AppendTo[t,ReadTrack[f]],{n}];
      Close[f];
      Midi[t,Sequence @@ Sort[h]],
      _,
      (Print["failure ",#1];Close[f];#1)&
      ]
    ]

Midi[mx_Midi, opts___?OptionQ] :=
  Module[{m=mx,tuout=TimeUnit/.{opts}/.(TimeUnit->TimeUnit[mx]),tuin=TimeUnit[mx]},
    If[tuout=!=tuin,m=Convert[m,tuin,tuout]];
    m
    ]

Midi /: QPM[x_Midi] :=
  Module[{u = {#[[1]],#[[2,2]]}& /@ (Data /@ Select[x[[1]],MatchQ[Data[#],{_,{EventTypeTempo,{_,_,_}}}]&]),tpq = TPQ[x]},
    (* convert to USPQ *)
    u = {#[[1]], Total[{65536, 256, 1}*#[[2]]]} & /@ u;
    (* convert to QPM *)
    u = {#[[1]], 60000000/#[[2]]} & /@ u;
    (* add a default tempo if needed *)
    If[Length[u] == 0 || u[[1, 1]] != 0, u = Prepend[u, {0, QPM /. Info[x] /. Options[Midi]}]];
    u
    ] /; MidiQ[x]

SecondToTickFunction[m_?MidiQ] :=
  Module[
    {
      tpq = TPQ[m],
      u = QPM[m],
      k, sa, sd, ta, td, f
      },
    (* convert to TPM *)
    u = {#[[1]], tpq#[[2]]} & /@ u;
    (* convert to TPS *)
    u = {#[[1]], #[[2]]/60} & /@ u;
    (* transpose u *)
    u = Transpose[u];

    (* get k *)
    k = u[[2]];
    (* get sec *)
    sa = u[[1]];
    sd = ValuesToDeltas[sa];
    (* get tick *)
    td = Drop[k, -1]sd;
    ta = DeltasToValues[td];

    (* make a list of {s, k, t} *)
    f = Transpose[{sa, k, ta}];
    (* make a list of functions *)
    f = Function[x, Evaluate[(x - #[[1]])#[[2]] + #[[3]]]] & /@ f;

    (* make the resulting function *)
    Function[t, Evaluate[MakeNestedIfs[Transpose[{sd, Drop[f, -1]}], f[[1]], f[[-1]]][t][t]]]
    ] /; TimeUnit[m] === Second

Midi /: Show[x_Midi,opts___?OptionQ] := Show[Mix[Sound[x,opts],2]] /; MidiQ[x]

Midi /: Sound[x_Midi,opts___?OptionQ] := Sound[Counterpoint[Midi[x,TimeUnit->Second]],opts] /; MidiQ[x]

TickToSecondFunction[m_?MidiQ] :=
  Module[
    {
      tpq = TPQ[m],
      u = QPM[m],
      k, sa, sd, ta, td, f
      },
    (* convert to TPM *)
    u = {#[[1]], tpq#[[2]]} & /@ u;
    (* convert to TPS *)
    u = {#[[1]], #[[2]]/60} & /@ u;
    (* convert to SPT *)
    u = {#[[1]], 1/#[[2]]} & /@ u;
    (* transpose u *)
    u = Transpose[u];

    (* get k *)
    k = u[[2]];
    (* get tick *)
    ta = u[[1]];
    td = ValuesToDeltas[ta];
    (* get sec *)
    sd = Drop[k, -1]td;
    sa = DeltasToValues[sd];

    (* make a list of {t, k, s} *)
    f = Transpose[{ta, k, sa}];
    (* make a list of functions *)
    f = Function[x, Evaluate[(x - #[[1]])#[[2]] + #[[3]]]] & /@ f;

    (* make the resulting function *)
    Function[t, Evaluate[MakeNestedIfs[Transpose[{td, Drop[f, -1]}], f[[1]], f[[-1]]][t][t]]]
    ] /; TimeUnit[m] === Tick

Midi /: TimeUnit[x_Midi] := (TimeUnit /. Info[x] /. Options[Midi]) /; MidiQ[x]

Midi /: TPQ[x_Midi] := (TPQ /. Info[x] /. Options[Midi]) /; MidiQ[x]

(******** private functions used by ImportSMF and ExportSMF ********)

ReadInt[f_,n_]:=Total[ReadList[f,Byte,n] Table[256^i,{i,n-1,0,-1}]]

WriteInt[f_,n_,i_]:=
  Module[{b=IntegerDigits[i,256]},
    b=Join[Table[0,{n-Length[b]}],b];
    WriteBinary[f,b,ByteConversion->Identity]
    ]

ReadVarLen[f_]:=
  Module[{r=0,b=16^^80},
    While[16^^80<=b,
      b=Read[f,Byte];
      r=r*16^^80+BitAnd[b,16^^7F]
      ];
    r
    ]

ListVarLen[i_]:=
  Module[{r={}},
    r=IntegerDigits[i,16^^80];
    Join[16^^80+Drop[r,-1],Take[r,-1]]
    ]

ReadSysX[f_,type_] :=
  Module[{s},
    s=ReadVarLen[f];
    If[s!=0,
      {type,ReadList[f,Byte,s]},
      {type,{}}
      ]
    ]

ReadMeta[f_] :=
  Module[{subtype,s},
    subtype=Read[f,Byte];
    s=ReadVarLen[f];
    If[s!=0,
      {{EventTypeMeta,subtype},ReadList[f,Byte,s]},
      {{EventTypeMeta,subtype},{}}
      ]
    ]

ReadChannel[f_,r_] :=
  Module[{type,channel,s,x={}},
    type=BitAnd[IntegerPart[r[[1]]/2^4],16^^7];
    channel=BitAnd[r[[1]],16^^F];
    s={3,3,3,3,2,2,3}[[type+1]]-Length[r];
    If[s!=0,x=ReadList[f,Byte,s]];
    {type,Join[{channel},Drop[r,1],x]}
    ]

ReadEvent[f_,rt_]:=
  Module[{d=ReadVarLen[f],b,r},
    b=Read[f,Byte];
    If[b==EventTypeSysX0||b==EventTypeSysX7,
      {rt,{d,ReadSysX[f,b]}},
      If[b==EventTypeMeta,
        {rt,{d,ReadMeta[f]}},
        r=If[b<16^^80,{rt,b},{b}];
        {r[[1]],{d,ReadChannel[f,r]}}
        ]
      ]
    ]

ReadTrack[f_]:=
  Module[{e={0,{0,{}}},rt=0,ti=0,pos},
    If[StringJoin@@ReadList[f,Character,4]!="MTrk",error];
    pos=StreamPosition[f]+ReadInt[f,4]+4;
    If[0<Length[#],#[[1]],{}]&[
      Reap[
        While[e[[2,2]]!=EOT && StreamPosition[f]<pos,
          e=ReadEvent[f,rt];
          rt=e[[1]];
          e[[2,1]] = ti += e[[2,1]];
          Sow[e[[2]]];
          ];
        If[StreamPosition[f]!=pos,error];
        ][[2]]
      ]
    ]

ListSysX[{type_,data_}]:=
  {type,Length[data],data}

ListMeta[{type:{_,sybtype_},data_}]:=
  {type,Length[data],data}

ListChannel[{type_,{channel_,data__}}]:=
  {16^^80+2^4type+channel,data}

ListTrack[t_]:=
  Function[e,
    {
      ListVarLen[e[[1]]],
      If[MatchQ[e[[2,1]],EventTypeSysX0|EventTypeSysX7],
        ListSysX[e[[2]]],
        If[MatchQ[e[[2,1]],{EventTypeMeta,_}],
          ListMeta[e[[2]]],
          ListChannel[e[[2]]]
          ]
        ]
      }
    ]/@t

WriteTrack[f_,t_]:=
  Module[{w=t},
    w=ListTrack[w];
    w=Mod[#,256]&/@(Abs/@(Round/@Flatten[w]));
    WriteString[f,"MTrk"];
    WriteInt[f,4,Length[w]];
    WriteBinary[f,w,ByteConversion->Identity]
    ]

End[]

Protect[
  ];

EndPackage[ ]

