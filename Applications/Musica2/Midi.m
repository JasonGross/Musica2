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
  2004-11-29  bch :  added use of Convert for getting ConversionFunctions
  2004-11-28  bch :  added EventTypeKeySignature and EventTypeTimeSignature
  2004-10-06  bch :  Tidy[Track] now also converts all NoteOn's with zero velocity till NoteOff's
                     Counterpoint[Track] calls Tidy[Track]
  2004-10-04  bch :  not much, lost track... sorry
  2004-09-22  bch :  changed Show to Play2
  2004-09-18  bch :  added Tempo,TempoTrack and TempoFunction, removed SecondToTickFunction and TickToSecondFunction
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
                     added message ToDo
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
    "Musica2`Sound`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
]

Unprotect[
  Convert,
  Mix,
  Par,
  Seq
  ];

Unprotect[
  Event,
  EventQ,
  EventData,
  EventTime,
  EventType,
  EventTypeEOT,
  EventTypeMeta,
  EventTypeNoteOff,
  EventTypeNoteOn,
  EventTypeSysX0,
  EventTypeSysX7,
  EventTypeKeySignature,
  EventTypeTempo,
  EventTypeTimeSignature,
  FileFormat,
  Midi,
  MidiChannel,
  MidiQ,
  MilliSecond,
  QPM,
  Tempo,
  TempoFunction, (* to be removed *)
  TempoQ,
  TempoTime,
  TempoTrack,
  TempoTrackQ,
  Tick,
  TimeUnit,
  TPQ,
  Track,
  TrackQ
  ];

CreateElement[Event,{EventTime_,{EventType_,EventData_}},"todo\[NewLine]"];
CreateContainer[Track,Event,"todo\[NewLine]"];
CreateContainer[Midi,Track,"todo\[NewLine]"];

CreateElement[Tempo,{TempoTime_,QPM_},"todo\[NewLine]"];
CreateContainer[TempoTrack,Tempo,"todo\[NewLine]"];

EventData::usage = "todo"
EventTime::usage = "todo"
EventType::usage = "todo"
EventTypeEOT::usage = "todo"
EventTypeMeta::usage = "todo"
EventTypeNoteOff::usage = "todo"
EventTypeNoteOn::usage = "todo"
EventTypeSysX0::usage = "todo"
EventTypeSysX7::usage = "todo"
EventTypeKeySignature::usage = "todo"
EventTypeTempo::usage = "todo"
EventTypeTimeSignature::usage = "todo"
FileFormat::usage = "todo"
MidiChannel::usage = "todo"
MilliSecond::usage = "todo"
TempoFunction::usage = "" (* to be removed *)
Tick::usage = "todo"
TimeUnit::usage = "todo"
TPQ::usage = "todo"

Begin["`Private`"]

(*****************)

Options[Midi] = {QPM -> 120,FileFormat -> 1,TimeUnit -> Tick,TPQ -> 960}

Tidy[Midi] = Module[{r = #,eot = Duration[#]},
  r = Append[#,Event[{eot,EOT}]]& /@ r;
  r = Tidy /@ r;
  r
  ]&

Tidy[Track] = Module[{r = #,eot},
  r[[0]] = List;
  r[[2]] = Sort[r[[2]]]; (* todo: what about if the events has options? *)
  eot = r[[2,-1,1]];
  r[[2]] = Select[r[[2]],(#[[2,1]]=!=EventTypeEOT)&]; (* todo: what about if the events has options? *)
  r[[2]] = Append[r[[2]],{eot,EOT}];
  r[[2]] = If[MatchQ[#,{_,{EventTypeNoteOn,{_,_,0}}}],ReplacePart[#,EventTypeNoteOff,{2,1}],#]& /@ r[[2]]; (* todo: what about if the events has options? *)
  r[[0]] = Track;
  r
  ]&

Tidy[TempoTrack] = Module[{r = #},
  r[[0]] = List;
  r[[2]] = Sort[r[[2]]]; (* todo: what about if the events has options? *)
  r[[0]] = TempoTrack;
  r
  ]&

(*****************)

Midi  /: Chord[x_Midi] := Chord[Counterpoint[x]]
Track /: Chord[x_Track] := Chord[Counterpoint[x]]

Convert[Second,Tick,x_Midi] := TempoFunction[TempoTrack[x], True, TPQ->TPQ[x], QPM->QPM[x]]
Convert[Tick,Second,x_Midi] := TempoFunction[TempoTrack[x], False, TPQ->TPQ[x], QPM->QPM[x]]

Convert[Second,Tick,x_TempoTrack] := TempoFunction[x, True]
Convert[Tick,Second,x_TempoTrack] := TempoFunction[x, False]

Convert[Second,Tick,x_?NumberQ] := TempoFunction[TempoTrack[Tempo[{0,x}]], True]
Convert[Tick,Second,x_?NumberQ] := TempoFunction[TempoTrack[Tempo[{0,x}]], False]

Convert[m:{__Midi}, opts___?OptionQ] :=
  Module[{o},
    o = {
      TimeUnit->(TimeUnit/.{opts}/.(TimeUnit->TimeUnit[m[[1]]])),
      TPQ     ->(TPQ     /.{opts}/.(TPQ     ->TPQ[m[[1]]])),
      QPM     ->(QPM     /.{opts}/.(QPM     ->QPM[m[[1]]]))
      };
    Midi[#,Sequence @@ o]& /@ m (* todo: handle opts *)
    ]

Convert[m_Midi, MilliSecond, MilliSecond] := m
Convert[m_Midi, Second, Second] := m
Convert[m_Midi, Tick, Tick] := m

Convert[m_Midi, Tick, Second] :=
  Module[{f = TempoFunction[TempoTrack[m], False, TPQ->TPQ[m]]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
      Sequence @@ AddOpts[Opts[m],TimeUnit->Second]
      ]
    ]

Convert[m_Midi, Second, Tick] :=
  Module[{f = TempoFunction[TempoTrack[m], True, TPQ->TPQ[m]]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
      Sequence @@ AddOpts[Opts[m],TimeUnit->Tick]
      ]
    ]

Convert[m_Midi, Second, MilliSecond] :=
  Midi[
    (Event[{EventTime[#]*1000,{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
    Sequence @@ AddOpts[Opts[m],TimeUnit->MilliSecond]
    ]

Convert[m_Midi, MilliSecond, Second] :=
  Midi[
    (Event[{EventTime[#]/1000,{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
    Sequence @@ AddOpts[Opts[m],TimeUnit->Second]
    ]

Convert[m_Midi, Tick, MilliSecond] := Convert[Convert[m, Tick, Second], Second, MilliSecond]
Convert[m_Midi, MilliSecond, Tick] := Convert[Convert[m, MilliSecond, Second], Second, Tick]

Midi  /: Counterpoint[x_Midi,rtf_:(0&)] := Counterpoint[Select[Flatten[Melody[Counterpoint[#,rtf]]& /@ x],(0<Length[#])&]]
Track /: Counterpoint[x_Track,rtf_:(0&)] := (* todo: parameters of rtf are not set yet *)
  Module[{t=Tidy[x],on,off,n},
    (* get all note-on's as {{ch,p,time,v}...} *)
    on = {EventData[#][[1]],EventData[#][[2]],EventTime[#],EventData[#][[3]]}& /@ Select[t,MatchQ[Data[#],{_,{EventTypeNoteOn,{_,_,_}}}]&];
    (* get all note-on's as {{ch,p,time,v}...} *)
    off = {EventData[#][[1]],EventData[#][[2]],EventTime[#],EventData[#][[3]]}& /@ Select[t,MatchQ[Data[#],{_,{EventTypeNoteOff,{_,_,_}}}]&];
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
                If[tim[[v,1]]!=#[[1,1]], Sow[{{tim[[v,1]],#[[1,1]]},{DataNoValue,DataNoValue},#[[3]]},v]];
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
    ]

Midi  /: Duration[x_Midi]  := Max[Duration /@ x]
Track /: Duration[x_Track] := Max[EventTime /@ x]

Event[x_Chord]                  := Event[Midi[x]]
Event[x_Counterpoint]           := Event[Midi[x]]
Event[x_Melody]                 := Event[Track[x]]
Event[x_Note]                   := Event[Track[x]]
Event[x_Progression]            := Event[Midi[x]]
Event[x_Tempo, opts___?OptionQ] := Event[{#[[1]],{EventTypeTempo,{IntegerPart[#/65536],Mod[IntegerPart[#/256],256],Mod[#,256]}&[60000000/#[[2]]]}},opts]&[Data[x]]

EventTypeEOT = {EventTypeMeta,16^^2F};
EventTypeMeta = 16^^FF;
EventTypeNoteOff = 0;
EventTypeNoteOn = 1;
EventTypeSysX0 = 16^^F0;
EventTypeSysX7 = 16^^F7;
EventTypeKeySignature = {EventTypeMeta,16^^59};
EventTypeTempo = {EventTypeMeta,16^^51};
EventTypeTimeSignature = {EventTypeMeta,16^^58};

EOT = {EventTypeEOT,{}};

Midi /: Export[fn_String,mx_Midi] :=
  Module[{m=Midi[Tidy[mx],TimeUnit->Tick],f=Null},
    (* get the data and change timing to delta *)
    t = (Transpose[{ValuesToDeltas[Prepend[#[[1]],0]],#[[2]]}]&[Transpose[Data/@#]])&/@m;
    f = OpenWriteBinary[fn];
    Catch[
      WriteString[f,"MThd"];
      WriteInt[f,4,6];
      WriteInt[f,2,FileFormat /. Opts[m] /. Options[Midi]];
      WriteInt[f,2,Length[t]];
      WriteInt[f,2,TPQ /. Opts[m] /. Options[Midi]];
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

Midi  /: Melody[x_Midi]  := Melody[Counterpoint[x]]
Track /: Melody[x_Track] := Melody[Counterpoint[x]]

Midi[x_Chord,        opts___?OptionQ] := Midi[Track[x],opts]
Midi[x_Counterpoint, opts___?OptionQ] := Midi[Track[x],opts]
Midi[x_Melody,       opts___?OptionQ] := Midi[Track[x],opts]
Midi[x_Note,         opts___?OptionQ] := Midi[Track[x],opts]
Midi[x_Progression,  opts___?OptionQ] := Midi[Track[x],opts]

Midi[x_Event, opts___?OptionQ] := Midi[Track[x],opts]
Midi[x_Midi, opts___?OptionQ] :=
  Module[
    {
      m = x,
      qpmout = QPM /. {opts} /. (QPM->QPM[x]),
      qpmin  = QPM[x],
      tpqout = TPQ /. {opts} /. (TPQ->TPQ[x]),
      tpqin  = TPQ[x],
      tuout  = TimeUnit /. {opts} /. (TimeUnit->TimeUnit[x]),
      tuin   = TimeUnit[x]
      },
    If[qpmout!=qpmin || tpqout!=tpqin,
      If[tuin===Tick,m=Convert[m,tuin,tuin=Second]];
      m=Midi[Data[m],Sequence @@ AddOpts[Opts[m],TPQ->tpqout,QPM->qpmout]];
      ];
    If[tuout=!=tuin,m=Convert[m,tuin,tuout]];
    Midi[Data[m],Sequence @@ AddOpts[Opts[m],TimeUnit->tuout]] (* todo: handle opts *)
    ]

Mix[x:{__Midi}, opts___?OptionQ] :=
  Module[{t = Max[Length /@ x]},
    Midi[Mix /@ Transpose[Join[Track[#],Table[Track[Event[{}]],{t-Length[#]}]]& /@ Convert[x,opts]]]
    ]
Mix[x:{__Track}] := Tidy[Track[Flatten[Event /@ x]]]

Midi  /: Note[x_Midi]  := Note[Counterpoint[x]]
Track /: Note[x_Track] := Note[Counterpoint[x]]

Midi  /: NotePlot[x_Midi , s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]
Track /: NotePlot[x_Track, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Par[x:{__Midi}, opts___?OptionQ]  :=
  Module[{m=x,o},
    m = Convert[x,opts];
    o = Opts[m[[1]]];
    Midi[Flatten[Track /@ m],Sequence @@ o]
    ]
Par[x:{__Track}, opts___?OptionQ] := Midi[x,opts]

Midi  /: Play2[x_Midi, opts___?OptionQ] := Play2[Counterpoint[Midi[x,TimeUnit->Second]],opts]
Track /: Play2[x_Track,opts___?OptionQ] := Play2[Counterpoint[x],opts]

Midi  /: Progression[x_Midi]  := Progression[Counterpoint[x]]
Track /: Progression[x_Track] := Progression[Counterpoint[x]]

QPM[x_Midi] := QPM /. Opts[x] /. Options[Midi]

Seq[x:{__Midi}, opts___?OptionQ] :=
  Module[{t = Max[Length /@ x],m=x,o},
    m = Convert[m,opts];
    o = Opts[m[[1]]];
    m = Join[Track[#],Table[Track[Event[{}]],{t-Length[#]}]]& /@ m;
    m = Tidy[Midi[#]]& /@ m;
    m = Track /@ m;
    m = Transpose[m];
    m = Seq /@ m;
    m = Midi[m,Sequence @@ o];
    m
    ]
Seq[x:{__Track}] :=
  Module[{t=0,s},
    Tidy[Track[Flatten[Data[s=t;t+=Duration[#];Map[#+s&,#,EventTime]]& /@ x,1]]]
    ]

Midi  /: Snippet[x_Midi,  opts___?OptionQ] := Snippet[Counterpoint[Midi[x,TimeUnit->Second]],opts]
Track /: Snippet[x_Track, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Midi  /: Sound[x_Midi ,opts___?OptionQ] := Sound[Counterpoint[Midi[x,TimeUnit->Second]],opts]
Track /: Sound[x_Track,opts___?OptionQ] := Sound[Counterpoint[x],opts]

Tempo[x_Event, opts___?OptionQ] := Tempo[{#[[1]],60000000/Total[{65536, 256, 1}*#[[2,2]]]},opts]&[Data[x]] /; MatchQ[Data[x],{_,{EventTypeTempo,{_,_,_}}}]

TempoFunction[x_TempoTrack, inv:(True|False), opts___?OptionQ] := (* tick->sec *)
  Module[
    {
      tpq = TPQ/.{opts}/.Opts[x]/.Options[Midi],
      u = Data /@ Tidy[x],
      k, sa, sd, ta, td, f
      },
    If[Length[u]==0 || u[[1,1]] != 0, u = Prepend[u, {0, QPM /. {opts} /. Options[Midi]}]];
    (* convert to TPM *)
    u = {#[[1]], tpq#[[2]]} & /@ u;
    (* convert to TPS *)
    u = {#[[1]], #[[2]]/60} & /@ u;
    (* convert to SPT *)
    u = If[inv,{#[[1]], #[[2]]} & /@ u,{#[[1]], 1/#[[2]]} & /@ u];
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
    f = Function[s, Evaluate[(s - #[[1]])#[[2]] + #[[3]]]] & /@ f;

    (* make the resulting function *)
    Function[s, Evaluate[MakeNestedIfs[Transpose[{td, Drop[f, -1]}], f[[1]], f[[-1]]][s][s]]]
    ]

TempoTrack[x_Midi , opts___?OptionQ] := TempoTrack[x[[1]], opts, QPM -> (QPM /. {opts} /. Opts[x] /. Options[Midi])]
TempoTrack[x_Track, opts___?OptionQ] :=
  Module[{u = Data /@ Select[Tempo /@ x, TempoQ],tpq = TPQ[x]},
    (* add a default tempo if needed *)
    If[Length[u] == 0 || u[[1, 1]] != 0, u = Prepend[u, {0, QPM /. {opts} /. Options[Midi]}]];
    TempoTrack[u, Sequence@@RemOpts[{opts},QPM]]
    ]

TimeUnit[x_Midi] := TimeUnit /. Opts[x] /. Options[Midi]

TPQ[x_Midi] := TPQ /. Opts[x] /. Options[Midi]

Track[x_Chord]                         := Track[Counterpoint[x]]
Track[x_Counterpoint, opts___?OptionQ] := Track /@ x
Track[x_Melody, opts___?OptionQ]       :=
  Module[{d,t=0,c=MidiChannel/.{opts}/.Opts[x]/.{MidiChannel->0}},
    Track[
      If[#==={},#,#[[1]]]&
        [
          Reap[
            Scan[(
              d = NoteDuration[#];
              p = PitchCode[#];
              v = Velocity[#];
              If[!(DataNoValueQ[p] || DataNoValueQ[v]),
                Sow[Event[{t  ,{EventTypeNoteOn, {c,p,v}}}]];
                Sow[Event[{t+d,{EventTypeNoteOff,{c,p,v}}}]];
                ];
              t += d;
              )&,
              x
              ];
            ][[2]]
          ]
      ,
      Sequence@@RemOpts[{opts},MidiChannel]
      ]
    ]
Track[x_Note]                          := Track[Melody[x]]
Track[x_Progression]                   := Track[Counterpoint[x]]
Track[x_TempoTrack, opts___?OptionQ]   := Track[Event/@x]

(******** private functions used by Import and Export ********)

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
    r=IntegerDigits[Round[i],16^^80];
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

ListEvent[e_]:=
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

ListTrack[t_]:= ListEvent /@ t

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
  Convert,
  Mix,
  Par,
  Seq
  ];

Protect[
  Event,
  EventQ,
  EventData,
  EventTime,
  EventType,
  EventTypeEOT,
  EventTypeMeta,
  EventTypeNoteOff,
  EventTypeNoteOn,
  EventTypeSysX0,
  EventTypeSysX7,
  EventTypeKeySignature,
  EventTypeTempo,
  EventTypeTimeSignature,
  FileFormat,
  Midi,
  MidiChannel,
  MidiQ,
  MilliSecond,
  QPM,
  Tempo,
  TempoFunction,
  TempoQ,
  TempoTime,
  TempoTrack,
  TempoTrackQ,
  Tick,
  TimeUnit,
  TPQ,
  Track,
  TrackQ
  ];

EndPackage[ ]

