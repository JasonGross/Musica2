(* :Title: Midi *)

(* :Summary: Functions for Midi *)

(* :Author: Bo C. Herlin *)

(* :License: GPL

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
  2005-02-16  bch :  initiated usage of Usage ;-)
  2005-02-13  bch :  reorganized code in file, hopefully in an uniform manner
  2005-02-09  bch :  moved the DurVal-code to DurVal.m and commented it out
  2005-02-08  bch :  bugfix, and changed error-handling in Import/Export
  2005-01-31  bch :  moved TimedNote to doc as an example
                     bugfix in TotalDuration[x_Track]
  2005-01-31  bch :  added some EventType* constants
                     added the experimental object-type TimedNote
  2005-01-25  bch :  made Export work with both 5.0 and 5.1
                     yet another bugfix in Tidy[Track]
  2005-01-23  bch :  changed how channel-events are stored as Event's
  2005-01-12  bch :  bugfix in Tidy[Track]
  2004-12-21  bch :  Tidy[Track] now much slower, but handles opts, i think...
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

Module[
  {
    p = {
      "Musica2`Common`",
      "Musica2`Note`",
      "Musica2`ObjectType`",
      "Musica2`Sound`",
      "Musica2`Test`",
      "Musica2`Usage`",
      "Musica2`Utils`"
      }
    },
    If[$VersionNumber <= 5.0,p = Prepend[p,"Utilities`BinaryFiles`"]];
    BeginPackage["Musica2`Midi`",p]
  ];

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
  EventTypeKeyPressure,
  EventTypeControlChange,
  EventTypeProgramChange,
  EventTypeChannelPressure,
  EventTypePitchBend,
  EventTypeSysX0,
  EventTypeSysX7,
  EventTypeKeySignature,
  EventTypeTempo,
  EventTypeTimeSignature,
  FileFormat,
  Midi,
  MidiQ,
  MilliSecond,
  QPM,
  Tempo,
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

CreateElement[Musica2,Event,{EventTime_,{EventType_,EventData_}},{0,{EventTypeEOT,{}}},
"todo.\[NewLine]"
];
CreateContainer[Musica2,Track,Event,
"todo.\[NewLine]"
];
CreateContainer[Musica2,Midi,Track,
"todo.\[NewLine]"
];

CreateElement[Musica2,Tempo,{TempoTime_,QPM_},{0,120},
"todo.\[NewLine]"
];
CreateContainer[Musica2,TempoTrack,Tempo,
"todo.\[NewLine]"
];

EventData::usage = "todo"
EventTime::usage = "todo"
EventType::usage = "todo"
EventTypeEOT::usage = "todo"
EventTypeMeta::usage = "todo"
EventTypeNoteOff::usage = "todo"
EventTypeNoteOn::usage = "todo"
EventTypeKeyPressure::usage = "todo"
EventTypeControlChange::usage = "todo"
EventTypeProgramChange::usage = "todo"
EventTypeChannelPressure::usage = "todo"
EventTypePitchBend::usage = "todo"
EventTypeSysX0::usage = "todo"
EventTypeSysX7::usage = "todo"
EventTypeKeySignature::usage = "todo"
EventTypeTempo::usage = "todo"
EventTypeTimeSignature::usage = "todo"
FileFormat::usage = "todo"
MilliSecond::usage = "todo"
QPM::usage = "todo"
TempoTime::usage = "todo"
Tick::usage = "todo"
TimeUnit::usage = "todo"
TPQ::usage = "todo"

Begin["`Private`"]

(* Event +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Event modifications and interceptions ****************************************************)

(* Event constructors ***********************************************************************)

Usage[Append,Musica2,Event,{_Chord},{{___Event}..},"todo"];
Event[x_Chord] := Event[Midi[x]]

Usage[Append,Musica2,Event,{_Counterpoint},{{___Event}..},"todo"];
Event[x_Counterpoint] := Event[Midi[x]]

Usage[Append,Musica2,Event,{_Melody},{___Event},"todo"];
Event[x_Melody] := Event[Track[x]]

Usage[Append,Musica2,Event,{_Note},{___Event},"todo"];
Event[x_Note] := Event[Track[x]]

Usage[Append,Musica2,Event,{_Progression},{{___Event}..},"todo"];
Event[x_Progression] := Event[Midi[x]]

Usage[Append,Musica2,Event,{_Tempo, ___?OptionQ},_Event,"todo"];
Event[x_Tempo, opts___?OptionQ] := Event[{#[[1]],{EventTypeTempo,{IntegerPart[#/65536],Mod[IntegerPart[#/256],256],Mod[#,256]}&[60000000/#[[2]]]}},opts]&[Data[x]]

(* Event reverse constructors ***************************************************************)

(* Event common functions *******************************************************************)

(* todo: is this wise? *)
Usage[Append,Musica2,TotalDuration,{_Event},_,"todo"];
Event /: TotalDuration[x_Event] := EventTime[x]

(* Event unique functions *******************************************************************)

EventTypeEOT = {EventTypeMeta,16^^2F};
EventTypeMeta = 16^^FF;
EventTypeNoteOff = 0;
EventTypeNoteOn = 1;
EventTypeKeyPressure = 2;
EventTypeControlChange = 3;
EventTypeProgramChange = 4;
EventTypeChannelPressure = 5;
EventTypePitchBend = 6;
EventTypeSysX0 = 16^^F0;
EventTypeSysX7 = 16^^F7;
EventTypeKeySignature = {EventTypeMeta,16^^59};
EventTypeTempo = {EventTypeMeta,16^^51};
EventTypeTimeSignature = {EventTypeMeta,16^^58};

EOT = {EventTypeEOT,{}};

(* Event tests ******************************************************************************)

Event /: TestSuite[Event] = Join[TestSuite[Event],{
  }];

(* Event -----------------------------------------------------------------------------------*)

(* Track +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Track modifications and interceptions *)

(* Track constructors ***********************************************************************)

Usage[Append,Musica2,Track,{{DataNoValue, _},___?OptionQ},_Track,"todo"];
Track[{DataNoValue, d_},opts___?OptionQ] := Track[Event[{d, {EventTypeEOT, {}}}],opts]

Usage[Append,Musica2,Track,{_?NumberQ,___?OptionQ},_Track,"todo"];
Track[x_?NumberQ,opts___?OptionQ] := Track[{DataNoValue, x},opts]

Usage[Append,Musica2,Track,{_Chord},{___Track},"todo"];
Track[x_Chord] := Track[Counterpoint[x]]

Usage[Append,Musica2,Track,{_Counterpoint, ___?OptionQ},{___Track},"todo"];
Track[x_Counterpoint, opts___?OptionQ] := Track /@ x

Usage[Append,Musica2,Track,{_Melody, ___?OptionQ},_Track,"todo"];
Track[x_Melody, opts___?OptionQ]       :=
  Module[{d,t=0,c=MidiChannel/.{opts}/.Opts[x]/.{MidiChannel->0}},
    Track[
      If[#==={},#,#[[1]]]&
        [
          Reap[
            Scan[(
              d = Duration[#];
              p = PitchCode[#];
              v = Velocity[#];
              If[!(DataNoValueQ[p] || DataNoValueQ[v]),
                Sow[Event[{t  ,{{EventTypeNoteOn,c}, {p,v}}}]];
                Sow[Event[{t+d,{{EventTypeNoteOff,c},{p,v}}}]];
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

Usage[Append,Musica2,Track,{_Note},_Track,"todo"];
Track[x_Note] := Track[Melody[x]]

Usage[Append,Musica2,Track,{_Progression},{___Track},"todo"];
Track[x_Progression] := Track[Counterpoint[x]]

Usage[Append,Musica2,Track,{_TempoTrack, ___?OptionQ},_Track,"todo"];
Track[x_TempoTrack, opts___?OptionQ] := Track[Event/@x]

(* Track reverse constructors ***************************************************************)

Usage[Append,Musica2,Chord,{_Track},{___Chord},"todo"];
Track /: Chord[x_Track] := Chord[Counterpoint[x]]

Usage[Append,Musica2,Counterpoint,{_Track, _:(0&)},_Counterpoint,"todo"];
Track /: Counterpoint[x_Track,rtf_:(0&)] := (* todo: parameters of rtf are not set yet *)
  Module[{t=Tidy[x],on,off,n},
    (* get all note-on's as {{ch,p,time,v}...} *)
    on = {EventType[#][[2]],EventData[#][[1]],EventTime[#],EventData[#][[2]]}& /@ Event[Select[t,MatchQ[Data[#],{_,{{EventTypeNoteOn,_},{_,_}}}]&]];
    (* get all note-on's as {{ch,p,time,v}...} *)
    off = {EventType[#][[2]],EventData[#][[1]],EventTime[#],EventData[#][[2]]}& /@ Event[Select[t,MatchQ[Data[#],{_,{{EventTypeNoteOff,_},{_,_}}}]&]];
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

Usage[Append,Musica2,Melody,{_Track},{___Melody},"todo"];
Track /: Melody[x_Track] := Melody[Counterpoint[x]]

Usage[Append,Musica2,Note,{_Track},{{___Note}..},"todo"];
Track /: Note[x_Track] := Note[Counterpoint[x]]

Usage[Append,Musica2,Progression,{_Track},_Progression,"todo"];
Track /: Progression[x_Track] := Progression[Counterpoint[x]]

Usage[Append,Musica2,Snippet,{_Track},{___Snippet},"todo"];
Track /: Snippet[x_Track, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Usage[Append,Musica2,Sound,{_Track},_Sound,"todo"];
Track /: Sound[x_Track,opts___?OptionQ] := Sound[Counterpoint[x],opts]

(* Track common functions *******************************************************************)

Usage[Append,Musica2,Mix,{_Track},_Track,"todo"];
Mix[x_Track] := Tidy[x]

Usage[Append,Musica2,Mix,{{__Track}},_Track,"todo"];
Mix[x:{__Track}] := Tidy[Track[Flatten[Event /@ x]]]

Usage[Append,Musica2,Par,{{__Track},___?OptionQ},_Midi,"todo"];
Par[x:{__Track}, opts___?OptionQ] := Midi[x,opts]

Usage[Append,Musica2,Play2,{_Track,___?OptionQ},_Sound,"todo"];
Track /: Play2[x_Track,opts___?OptionQ] := Play2[Counterpoint[x],opts]

Usage[Append,Musica2,Seq,{{__Track}},_Track,"todo"];
Seq[x:{__Track}] :=
  Module[{t=0,s},
    Tidy[Track[Flatten[Data[s=t;t+=TotalDuration[#];Map[#+s&,#,EventTime]]& /@ x,1]]]
    ]

Usage[Append,Musica2,Tidy,{Track},_Function,"todo"];
Tidy[Track] = Module[{r = #,eot},
  r = Sort[r,{EventTime,EventType}];
  r = Event[r];
  eot = EventTime[r[[-1]]];
  r = Select[r,(EventType[#]=!=EventTypeEOT)&];
  r = Append[r,Event[{eot,EOT}]];
  (* todo rewrite next line *)
  r = (
    If[MatchQ[EventType[#],{EventTypeNoteOn,_}] && EventData[#][[2]]===0,
      ReplacePart[#,{EventTypeNoteOff,EventType[#][[2]]},EventType],
      #
      ]
    )& /@ r;
  r = Track[r, Sequence @@ Opts[#]];
  r
  ]&

Usage[Append,Musica2,TotalDuration,{_Track},_,"todo"];
Track /: TotalDuration[x_Track] := Max[EventTime /@ Event[x]]

(* Track unique functions *******************************************************************)

Usage[Append,Musica2,NotePlot,{_Track, _Symbol, ___?OptionQ},_Graphics,"todo"];
Track /: NotePlot[x_Track, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

(* Track tests ******************************************************************************)

Track /: TestSuite[Track] = Join[TestSuite[Track],{
  }];

(* Track -----------------------------------------------------------------------------------*)

(* Midi ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Midi modifications and interceptions *****************************************************)

Options[Midi] = {QPM -> 120,FileFormat -> 1,TimeUnit -> Tick,TPQ -> 960};

Midi /: Opts[x_Midi,y_,TimeUnit] :=
  Module[{m = x},
    If[y =!= TimeUnit[x] && MemberQ[{Tick,Second,MilliSecond},y],
      m = TimeUnitChange[m,TimeUnit[m],y];
      m = Opts[m,y,TimeUnit,False];
      ];
    m
    ]

Midi /: Opts[x_Midi,y_,TPQ] :=
  Module[{m = x,tpq},
    If[y =!= TPQ[x] && NumberQ[y],
      If[TimeUnit[m] === Tick, m = Opts[m,Second,TimeUnit]];
      m = Opts[m,y,TPQ,False];
      m = Opts[m,TimeUnit[x],TimeUnit];
      ];
    If[y === Tidy,
      m = Opts[Opts[m,1,TPQ],Tick,TimeUnit];
      tpq = LCM @@ (Denominator /@ Complement[Union[Flatten[EventTime[m]]], {0}]);
      m = Opts[x,tpq,TPQ];
      ];
    m
    ]

(* Midi constructors ************************************************************************)

Usage[Append,Musica2,Midi,{_Chord, ___?OptionQ},_Midi,"todo"];
Midi[x_Chord, opts___?OptionQ] := Midi[Track[x],opts]

Usage[Append,Musica2,Midi,{_Counterpoint, ___?OptionQ},_Midi,"todo"];
Midi[x_Counterpoint, opts___?OptionQ] := Midi[Track[x],opts]

Usage[Append,Musica2,Midi,{_Event, ___?OptionQ},_Midi,"todo"];
Midi[x_Event, opts___?OptionQ] := Midi[Track[x],opts]

Usage[Append,Musica2,Midi,{_Melody, ___?OptionQ},_Midi,"todo"];
Midi[x_Melody, opts___?OptionQ] := Midi[Track[x],opts]

Usage[Append,Musica2,Midi,{_Note, ___?OptionQ},_Midi,"todo"];
Midi[x_Note, opts___?OptionQ] := Midi[Track[x],opts]

Usage[Append,Musica2,Midi,{_Progression, ___?OptionQ},_Midi,"todo"];
Midi[x_Progression, opts___?OptionQ] := Midi[Track[x],opts]

(* Midi reverse constructors ****************************************************************)

Usage[Append,Musica2,Chord,{_Midi},{___Chord},"todo"];
Midi  /: Chord[x_Midi] := Chord[Counterpoint[x]]

Usage[Append,Musica2,Counterpoint,{_Midi},_Counterpoint,"todo"];
Midi  /: Counterpoint[x_Midi,rtf_:(0&)] := Counterpoint[Select[Flatten[Melody[Counterpoint[#,rtf]]& /@ x],(0<Length[#])&]]

Usage[Append,Musica2,Melody,{_Midi},{___Melody},"todo"];
Midi  /: Melody[x_Midi]  := Melody[Counterpoint[x]]

Usage[Append,Musica2,Note,{_Midi},{{___Note}..},"todo"];
Midi  /: Note[x_Midi]  := Note[Counterpoint[x]]

Usage[Append,Musica2,Progression,{_Midi},_Progression,"todo"];
Midi  /: Progression[x_Midi]  := Progression[Counterpoint[x]]

Usage[Append,Musica2,Snippet,{_Midi,___?OptionQ},{___Snippet},"todo"];
Midi  /: Snippet[x_Midi,  opts___?OptionQ] := Snippet[Counterpoint[Midi[x,TimeUnit->Second]],opts]

Usage[Append,Musica2,Sound,{_Midi,___?OptionQ},_Sound,"todo"];
Midi  /: Sound[x_Midi ,opts___?OptionQ] := Sound[Counterpoint[Midi[x,TimeUnit->Second]],opts]

(* Midi common functions ********************************************************************)

Usage[Append,Musica2,Convert,{Second,Tick,_Midi},_Function,"todo"];
Convert[Second,Tick,x_Midi] := TempoFunction[TempoTrack[x], True, TPQ->TPQ[x], QPM->QPM[x]]

Usage[Append,Musica2,Convert,{Tick,Second,_Midi},_Function,"todo"];
Convert[Tick,Second,x_Midi] := TempoFunction[TempoTrack[x], False, TPQ->TPQ[x], QPM->QPM[x]]

Usage[Append,Musica2,Mix,{{___Midi},___?OptionQ},_Midi,"todo"];
Mix[x:{__Midi}, opts___?OptionQ] :=
  Module[{t = Max[Length /@ x]},
    Midi[Mix /@ Transpose[Join[Track[#],Table[Track[Event[{}]],{t-Length[#]}]]& /@ TimeUnitUnify[x,opts]]]
    ]

Usage[Append,Musica2,Par,{{___Midi},___?OptionQ},_Midi,"todo"];
Par[x:{__Midi}, opts___?OptionQ]  :=
  Module[{m=x,o},
    m = TimeUnitUnify[x,opts];
    o = Opts[m[[1]]];
    Midi[Flatten[Track /@ m],Sequence @@ o]
    ]

Usage[Append,Musica2,Play2,{_Midi,___?OptionQ},_Sound,"todo"];
Midi /: Play2[x_Midi, opts___?OptionQ] := Play2[Counterpoint[Midi[x,TimeUnit->Second]],opts]

Usage[Append,Musica2,Seq,{{___Midi},___?OptionQ},_Midi,"todo"];
Seq[x:{__Midi}, opts___?OptionQ] :=
  Module[{t = Max[Length /@ x],m=x,o},
    m = TimeUnitUnify[m,opts];
    o = Opts[m[[1]]];
    m = Join[Track[#],Table[Track[Event[{}]],{t-Length[#]}]]& /@ m;
    m = Tidy[Midi[#]]& /@ m;
    m = Track /@ m;
    m = Transpose[m];
    m = Seq /@ m;
    m = Midi[m,Sequence @@ o];
    m
    ]
    
Usage[Append,Musica2,Tidy,{Midi},_Function,"todo"];
Tidy[Midi] = Module[{r = #,eot = TotalDuration[#]},
  r = Append[#,Event[{eot,EOT}]]& /@ r;
  r = Tidy /@ r;
  r
  ]&

Usage[Append,Musica2,TotalDuration,{_Midi},_,"todo"];
Midi /: TotalDuration[x_Midi]  := Max[TotalDuration /@ x]

(* Midi unique functions ********************************************************************)

MidiExport::"error 1"="Unable to open `1` for export";

Usage[Append,Musica2,Export,{_String,_Midi},_String,"todo"];
Midi /: Export[fn_String,mx_Midi] :=
  Module[
    {
      f=$Failed,
      r=$Failed,
      m=Midi[Tidy[mx],TimeUnit->Tick]
      },
    (* get the data and change timing to delta *)
    t = (Transpose[{ValuesToDeltas[Prepend[#[[1]],0]],#[[2]]}]&[Transpose[Data/@#]])&/@m;

    f = If[$VersionNumber <= 5.0,
      OpenWriteBinary[fn],
      OpenWrite[fn,BinaryFormat->True]
      ];
    If[f === $Failed,
      Message[MidiExport::"error 1",fn],
      WriteString[f,"MThd"];
      WriteInt[f,4,6];
      WriteInt[f,2,FileFormat /. Opts[m] /. Options[Midi]];
      WriteInt[f,2,Length[t]];
      WriteInt[f,2,TPQ /. Opts[m] /. Options[Midi]];
      WriteTrack[f,#]&/@t;
      r = fn
      ];
    If[f =!= $Failed,Flush[f];Close[f]];
    r
    ]

MidiImport::"error 1"="Unable to open `1` for import";
MidiImport::"error 2"="No MThd header in `1`, probably not a MIDI-file";
MidiImport::"error 3"="Bad MThd length in header in `1`, should be 6 but is `2`, probably not a MIDI-file";
    
Usage[Append,Musica2,Import,{_String,Midi},_Midi,"todo"];
Midi /: Import[fn_String,Midi]:=
  Module[
    {
      f=$Failed,
      r=$Failed,
      h={TimeUnit->Tick},
      t={},
      n,tmp
      },
    f = OpenRead[fn];
    If[f === $Failed,
      Message[MidiImport::"error 1",fn],
      If[StringJoin@@ReadList[f,Character,4] =!= "MThd",
        Message[MidiImport::"error 2",fn],
        If[(tmp=ReadInt[f,4]) =!= 6,
          Message[MidiImport::"error 3",fn,tmp],
          AppendTo[h, FileFormat->ReadInt[f,2]];
          n=ReadInt[f,2];
          AppendTo[h, TPQ->ReadInt[f,2]];
          Do[AppendTo[t, ReadTrack[f]],{n}];
          r = Midi[t,Sequence @@ Sort[h]]
          ]
         ]
      ];
    If[f =!= $Failed,Close[f]];
    r
    ]

Usage[Append,Musica2,NotePlot,{_Midi , _Symbol, ___?OptionQ},_Graphics,"todo"];
Midi /: NotePlot[x_Midi , s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Usage[Append,Musica2,QPM,{_Midi},_,"todo"];
QPM[x_Midi] := QPM /. Opts[x] /. Options[Midi]

Usage[Append,Musica2,TimeUnit,{_Midi},(Tick|Second|MilliSecond),"todo"];
TimeUnit[x_Midi] := TimeUnit /. Opts[x] /. Options[Midi]

TimeUnitChange[m_Midi, MilliSecond, MilliSecond] := m
TimeUnitChange[m_Midi, Second, Second] := m
TimeUnitChange[m_Midi, Tick, Tick] := m

TimeUnitChange[m_Midi, Tick, Second] :=
  Module[{f = TempoFunction[TempoTrack[m], False, TPQ->TPQ[m]]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
      Sequence @@ AddOpts[Opts[m],TimeUnit->Second]
      ]
    ]

TimeUnitChange[m_Midi, Second, Tick] :=
  Module[{f = TempoFunction[TempoTrack[m], True, TPQ->TPQ[m]]},
    Midi[
      (Event[{f[EventTime[#]],{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
      Sequence @@ AddOpts[Opts[m],TimeUnit->Tick]
      ]
    ]

TimeUnitChange[m_Midi, Second, MilliSecond] :=
  Midi[
    (Event[{EventTime[#]*1000,{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
    Sequence @@ AddOpts[Opts[m],TimeUnit->MilliSecond]
    ]

TimeUnitChange[m_Midi, MilliSecond, Second] :=
  Midi[
    (Event[{EventTime[#]/1000,{EventType[#],EventData[#]}},Sequence @@ Opts[#]]& /@ #)& /@ Track[m],
    Sequence @@ AddOpts[Opts[m],TimeUnit->Second]
    ]

TimeUnitChange[m_Midi, Tick, MilliSecond] := TimeUnitChange[TimeUnitChange[m, Tick, Second], Second, MilliSecond]
TimeUnitChange[m_Midi, MilliSecond, Tick] := TimeUnitChange[TimeUnitChange[m, MilliSecond, Second], Second, Tick]

TimeUnitUnify[m:{__Midi}, opts___?OptionQ] :=
  Midi[
    m,
    Sequence @@ (
      (# -> (# /. {opts} /. (# -> #[m[[1]]])))& /@ {TimeUnit,TPQ,QPM}
      )
    ]

Usage[Append,Musica2,TPQ,{_Midi},_,"todo"];
TPQ[x_Midi] := TPQ /. Opts[x] /. Options[Midi]

(* Midi tests *******************************************************************************)

Midi /: TestSuite[Midi] = Join[TestSuite[Midi],{
  TestCase[
    Module[{o, c},
      Export["tmp.mid", "almost empty", "Text"];
      o = Import["Musica2/Documentation/Data/cor.mid", Midi];
      Export["tmp.mid", o];
      c = Import["tmp.mid", Midi];
      Data /@ Melody[o] === Data /@ Melody[c]
      ],
    True
    ]
  }]
  
(* Midi ------------------------------------------------------------------------------------*)

(* Tempo +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Tempo modifications and interceptions ****************************************************)

(* Tempo constructors ***********************************************************************)

Tempo[x_Event, opts___?OptionQ] := Tempo[{#[[1]],60000000/Total[{65536, 256, 1}*#[[2,2]]]},opts]&[Data[x]] /; MatchQ[Data[x],{_,{EventTypeTempo,{_,_,_}}}]

(* Tempo reverse constructors ***************************************************************)

(* Tempo common functions *******************************************************************)

Usage[Append,Musica2,Convert,{Second,Tick,_?NumberQ},_Function,"todo"];
Convert[Second,Tick,x_?NumberQ] := TempoFunction[TempoTrack[Tempo[{0,x}]], True]

Usage[Append,Musica2,Convert,{Tick,Second,_?NumberQ},_Function,"todo"];
Convert[Tick,Second,x_?NumberQ] := TempoFunction[TempoTrack[Tempo[{0,x}]], False]

(* Tempo unique functions *******************************************************************)

(* Tempo tests ******************************************************************************)

Tempo /: TestSuite[Tempo] = Join[TestSuite[Tempo],{
  }];

(* Tempo -----------------------------------------------------------------------------------*)

(* TempoTrack ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* TempoTrack modifications and interceptions ***********************************************)

(* TempoTrack constructors ******************************************************************)

Usage[Append,Musica2,TempoTrack,{_Midi , ___?OptionQ},_TempoTrack,"todo"];
TempoTrack[x_Midi , opts___?OptionQ] := TempoTrack[x[[1]], opts, QPM -> (QPM /. {opts} /. Opts[x] /. Options[Midi])]

Usage[Append,Musica2,TempoTrack,{_Track , ___?OptionQ},_TempoTrack,"todo"];
TempoTrack[x_Track, opts___?OptionQ] :=
  Module[{u = Data /@ Select[Tempo /@ x, TempoQ],tpq = TPQ[x]},
    (* add a default tempo if needed *)
    If[Length[u] == 0 || u[[1, 1]] != 0, u = Prepend[u, {0, QPM /. {opts} /. Options[Midi]}]];
    TempoTrack[u, Sequence@@RemOpts[{opts},QPM]]
    ]

(* TempoTrack reverse constructors **********************************************************)

(* TempoTrack common functions **************************************************************)

Usage[Append,Musica2,Convert,{Second,Tick,_TempoTrack},_Function,"todo"];
Convert[Second,Tick,x_TempoTrack] := TempoFunction[x, True]

Usage[Append,Musica2,Convert,{Tick,Second,_TempoTrack},_Function,"todo"];
Convert[Tick,Second,x_TempoTrack] := TempoFunction[x, False]

Usage[Append,Musica2,Tidy,{TempoTrack},_Function,"todo"];
Tidy[TempoTrack] = Module[{r = #},
  r = Sort[r,TempoTime];
  r
  ]&

(* TempoTrack unique functions **************************************************************)

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

(* TempoTrack tests *************************************************************************)

TempoTrack /: TestSuite[TempoTrack] = Join[TestSuite[TempoTrack],{
  }];

(* TempoTrack ------------------------------------------------------------------------------*)

(******** private functions used by Import and Export ********)

If[$VersionNumber <= 5.0,
  wrt[f_,b_]:=WriteBinary[f,b,ByteConversion->Identity],
  wrt[f_,b_]:=BinaryWrite[f,b]
  ];

ReadInt[f_,n_] := Total[ReadList[f,Byte,n] Table[256^i,{i,n-1,0,-1}]]

WriteInt[f_,n_,i_] :=
  Module[{b=IntegerDigits[i,256]},
    b=Join[Table[0,{n-Length[b]}],b];
    wrt[f,b]
    ]

ReadVarLen[f_] :=
  Module[{r=0,b=16^^80},
    While[16^^80<=b,
      b=Read[f,Byte];
      r=r*16^^80+BitAnd[b,16^^7F]
      ];
    r
    ]

ListVarLen[i_] :=
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
    {{type,channel},Join[Drop[r,1],x]}
    ]

ReadEvent[f_,rt_] :=
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

ReadTrack[f_] :=
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

ListSysX[{type_,data_}] :=
  {type,Length[data],data}

ListMeta[{type:{_,sybtype_},data_}] :=
  {type,Length[data],data}

ListChannel[{{type_,channel_},{data__}}] :=
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

WriteTrack[f_,t_] :=
  Module[{w=t},
    w=ListTrack[w];
    w=Mod[#,256]&/@(Abs/@(Round/@Flatten[w]));
    WriteString[f,"MTrk"];
    WriteInt[f,4,Length[w]];
    wrt[f,w]
    ]

(*************************)
    
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
  EventTypeKeyPressure,
  EventTypeControlChange,
  EventTypeProgramChange,
  EventTypeChannelPressure,
  EventTypePitchBend,
  EventTypeSysX0,
  EventTypeSysX7,
  EventTypeKeySignature,
  EventTypeTempo,
  EventTypeTimeSignature,
  FileFormat,
  Midi,
  MidiQ,
  MilliSecond,
  QPM,
  Tempo,
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

