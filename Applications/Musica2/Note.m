(* :Title: Note *)

(* :Summary: Functions for Notes in Chords and Melodys
             If you are looking for a Note-object, this is the place to look,
              but there is no such object, yet (ever?), use a one-note melody or chord.
*)

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

(* :Context: Musica2`Note` *)

(* :History:
  2005-02-16  bch :  initiated usage of Usage ;-)
  2005-02-13  bch :  reorganized code in file, hopefully in an uniform manner
  2005-02-09  bch :  moved the DurVal-code to DurVal.m and commented it out
  2005-01-27  bch :  renamed Scale to KeyMode
  2005-01-24  bch :  added use of DurVal
  2005-01-22  bch :  added NoteRest and NoteTie
  2005-01-21  bch :  changed Duration to TotalDuration and NoteDuration to Duration
  2005-01-19  bch :  major change in Scale
  2005-01-07  bch :  made Melody[c_Chord] return a list of melodies
  2004-12-13  bch :  removed PcV from Note
  2004-11-29  bch :  added use of Convert for getting ConversionFunctions
                     added ScaleStep
  2004-11-28  bch :  moved PitchCode to Common.m
  2004-11-27  bch :  changed Bass in FigBass to be a list of integers
  2004-11-20  bch :  added FigBass,Intervals and ThirdStack
                     added use of Tuning
                     changed conversion between Chord and Melody to be more useful (?)
                     removed conversion from Snippet to Chord, it will reappear somewhere else
  2004-10-06  bch :  added Tidy[Melody] to:
                       turn a leading tie to a rest
                       remove notes with zero duration
                       concatenate consecutive rests and ties (surely ties shouldnt be here?)
                     added NoteRestQ and NoteTieQ
  2004-10-04  bch :  not much, lost track... sorry
  2004-09-27  bch :  added some Scale constructors and the church-modes (plus locrian)
                     renamed PitchCode in Scale to Content
  2004-09-22  bch :  changed Show to Play2
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-03  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Note`",
  {
    "Musica2`Common`",
    "Musica2`Instrument`",
    "Musica2`ObjectType`",
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Tuning`",
    "Musica2`Usage`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Content,
  Convert,
  Octave,
  Par,
  PitchCode,
  Seq
  ];

Unprotect[
  Chord,
  ChordQ,
  Counterpoint,
  CounterpointQ,
  FigBass,
  FigBassQ,
  Intervals,
  IntervalsQ,
  KeyMode,
  KeyModeQ,
  Melody,
  MelodyQ,
  ModeAeolian,
  ModeDorian,
  ModeIonian,
  ModeLocrian,
  ModeLydian,
  ModeMajor,
  ModeMinor,
  ModeMixolydian,
  ModePhrygian,
  Note,
  Duration,
  NotePlot,
  NoteRest,
  NoteRestQ,
  NoteTie,
  NoteTieQ,
  NoteQ,
  Octave,
  Progression,
  ProgressionQ,
  ScaleStep,
  ThirdStack,
  ThirdStackQ,
  Velocity
  ];

CreateElement[Musica2,Note, {Duration_, {PitchCode_,Velocity_}},{1,{60,64}},
"Note represents a note with a duration.\[NewLine]
A rest is stored with PitchCode or Velocity as DataNoValue.\[NewLine]"
];
CreateContainer[Musica2,Chord,Note,
"Chord represents a list of Note's to be played in parallell and all whith the same duration.\[NewLine]
The duration of the chord is stored as an opt.\[NewLine]"
];
CreateContainer[Musica2,Melody,Note,
"Melody represents a list of Note's to be played in sequence.\[NewLine]"
];
CreateContainer[Musica2,Progression,Chord,
"Progression represents a list of Chord's to be played in sequence.\[NewLine]"
];
CreateContainer[Musica2,Counterpoint,Melody,
"Chord represents a list of Chord's to be played in parallell.\[NewLine]"
];

CreateElement[Musica2,FigBass, {Octave:(Infinity|_Integer), {Bass:{__Integer}, Code_Integer}},{12,{{0},1}},
"todo.\[NewLine]",
{DirectedInfinity}
];
CreateElement[Musica2,Intervals, {Octave:(Infinity|_Integer), Content:{__Integer}},{12,{0,0,0,0,0,0,0}},
"todo.\[NewLine]",
{DirectedInfinity}
];
CreateElement[Musica2,KeyMode, {Tonic_Integer, Steps:{__Integer}},{0,{2,2,1,2,2,2,1}},
"todo.\[NewLine]"
];
CreateElement[Musica2,ThirdStack, {Base:{__Integer}, {Bass_Integer, Code_Integer}},{{3,4},{0,1}},
"todo.\[NewLine]"
];

Base::usage = "Base is a member of ThirdStack."
Bass::usage = "Base is a member of FigBass and ThirdStack."

ModeAeolian::usage = "todo"
ModeDorian::usage = "todo"
ModeIonian::usage = "todo"
ModeLocrian::usage = "todo"
ModeLydian::usage = "todo"
ModeMajor::usage = "todo"
ModeMinor::usage = "todo"
ModeMixolydian::usage = "todo"
ModePhrygian::usage = "todo"
NotePlot::usage = "todo"
NoteRest::usage = "todo"
NoteRestQ::usage = "todo"
NoteTie::usage = "todo"
NoteTieQ::usage = "todo"
ScaleStep::usage = "todo"

Begin["`Private`"]

Usage[Musica2,NotePlot,{_Chord, _Symbol, ___?OptionQ},_Graphics,"todo"];
NotePlot[x_Chord, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Usage[Musica2,NotePlot,{_Counterpoint, _Symbol, ___?OptionQ},_Graphics,"todo"];
NotePlot[x_Counterpoint, s:(PitchCode|Velocity), opts___?OptionQ] := Plot[Evaluate[#[t] & /@ (NoteFunction[#,s]& /@ x)], {t,0,TotalDuration[x]},opts]

Usage[Musica2,NotePlot,{_Melody, _Symbol, ___?OptionQ},_Graphics,"todo"];
NotePlot[x_Melody, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Usage[Musica2,NotePlot,{_Note, _Symbol, ___?OptionQ},_Graphics,"todo"];
NotePlot[x_Note, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Usage[Musica2,NotePlot,{_Progression, _Symbol, ___?OptionQ},_Graphics,"todo"];
NotePlot[x_Progression, s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

(* Note ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Note modifications and interceptions *****************************************************)

(* Note constructors ************************************************************************)

Usage[Musica2,Note,{{DataNoValue,_Note}, ___?OptionQ},_Note,"turn a Note into a rest"]
Note[{DataNoValue,x_Note}, opts___?OptionQ] := Note[{DataNoValue,Duration[x]},Sequence@@AddOpts[Opts[x],opts]]

Usage[Musica2,Note,{{DataNoValue,_}, ___?OptionQ},_Note,"create a rest"]
Note[{DataNoValue,x_}, opts___?OptionQ] := Note[{x,{DataNoValue,DataNoValue}},opts]

Usage[Musica2,Note,{{DataTie,_Note}, ___?OptionQ},_Note,"turn a Note into a tie"]
Note[{DataTie,x_Note}, opts___?OptionQ] := Note[{Duration[x],{DataTie[PitchCode[x]],DataTie[Velocity[x]]}},Sequence@@AddOpts[Opts[x],opts]]

Usage[Musica2,Note,{{DataUnTie,_Note}, ___?OptionQ},_Note,"turn a tie into a Note"]
Note[{DataUnTie,x_Note}, opts___?OptionQ] := Note[{Duration[x],{DataUnTie[PitchCode[x]],DataUnTie[Velocity[x]]}},Sequence@@AddOpts[Opts[x],opts]]

Usage[Musica2,Note,{{_,_}, ___?OptionQ},_Note,"create a Note from {Duration,PitchCode}"]
Note[{d_,p_}, opts___?OptionQ] := Note[{d,{p,Velocity/.{opts}/.Options[Note]}},Sequence@@RemOpts[{opts},Velocity]]

Usage[Musica2,Note,{_?AtomQ, ___?OptionQ},_Note,"create a Note from PitchCode"]
Note[p_?AtomQ, opts___?OptionQ] := Note[{Duration/.{opts}/.Options[Note],{p,Velocity/.{opts}/.Options[Note]}},Sequence@@RemOpts[{opts},Duration,Velocity]]

(* todo, depricated *)
Note /: DataNoValue[x_Note] := (
  Deprecated["DataNoValue[x_Note]",20050214,"Note[{DataNoValue,x_Note}, opts___?OptionQ]"];
  Note[{DataNoValue,x}]
  )
NoteRest[x_] := (
  Deprecated["NoteRest[x_]",20050214,"Note[{DataNoValue,x_}, opts___?OptionQ]"];
  Note[{DataNoValue,x}]
  )
NoteRest[x_Note] := (
  Deprecated["NoteRest[x_Note]",20050214,"Note[{DataNoValue,x_Note}, opts___?OptionQ]"];
  Note[{DataNoValue,x}]
  )
NoteTie[x_Note] := (
  Deprecated["NoteTie[x_Note]",20050214,"Note[{DataTie,x_Note}, opts___?OptionQ]"];
  Note[{DataTie,x}]
  )

(* Note reverse constructors ****************************************************************)

Usage[Musica2,Snippet,{_Note, ___?OptionQ},_Snippet,"create a Snippet from a Note"]
Note /: Snippet[x_Note, opts___?OptionQ] := Snippet[Melody[x],opts]

Usage[Musica2,Sound,{_Note, ___?OptionQ},_Sound,"create a Sound from a Note"]
Note /: Sound[x_Note, opts___?OptionQ] := Sound[Snippet[x,opts]]

(* Note common functions ********************************************************************)

Usage[Musica2,DataAnyValueQ,{_Note},(True|False),"returns True if the Note contains a DataNoValue"]
Note /: DataAnyValueQ[x_Note] := DataNoValueQ[Data[x]]

Usage[Musica2,DataNoValueQ,{_Note},(True|False),"returns True if the Note is a rest"]
Note /: DataNoValueQ[x_Note] := DataNoValueQ[PitchCode[x]] || DataNoValueQ[Velocity[x]] || (Velocity[x] === 0)

Usage[Musica2,DataTieQ,{_Note},(True|False),"returns True if the Note is a tie"]
Note /: DataTieQ[x_Note] := DataTieQ[PitchCode[x]] || DataTieQ[Velocity[x]]

Usage[Musica2,Par,{{__Note}},_Chord,"creates a Chord"]
Par[x:{__Note}] := Chord[x]

Usage[Musica2,Play2,{_Note},_Sound,"playes the Note in stereo"]
Note /: Play2[x_Note] := Play2[Sound[x]]

Usage[Musica2,Seq,{{__Note}},_Melody,"creates a Melody"]
Seq[x:{__Note}] := Melody[x]

Usage[Musica2,TotalDuration,{_Note},_,"returns the Duration"]
Note /: TotalDuration[x_Note] := Duration[x]

(* Note unique functions ********************************************************************)

(* todo, depricated *)
NoteRestQ[x_Note] := (
  Deprecated["NoteRestQ[x_Note]",20050214,"DataNoValueQ[x_Note]"];
  DataNoValueQ[x]
  )
NoteTieQ[x_Note] := (
  Deprecated["NoteTieQ[x_Note]",20050214,"DataTieQ[x_Note]"];
  DataTieQ[x]
  )

(* Note tests *******************************************************************************)

Note /: TestSuite[Note] = Join[TestSuite[Note],{
  TestCase[
    Module[{m = {60, 64, 67}, m1, m2, m3, cp, du = 100/(SampleRate /. Options[Sound])},
      Export["tmp.wav", "almost empty", "Text"]; 
      m1 = Melody[m + 1, Duration -> du];
      m2 = Melody[m + 2, Duration -> du];
      m3 = Melody[m + 3, Duration -> du];
      cp = Counterpoint[{m1, m2, m3}];
      s = Sound[cp];
      Export["tmp.wav", Mix[s, 2]];
      Total[Flatten[Content[Import["tmp.wav"]]]]
      ],
    2.2421875
    ]
  }]
  
(* Note ------------------------------------------------------------------------------------*)

(* Chord +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Chord modifications and interceptions ****************************************************)

DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{{__?OptionQ},{_,_}}],Note[{Duration/.Opts[sup],sub[[2]]}, Sequence @@ sub[[1]]],Note[{Duration/.Opts[sup],sub}]]];
UnPack[Chord] = Function[{sub,opts},If[Opts[sub]=={},Data[sub][[2]],{Opts[sub],{PitchCode[sub],Velocity[sub]}}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,Duration->Duration[subs[[1]]]]];

(* Chord constructors ***********************************************************************)

Usage[Musica2,Chord,{{_,{{_,_}...}},___?OptionQ},_Chord,"todo"]
Chord[d:{_,{{_,_}...}},opts___?OptionQ] := Chord[Note[{d[[1]],#}]&/@d[[2]],opts]

Usage[Musica2,Chord,{_Counterpoint},_Chord,"todo"]
Chord[x_Counterpoint] := Chord[Progression[x]]

Usage[Musica2,Chord,{_FigBass,___?OptionQ},_Chord,"todo"]
Chord[x_FigBass, opts___?OptionQ] := Chord[PitchCode[x],opts]

Usage[Musica2,Chord,{_Intervals, ___?OptionQ},_Chord,"todo"]
Chord[x_Intervals, opts___?OptionQ] := Chord[PitchCode[x],opts]

Usage[Musica2,Chord,{_Melody, ___?OptionQ},_Chord,"todo"]
Chord[x_Melody, opts___?OptionQ] := Chord[#,opts]& /@ Note[x]

Usage[Musica2,Chord,{{__?AtomQ}, ___?OptionQ},_Chord,"todo"]
Chord[p:{__?AtomQ}, opts___?OptionQ] := Chord[Note[#,opts]& /@ p,opts]

Usage[Musica2,Chord,{_ThirdStack, ___?OptionQ},_Chord,"todo"]
Chord[x_ThirdStack, opts___?OptionQ] := Chord[PitchCode[x],opts]

(* Chord reverse constructors ***************************************************************)

Usage[Musica2,Snippet,{_Chord, ___?OptionQ},{___Snippet},"todo"]
Chord /: Snippet[x_Chord, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Usage[Musica2,Sound,{_Chord, ___?OptionQ},_Sound,"todo"]
Chord /: Sound[x_Chord, opts___?OptionQ] := Sound[Snippet[x,opts]]

(* Chord common functions *******************************************************************)

Usage[Musica2,Par,{{__Chord}},_Chord,"todo"]
Par[x:{__Chord}] := Chord[Flatten[Note /@ x]]

Usage[Musica2,Play2,{_Chord},_Sound,"todo"]
Chord /: Play2[x_Chord] := Play2[Sound[x]]

Usage[Musica2,Par,{{__Chord}},_Progression,"todo"]
Seq[x:{__Chord}] := Progression[x]

Usage[Musica2,TotalDuration,{_Chord},_,"todo"]
Chord /: TotalDuration[x_Chord] := Duration /. Opts[x]

(* Chord unique functions *******************************************************************)

(* Chord tests ******************************************************************************)

Chord /: TestSuite[Chord] = Join[TestSuite[Chord],{
  }];

(* Chord -----------------------------------------------------------------------------------*)

(* Melody ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Melody modifications and interceptions ***************************************************)

(* Melody constructors **********************************************************************)

Usage[Musica2,Melody,{_Chord,___?OptionQ},_Melody,"todo"]
Melody[x_Chord,opts___?OptionQ] := Melody[#,opts]& /@ Note[x]

Usage[Musica2,Melody,{_Intervals,___?OptionQ},_Melody,"todo"]
Melody[x_Intervals,opts___?OptionQ] := Melody[PitchCode[x],opts]

Usage[Musica2,Melody,{_FigBass,___?OptionQ},_Melody,"todo"]
Melody[x_FigBass,opts___?OptionQ] := Melody[PitchCode[x],opts]

Usage[Musica2,Melody,{_Progression,___?OptionQ},{___Melody},"todo"]
Melody[x_Progression] := Melody[Counterpoint[x]]

Usage[Musica2,Melody,{{__?AtomQ},___?OptionQ},_Melody,"todo"]
Melody[p:{__?AtomQ}, opts___?OptionQ] := Melody[Note[#,opts]& /@ p,Sequence@@RemOpts[{opts},Duration,Velocity]]

Usage[Musica2,Melody,{{{_?AtomQ,_?AtomQ}..},___?OptionQ},_Melody,"todo"]
Melody[dp:{{_?AtomQ,_?AtomQ}..}, opts___?OptionQ] := Melody[Note[#,opts]& /@ dp,Sequence@@RemOpts[{opts},Duration,Velocity]]

Usage[Musica2,Melody,{_ThirdStack,___?OptionQ},_Melody,"todo"]
Melody[x_ThirdStack,opts___?OptionQ] := Melody[PitchCode[x],opts]

(* Melody reverse constructors **************************************************************)

Usage[Musica2,Snippet,{_Melody,___?OptionQ},_Snippet,"todo"]
Melody /: Snippet[x_Melody, opts___?OptionQ] := Convert[Melody,Snippet,Instrument,opts][x]

Usage[Musica2,Sound,{_Melody,___?OptionQ},_Sound,"todo"]
Melody /: Sound[x_Melody, opts___?OptionQ] := Sound[Snippet[x,opts]]

(* Melody common functions ******************************************************************)

Usage[Musica2,Convert,{Time,PitchCode,_Melody},_,"todo"]
Convert[Time,PitchCode,x_Melody] := NoteFunction[x, PitchCode]

Usage[Musica2,Convert,{Time,Velocity,_Melody},_,"todo"]
Convert[Time,Velocity,x_Melody] := NoteFunction[x, Velocity]

NoteFunction[x_Melody, s:(PitchCode|Velocity)] := MakeNestedIfs[Transpose[{Duration /@ x,s /@ x /. {DataNoValue -> 0}}]]

Usage[Musica2,Par,{{__Melody}},_Counterpoint,"todo"]
Par[x:{__Melody}] := Counterpoint[x]

Usage[Musica2,Play2,{_Melody},_Sound,"todo"]
Melody /: Play2[x_Melody] := Play2[Sound[x]]

Usage[Musica2,Seq,{{__Melody}},_Melody,"todo"]
Seq[x:{__Melody}] := Melody[Flatten[Note /@ x]]

Usage[Musica2,Tidy,{Melody},_,"todo"]
Tidy[Melody] = Module[{n = Note[#],i},
  For[i = 1, i < Length[n], i++,
    If[Duration[n[[i]]]===0,
      n = Delete[n,i];
      i -= 1,
      If[i == 1 && NoteTieQ[n[[1]]],
        n[[1]] = ReplacePart[n[[1]],DataNoValue,PitchCode];
        n[[1]] = ReplacePart[n[[1]],DataNoValue,Velocity]
        ];
      If[(NoteRestQ[n[[i]]] && NoteRestQ[n[[i+1]]]) || NoteTieQ[n[[i+1]]],
        n[[i]] = ReplacePart[n[[i]],Duration[n[[i]]]+Duration[n[[i+1]]],Duration];
        n = Delete[n,i+1];
        i -= 1;
        ]
      ];
    ];
  Melody[n,Sequence @@ Opts[#]]
  ]&

Usage[Musica2,TotalDuration,{_Melody},_,"todo"]
Melody /: TotalDuration[x_Melody] := Total[Duration /@ x]

(* Melody unique functions ******************************************************************)

(* Melody tests *****************************************************************************)

Melody /: TestSuite[Melody] = Join[TestSuite[Melody],{
 TestCase[PitchCode[Append[Melody[{1, 2, 3}], Note[4]]], {1, 2, 3, 4}], 
 TestCase[PitchCode[Delete[Melody[{1, 2, 3}], 2]], {1, 3}], 
 TestCase[PitchCode[Drop[Melody[{1, 2, 3}], 2]], {3}], 
 TestCase[PitchCode[Extract[Melody[{1, 2, 3}], 2]], 2], 
 TestCase[PitchCode[First[Melody[{1, 2, 3}]]], 1], 
 TestCase[PitchCode[Insert[Melody[{1, 2, 3}], Note[4], 2]], {1, 4, 2, 3}], 
 TestCase[PitchCode[Last[Melody[{1, 2, 3}]]], 3], TestCase[Length[Melody[{1, 2, 3}]], 3], 
 TestCase[PitchCode /@ Melody[{1, 2, 3}], {1, 2, 3}], 
 TestCase[MapIndexed[{PitchCode[#1], #2} & , Melody[{1, 2, 3}]], {{1, {1}}, {2, {2}}, {3, {3}}}],
 TestCase[PitchCode[Most[Melody[{1, 2, 3}]]], {1, 2}],
 TestCase[PitchCode[Melody[{1, 2, 3}][[2]]], 2], 
 TestCase[PitchCode[Prepend[Melody[{1, 2, 3}], Note[0]]], {0, 1, 2, 3}], 
 TestCase[PitchCode[ReplacePart[Melody[{1, 2, 3}], Note[7], 2]], {1, 7, 3}], 
 TestCase[PitchCode[Reverse[Melody[{1, 2, 3}]]], {3, 2, 1}], 
 TestCase[PitchCode[Rest[Melody[{1, 2, 3}]]], {2, 3}], 
 TestCase[PitchCode[Select[Melody[{1, 2, 3}], Mod[PitchCode[#1], 2] == 1 & ]], {1, 3}], 
 TestCase[PitchCode[Take[Melody[{1, 2, 3}], 2]], {1, 2}], 
 TestCase[PitchCode[Map[#1 + 1 & , Melody[{1, 2, 3}], PitchCode]], {2, 3, 4}], 
 TestCase[PitchCode[MapIndexed[#1 + #2[[1]] & , Melody[{1, 2, 3}], PitchCode]], {2, 4, 6}], 
 TestCase[PitchCode[Sort[Melody[{3, 2, 1}], PitchCode]], {1, 2, 3}], 
 TestCase[PitchCode[ReplacePart[Melody[{1, 2, 3}], 6, PitchCode]], {6, 6, 6}], 
 TestCase[PitchCode[ReplacePart[Melody[{1, 2, 3}], {4, 3, 2}, PitchCode]], {4, 3, 2}]
  }]

(* Melody ----------------------------------------------------------------------------------*)

(* Progression +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Progression modifications and interceptions **********************************************)

(* Progression constructors *****************************************************************)

Usage[Musica2,Progression,{_Counterpoint, ___?OptionQ},_Progression,"todo"]
Progression[x_Counterpoint, opts___?OptionQ] := Progression[Chord[#]& /@ ParOfSeqToSeqOfPar[Data /@ x]]

Usage[Musica2,Progression,{_Melody},_Progression,"todo"]
Progression[x_Melody] := Progression[Chord[x]]

(* Progression reverse constructors *********************************************************)

Usage[Musica2,Snippet,{_Progression, ___?OptionQ},{___Snippet},"todo"]
Progression /: Snippet[x_Progression, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Usage[Musica2,Sound,{_Progression, ___?OptionQ},_Sound,"todo"]
Progression /: Sound[x_Progression,  opts___?OptionQ] := Sound[Snippet[x,opts]]

(* Progression common functions *************************************************************)

Usage[Musica2,Par,{{__Progression}},_Progression,"todo"]
Par[x:{__Progression}]  := Progression[Par[Counterpoint /@ x]]

Usage[Musica2,Play2,{_Progression},_Sound,"todo"]
Progression /: Play2[x_Progression] := Play2[Sound[x]]

Usage[Musica2,Seq,{{__Progression}},_Progression,"todo"]
Seq[x:{__Progression}] := Progression[Flatten[Chord /@ x]]

Usage[Musica2,TotalDuration,{_Progression},_,"todo"]
Progression /: TotalDuration[x_Progression] := Total[TotalDuration /@ x]

(* Progression unique functions *************************************************************)

(* Progression tests ************************************************************************)

Progression /: TestSuite[Progression] = Join[TestSuite[Progression],{
  }];

(* Progression -----------------------------------------------------------------------------*)

(* Counterpoint ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Counterpoint modifications and interceptions *********************************************)

(* Counterpoint constructors ****************************************************************)

Usage[Musica2,Counterpoint,{_Chord},_Counterpoint,"todo"]
Counterpoint[x_Chord] := Counterpoint[Melody[x]]

Usage[Musica2,Counterpoint,{_Progression, ___?OptionQ},_Counterpoint,"todo"]
Counterpoint[x_Progression, opts___?OptionQ] := Counterpoint[Melody[#]& /@ SeqOfParToParOfSeq[{TotalDuration[#],Data[#]}& /@ x]]

(* Counterpoint reverse constructors ********************************************************)

Usage[Musica2,Snippet,{_Counterpoint, ___?OptionQ},{___Snippet},"todo"]
Counterpoint /: Snippet[x_Counterpoint, opts___?OptionQ] := Snippet[#,opts]& /@ x

Usage[Musica2,Sound,{_Counterpoint, ___?OptionQ},_Sound,"todo"]
Counterpoint /: Sound[x_Counterpoint, opts___?OptionQ] := Sound[Snippet[x,opts]]

(* Counterpoint common functions ************************************************************)

Usage[Musica2,Par,{{__Counterpoint}},_Progression,"todo"]
Par[x:{__Counterpoint}] := Counterpoint[Flatten[Melody /@ x]]

Usage[Musica2,Play2,{_Counterpoint},_Sound,"todo"]
Counterpoint /: Play2[x_Counterpoint] := Play2[Sound[x]]

Usage[Musica2,Seq,{{__Counterpoint}},_Progression,"todo"]
Seq[x:{__Counterpoint}] := Counterpoint[Seq[Progression /@ x]]

Usage[Musica2,TotalDuration,{_Counterpoint},_,"todo"]
Counterpoint /: TotalDuration[x_Counterpoint] := Max[TotalDuration /@ x]

(* Counterpoint unique functions ************************************************************)

(* Counterpoint tests ***********************************************************************)

Counterpoint /: TestSuite[Counterpoint] = Join[TestSuite[Counterpoint],{
  }];

(* Counterpoint ----------------------------------------------------------------------------*)

(* FigBass +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* FigBass modifications and interceptions **************************************************)

(* FigBass constructors *********************************************************************)

Usage[Musica2,FigBass,{_Chord, ___?OptionQ},_FigBass,"todo"]
FigBass[x_Chord, opts___?OptionQ] := FigBass[PitchCode[x],opts]

Usage[Musica2,FigBass,{_Intervals, ___?OptionQ},_FigBass,"todo"]
FigBass[x_Intervals, opts___?OptionQ] := FigBass[PitchCode[x],opts]

Usage[Musica2,FigBass,{_Melody, ___?OptionQ},_FigBass,"todo"]
FigBass[x_Melody, opts___?OptionQ] := FigBass[PitchCode[x],opts]

Usage[Musica2,FigBass,{_KeyMode, ___?OptionQ},_FigBass,"todo"]
FigBass[x_KeyMode, opts___?OptionQ] := FigBass[PitchCode[x],opts]

Usage[Musica2,FigBass,{_ThirdStack, ___?OptionQ},_FigBass,"todo"]
FigBass[x_ThirdStack, opts___?OptionQ] := FigBass[PitchCode[x],opts]

Usage[Musica2,FigBass,{_Integer, ___?OptionQ},_FigBass,"todo"]
FigBass[x_Integer, opts___?OptionQ] := FigBass[{Octave/.{opts}/.{Octave->Infinity},{{0},x}}, Sequence @@ RemOpts[{opts},Octave]]

Usage[Musica2,FigBass,{{__Integer}, ___?OptionQ},_FigBass,"todo"]
FigBass[x:{__Integer}, opts___?OptionQ] :=
  Module[{o=Octave/.{opts}/.{Octave->Infinity},p=Select[x,DataPlainValueQ],r,c,s,t},
    If[o =!= Infinity, p = Mod[p,o]];
    p = Union[p];
    If[o =!= Infinity,
      r = {0};
      t = c = Total[2^p];
      For[s = 1, s < o, s++,
        t = BitAnd[2t,2^o-1] + If[BitAnd[t,2^(o-1)]!=0, 1, 0];
        If[t < c, c = t; r = {o-s},
          If[t == c, r = Append[r,o-s]]
          ]
        ],
      r = p[[1]];
      c = Total[2^(p-r)];
      r = {r};
      ];
    FigBass[{o,{Sort[r],c}}, Sequence @@ RemOpts[{opts},Octave]]
    ]

(* FigBass reverse constructors *************************************************************)

(* FigBass common functions *****************************************************************)

Usage[Musica2,PitchCode,{_FigBass},{___Integer},"todo"]
PitchCode[x_FigBass] :=
  If[Octave[x]=!=Infinity,Mod[#,Octave[x]],#]&[
    ((#[[1]]-1)&/@Position[Reverse[IntegerDigits[Code[x],2]],1])
    ]+Bass[x][[1]]

(* FigBass unique functions *****************************************************************)

(* FigBass tests ****************************************************************************)

FigBass /: TestSuite[FigBass] = Join[TestSuite[FigBass],{
  }];

(* FigBass ---------------------------------------------------------------------------------*)

(* Intervals +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* Intervals modifications and interceptions ************************************************)

(* Intervals constructors *******************************************************************)

Usage[Musica2,Intervals,{_Chord, ___?OptionQ},_Intervals,"todo"]
Intervals[x_Chord, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Usage[Musica2,Intervals,{_FigBass, ___?OptionQ},_Intervals,"todo"]
Intervals[x_FigBass, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Usage[Musica2,Intervals,{_Melody, ___?OptionQ},_Intervals,"todo"]
Intervals[x_Melody, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Usage[Musica2,Intervals,{_KeyMode, ___?OptionQ},_Intervals,"todo"]
Intervals[x_KeyMode, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Usage[Musica2,Intervals,{_ThistStack, ___?OptionQ},_Intervals,"todo"]
Intervals[x_ThirdStack, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Usage[Musica2,Intervals,{{__Integer}, ___?OptionQ},_Intervals,"todo"]
Intervals[x:{__Integer}, opts___?OptionQ] :=
  ReplacePart[
    #,
    ReplacePart[Content[#]/2,(Content[#][[1]]-Length[x])/2,{1}],
    Content
    ]&[Intervals[x,x,opts]]

Usage[Musica2,Intervals,{{__Integer},{__Integer}, ___?OptionQ},_Intervals,"todo"]
Intervals[x:{__Integer},y:{__Integer}, opts___?OptionQ] :=
  Module[{o=Octave/.{opts}/.{Octave->Infinity},px=Select[x,DataPlainValueQ],py=Select[y,DataPlainValueQ],c,h,i,j,m},
    c = Flatten[Table[Abs[px[[i]]-py[[j]]],{i,Length[px]},{j,Length[py]}]];
    m = Max[c];
    If[o =!= Infinity,
      h = IntegerPart[o/2];
      c = Abs[Mod[#+h,o]-h]& /@ c;
      m = h;
      ];
    Intervals[{o,Table[Count[c,i],{i,0,m}]},Sequence@@RemOpts[{opts},Octave]]
    ]

(* Intervals reverse constructors ***********************************************************)

(* Intervals common functions ***************************************************************)

Usage[Musica2,PitchCode,{_Intervals},{___Integer},"todo"]
PitchCode[x_Intervals] :=
  Module[{n = (1 + Sqrt[1 + 8*Total[Content[x]]])/2,t},
    (* a good number of notes? *)
    If[FractionalPart[n]==0,
      (* generate all *)
      t = Union[Sort /@ Flatten[Array[{##} &, Prepend[Table[Octave[x],{n-1}],1]]-1, n-1]];(* todo: speed *)
      (* convert to intervals *)
      t = {Intervals[#,Octave->Octave[x]],#}& /@ t;
      (* get the good ones *)
      Cases[t,{i$_Intervals,c$_}/;(Content[x]===Content[i$])->c$],
      {}
      ]
    ]

(* Intervals unique functions ***************************************************************)

(* Intervals tests **************************************************************************)

Intervals /: TestSuite[Intervals] = Join[TestSuite[Intervals],{
  }];

(* Intervals -------------------------------------------------------------------------------*)

(* KeyMode +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* KeyMode modifications and interceptions **************************************************)

(* KeyMode constructors *********************************************************************)

f712[x_] := Round[12/7(x-1)+2]
f712[x_,k_,m_] := Module[{r=f712[x+m]+7k,z=f712[0+m]+7k},r-(z-Mod[z,12])]

Usage[Musica2,KeyMode,{_Integer,_Integer, ___?OptionQ},_KeyMode,"todo"]
KeyMode[k_Integer,m_Integer, opts___?OptionQ] := KeyMode[Table[f712[i,k,m],{i,0,7}],opts]

Usage[Musica2,KeyMode,{_Integer, ___?OptionQ},_KeyMode,"todo"]
KeyMode[k_Integer, opts___?OptionQ] := KeyMode[Table[f712[i,k,3k],{i,0,7}]+12Quotient[k + 5,7*12],opts]

Usage[Musica2,KeyMode,{_Intervals, ___?OptionQ},_KeyMode,"todo"]
KeyMode[x_Intervals,   opts___?OptionQ] := KeyMode[PitchCode[x],opts]

Usage[Musica2,KeyMode,{{__Integer}, ___?OptionQ},_KeyMode,"todo"]
KeyMode[x:{__Integer}, opts___?OptionQ] := KeyMode[{Min[x],ValuesToDeltas[x]},opts] (* todo: what about strange, unsorted scales? *)

(* KeyMode reverse constructors *************************************************************)

(* KeyMode common functions *****************************************************************)

Usage[Musica2,Convert,{ScaleStep,PitchCode,_KeyMode},_,"todo"]
Convert[ScaleStep,PitchCode,x_KeyMode] :=
  Module[
    {
      scalesize = Length[x],
      pitchcode = PitchCode[x],
      octave = Octave[x],
      f
      },
    f = Function[ss, Floor[ss/scalesize]*octave + pitchcode[[Mod[ss, scalesize] + 1]]];
    Function[ss,DataApply[f,ss]]
    ]

Usage[Musica2,Convert,{PitchCode,ScaleStep,_KeyMode},_,"todo"]
Convert[PitchCode,ScaleStep,x_KeyMode] :=
  Module[
    {
      scalesize = Length[x],
      pitchcode = PitchCode[x],
      octave = Octave[x],
      tonic = Tonic[x],
      f
      },
    f = Function[pc,
      If[#==={},DataAnyValue,
        #[[1,1]]-1+Floor[(pc-tonic)/octave]*scalesize
        ]&[Position[pitchcode,Mod[pc,octave,tonic]]]
      ];
    Function[ss,DataApply[f,ss]]
    ]

Convert[ScaleStep,PitchCode] := Convert[ScaleStep,PitchCode,KeyMode[]]
Convert[PitchCode,ScaleStep] := Convert[PitchCode,ScaleStep,KeyMode[]]

KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Chord]        := Module[{f=Convert[ScaleStep,PitchCode,KeyMode[o,d]]},Map[f,x,PitchCode]]
KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Counterpoint] := Module[{f=Convert[ScaleStep,PitchCode,KeyMode[o,d]]},Map[f,x,PitchCode]]
KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Integer]      := Convert[ScaleStep,PitchCode,KeyMode[o,d]][x]
KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Melody]       := Module[{f=Convert[ScaleStep,PitchCode,KeyMode[o,d]]},Map[f,x,PitchCode]]
KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Note]         := Module[{f=Convert[ScaleStep,PitchCode,KeyMode[o,d]]},ReplacePart[x,f[PitchCode[x]],PitchCode]]
KeyMode[o_?OptionQ, d_?(DataQ[KeyMode])][x_Progression]  := Module[{f=Convert[ScaleStep,PitchCode,KeyMode[o,d]]},Map[f,x,PitchCode]]

Usage[Musica2,Length,{_KeyMode},_Integer,"todo"]
KeyMode /: Length[x_KeyMode] := Length[Steps[x]]

Usage[Musica2,PitchCode,{_KeyMode},{__Integer},"todo"]
PitchCode[x_KeyMode] := DeltasToValues[Steps[x],Tonic[x]]

(* KeyMode unique functions *****************************************************************)

ModeAeolian = 5
ModeDorian = 1
ModeIonian = 0
ModeLocrian = 6
ModeLydian = 3
ModeMajor = ModeIonian
ModeMinor = ModeAeolian
ModeMixolydian = 4
ModePhrygian = 2

Usage[Musica2,Octave,{_KeyMode},_Integer,"todo"]
KeyMode /: Octave[x_KeyMode] := Total[Steps[x]]

(* KeyMode tests ****************************************************************************)

KeyMode /: TestSuite[KeyMode] = Join[TestSuite[KeyMode],{
  TestCase[Convert[PitchCode, ScaleStep] /@ Convert[ScaleStep, PitchCode] /@ Range[-10, 10], Range[-10, 10]],
  TestCase[Data[KeyMode[PitchCode[KeyMode[]]]],{0, {2, 2, 1, 2, 2, 2, 1}}]
  }]


(* KeyMode ---------------------------------------------------------------------------------*)

(* ThirdStack ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* ThirdStack modifications and interceptions ***********************************************)

(* ThirdStack constructors ******************************************************************)

Usage[Musica2,ThirdStack,{_Chord, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_Chord, opts___?OptionQ] := ThirdStack[PitchCode[x],opts]

Usage[Musica2,ThirdStack,{_FigBass, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_FigBass, opts___?OptionQ] := ThirdStack[PitchCode[x],opts]

Usage[Musica2,ThirdStack,{_Intervals, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_Intervals, opts___?OptionQ] := ThirdStack[#,opts]& /@ PitchCode[x]

Usage[Musica2,ThirdStack,{_Melody, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_Melody, opts___?OptionQ] := ThirdStack[PitchCode[x],opts]

Usage[Musica2,ThirdStack,{_KeyMode, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_KeyMode, opts___?OptionQ] := ThirdStack[PitchCode[x],opts]

Usage[Musica2,ThirdStack,{_Integer, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x_Integer, opts___?OptionQ] := ThirdStack[{{3,4},{0,x}},opts]

Usage[Musica2,ThirdStack,{{{__Integer},{__Integer}}, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[{base:{__Integer},pc:{__Integer}}, opts___?OptionQ] := (* todo: this is too simple, should be the last try really *)
  Module[{p,b,r},
    p = Union[pc];
    r = p[[1]];
    p = ValuesToDeltas[p];
    b = Union[base,p];
    If[Length[b]==1, b=Union[b,{3}]];
    If[Length[b]==1, b=Union[b,{4}]];
    p = Prepend[Reverse[(Position[b,#][[1,1]]-1)& /@ p],1];
    ThirdStack[{b,{r,FromDigits[p,Length[b]]}},opts]
    ]

Usage[Musica2,ThirdStack,{{__Integer}, ___?OptionQ},_ThirdStack,"todo"]
ThirdStack[x:{__Integer}, opts___?OptionQ] :=  ThirdStack[{Union[ValuesToDeltas[Union[x]]],x},opts] (* todo: this is too simple, should be the last try really *)

(* ThirdStack reverse constructors **********************************************************)

(* ThirdStack common functions **************************************************************)

Usage[Musica2,PitchCode,{_ThirdStack},{___Integer},"todo"]
PitchCode[x_ThirdStack] :=
  Module[{b=Base[x]},
    If[Code[x]==0,{},
      DeltasToValues[b[[#]]& /@ Reverse[Drop[IntegerDigits[Code[x],Length[b]],1]+1]]+Bass[x]
      ]
    ]

(* ThirdStack unique functions **************************************************************)

(* ThirdStack tests *************************************************************************)

ThirdStack /: TestSuite[ThirdStack] = Join[TestSuite[ThirdStack],{
  }];

(* ThirdStack ------------------------------------------------------------------------------*)

End[]

Protect[
  Content,
  Convert,
  Octave,
  Par,
  PitchCode,
  Seq
  ];

Protect[
  Chord,
  ChordQ,
  Counterpoint,
  CounterpointQ,
  FigBass,
  FigBassQ,
  Intervals,
  IntervalsQ,
  KeyMode,
  KeyModeQ,
  Melody,
  MelodyQ,
  ModeAeolian,
  ModeDorian,
  ModeIonian,
  ModeLocrian,
  ModeLydian,
  ModeMajor,
  ModeMinor,
  ModeMixolydian,
  ModePhrygian,
  Note,
  Duration,
  NotePlot,
  NoteRest,
  NoteRestQ,
  NoteTie,
  NoteTieQ,
  NoteQ,
  Octave,
  Progression,
  ProgressionQ,
  ScaleStep,
  ThirdStack,
  ThirdStackQ,
  Velocity
  ];

EndPackage[]
