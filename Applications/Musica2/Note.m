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
  2004-09-22  bch :  changed Show to Play2
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-03  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Note`",
  {
    "Musica2`Common`",
    "Musica2`Sound`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Par,
  Seq
  ];

Unprotect[
  Chord,
  ChordQ,
  Counterpoint,
  CounterpointQ,
  Melody,
  MelodyQ,
  Note,
  NoteDuration,
  NoteFunction,
  NotePlot,
  NoteQ,
  PitchCode,
  PitchCodeClassCount,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction,
  Velocity
  ];

CreateElement[Note, {NoteDuration_, PcV:{PitchCode_,Velocity_}}];
CreateContainer[Chord,Note];
CreateContainer[Melody,Note];
CreateContainer[Progression,Chord];
CreateContainer[Counterpoint,Melody];

CreateElement[Scale, {PitchCodeClassCount_Integer, PitchCode:{__}}];

NoteFunction::usage = ""
NotePlot::usage = ""

Begin["`Private`"]

(*****************)

DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{_Integer,_Integer}],Note[{NoteDuration/.Opts[sup],sub}],Note[{NoteDuration/.Opts[sup],sub[[2]]}, Sequence @@ sub[[1]]]]];
UnPack[Chord] = Function[{sub,opts},If[Opts[sub]=={},Data[sub][[2]],{Opts[sub],PcV[sub]}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,NoteDuration->NoteDuration[subs[[1]]]]];

Options[Note] = {NoteDuration->1,Velocity->64}

(*****************)

Chord[d:{_,{{_,_}...}},opts___?OptionQ] := Chord[Note[{d[[1]],#}]&/@d[[2]],opts]
Chord[x_Counterpoint]                   := Chord[Progression[x]]
Chord[x_Melody]                         := Chord /@ x
Chord[p:{__?AtomQ}, opts___?OptionQ]    := Chord[Note[#,opts]& /@ p,opts]

Counterpoint[x_Chord]                        := Counterpoint[Melody[x]]
Counterpoint[x_Progression, opts___?OptionQ] := Counterpoint[Melody[#]& /@ SeqOfParToParOfSeq[{Duration[#],Data[#]}& /@ x]]

Chord        /: Duration[x_Chord]        := NoteDuration /. Opts[x]
Counterpoint /: Duration[x_Counterpoint] := Max[Duration /@ x]
Melody       /: Duration[x_Melody]       := Total[NoteDuration /@ x]
Note         /: Duration[x_Note]         := NoteDuration[x]
Progression  /: Duration[x_Progression]  := Total[Duration /@ x]

Scale /: Inverse[x_Scale] := Scale[{Length[PitchCode[x]],Table[If[#==={},DataAnyValue,#[[1,1]]-1]&[Position[PitchCode[x],i]],{i,0,PitchCodeClassCount[x]-1}]},Sequence @@ Opts[x]]

Melody[x_Chord]                       := Melody /@ x
Melody[x_Progression]                 := Melody[Counterpoint[x]]
Melody[p:{__?AtomQ}, opts___?OptionQ] := Melody[Note[#,opts]& /@ p,opts]

Note[p_?AtomQ, opts___?OptionQ] := Note[{NoteDuration/.{opts}/.Options[Note],{p,Velocity/.{opts}/.Options[Note]}},opts]

NoteFunction[x_Melody, s:(PitchCode|Velocity)] := MakeNestedIfs[Transpose[{NoteDuration /@ x,s /@ x /. {DataNoValue -> 0}}]]

NotePlot[x_Chord,        s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]
NotePlot[x_Counterpoint, s:(PitchCode|Velocity), opts___?OptionQ] := Plot[Evaluate[#[t] & /@ (NoteFunction[#,s]& /@ x)], {t,0,Duration[x]},opts]
NotePlot[x_Melody,       s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]
NotePlot[x_Note,         s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]
NotePlot[x_Progression,  s_Symbol, opts___?OptionQ] := NotePlot[Counterpoint[x],s,opts]

Par[x:{__Chord}]        := Chord[Flatten[Note /@ x]]
Par[x:{__Counterpoint}] := Counterpoint[Flatten[Melody /@ x]]
Par[x:{__Melody}]       := Counterpoint[x]
Par[x:{__Note}]         := Chord[x]
Par[x:{__Progression}]  := Progression[Par[Counterpoint /@ x]]

Chord        /: Play2[x_Chord]        := Play2[Sound[x]]
Counterpoint /: Play2[x_Counterpoint] := Play2[Sound[x]]
Melody       /: Play2[x_Melody]       := Play2[Sound[x]]
Note         /: Play2[x_Note]         := Play2[Sound[x]]
Progression  /: Play2[x_Progression]  := Play2[Sound[x]]

Progression[x_Counterpoint, opts___?OptionQ] := Progression[Chord[#]& /@ ParOfSeqToSeqOfPar[Data /@ x]]
Progression[x_Melody]                        := Progression[Chord[x]]

Scale[] := Scale[{12,{0,2,4,5,7,9,11}}]

Scale[o_,d_][ss_] := ScaleFunction[Scale[d,Sequence @@ o]][ss]

ScaleFunction[x_Scale] :=
  Module[{f=Function[ss,Module[{scalesize = Length[PitchCode[x]]},Floor[ss/scalesize]*PitchCodeClassCount[x] + x[[PitchCode, Mod[ss, scalesize] + 1]]]]},
    Function[ss,DataApply[f,ss]]
    ]

Seq[x:{__Chord}]         := Progression[x]
Seq[x:{___Counterpoint}] := Counterpoint[Seq[Progression /@ x]]
Seq[x:{__Melody}]        := Melody[Flatten[Note /@ x]]
Seq[x:{__Note}]          := Melody[x]
Seq[x:{__Progression}]   := Progression[Flatten[Chord /@ x]]

p2f = Function[p, 220*2^((p - 57)/12)];
v2a = Function[v, v/127];
zin = Function[{f, a, sr}, N[a Sin[2Pi f#/sr]]&];

Chord        /: Snippet[x_Chord, opts___?OptionQ] := Snippet[Counterpoint[x],opts]
Counterpoint /: Snippet[x_Counterpoint, opts___?OptionQ] := Snippet[#,opts]& /@ x
Melody       /: Snippet[x_Melody, opts___?OptionQ] :=
  Module[{d,p,v,f,a,sr=SampleRate/.{opts}/.Options[Sound]},
    (* todo: use NoteFunction here *)
    d = (Duration /@ x) * sr;
    p = PitchCode /@ x /. {DataNoValue -> 0};
    v = Velocity /@ x /. {DataNoValue -> 0};
    f = MakeNestedIfs[Transpose[{d,p2f /@ p}]];
    a = MakeNestedIfs[Transpose[{d,v2a /@ v}]];
    Snippet[{SampledSoundFunction,Function[t, zin[f[t], a[t], sr][t]],Round[sr],Round[Duration[x]sr]}]
    ]
Note         /: Snippet[x_Note, opts___?OptionQ] := Snippet[Melody[x],opts]
Progression  /: Snippet[x_Progression, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Chord        /: Sound[x_Chord, opts___?OptionQ] := Sound[Snippet[x,opts]]
Counterpoint /: Sound[x_Counterpoint, opts___?OptionQ] := Sound[Snippet[x,opts]]
Melody       /: Sound[x_Melody, opts___?OptionQ] := Sound[Snippet[x,opts]]
Note         /: Sound[x_Note, opts___?OptionQ] := Sound[Snippet[x,opts]]
Progression  /: Sound[x_Progression, opts___?OptionQ] := Sound[Snippet[x,opts]]

End[]

Protect[
  Par,
  Seq
  ];

Protect[
  Chord,
  ChordQ,
  Counterpoint,
  CounterpointQ,
  Melody,
  MelodyQ,
  Note,
  NoteDuration,
  NoteFunction,
  NotePlot,
  NoteQ,
  PitchCode,
  PitchCodeClassCount,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction,
  Velocity
  ];

EndPackage[]
