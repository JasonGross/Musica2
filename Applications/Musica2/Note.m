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
    "Musica2`Sound`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Content,
  Octave,
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
  NoteDuration,
  NoteFunction,
  NotePlot,
  NoteQ,
  Octave,
  PitchCode,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction,
  ScaleQ,
  Velocity
  ];

CreateElement[Note, {NoteDuration_, PcV:{PitchCode_,Velocity_}}];
CreateContainer[Chord,Note];
CreateContainer[Melody,Note];
CreateContainer[Progression,Chord];
CreateContainer[Counterpoint,Melody];

CreateElement[Scale, {Octave_Integer, Content:{__}}];

ModeAeolian::usage = "todo"
ModeDorian::usage = "todo"
ModeIonian::usage = "todo"
ModeLocrian::usage = "todo"
ModeLydian::usage = "todo"
ModeMajor::usage = "todo"
ModeMinor::usage = "todo"
ModeMixolydian::usage = "todo"
ModePhrygian::usage = "todo"
NoteFunction::usage = ""
NotePlot::usage = ""

Begin["`Private`"]

(*****************)

DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{{__?OptionQ},{_,_}}],Note[{NoteDuration/.Opts[sup],sub[[2]]}, Sequence @@ sub[[1]]],Note[{NoteDuration/.Opts[sup],sub}]]];
UnPack[Chord] = Function[{sub,opts},If[Opts[sub]=={},Data[sub][[2]],{Opts[sub],PcV[sub]}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,NoteDuration->NoteDuration[subs[[1]]]]];

Options[Note] = {NoteDuration->1,Velocity->64}

(*****************)

Chord[d:{_,{{_,_}...}},opts___?OptionQ] := Chord[Note[{d[[1]],#}]&/@d[[2]],opts]
Chord[x_Counterpoint]                   := Chord[Progression[x]]
Chord[x_Melody]                         := Chord /@ x
Chord[p:{__?AtomQ}, opts___?OptionQ]    := Chord[Note[#,opts]& /@ p,opts]

f2p = Function[f, N[12*Log[2,f/220]+57]];

(* threshold ranging from 0 to 1 *)
Chord[x_Snippet, threshold_] := (* just for fun, when generating snippets from chords (and then back) use pitchclass=f2p[f_Integer * sc/sr] if you want exact results back *)
  Module[
    {
      s = Snippet[x,SoundType -> SampledSoundList],
      sr = SampleRate[x],
      sc = SampleCount[x],
      c,d,m
      },
    d = Fourier[Content[s],FourierParameters -> {-1, 1}];
    (* skip zero-freq *)
    d = Rest[d];
    (* nyquist theorem *)
    d = Take[d,Ceiling[Length[d]/2]];
    d = Abs[d];
    m = Max[d];
    c = Select[MapIndexed[{sr#2[[1]]/sc, #1} &, d], ((threshold m) <= #[[2]]) &];
    c = {f2p[#[[1]]],127 #[[2]]/m}& /@ c;
    Chord[{Duration[x],c}]
    ]

Counterpoint[x_Chord]                        := Counterpoint[Melody[x]]
Counterpoint[x_Progression, opts___?OptionQ] := Counterpoint[Melody[#]& /@ SeqOfParToParOfSeq[{Duration[#],Data[#]}& /@ x]]

Chord        /: Duration[x_Chord]        := NoteDuration /. Opts[x]
Counterpoint /: Duration[x_Counterpoint] := Max[Duration /@ x]
Melody       /: Duration[x_Melody]       := Total[NoteDuration /@ x]
Note         /: Duration[x_Note]         := NoteDuration[x]
Progression  /: Duration[x_Progression]  := Total[Duration /@ x]

Scale /: Inverse[x_Scale] :=
  Module[{pc = Mod[Content[x],Octave[x]],c,o=Length[Content[x]]},
    c = Table[
      If[#==={},DataAnyValue,
        #[[1,1]]-1-o*((Content[x][[#[[1,1]]]]-i)/Octave[x])
        ]&[Position[pc,i]],
      {i,0,Octave[x]-1}
      ];
    Scale[{o,c},Sequence @@ Opts[x]]
    ]

Scale /: Length[x_Scale] := Length[Content[x]]

Melody[x_Chord]                       := Melody /@ x
Melody[x_Progression]                 := Melody[Counterpoint[x]]
Melody[p:{__?AtomQ}, opts___?OptionQ] := Melody[Note[#,opts]& /@ p,Sequence@@RemOpts[{opts},NoteDuration]]

ModeAeolian = 5
ModeDorian = 1
ModeIonian = 0
ModeLocrian = 6
ModeLydian = 3
ModeMajor = ModeIonian
ModeMinor = ModeAeolian
ModeMixolydian = 4
ModePhrygian = 2

Note[p_?AtomQ, opts___?OptionQ] := Note[{NoteDuration/.{opts}/.Options[Note],{p,Velocity/.{opts}/.Options[Note]}},Sequence@@RemOpts[{opts},NoteDuration,Velocity]]

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

f712[x_] := Round[12/7(x-1)+2]
f712[x_,k_,m_] := Module[{r=f712[x+m]+7k,z=f712[0+m]+7k},r-(z-Mod[z,12])]
Scale[k_Integer,m_Integer, opts___?OptionQ] := Scale[{12,Table[f712[i,k,m],{i,0,6}]},opts]
Scale[k_Integer, opts___?OptionQ] := Scale[{12,Table[f712[i,k,3k],{i,0,6}]+12Quotient[k + 5,7*12]},opts]
Scale[] := Scale[0,ModeMajor]

Scale[x:{__Integer}, opts___?OptionQ] := Scale[{Max[x]-Min[x],Drop[x,-1]},opts] (* todo: what about strange, unsorted scales? *)

PitchCode[x_Scale] := Append[Content[x],Content[x][[1]]+Octave[x]] (* todo: what about strange, unsorted scales? *)

Scale[o_,d_][x_Chord]        := Module[{f=ScaleFunction[Scale[d,Sequence @@ o]]},Map[f,x,PitchCode]]
Scale[o_,d_][x_Counterpoint] := Module[{f=ScaleFunction[Scale[d,Sequence @@ o]]},Map[f,x,PitchCode]]
Scale[o_,d_][x_Integer]      := ScaleFunction[Scale[d,Sequence @@ o]][x]
Scale[o_,d_][x_Melody]       := Module[{f=ScaleFunction[Scale[d,Sequence @@ o]]},Map[f,x,PitchCode]]
Scale[o_,d_][x_Note]         := Module[{f=ScaleFunction[Scale[d,Sequence @@ o]]},ReplacePart[x,f[PitchCode[x]],PitchCode]]
Scale[o_,d_][x_Progression]  := Module[{f=ScaleFunction[Scale[d,Sequence @@ o]]},Map[f,x,PitchCode]]

ScaleFunction[x_Scale] :=
  Module[{f=Function[ss,Module[{scalesize = Length[Content[x]]},Floor[ss/scalesize]*Octave[x] + x[[Content, Mod[ss, scalesize] + 1]]]]},
    Function[ss,DataApply[f,ss]]
    ]

Seq[x:{__Chord}]        := Progression[x]
Seq[x:{__Counterpoint}] := Counterpoint[Seq[Progression /@ x]]
Seq[x:{__Melody}]       := Melody[Flatten[Note /@ x]]
Seq[x:{__Note}]         := Melody[x]
Seq[x:{__Progression}]  := Progression[Flatten[Chord /@ x]]

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
    Snippet[{SampledSoundFunction,Function[t, zin[f[t], a[t], sr][t]],Round[sr],Round[Duration[x]sr]},Sequence@@RemOpts[{opts},SampleRate]]
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
  Content,
  Octave,
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
  NoteDuration,
  NoteFunction,
  NotePlot,
  NoteQ,
  Octave,
  PitchCode,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction,
  ScaleQ,
  Velocity
  ];

EndPackage[]
