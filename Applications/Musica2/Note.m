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
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Tuning`",
    "Musica2`Type`",
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
  NoteFunction, (* to be removed *)
  NotePlot,
  NoteRestQ,
  NoteTieQ,
  NoteQ,
  Octave,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction, (* to be removed *)
  ScaleQ,
  ScaleStep,
  ThirdStack,
  ThirdStackQ,
  Velocity
  ];

CreateElement[Note, {NoteDuration_, {PitchCode_,Velocity_}},{1,{60,64}},"todo"];
CreateContainer[Chord,Note,"todo"];
CreateContainer[Melody,Note,"todo"];
CreateContainer[Progression,Chord,"todo"];
CreateContainer[Counterpoint,Melody,"todo"];

CreateElement[FigBass, {Octave:(Infinity|_Integer), {Bass:{__Integer}, Code_Integer}},{12,{{0},1}},"todo",{DirectedInfinity}];
CreateElement[Intervals, {Octave:(Infinity|_Integer), Content:{__Integer}},{12,{0,0,0,0,0,0,0}},"todo",{DirectedInfinity}];
CreateElement[Scale, {Tonic_Integer, Steps:{__Integer}},{0,{2,2,1,2,2,2,1}},"todo"];
CreateElement[ThirdStack, {Base:{__Integer}, {Bass_Integer, Code_Integer}},{{3,4},{0,1}},"todo"];

ModeAeolian::usage = "todo"
ModeDorian::usage = "todo"
ModeIonian::usage = "todo"
ModeLocrian::usage = "todo"
ModeLydian::usage = "todo"
ModeMajor::usage = "todo"
ModeMinor::usage = "todo"
ModeMixolydian::usage = "todo"
ModePhrygian::usage = "todo"
NoteFunction::usage = "todo" (* to be removed *)
NotePlot::usage = "todo"
NoteRestQ::usage = "todo"
NoteTieQ::usage = "todo"
ScaleStep::usage = "todo"

Begin["`Private`"]

(*****************)

DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{{__?OptionQ},{_,_}}],Note[{NoteDuration/.Opts[sup],sub[[2]]}, Sequence @@ sub[[1]]],Note[{NoteDuration/.Opts[sup],sub}]]];
UnPack[Chord] = Function[{sub,opts},If[Opts[sub]=={},Data[sub][[2]],{Opts[sub],{PitchCode[sub],Velocity[sub]}}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,NoteDuration->NoteDuration[subs[[1]]]]];

Tidy[Melody] = Module[{n = Note[#],i},
  For[i = 1, i < Length[n], i++,
    If[NoteDuration[n[[i]]]===0,
      n = Delete[n,i];
      i -= 1,
      If[i == 1 && NoteTieQ[n[[1]]],
        n[[1]] = ReplacePart[n[[1]],DataNoValue,PitchCode];
        n[[1]] = ReplacePart[n[[1]],DataNoValue,Velocity]
        ];
      If[(NoteRestQ[n[[i]]] && NoteRestQ[n[[i+1]]]) || NoteTieQ[n[[i+1]]],
        n[[i]] = ReplacePart[n[[i]],NoteDuration[n[[i]]]+NoteDuration[n[[i+1]]],NoteDuration];
        n = Delete[n,i+1];
        i -= 1;
        ]
      ];
    ];
  Melody[n,Sequence @@ Opts[#]]
  ]&

(*****************)

Chord[d:{_,{{_,_}...}},opts___?OptionQ] := Chord[Note[{d[[1]],#}]&/@d[[2]],opts]
Chord[x_Counterpoint]                   := Chord[Progression[x]]
Chord[x_FigBass,       opts___?OptionQ] := Chord[PitchCode[x],opts]
Chord[x_Intervals,     opts___?OptionQ] := Chord[PitchCode[x],opts]
Chord[x_Melody,        opts___?OptionQ] := Chord[#,opts]& /@ Note[x]
Chord[p:{__?AtomQ},    opts___?OptionQ] := Chord[Note[#,opts]& /@ p,opts]
Chord[x_ThirdStack,    opts___?OptionQ] := Chord[PitchCode[x],opts]

Counterpoint[x_Chord]                        := Counterpoint[Melody[x]]
Counterpoint[x_Progression, opts___?OptionQ] := Counterpoint[Melody[#]& /@ SeqOfParToParOfSeq[{Duration[#],Data[#]}& /@ x]]

Chord        /: Duration[x_Chord]        := NoteDuration /. Opts[x]
Counterpoint /: Duration[x_Counterpoint] := Max[Duration /@ x]
Melody       /: Duration[x_Melody]       := Total[NoteDuration /@ x]
Note         /: Duration[x_Note]         := NoteDuration[x]
Progression  /: Duration[x_Progression]  := Total[Duration /@ x]

Convert[Time,PitchCode,x_Melody] := NoteFunction[x, PitchCode]
Convert[Time,Velocity,x_Melody] := NoteFunction[x, Velocity]

Convert[ScaleStep,PitchCode,x_Scale] :=
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

Convert[PitchCode,ScaleStep,x_Scale] :=
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

Convert[ScaleStep,PitchCode] := Convert[ScaleStep,PitchCode,Scale[]]
Convert[PitchCode,ScaleStep] := Convert[PitchCode,ScaleStep,Scale[]]

FigBass[x_Chord,      opts___?OptionQ] := FigBass[PitchCode[x],opts]
FigBass[x_Intervals,  opts___?OptionQ] := FigBass[PitchCode[x],opts]
FigBass[x_Melody,     opts___?OptionQ] := FigBass[PitchCode[x],opts]
FigBass[x_Scale,      opts___?OptionQ] := FigBass[PitchCode[x],opts]
FigBass[x_ThirdStack, opts___?OptionQ] := FigBass[PitchCode[x],opts]

FigBass[x_Integer, opts___?OptionQ] := FigBass[{Octave/.{opts}/.{Octave->Infinity},{{0},x}}, Sequence @@ RemOpts[{opts},Octave]]

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

Intervals[x_Chord,      opts___?OptionQ] := Intervals[PitchCode[x],opts]
Intervals[x_FigBass,    opts___?OptionQ] := Intervals[PitchCode[x],opts]
Intervals[x_Melody,     opts___?OptionQ] := Intervals[PitchCode[x],opts]
Intervals[x_Scale,      opts___?OptionQ] := Intervals[PitchCode[x],opts]
Intervals[x_ThirdStack, opts___?OptionQ] := Intervals[PitchCode[x],opts]

Intervals[x:{__Integer}, opts___?OptionQ] :=
  ReplacePart[
    #,
    ReplacePart[Content[#]/2,(Content[#][[1]]-Length[x])/2,{1}],
    Content
    ]&[Intervals[x,x,opts]]

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

Scale /: Length[x_Scale] := Length[Steps[x]]

Melody[x_Chord,opts___?OptionQ]       := Melody[#,opts]& /@ Note[x]
Melody[x_Intervals,opts___?OptionQ]   := Melody[PitchCode[x],opts]
Melody[x_FigBass,opts___?OptionQ]     := Melody[PitchCode[x],opts]
Melody[x_Progression]                 := Melody[Counterpoint[x]]
Melody[p:{__?AtomQ}, opts___?OptionQ] := Melody[Note[#,opts]& /@ p,Sequence@@RemOpts[{opts},NoteDuration]]
Melody[x_ThirdStack,opts___?OptionQ]  := Melody[PitchCode[x],opts]

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

NoteRestQ[x_Note] := DataNoValueQ[PitchCode[x]] || DataNoValueQ[Velocity[x]] || (Velocity[x] === 0)

NoteTieQ[x_Note] := DataTieQ[PitchCode[x]] || DataTieQ[Velocity[x]]

Scale /: Octave[x_Scale] := Total[Steps[x]]

Par[x:{__Chord}]        := Chord[Flatten[Note /@ x]]
Par[x:{__Counterpoint}] := Counterpoint[Flatten[Melody /@ x]]
Par[x:{__Melody}]       := Counterpoint[x]
Par[x:{__Note}]         := Chord[x]
Par[x:{__Progression}]  := Progression[Par[Counterpoint /@ x]]

PitchCode[x_FigBass] :=
  If[Octave[x]=!=Infinity,Mod[#,Octave[x]],#]&[
    ((#[[1]]-1)&/@Position[Reverse[IntegerDigits[Code[x],2]],1])
    ]+Bass[x][[1]]

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

PitchCode[x_Scale] := DeltasToValues[Steps[x],Tonic[x]]

PitchCode[x_ThirdStack] :=
  Module[{b=Base[x]},
    If[Code[x]==0,{},
      DeltasToValues[b[[#]]& /@ Reverse[Drop[IntegerDigits[Code[x],Length[b]],1]+1]]+Bass[x]
      ]
    ]

Chord        /: Play2[x_Chord]        := Play2[Sound[x]]
Counterpoint /: Play2[x_Counterpoint] := Play2[Sound[x]]
Melody       /: Play2[x_Melody]       := Play2[Sound[x]]
Note         /: Play2[x_Note]         := Play2[Sound[x]]
Progression  /: Play2[x_Progression]  := Play2[Sound[x]]

Progression[x_Counterpoint, opts___?OptionQ] := Progression[Chord[#]& /@ ParOfSeqToSeqOfPar[Data /@ x]]
Progression[x_Melody]                        := Progression[Chord[x]]

f712[x_] := Round[12/7(x-1)+2]
f712[x_,k_,m_] := Module[{r=f712[x+m]+7k,z=f712[0+m]+7k},r-(z-Mod[z,12])]
Scale[k_Integer,m_Integer, opts___?OptionQ] := Scale[Table[f712[i,k,m],{i,0,7}],opts]
Scale[k_Integer, opts___?OptionQ] := Scale[Table[f712[i,k,3k],{i,0,7}]+12Quotient[k + 5,7*12],opts]

Scale[x_Intervals,   opts___?OptionQ] := Scale[PitchCode[x],opts]
Scale[x:{__Integer}, opts___?OptionQ] := Scale[{Min[x],ValuesToDeltas[x]},opts] (* todo: what about strange, unsorted scales? *)

Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Chord]        := Module[{f=Convert[ScaleStep,PitchCode,Scale[o,d]]},Map[f,x,PitchCode]]
Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Counterpoint] := Module[{f=Convert[ScaleStep,PitchCode,Scale[o,d]]},Map[f,x,PitchCode]]
Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Integer]      := Convert[ScaleStep,PitchCode,Scale[o,d]][x]
Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Melody]       := Module[{f=Convert[ScaleStep,PitchCode,Scale[o,d]]},Map[f,x,PitchCode]]
Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Note]         := Module[{f=Convert[ScaleStep,PitchCode,Scale[o,d]]},ReplacePart[x,f[PitchCode[x]],PitchCode]]
Scale[o_?OptionQ, d_?(DataQ[Scale])][x_Progression]  := Module[{f=Convert[ScaleStep,PitchCode,Scale[o,d]]},Map[f,x,PitchCode]]

Seq[x:{__Chord}]        := Progression[x]
Seq[x:{__Counterpoint}] := Counterpoint[Seq[Progression /@ x]]
Seq[x:{__Melody}]       := Melody[Flatten[Note /@ x]]
Seq[x:{__Note}]         := Melody[x]
Seq[x:{__Progression}]  := Progression[Flatten[Chord /@ x]]

v2a = Function[v, v/127];
zin = Function[{f, a, sr}, N[a Sin[2Pi f#/sr]]&];

Chord        /: Snippet[x_Chord, opts___?OptionQ] := Snippet[Counterpoint[x],opts]
Counterpoint /: Snippet[x_Counterpoint, opts___?OptionQ] := Snippet[#,opts]& /@ x
Melody       /: Snippet[x_Melody, opts___?OptionQ] := Convert[Melody,Snippet,Instrument,opts][x]
Note         /: Snippet[x_Note, opts___?OptionQ] := Snippet[Melody[x],opts]
Progression  /: Snippet[x_Progression, opts___?OptionQ] := Snippet[Counterpoint[x],opts]

Chord        /: Sound[x_Chord,        opts___?OptionQ] := Sound[Snippet[x,opts]]
Counterpoint /: Sound[x_Counterpoint, opts___?OptionQ] := Sound[Snippet[x,opts]]
Melody       /: Sound[x_Melody,       opts___?OptionQ] := Sound[Snippet[x,opts]]
Note         /: Sound[x_Note,         opts___?OptionQ] := Sound[Snippet[x,opts]]
Progression  /: Sound[x_Progression,  opts___?OptionQ] := Sound[Snippet[x,opts]]

ThirdStack[x_Chord,     opts___?OptionQ] := ThirdStack[PitchCode[x],opts]
ThirdStack[x_FigBass,   opts___?OptionQ] := ThirdStack[PitchCode[x],opts]
ThirdStack[x_Intervals, opts___?OptionQ] := ThirdStack[#,opts]& /@ PitchCode[x]
ThirdStack[x_Melody,    opts___?OptionQ] := ThirdStack[PitchCode[x],opts]
ThirdStack[x_Scale,     opts___?OptionQ] := ThirdStack[PitchCode[x],opts]

ThirdStack[x_Integer, opts___?OptionQ] := ThirdStack[{{3,4},{0,x}},opts]

ThirdStack[{base:{__Integer},pc:{__Integer}}, opts___?OptionQ] := (* todo: this is too easy, should be the last try really *)
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

ThirdStack[x:{__Integer}, opts___?OptionQ] :=  ThirdStack[{Union[ValuesToDeltas[Union[x]]],x},opts] (* todo: this is too easy, should be the last try really *)

Scale /: TestSuite[Scale] = Join[TestSuite[Scale],{
  TestCase[Convert[PitchCode, ScaleStep] /@ Convert[ScaleStep, PitchCode] /@ Range[-10, 10], Range[-10, 10]],
  TestCase[Data[Scale[PitchCode[Scale[]]]],{0, {2, 2, 1, 2, 2, 2, 1}}]
  }]
  
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
  NoteRestQ,
  NoteTieQ,
  NoteQ,
  Octave,
  Progression,
  ProgressionQ,
  Scale,
  ScaleFunction,
  ScaleQ,
  ScaleStep,
  ThirdStack,
  ThirdStackQ,
  Velocity
  ];

EndPackage[]
