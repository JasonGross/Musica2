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
  ];

CreateElement[Note, {NoteDuration_, PcV:{PitchCode_,Velocity_}}];
CreateContainer[Chord,Note];
CreateContainer[Melody,Note];
CreateContainer[Progression,Chord];
CreateContainer[Counterpoint,Melody];

PitchCode::usage = ""
Velocity::usage = ""

Begin["`Private`"]

(*****************)

Options[Chord] = {NoteDuration:>(NoteDuration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}
DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{_Integer,_Integer}],Note[{NoteDuration/.Opts[sup],sub}],Note[{NoteDuration/.Opts[sup],sub[[2]]}, Sequence @@ sub[[1]]]]];
UnPack[Chord] = Function[{sub,opts},If[Opts[sub]=={},Data[sub][[2]],{Opts[sub],PcV[sub]}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,NoteDuration->NoteDuration[subs[[1]]]]];

Options[Counterpoint] = {NoteDuration:>(NoteDuration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

Options[Melody] = {NoteDuration:>(NoteDuration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

Options[Note] = {NoteDuration->1,Velocity->64}

Options[Progression] = {NoteDuration:>(NoteDuration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

(*****************)

Chord /: Duration[x_Chord] := Duration /. Opts[x]
Counterpoint /: Duration[x_Counterpoint] := Max[Duration /@ x]
Melody /: Duration[x_Melody] := Total[Duration /@ x]
Note /: Duration[x_Note] := NoteDuration[x]
Progression /: Duration[x_Progression] := Total[Duration /@ x]

Note[p_, opts___?OptionQ] := Note[{NoteDuration/.{opts}/.Options[Note],{p,Velocity/.{opts}/.Options[Note]}},opts]

p2f = Function[p, 220*2^((p - 57)/12)];
v2a = Function[v, v/127];
zin = Function[{f, a, sr}, N[a Sin[2Pi f#/sr]] &];

Melody /: Show[x_Melody] := Show[Snippet[x]]

Melody /: Snippet[x_Melody, opts___?OptionQ] :=
  Module[{d,p,v,f,a,sr=SampleRate/.{opts}/.Options[Snippet]},
    d = (Duration /@ x) * sr;
    p = PitchCode /@ x /. {DataNoValue -> 0};
    v = Velocity /@ x /. {DataNoValue -> 0};
    f = MakeNestedIfs[Transpose[{d,p2f /@ p}]];
    a = MakeNestedIfs[Transpose[{d,v2a /@ v}]];
    Snippet[{SampledSoundFunction,Function[t, zin[f[t], a[t], sr][t]],Round[sr],Round[Duration[x]sr]}]
    ]

Counterpoint /: Sound[x_Counterpoint, opts___?OptionQ] := Sound[Snippet[#,opts]& /@ x]

End[]

Protect[
  ];

EndPackage[]
