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
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ];

Chord::usage = ""
ChordQ::usage = ""
Counterpoint::usage = ""
CounterpointQ::usage = ""
Melody::usage = ""
MelodyQ::usage = ""
Note::usage = ""
NoteQ::usage = ""
PitchCode::usage = ""
Progression::usage = ""
ProgressionQ::usage = ""
Velocity::usage = ""

Begin["`Private`"]

(*****************)

DefineSup[Chord,Note];
Options[Chord] = {Duration:>(Duration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}
DataQ[Chord] = MatchQ[#, {({_,_}|{_?OptionQ,{_,_}})...}]&
Pack[Chord] = Function[{sup,sub},If[MatchQ[sub,{_Integer,_Integer}],Note[{Duration/.Info[sup],sub}],Note[{Duration/.Info[sup],sub[[2]]}, Sequence @@ sub[[1]]]]];
UnPack[Chord] = Function[{sub,opts},If[Info[sub]=={},Data[sub][[2]],{Info[sub],Data[sub][[2]]}]];
UnPackOpts[Chord] = Function[{subs,opts},Prepend[opts,Duration->Duration[subs[[1]]]]];

DefineSup[Counterpoint,Melody];
Options[Counterpoint] = {Duration:>(Duration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

DefineSup[Melody,Note];
Options[Melody] = {Duration:>(Duration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

DefineSub[Note];
DataQ[Note] = MatchQ[#, {_, {_,_}}]&
Options[Note] = {Duration->1,Velocity->64}

DefineSup[Progression,Chord];
Options[Progression] = {Duration:>(Duration/.Options[Note]),Velocity:>(Velocity/.Options[Note])}

(*****************)

Chord /: Duration[x_Chord] := Duration /. Info[x]
Counterpoint /: Duration[x_Counterpoint] := Max[Duration /@ x]
Melody /: Duration[x_Melody] := Total[Duration /@ x]
Note /: Duration[x_Note] := Data[x][[1]]
Progression /: Duration[x_Progression] := Total[Duration /@ x]

Note[p_, opts___?OptionQ] := Note[{Duration/.{opts}/.Options[Note],{p,Velocity/.{opts}/.Options[Note]}},opts]

Note /: PitchCode[x_Note] := Data[x][[2,1]]

p2f = Function[p, 220*2^((p - 57)/12)];
v2a = Function[v, v/127];
zin = Function[{f, a, sr}, N[a Sin[2Pi f#/sr]] &];

Melody /: Show[x_Melody] := Show[Snippet[x]] /; MelodyQ[x]

Melody /: Snippet[x_Melody, opts___?OptionQ] :=
  Module[{v,f,a,sr=SampleRate/.{opts}/.Options[Snippet]},
    v = N[Data[x] /. {DataNoValue -> 0}];
    f = MakeNestedIfs[{#[[1]]sr, p2f[#[[2, 1]]]} & /@ v];
    a = MakeNestedIfs[{#[[1]]sr, v2a[#[[2, 2]]]} & /@ v];
    Snippet[{Function[t, zin[f[t], a[t], sr][t]],Duration[x]sr,sr}]
    ] /; MelodyQ[x]

Counterpoint /: Sound[x_Counterpoint, opts___?OptionQ] := Sound[Snippet[#,opts]& /@ x] /; CounterpointQ[x]

Note /: Velocity[x_Note] := Data[x][[2,2]]

End[]

Protect[
  ];

EndPackage[]
