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
  2004-09-03  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Note`",
  {
    "Musica2`Common`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  GetData,
  GetDuration,
  GetDurations,
  GetInfo,
  SetDuration,
  SetDurations
  ];

Unprotect[
  Chord,
  ChordOfSilence,
  ChordQ,
  ChordsToMelodies,
  GetDurationCenter,
  GetDurationGCD,
  GetDurationLCM,
  GetDurationMean,
  GetDurationRange,
  GetNoteCount,
  GetPitchCodes,
  GetPitchCodeCenter,
  GetPitchCodeMean,
  GetPitchCodeRange,
  GetVelocitys,
  GetVelocityCenter,
  GetVelocityMean,
  GetVelocityRange,
  MelodiesToChords,
  Melody,
  MelodyOfSilence,
  MelodyQ,
  Note,
  Scale,
  ScaleStepToPitchCode,
  ScaleQ,
  SetPitchCode,
  SetPitchCodes,
  SetVelocity,
  SetVelocitys,
  Velocity
  ];

Note::todo = "Write help/usage-text's for the package."

Chord::usage = "todo"
ChordOfSilence::usage = "todo"
ChordQ::usage = "todo"
ChordsToMelodies::usage = "todo"
GetDurationCenter::usage = "todo"
GetDurationGCD::usage = "todo"
GetDurationLCM::usage = "todo"
GetDurationMean::usage = "todo"
GetDurationRange::usage = "todo"
GetNoteCount::usage = "todo"
GetPitchCodes::usage = "todo"
GetPitchCodeCenter::usage = "todo"
GetPitchCodeMean::usage = "todo"
GetPitchCodeRange::usage = "todo"
GetVelocitys::usage = "todo"
GetVelocityCenter::usage = "todo"
GetVelocityMean::usage = "todo"
GetVelocityRange::usage = "todo"
MelodiesToChords::usage = "todo"
Melody::usage = "todo"
MelodyOfSilence::usage = "todo"
MelodyQ::usage = "todo"
Note::usage = "todo"<>ToDoString<>Note::todo
Scale::usage = "todo"
ScaleStepToPitchCode::usage = "todo"
ScaleQ::usage = "todo"
SetPitchCode::usage = "todo"
SetPitchCodes::usage = "todo"
SetVelocity::usage = "todo"
SetVelocitys::usage = "todo"
Velocity::usage = "todo"

Begin["`Private`"]

Format[n_Chord] := "\[SkeletonIndicator]Chord\[SkeletonIndicator]" /; ChordQ[n]
Format[m_Melody] := "\[SkeletonIndicator]Melody\[SkeletonIndicator]" /; MelodyQ[m]
Format[s_Scale] := "\[SkeletonIndicator]Scale\[SkeletonIndicator]" /; ScaleQ[s]

Options[Chord] =
  {
    Duration :> (Duration /. Options[Note]),
    Velocity :> (Velocity /. Options[Note])
    };

Options[Melody] =
  {
    Duration :> (Duration /. Options[Note]),
    Velocity :> (Velocity /. Options[Note])
    };

Options[Note] =
  {
    Duration -> 1,
    Velocity -> 64
    };

Chord[dpv:{_,{{_,_},{_,_}...}}, opts___?OptionQ] := Chord[{opts},dpv](* TODO: remove any Duration and Velocity from opts *)
Chord[pv:{{_,_},{_,_}...}, opts___?OptionQ] := Chord[{Duration/.{opts}/.Options[Chord],pv},opts]
Chord[{d_,p:{__}}, opts___?OptionQ] := Chord[{d,{#,Velocity/.{opts}/.Options[Chord]}& /@ p},opts]
Chord[p:{__}, opts___?OptionQ] := Chord[{Duration/.{opts}/.Options[Chord],p},opts]

ChordOfSilence[n_Integer,opts___?OptionQ] := Chord[Table[DataNoValue,{n}],opts]

ChordQ[expr_] := MatchQ[expr,Chord[_?OptionQ,{_,{{_,_}...}}]]

ChordsToMelodies[c:{_Chord,_Chord...}] := Melody/@SeqOfParToParOfSeq[#[[2]]&/@c]

GetData[x_?((ChordQ[#]||MelodyQ[#])&)] := x[[2]]

GetDuration[c_?ChordQ] := c[[2,1]]
GetDuration[m_?MelodyQ] := Total[GetDurations[m]]
GetDurations[m_?MelodyQ] := #[[1]]& /@ m[[2]]
GetDurationCenter[x_] := Mean[GetDurationRange[x]]
GetDurationGCD[x_] := GCD@@Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]
GetDurationLCM[x_] := LCM@@Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]
GetDurationMean[x_] := Mean[Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]]
GetDurationRange[x_] := {Min[#],Max[#]}&[Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]]

GetInfo[x_?((ChordQ[#]||MelodyQ[#])&)] := x[[1]]

GetNoteCount[c_?ChordQ] := Length[c[[2,2]]]
GetNoteCount[m_?MelodyQ] := Length[m[[2]]]

GetPitchCodes[c_?ChordQ] := #[[1]]& /@ c[[2,2]]
GetPitchCodes[m_?MelodyQ] := #[[2,1]]& /@ m[[2]]
GetPitchCodes[s_?ScaleQ] := s[[2,2]]
GetPitchCodeCenter[x_] := Mean[GetPitchCodeRange[x]]
GetPitchCodeMean[x_] := Mean[Cases[Flatten[GetPitchCodes[x]],p$_/;(!DataNoValueQ[p$])]]
GetPitchCodeRange[x_] := {Min[#],Max[#]}&[Cases[Flatten[GetPitchCodes[x]],p$_/;(!DataNoValueQ[p$])]]

GetVelocitys[c_?ChordQ] := #[[2]]& /@ c[[2,2]]
GetVelocitys[m_?MelodyQ] := #[[2,2]]& /@ m[[2]]
GetVelocityCenter[x_] := Mean[GetVelocityRange[x]]
GetVelocityMean[x_] := Mean[Cases[Flatten[GetVelocitys[x]],p$_/;(!DataNoValueQ[p$])]]
GetVelocityRange[x_] := {Min[#],Max[#]}&[Cases[Flatten[GetVelocitys[x]],p$_/;(!DataNoValueQ[p$])]]

MelodiesToChords[m:{_Melody,_Melody...}] := Chord/@ParOfSeqToSeqOfPar[#[[2]]&/@m]

Melody[dpv:{{_,{_,_}}...}, opts___?OptionQ] := Melody[{opts},dpv](* TODO: remove any Duration and Velocity from opts *)
Melody[dp:{{_,_}...}, opts___?OptionQ] := Melody[{#[[1]],{#[[2]],Velocity/.{opts}/.Options[Melody]}}& /@ dp,opts]
Melody[p:{__}, opts___?OptionQ] := Melody[{Duration/.{opts}/.Options[Melody],#}& /@ p,opts]

MelodyOfSilence[n_Integer, opts___?OptionQ] := Melody[Table[DataNoValue,{n}],opts]

MelodyQ[expr_] := MatchQ[expr,Melody[_?OptionQ,{{_,{_,_}}...}]]

Scale[opts___?OptionQ] := Scale[{12,{0,2,4,5,7,9,11}},opts]
Scale[p:{__Integer}, opts___?OptionQ] := Scale[{12,p},opts]
Scale[op:{_Integer,{__Integer}}, opts___?OptionQ] := Scale[{opts},op]

ScaleStepToPitchCode[ss_Integer,s_?ScaleQ] :=
  Module[{scalesize=Length[s[[2,2]]]},
    Floor[ss/scalesize]*s[[2,1]] + s[[2,2,Mod[ss,scalesize]+1]]
    ]

ScaleStepToPitchCode[x_?((ChordQ[#]||MelodyQ[#])&),s_?ScaleQ] :=
  SetPitchCodes[x,
    If[DataNoValueQ[#],#,
      If[DataTieQ[#],
        DataTie[ScaleStepToPitchCode[DataUnTie[#],s]],
        ScaleStepToPitchCode[#,s]
        ]
      ]& /@ GetPitchCodes[x]
    ]

ScaleQ[expr_] := MatchQ[expr,Scale[_?OptionQ,{_Integer,{__Integer}}]]

SetDuration[c_?ChordQ, d_] := ReplacePart[c,d,{2,1}]
SetDuration[m_?MelodyQ, f_] := Module[{r=m}, Do[r[[2,i,1]] = f[r[[2,i,1]],i],{i,GetNoteCount[m]}]; r]
SetDurations[m_?MelodyQ, d_List] := Module[{r=m}, Do[r[[2,i,1]] = d[[i]], {i,GetNoteCount[m]}]; r] /; Length[d]==GetNoteCount[m]

SetPitchCode[c_?ChordQ, f_] := Module[{r=c}, Do[r[[2,2,i,1]] = f[r[[2,2,i,1]],i], {i,GetNoteCount[c]}]; r]
SetPitchCode[m_?MelodyQ, f_] := Module[{r=m}, Do[r[[2,i,2,1]] = f[r[[2,i,2,1]],i], {i,GetNoteCount[m]}]; r]

SetPitchCodes[c_?ChordQ, p_List] := Module[{r=c}, Do[r[[2,2,i,1]] = p[[i]], {i,GetNoteCount[c]}]; r] /; Length[p]==GetNoteCount[c]
SetPitchCodes[m_?MelodyQ, p_List] := Module[{r=m}, Do[r[[2,i,2,1]] = p[[i]], {i,GetNoteCount[m]}]; r] /; Length[p]==GetNoteCount[m]

SetVelocity[c_?ChordQ, f_] := Module[{r=c}, Do[r[[2,2,i,2]] = f[r[[2,2,i,2]],i], {i,GetNoteCount[c]}]; r]
SetVelocity[m_?MelodyQ, f_] := Module[{r=m}, Do[r[[2,i,2,2]] = f[r[[2,i,2,2]],i], {i,GetNoteCount[m]}]; r]

SetVelocitys[c_?ChordQ, v_List] := Module[{r=c}, Do[r[[2,2,i,2]] = v[[i]], {i,GetNoteCount[c]}]; r] /; Length[v]==GetNoteCount[c]
SetVelocitys[m_?MelodyQ, v_List] := Module[{r=m}, Do[r[[2,i,2,2]] = v[[i]], {i,GetNoteCount[m]}]; r] /; Length[v]==GetNoteCount[m]

End[]

Protect[
  GetData,
  GetDuration,
  GetDurations,
  GetInfo,
  SetDuration,
  SetDurations
  ];

Protect[
  Chord,
  ChordOfSilence,
  ChordQ,
  ChordsToMelodies,
  GetDurationCenter,
  GetDurationGCD,
  GetDurationLCM,
  GetDurationMean,
  GetDurationRange,
  GetNoteCount,
  GetPitchCodes,
  GetPitchCodeCenter,
  GetPitchCodeMean,
  GetPitchCodeRange,
  GetVelocitys,
  GetVelocityCenter,
  GetVelocityMean,
  GetVelocityRange,
  MelodiesToChords,
  Melody,
  MelodyOfSilence,
  MelodyQ,
  Note,
  Scale,
  ScaleStepToPitchCode,
  ScaleQ,
  SetPitchCode,
  SetPitchCodes,
  SetVelocity,
  SetVelocitys,
  Velocity
  ];

EndPackage[]
