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
  GetDuration,
  GetDurations,
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
  ScaleQ,
  SetPitchCode,
  SetPitchCodes,
  SetVelocity,
  SetVelocitys,
  Velocity
  ];

Chord::todo = "Make Midi(Add|Get|Set)Chords use Chord as data."
Melody::todo = "Make Midi(Add|Get|Set)Melodies use Melody as data."

Chord::usage = ""<>ToDoString<>Chord::todo
ChordOfSilence::usage = ""
ChordQ::usage = ""
ChordsToMelodies::usage = ""
GetDurationCenter::usage = ""
GetDurationGCD::usage = ""
GetDurationLCM::usage = ""
GetDurationMean::usage = ""
GetDurationRange::usage = ""
GetNoteCount::usage = ""
GetPitchCodes::usage = ""
GetPitchCodeCenter::usage = ""
GetPitchCodeMean::usage = ""
GetPitchCodeRange::usage = ""
GetVelocitys::usage = ""
GetVelocityCenter::usage = ""
GetVelocityMean::usage = ""
GetVelocityRange::usage = ""
MelodiesToChords::usage = ""
Melody::usage = ""<>ToDoString<>Melody::todo
MelodyOfSilence::usage = ""
MelodyQ::usage = ""
Note::usage = ""
Scale::usage = ""
ScaleQ::usage = ""
SetPitchCode::usage = ""
SetPitchCodes::usage = ""
SetVelocity::usage = ""
SetVelocitys::usage = ""
Velocity::usage = ""

Begin["`Private`"]

(*
Format[n_Chord] := "\[SkeletonIndicator]Chord\[SkeletonIndicator]" /; ChordQ[n]
Format[n_Melody] := "\[SkeletonIndicator]Melody\[SkeletonIndicator]" /; MelodyQ[n]
Format[s_Scale] := "\[SkeletonIndicator]Scale\[SkeletonIndicator]" /; ScaleQ[n]
*)

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

GetDuration[c_?ChordQ] := c[[2,1]]
GetDuration[m_?MelodyQ] := Total[GetDurations[m]]
GetDurations[m_?MelodyQ] := #[[1]]& /@ m[[2]]
GetDurationCenter[x_] := Mean[GetDurationRange[x]]
GetDurationGCD[x_] := GCD@@Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]
GetDurationLCM[x_] := LCM@@Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]
GetDurationMean[x_] := Mean[Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]]
GetDurationRange[x_] := {Min[#],Max[#]}&[Cases[Flatten[GetDurations[x]],p$_/;(!DataNoValueQ[p$])]]

GetNoteCount[c_?ChordQ] := Length[c[[2,2]]]
GetNoteCount[m_?MelodyQ] := Length[m[[2]]]

GetPitchCodes[c_?ChordQ] := #[[1]]& /@ c[[2,2]]
GetPitchCodes[m_?MelodyQ] := #[[2,1]]& /@ m[[2]]
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

Scale[] := Scale[{12,{0,2,4,5,7,9,11}}]

Scale[x:{_Integer,{__Integer}}, opts___?OptionQ] := Scale[{opts},x]

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
  GetDuration,
  GetDurations,
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
  ScaleQ,
  SetPitchCode,
  SetPitchCodes,
  SetVelocity,
  SetVelocitys,
  Velocity
  ];

EndPackage[]
