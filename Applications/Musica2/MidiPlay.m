(* :Title: MidiPlay *)

(* :Summary: Functions for MidiPlay *)

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

(* :Context: Musica2`MidiPlay` *)

(* :History:
  2004-08-28  bch :  made MidiToSound call SoundMixStereo
                     made a new version of MidiPlay based on the new function m2s wich seems to be faster than MidiToSound and SoundBySine
                     changed p2f to one octave down
  2004-08-26  bch :  added some help/usage-text
  2004-08-23  bch :  added MidiExportWav
  2004-08-19  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`MidiPlay`",
  {
    "Musica2`Midi`",
    "Musica2`Sound`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  MidiExportWav,
  MidiPlay,
  MidiToSound,
  SoundBySample,
  SoundBySine
  ];

MidiExportWav::usage = "MidiExportWav[fn_String, m_Midi] writes a WAV file out of the Midi-object using sine-waves, just as MidiPlay."
MidiPlay::usage = "MidiPlay[m_Midi] plays the Midi-object using sine.waves."
MidiToSound::usage = "MidiToSound[m_Midi, f_, opts___] creates a Sound-object from a Midi-object. The f-argument must be a function taking duration, pitch, velocity and mean-pitch as arguments and returning a Sound-object. MidiToSound then uses SoundSeq and SoundPar to assemble the Sound-object returned."
SoundBySample::usage = "SoundBySample[s_Sound, opts___] returns a possible f-argument to MidiToSound that uses SoundPitchShift and SoundSetDuration."
SoundBySine::usage = "SoundBySine[opts___] returns a possible f-argument to MidiToSound."

Begin["`Private`"]

p2f = Function[p, 220*2^((p - 57)/12)];
v2a = Function[v, v/127];
zin = Function[{f, a}, N[a Sin[2Pi f#]] &];
saw = Function[{f, a}, N[2a(FractionalPart[f#] - 0.5)] &];
sqr = Function[{f, a}, N[-a Sign[FractionalPart[f#] - 0.5]] &];

m2s[mx_Midi] := (* this one should be even faster, but i cant get it to work *)
  Module[{m = MidiSetState[mx, {MidiShape -> MidiMelody, MidiTimeUnit -> MidiSec, MidiTiming -> MidiDelta}], v},
    v = MidiGetMelodies[m] /. {DataNoValue -> 0};
    v = ({#[[1]], zin[p2f[#[[2, 1]]], v2a[#[[2, 2]]]]} & /@ #[[2]]) & /@ v;
    v = MakeNestedIfs[#, 0. &] & /@ v;
    v = Function[t, Evaluate[#[t][t]]] & /@ v;
    s = SoundMakeFunc[v, SoundDuration -> MidiGetDuration[m]];
    SoundMixStereo[s]
    ]

m2s[mx_Midi] := (* this one seems to be a little bit faster than the previous version *)
  Module[{m = MidiSetState[mx, {MidiShape -> MidiMelody, MidiTimeUnit -> MidiSec, MidiTiming -> MidiDelta}],v,f,a,w,s},
    v = MidiGetMelodies[m] /. {DataNoValue -> 0};
    f = MakeNestedIfs[{#[[1]], p2f[#[[2, 1]]]} & /@ #[[2]]] & /@ v;
    a = MakeNestedIfs[{#[[1]], v2a[#[[2, 2]]]} & /@ #[[2]]] & /@ v;
    w = MapThread[Function[{fi, ai}, Evaluate[Function[t, zin[fi[t], ai[t]][t]]]], {f, a}];
    s = SoundMakeFunc[w,SoundDuration->MidiGetDuration[m]];
    SoundMixStereo[s]
    ]

MidiExportWav[fn_String, m_Midi] := SoundExportWav[fn, m2s[m]]

MidiPlay[m_Midi] := Show[m2s[m]]

MidiToSound[m_Midi, f_, opts___] :=
  Module[
    {mp,r},
    (* get the music as {{{timing, {p, v}} ...} ...} *)
    r = Cases[
      MidiSetState[m, {MidiTimeUnit -> MidiSec, MidiShape -> MidiMelody, MidiTiming -> MidiDelta}][[2]],
      {{MidiNoteOn, _},
      d$_} -> d$
      ];
    (* calculate mean pitch *)
    mp = N[Mean[Flatten[(If[! DataNoValueQ[#[[2]]], #[[2, 1]], {}] & /@ #) & /@ r]]];
    (* make them into lists of sound - objects *)
    r = (
      If[DataNoValueQ[#[[2]]],
        SoundOfSilence[SoundChannelCount -> 1, SoundDuration -> #[[1]], opts],
        f[#[[1]], #[[2, 1]], #[[2, 2]], mp]
        ] & /@ #
      ) & /@ r;
    (* then use SoundSeq *)
    r = SoundSeq /@ r;
    (* then use SoundPar *)
    r = SoundPar[r];
    (* create a simple mix for two output - channels *)
    SoundMixStereo[r]
    ]

SoundBySample[s_Sound, opts___] := Function[{d, p, v, mp}, SoundSetDuration[SoundPitchShift[s, p2f[p]/p2f[mp], opts], d, opts]]

SoundBySine[opts___] := Function[{d, p, v, mp}, SoundMakeFunc[zin[p2f[p], v2a[v]], SoundDuration -> d, opts]]

End[]

Protect[
  MidiExportWav,
  MidiPlay,
  MidiToSound,
  SoundBySample,
  SoundBySine
  ];

EndPackage[]
