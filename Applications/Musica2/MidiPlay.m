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
MidiToSound::usage = "MidiToSound[m_Midi, f_, opts___] creates a Sound-object from a Midi-object. The f-argument must be a function taking duration, pitch, velocity and mean-pitch as arguments and returning a Sound-object. MidiToSound the uses SoundSeq and SoundPar to assemble the Sound-object returned."
SoundBySample::usage = "SoundBySample[s_Sound, opts___] returns a possible f-argument to MidiToSound and uses SoundPitchShift and SoundSetDuration."
SoundBySine::usage = "SoundBySine[opts___] returns a possible f-argument to MidiToSound."

Begin["`Private`"]

MidiExportWav[fn_String, m_Midi] := SoundExportWav[fn, MidiToSound[m, SoundBySine[]]]

MidiPlay[m_Midi] := Show[MidiToSound[m, SoundBySine[]]]

MidiToSound[m_Midi, f_, opts___] :=
  Module[
    {mp,r, c, mix},
    (* get the music as {{{timing, {p, v}} ...} ...} *)
    r = Cases[
      MidiSetState[m, {MidiTimeUnit -> MidiSec, MidiShape -> MidiVoice, MidiTiming -> MidiDelta}][[2]],
      {{MidiNoteOn, _},
      d$_} -> d$
      ];
    (* calculate mean pitch *)
    mp = N[Mean[Flatten[(If[! MidiDataNoValueQ[#[[2]]], #[[2, 1]], {}] & /@ #) & /@ r]]];
    (* make them into lists of sound - objects *)
    r = (
      If[MidiDataNoValueQ[#[[2]]],
        SoundOfSilence[SoundChannelCount -> 1, SoundDuration -> #[[1]], opts],
        f[#[[1]], #[[2, 1]], #[[2, 2]], mp]
        ] & /@ #
      ) & /@ r;
    (* then use SoundSeq *)
    r = SoundSeq /@ r;
    (* then use SoundPar *)
    r = SoundPar[r];
    (* create a simple mix for two output - channels *)
    c = SoundGetChannelCount[r];
    mix = If[c == 1, {{1 &, 1 &}},Table[N[{Evaluate[2(c - i)/(c^2 - c)] &, Evaluate[2(i - 1)/(c^2 - c)] &}], {i, c}]];
    (* make the mixdown *)
    SoundMix[r, mix]
    ]

p2f = Function[p, 440*2^((p - 57)/12)];
v2a = Function[v, v/127];
osc = Function[{f, a}, N[a Sin[2Pi f#]] &];

SoundBySample[s_Sound, opts___] := Function[{d, p, v, mp}, SoundSetDuration[SoundPitchShift[s, p2f[p]/p2f[mp], opts], d, opts]]

SoundBySine[opts___] := Function[{d, p, v, mp}, SoundMakeFunc[osc[p2f[p], v2a[v]], SoundDuration -> d, opts]]

End[]

Protect[
  MidiExportWav,
  MidiPlay,
  MidiToSound,
  SoundBySample,
  SoundBySine
  ];

EndPackage[]
