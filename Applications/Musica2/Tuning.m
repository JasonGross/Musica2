(* :Title: Tuning *)

(* :Summary: Functions for Tuning *)

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

(* :Context: Musica2`Tuning` *)

(* :History:
  2004-11-29  bch :  added use of Convert for getting ConversionFunctions
  2004-10-09  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Tuning`",
  {
    "Musica2`Common`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert
  ];

Unprotect[
  EqualTemperament,
  EqualTemperamentQ,
  Tuning,
  TuningFunction
  ];

CreateElement[EqualTemperament, {{FrequencyRef_,FrequencyOctave_},{PitchCodeRef_, PitchCodeOctave_}}];
(* todo: just intonation, meantone, ...

   for each type of tuning there must be a TuningFunction
   so if you write a new tuning called (with head) Meantone you must provide
    TuningFunction[x_Meantone, False] := Function[p,...]
    TuningFunction[x_Meantone, True ] := Function[f,...]
*)

Tuning::usage = "todo"
TuningFunction::usage = "todo"

Begin["`Private`"]

Convert[PitchCode,Frequency] := Convert[PitchCode,Frequency,Tuning]
Convert[Frequency,PitchCode] := Convert[Frequency,PitchCode,Tuning]

Convert[PitchCode,Frequency,x_EqualTemperament] := Function[p,FrequencyRef[x]*FrequencyOctave[x]^((p - PitchCodeRef[x])/PitchCodeOctave[x])]
Convert[Frequency,PitchCode,x_EqualTemperament] := Function[f,PitchCodeOctave[x]*Log[FrequencyOctave[x],f/FrequencyRef[x]]+PitchCodeRef[x]]

EqualTemperament[] := EqualTemperament[{{440,2},{69,12}}]
EqualTemperament[o_?OptionQ, d_?(DataQ[EqualTemperament])][p_] := TuningFunction[EqualTemperament[o,d]][p]

TuningFunction[x_EqualTemperament, False] := Function[p,FrequencyRef[x]*FrequencyOctave[x]^((p - PitchCodeRef[x])/PitchCodeOctave[x])]
TuningFunction[x_EqualTemperament, True ] := Function[f,PitchCodeOctave[x]*Log[FrequencyOctave[x],f/FrequencyRef[x]]+PitchCodeRef[x]]

Tuning := EqualTemperament[]
TuningFunction[x_] := TuningFunction[x, False]

End[]

Protect[
  Convert
  ];

Protect[
  EqualTemperament,
  EqualTemperamentQ,
  Tuning,
  TuningFunction
  ];

EndPackage[]
