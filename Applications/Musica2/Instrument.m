(* :Title: Instrument *)

(* :Summary: Functions for Instrument *)

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

(* :Context: Musica2`Instrument` *)

(* :History:
  2005-01-17  bch :  renamed SimpleSine to BasicInstrument and changed its struct
  2005-01-09  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Instrument`",
  {
    "Musica2`Common`",
    "Musica2`Note`",
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Tuning`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert,
  Snippet,
  Sound
  ];

Unprotect[
  ];

CreateElement[BasicInstrument, {P2F_,V2A_,T2S_},Null,"todo"];
  
Instrument::usage = "todo"

Begin["`Private`"]

Convert[Melody, Snippet, x_BasicInstrument, opts___?OptionQ] :=
  Module[
    {
      sr = SampleRate/.{opts}/.Options[Sound],
      p2f = P2F[x],
      v2a = V2A[x],
      t2s = T2S[x]
      },
    Function[m,
      Module[{d,p,v,f,a},
        (* todo: use NoteFunction here *)
        d = NoteDuration[m] * sr;
        {p,v}=Transpose[MapThread[If[DataPlainValueQ[{#1,#2}],{#1,#2},{0,0}]&,{PitchCode[m],Velocity[m]}]];
        f = MakeNestedIfs[Transpose[{d,p2f /@ p}]];
        a = MakeNestedIfs[Transpose[{d,v2a /@ v}]];
        Snippet[{SampledSoundFunction,Function[t, t2s[f[t],a[t],sr][t]],Round[sr],Round[Duration[m]sr]},Sequence @@ RemOpts[{opts},SampleRate]]
        ]
      ]
    ]

Convert[Melody, Snippet, opts___?OptionQ] := Convert[Melody, Snippet, Instrument, opts]

Instrument := BasicInstrument[]

BasicInstrument[] := BasicInstrument[{Convert[PitchCode,Frequency,Tuning],Function[v, v/127],Function[{f,a,sr},N[a Sin[2Pi f #/sr]]&]}]

End[]

Protect[
  ];

Protect[
  Convert,
  Snippet,
  Sound
  ];

EndPackage[]
