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
  Convert
  ];

Unprotect[
  ];

CreateElement[SimpleSine, {Tuner_},Null,"todo"];
  
Instrument::usage = "todo"

Begin["`Private`"]

v2a = Function[v, v/127];
zin = Function[{f, a, sr}, N[a Sin[2Pi f#/sr]]&];

Convert[Melody, Snippet, x_SimpleSine, opts___?OptionQ] :=
  Module[
    {
      sr = SampleRate/.{opts}/.Options[Sound],
      tf = Convert[PitchCode,Frequency,Tuner[x]]
      },
    Function[m,
      Module[{d,p,v,f,a},
        (* todo: use NoteFunction here *)
        d = NoteDuration[m] * sr;
        {p,v}=Transpose[MapThread[If[DataPlainValueQ[{#1,#2}],{#1,#2},{0,0}]&,{PitchCode[m],Velocity[m]}]];
        f = MakeNestedIfs[Transpose[{d,tf /@ p}]];
        a = MakeNestedIfs[Transpose[{d,v2a /@ v}]];
        Snippet[{SampledSoundFunction,Function[t, zin[f[t], a[t], sr][t]],Round[sr],Round[Duration[m]sr]},Sequence@@RemOpts[{opts},SampleRate]]
        ]
      ]
    ]

Convert[Melody, Snippet, opts___?OptionQ] := Convert[Melody, Snippet, Instrument, opts]

Instrument := SimpleSine[]

SimpleSine[] := SimpleSine[{Tuning}]

End[]

Protect[
  ];

Protect[
  Convert
  ];

EndPackage[]
