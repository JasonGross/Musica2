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
  2005-02-16  bch :  initiated usage of Usage ;-)
  2005-02-13  bch :  reorganized code in file, hopefully in an uniform manner
  2005-01-17  bch :  renamed SimpleSine to BasicInstrument and changed its struct
  2005-01-09  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Instrument`",
  {
    "Musica2`Common`",
    "Musica2`Note`",
    "Musica2`ObjectType`",
    "Musica2`Sound`",
    "Musica2`Test`",
    "Musica2`Tuning`",
    "Musica2`Usage`",
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

CreateElement[Musica2,BasicInstrument, {PitchCodeToFrequency_,VelocityToAmplitude_,TimeToSample_},Null,
"BasicInstrument can be used as a third parameter to Convert[Melody, Snippet, x_BasicInstrument, opts___?OptionQ], the result is a function that converts a Melody to a Snippet.\[NewLine]"
];
  
Instrument::usage = "todo"

Begin["`Private`"]

Instrument := BasicInstrument[]

Convert[Melody, Snippet, opts___?OptionQ] := Convert[Melody, Snippet, Instrument, opts]

(* BasicInstrument +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

(* BasicInstrument modifications and interceptions ******************************************)

(* BasicInstrument constructors *************************************************************)

Usage[Append,Musica2,BasicInstrument,{},_BasicInstrument,"todo"];
BasicInstrument[] := BasicInstrument[{Convert[PitchCode,Frequency,Tuning],Function[v, v/127],Function[{f,a,sr},N[a Sin[2Pi f #/sr]]&]}]

(* BasicInstrument reverse constructors *****************************************************)

(* BasicInstrument common functions *********************************************************)

Usage[Append,Musica2,Convert,{Melody, Snippet, _BasicInstrument, ___?OptionQ},_Function,"todo"];
Convert[Melody, Snippet, x_BasicInstrument, opts___?OptionQ] :=
  Module[
    {
      sr = SampleRate/.{opts}/.Options[Sound],
      p2f = PitchCodeToFrequency[x],
      v2a = VelocityToAmplitude[x],
      t2s = TimeToSample[x]
      },
    Function[m,
      Module[{d,p,v,f,a},
        (* todo: use NoteFunction here *)
        d = Duration[m] * sr;
        {p,v}=Transpose[MapThread[If[DataPlainValueQ[{#1,#2}],{#1,#2},{0,0}]&,{PitchCode[m],Velocity[m]}]];
        f = MakeNestedIfs[Transpose[{d,p2f /@ p}]];
        a = MakeNestedIfs[Transpose[{d,v2a /@ v}]];
        Snippet[{SampledSoundFunction,Function[t, t2s[f[t],a[t],sr][t]],Round[sr],Round[TotalDuration[m]sr]},Sequence @@ RemOpts[{opts},SampleRate]]
        ]
      ]
    ]

(* BasicInstrument unique functions *********************************************************)

(* BasicInstrument tests *)

BasicInstrument /: TestSuite[BasicInstrument] = Join[TestSuite[BasicInstrument],{
  }];

(* BasicInstrument -------------------------------------------------------------------------*)

End[]

Protect[
  ];

Protect[
  Convert,
  Snippet,
  Sound
  ];

EndPackage[]
