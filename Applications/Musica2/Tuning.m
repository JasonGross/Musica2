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
  2005-01-09  bch :  changed the struct in EqualTemperament
                     added CustomTuning
                     removed TuningFunction
  2004-11-29  bch :  added use of Convert for getting ConversionFunctions
                     added Convert[{PitchCode,Overtone},PitchCode]
  2004-10-09  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Tuning`",
  {
    "Musica2`Common`",
    "Musica2`ObjectType`",
    "Musica2`Test`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Convert
  ];

Unprotect[
  EqualTemperament,
  EqualTemperamentQ,
  FrequencyRatios
  ];

CreateElement[CustomTuning, {{PitchCodeRef_,FrequencyRef_},FrequencyRatios:{__}},{{69, 440}, {17/16, 18/17, 16/15, 25/24, 16/15, 21/20, 15/14, 16/15, 25/24, 27/25, 25/24, 16/15}},
"todo.\[NewLine]"
];
CreateElement[EqualTemperament, {{PitchCodeRef_,FrequencyRef_},{PitchCodeOctave_,FrequencyOctave_}},{{69,440},{12,2}},
"todo.\[NewLine]"
];

FrequencyRatios::usage = "todo"
Tuning::usage = "todo"

Begin["`Private`"]

Convert[PitchCode,Frequency] := Convert[PitchCode,Frequency,Tuning]
Convert[Frequency,PitchCode] := Convert[Frequency,PitchCode,Tuning]
Convert[{PitchCode,Overtone},PitchCode] := Convert[{PitchCode,Overtone},PitchCode,Tuning]

Convert[PitchCode, Frequency, x_CustomTuning] :=
  Module[{
      pr = PitchCodeRef[x],
      fr = FrequencyRef[x],
      po = PitchCodeOctave[x],
      fo = FrequencyOctave[x],
      v = RatiosToValues[FrequencyRatios[x]],
      i, f, t
      },
    t = Table[{i - 1, v[[i]]}, {i, po + 1}];
    f = Interpolation[t, InterpolationOrder -> 1];
    Function[p,
      Module[{o = Floor[(p - pr)/po], q = Mod[(p - pr), po]},
        fr*fo^o*f[q]
        ]
      ]
    ]

Convert[Frequency, PitchCode, x_CustomTuning] :=
  Module[{
      pr = PitchCodeRef[x],
      fr = FrequencyRef[x],
      po = PitchCodeOctave[x],
      fo = FrequencyOctave[x],
      v = RatiosToValues[FrequencyRatios[x]],
      i, r, t
      },
    t = Table[{v[[i]], i - 1}, {i, po + 1}];
    r = Interpolation[t, InterpolationOrder -> 1];
    Function[f,
      Module[{o = Log[fo, f/fr], q},
        q = fo^(o - Floor[o]);
        o = Floor[o];
        pr + o*po + r[q]
        ]
      ]
    ]


Convert[PitchCode,Frequency,x_EqualTemperament] := Function[p,FrequencyRef[x]*FrequencyOctave[x]^((p - PitchCodeRef[x])/PitchCodeOctave[x])]
Convert[Frequency,PitchCode,x_EqualTemperament] := Function[f,PitchCodeOctave[x]*Log[FrequencyOctave[x],f/FrequencyRef[x]]+PitchCodeRef[x]]

Convert[{PitchCode,Overtone},PitchCode,x_CustomTuning] := Function[{p,o},Evaluate[Convert[Frequency,PitchCode,x][o Convert[PitchCode,Frequency,x][p]]]]
Convert[{PitchCode,Overtone},PitchCode,x_EqualTemperament] := Function[{p,o},Evaluate[Convert[Frequency,PitchCode,x][o Convert[PitchCode,Frequency,x][p]]]]

CustomTuning[o_?OptionQ, d_?(DataQ[CustomTuning])][p_] := Convert[PitchCode,Frequency,CustomTuning[o,d]][p]
EqualTemperament[o_?OptionQ, d_?(DataQ[EqualTemperament])][p_] := Convert[PitchCode,Frequency,EqualTemperament[o,d]][p]

EqualTemperament[x_Integer,opts___?OptionQ] := EqualTemperament[{{69,440},{x,2}},opts]
CustomTuning[x:{__?NumberQ},opts___?OptionQ] := CustomTuning[{{69,440},x},opts]

FrequencyOctave[x_CustomTuning] := Times @@ FrequencyRatios[x]

FrequencyRatios[x_EqualTemperament] := Table[FrequencyOctave[x]^(1/PitchCodeOctave[x]),{PitchCodeOctave[x]}]

PitchCodeOctave[x_CustomTuning] := Length[FrequencyRatios[x]]

Tuning := EqualTemperament[]

EqualTemperament /: TestSuite[EqualTemperament] = Join[TestSuite[EqualTemperament],{
 TestCase[Convert[PitchCode, Frequency, EqualTemperament[{{69, 440}, {12, 2}}]][60], 220*2^(1/4)],
 TestCase[Convert[PitchCode, Frequency, EqualTemperament[{{69, 440}, {12, 2}}]][121/2], 220*2^(7/24)],
 TestCase[Convert[PitchCode, Frequency, EqualTemperament[{{69, 440}, {12, 2}}]][61], 220*2^(1/3)],
 TestCase[Convert[PitchCode, Frequency, EqualTemperament[{{69, 440}, {12, 2}}]][123/2], 220*2^(3/8)],
 TestCase[Convert[PitchCode, Frequency, EqualTemperament[{{69, 440}, {12, 2}}]][62], 220*2^(5/12)],
 TestCase[Convert[Frequency, PitchCode, EqualTemperament[{{69, 440}, {12, 2}}]][60], 69 - (12*Log[22/3])/Log[2]],
 TestCase[Convert[Frequency, PitchCode, EqualTemperament[{{69, 440}, {12, 2}}]][61], 69 - (12*Log[440/61])/Log[2]],
 TestCase[Convert[Frequency, PitchCode, EqualTemperament[{{69, 440}, {12, 2}}]][62], 69 - (12*Log[220/31])/Log[2]],
 TestCase[Convert[Frequency, PitchCode, EqualTemperament[{{69, 440}, {12, 2}}]][63], 69 - (12*Log[440/63])/Log[2]],
 TestCase[Convert[Frequency, PitchCode, EqualTemperament[{{69, 440}, {12, 2}}]][64], 69 - (12*Log[55/8])/Log[2]]
  }]

CustomTuning /: TestSuite[CustomTuning] = Join[TestSuite[CustomTuning],{
 TestCase[Convert[PitchCode, Frequency, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][60], 264],
 TestCase[Convert[PitchCode, Frequency, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24,16/15}}]][121/2], 539/2],
 TestCase[Convert[PitchCode, Frequency, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][61], 275],
 TestCase[Convert[PitchCode, Frequency, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24,16/15}}]][123/2], 1705/6],
 TestCase[Convert[PitchCode, Frequency, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][62], 880/3],
 TestCase[Convert[Frequency, PitchCode, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][60], 1909/55],
 TestCase[Convert[Frequency, PitchCode, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25,25/24, 16/15}}]][61], 9617/275],
 TestCase[Convert[Frequency, PitchCode, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][62], 387/11],
 TestCase[Convert[Frequency, PitchCode, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25,25/24, 16/15}}]][63], 1557/44],
 TestCase[Convert[Frequency, PitchCode, CustomTuning[{{69, 440}, {25/24, 16/15, 27/25, 25/24, 16/15, 25/24, 27/25, 25/24, 16/15, 27/25, 25/24, 16/15}}]][64], 783/22]
 }]
  
End[]

Protect[
  Convert
  ];

Protect[
  EqualTemperament,
  EqualTemperamentQ,
  FrequencyRatios
  ];

EndPackage[]
