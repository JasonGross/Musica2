(* :Title: Setup *)

(* :Summary: Functions for Setup, You should not need them really *)

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

(* :Context: Musica2`Setup` *)

(* :History:
  2005-01-24  bch :  added DurVal to the list of pkg's
  2005-01-15  bch :  added Test to the list of pkg's
                     added Setup
  2005-01-09  bch :  added Instrument to the list of pkg's
  2004-10-26  bch :  added Naming and PianoRoll to the list of pkg's
  2004-10-20  bch :  added Tuning and Spectrum to the list of pkg's
  2004-09-23  bch :  removed ToDo, I never have the time to use it anyway
  2004-09-19  bch :  added Type to the list of pkg's
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
                     removed MidiPlay from the list of pkg's
  2004-09-13  bch :  removed EventList from the list of pkg's
  2004-09-03  bch :  added Note and Common to the list of pkg's
  2004-08-27  bch :  added ToDo function
  2004-08-26  bch :  added some help/usage-text
  2004-08-19  bch :  added MidiPlay to the list of pkg's
  2004-08-08  bch :  added CalcMidiStateRoutes
  2004-08-07  bch :  created
                     added Setup to the list of pkg's
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Setup`",
  {
    "Musica2`Midi`",
    "Musica2`PianoRoll`",
    "Musica2`Test`",
    "Musica2`Type`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ClearInitDotEm,
  MakeInitDotEm,
  Setup
  ];

ClearInitDotEm::usage = "ClearInitDotEm[pkg_, pkgs_, fn_] clears the file init.m."
MakeInitDotEm::usage = "MakeInitDotEm[pkg_, pkgs_, fn_] rewrites the file init.m."
Setup::usage = "todo"

Begin["`Private`"]

fn="Musica2/Applications/Musica2/Kernel/init.m";
pkg="Musica2";
pkgs={"Common","DurVal","Instrument","Midi","Naming","Note","PianoRoll","Setup","Sound","Spectrum","Test","Tuning","Type","Utils"};

ClearInitDotEm[] := ClearInitDotEm[pkg, fn]
ClearInitDotEm[pkg_, fn_] := MakeInitDotEm[pkg,{},fn]

MakeInitDotEm[] := MakeInitDotEm[pkg, pkgs, fn]
MakeInitDotEm[pkg_, pkgs_, fn_] :=
  Module[{fout = OpenWrite[fn],d=ToString/@Date[],pr=True},
    WriteString[fout,"(* :Title: Master Declarations File for " <> pkg <> " *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :Summary: This file contains declarations of all the major symbols contained in files in this directory.\nWhen loaded, it sets up the symbols with attribute Stub, so the correct package will be loaded when the symbol is called. *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout,"(* :Author: This file was created by the function Musica2`Setup`MakeInitDotEm[], written by Bo C. Herlin *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "(* :History: File created "<>d[[1]]<>"-"<>d[[2]]<>"-"<>d[[3]]<>" at "<>d[[4]]<>":"<>d[[5]]<>" *)\n"];
    WriteString[fout, "\n"];
    WriteString[fout, "If[!MemberQ[$Packages,\"" <> pkg <> "`\"],\n  System`Private`p = Unprotect[$Packages];\n  PrependTo[$Packages,\"" <> pkg <> "`\"];\n  Protect @@ System`Private`p  \n];\n"];
    WriteString[fout, "\n"];
    (
      If[pr,Print[#]];
      Get[pkg <> "`" <> # <> "`"];
      n = Names[pkg <> "`" <> # <> "`*"];
      WriteString[fout,"DeclarePackage[\"" <> pkg <> "`" <> # <> "`\",\n"];
      Write[fout, n];
      WriteString[fout, "];\n"];
      WriteString[fout, "\n"];
      ) & /@ pkgs;
    WriteString[fout, "Null\n"];
    Close[fout];
    ]

Setup /: TestSuite[Setup] := (
  ContainerQ /@ pkgs;
  TestSuite /@ {Musica2`Type`Type,PianoRoll,Utils}
  )
    
End[]

Protect[
  ClearInitDotEm,
  MakeInitDotEm,
  Setup
  ];

EndPackage[]
