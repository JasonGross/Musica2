(* :Title: Common *)

(* :Summary: Functions for Common *)

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

(* :Context: Musica2`Common` *)

(* :History:
  2005-01-23  bch :  added TotalDuration
  2004-11-29  bch :  added Time and Overtone
  2004-11-28  bch :  moved PitchCode from Note.m
  2004-10-04  bch :  not much, lost track... sorry
  2004-09-22  bch :  added Play2
  2004-09-19  bch :  moved DefineSu(b|p) to Type.m
  2004-09-18  bch :  renamed Info to Opts
  2004-09-15  bch :  major rewrite, started using up-values and a kind of template for types.
  2004-09-03  bch :  created
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`Common`",
  {
    "Musica2`Utils`"
    }
  ]

Unprotect[
  Content,
  Convert,
  Duration,
  Frequency,
  Mix,
  Octave,
  Overtone,
  Par,
  PitchCode,
  Play2,
  Seq,
  Time,
  TotalDuration,
  UnPar,
  UnSeq
  ];

Content::usage = "todo"
Convert::usage = "todo"
Duration::usage = "todo"
Frequency::usage = "todo"
Mix::usage = "todo"
Octave::usage = "todo"
Par::usage = "todo"
PitchCode::usage = "todo"
Play2::usage = "todo"
Seq::usage = "todo"
Time::usage = "todo"
TotalDuration::usage = "todo"
UnPar::usage = "todo"
UnSeq::usage = "todo"

Begin["`Private`"]

End[]

Protect[
  Content,
  Convert,
  Duration,
  Frequency,
  Mix,
  Octave,
  Overtone,
  Par,
  PitchCode,
  Play2,
  Seq,
  Time,
  TotalDuration,
  UnPar,
  UnSeq
  ];

EndPackage[]
