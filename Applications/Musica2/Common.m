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
  Duration,
  GetDuration,
  GetDurations,
  SetDuration,
  SetDurations
  ];

Duration::todo = "Make Midi and Sound use the name Duration"
GetDuration::todo = "Make Midi and Sound use the name GetDuration"
GetDurations::todo = "Make Midi and Sound use the name GetDurations"
SetDuration::todo = "Make Midi and Sound use the name SetDuration"
SetDurations::todo = "Make Midi and Sound use the name SetDurations"

Duration::usage = ""
GetDuration::usage = ""
GetDurations::usage = ""
SetDuration::usage = ""
SetDurations::usage = ""

Begin["`Private`"]

End[]

Protect[
  Duration,
  GetDuration,
  GetDurations,
  SetDuration,
  SetDurations
  ];

EndPackage[]
