(* :Title: ObjectType *)

(* :Summary: Functions for ObjectType *)

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

(* :Context: Musica2`ObjectType` *)

(* :History:
  2005-02-17  bch :  added parameter Appl to Create*
  2005-02-16  bch :  initiated usage of Usage ;-)
  2005-01-28  bch :  OrderedQ now does N before comparison
  2005-01-27  bch :  moved the content from Type.m to here
                     renamed Type to ObjectType, Types to ObjectTypes and TypeQ to ObjectTypeQ
*)

(* :Keywords: midi, music, sound *)

(* :Mathematica Version: 5.0 *)

BeginPackage["Musica2`ObjectType`",
  {
    "Musica2`Common`",
    "Musica2`Test`",
    "Musica2`Usage`",
    "Musica2`Utils`"
    }
  ]

Unprotect[
  ];

ObjectType::usage = "todo"

CreateContainer::usage = Usage[Default,ObjectType,"CreateContainer"]
CreateElement::usage = Usage[Default,ObjectType,"CreateElement"]
ContainerQ::usage = Usage[Default,ObjectType,"ContainerQ"]
Data::usage = Usage[Default,ObjectType,"Data"]
DataToRules::usage = Usage[Default,ObjectType,"DataToRules"]
DataQ::usage = Usage[Default,ObjectType,"DataQ"]
ElementType::usage = Usage[Default,ObjectType,"ElementType"]
Members::usage = Usage[Default,ObjectType,"Members"]
Opts::usage = Usage[Default,ObjectType,"Opts"]
Pos::usage = Usage[Default,ObjectType,"Pos"]
Struct::usage = Usage[Default,ObjectType,"Struct"]
Tidy::usage = Usage[Default,ObjectType,"Tidy"]

DataDeep::usage = "todo"
(* EmptyQ::usage = "todo" *)
ObjectTypeQ::usage = "todo"
Pack::usage = "todo"
UnPack::usage = "todo"
UnPackOpts::usage = "todo"

Begin["`Private`"]

Usage[Append,ObjectType,CreateElement,{_Symbol,_Symbol, _, _, _String},Null,"CreateElement[A, T, P, D, U] calls CreateElement[T, P, D, U, {}]"];
CreateElement[A_Symbol,T_Symbol, P_, D_, U_String] := CreateElement[A, T, P, D, U, {}]

Usage[Append,ObjectType,CreateElement,{_Symbol, _, _, _String,_},Null,
"creates all object-types that are containers in Musica2. Parameters are {Application-name,Type-name,data-Pattern,Default-data,Usage-string,special-symbols}"
];
CreateElement[A_Symbol,T_Symbol, P_, D_, U_String, K_] := (
  DeclareElement[A, T, P, D, U, K];
  Begin["`Private`"];
  DefineElement[A, T, P, D, K];
  End[];
  );
  
Usage[Append,ObjectType,CreateContainer,{_Symbol, _Symbol, _String},Null,
"creates all object-types that are containers in Musica2. Parameters are {Application-name,Type-name,Element-Type,Usage-string}"
];
CreateContainer[A_Symbol,T_Symbol, ET_Symbol, U_String] := (
  DeclareContainer[A, T, ET, U];
  Begin["`Private`"];
  DefineContainer[A, T, ET];
  End[];
  );

M[T_Symbol, P_] := P[[Sequence @@ #]] & /@ (Append[Drop[#, -1], 1] & /@ Position[P, Pattern])

(* we dont want to mess with the already present usage-text for Sound *)
DeclareUsage[A_Symbol,T_Symbol,text_String] := MessageName[T,"usage"] = If[StringQ[T::usage],T::usage<>"\[NewLine]\[NewLine]Musica2: ",""]<>text;

DeclareCommon[A_Symbol,T_Symbol, U_String] := (
  DeclareUsage[A,T,
    U<>"\[NewLine]\[NewLine]"<>
    ToString[T]<>" is a generated object-type (see ObjectType.m).\[NewLine]"<>
    "An object of type "<>ToString[T]<>" has Head "<>ToString[T]<>" and two parts; opts and data.\[NewLine]"<>
    "The opts part must make OptionQ[opts] return True.\[NewLine]"<>
    "The data part must make DataQ["<>ToString[T]<>"][data] return True.\[NewLine]"<>
    "\[NewLine]"<>
    Usage[Default,A,ToString[T]]<>
    "\[NewLine]"
  ];
  DeclareUsage[A,Symbol[ToString[T] <> "Q"],ToString[T]<>"Q[expr] returns True if expr is an object of type "<>ToString[T]<>"\[NewLine]"];
  SetAttributes[T,ReadProtected];
  );

DeclareElement[A_Symbol,T_Symbol, P_, D_, U_String, K_] := (
  DeclareCommon[A,T,U];
  );

DeclareContainer[A_Symbol,T_Symbol, ET_Symbol, U_String] := (
  DeclareCommon[A,T,U];
  );

(* low level stuff, "backdoors" for setting opts and data (dont intercept the first two) *)
Data[x_, d_, pos_] := x[[0]][ReplacePart[Data[x], d, pos],Sequence @@ Opts[x]];
Opts[x_, d_, s_, False] := x[[0]][Data[x], Sequence @@ AddOpts[Opts[x],s->d]];
Data[x_, d_, s_, pos_] := Data[x,d,pos];
Opts[x_, d_, s_] := Opts[x, d, s, False];

DataDeep[x_?NumberQ] := x
DataDeep[x:{___}] := DataDeep /@ x

DefineCommon[A_Symbol,T_Symbol] := (
  ObjectType[] = Prepend[ObjectType[],T];

  (* no options, but preparing a placeholder *)
  Usage[Append,A,Options,{T},{___?OptionQ},"contains default-values for "<>ToString[T]<>"."];
  Options[T] = {};

  (* test-function *)
  Usage[Append,A,ObjectTypeQ,{T},_Function,"returns a test-function for "<>ToString[T]<>"-objects, used by "<>ToString[T]<>"Q."];
  ObjectTypeQ[T] = MatchQ[#, T[_?OptionQ, _?(DataQ[T])]]&;

  Usage[Append,A,Symbol[ToString[T] <> "Q"],{_},(True|False),"is the test-function for "<>ToString[T]<>"-objects, uses the function returned by ObjectTypeQ["<>ToString[T]<>"]."];
  Symbol[ToString[T] <> "Q"][expr_] := ObjectTypeQ[T][expr];

  (* default constructor *)
  Usage[Append,A,T,{_?(DataQ[T]),___?OptionQ},_T,"is the default-constructor for "<>ToString[T]<>"-objects."];
  T[d_?(DataQ[T]),opts___?OptionQ] := T[{opts},d];

  (* basic format, the precondition (/; ObjectTypeQ[T][x]) might go away later, currentli its good for debugging *)
  Format[x_T] := "\[SkeletonIndicator]"<>ToString[T]<>"\[SkeletonIndicator]" /; ObjectTypeQ[T][x];

  (* the "backdoor" to the opts and data *)
  Usage[Append,A,Opts,{_T},{___?OptionQ},"returns the options for a "<>ToString[T]<>"-object."];
  T /: Opts[x_T] := ReplacePart[x,List,{0}][[1]] /; ObjectTypeQ[T][x];
  Usage[Append,A,Data,{_T},_?(DataQ[T]),"returns the data for a "<>ToString[T]<>"-object."];
  T /: Data[x_T] := ReplacePart[x,List,{0}][[2]] /; ObjectTypeQ[T][x];

  DataDeep[T] = Data;
  T /: DataDeep[x_T] := DataDeep[T][x];
  T /: DataDeep[T,d_] := DataDeep[T[d]];
    

  (* tidy it up *)
  Usage[Append,A,Tidy,{T},_Function,"returns a Tidy function for "<>ToString[T]<>"-objects (which does nothing)."];
  Tidy[T] = #&;
  Usage[Append,A,Tidy,{_T},_T,"calls the Tidy function returnde by Tidy["<>ToString[T]<>"]."];
  T /: Tidy[x_T] := Tidy[T][x];

  Usage[Append,A,ContainerQ,{_T},(True|False),"test if the "<>ToString[T]<>"-object is a container."];
  T /: ContainerQ[x_T] := ContainerQ[T];

  Usage[Append,A,T,{_T,___?OptionQ},_T,"changes the values of members and options for the "<>ToString[T]<>"-object."];
  T[x_T, o___?OptionQ] :=
    Module[{r = x},
      Scan[(r = If[MemberQ[Members[T],#[[1]]],ReplacePart[r,#[[2]],#[[1]]],Opts[r,#[[2]],#[[1]]]])&,{o}];
      r
      ];
  Usage[Append,A,T,{{__T},___?OptionQ},{__T},"changes the values of members and options for the "<>ToString[T]<>"-objects."];
  T[x:{__T}, o__?OptionQ] := T[#,o]& /@ x;
  
  T /: TestSuite[T] := {};
  );

DefineElement[A_Symbol,T_Symbol, P_, D_, K_] :=
  Module[{i,k=Union[K,{List,Blank,BlankSequence,BlankNullSequence,RepeatedNull,Repeated}],m},
(*
    Print["BEGIN: ",T];
*)
    Usage[Append,A,DataQ,{T},_Function,"returns a test-function for the data in a "<>ToString[T]<>"."];
    DataQ[T] = MatchQ[#, P]&;

    Usage[Append,A,Struct,{T},P,"returns the structure/pattern for "<>ToString[T]<>" as passed to CreateElement."];
    Struct[T] = P;

    (* get all members *)
    Usage[Append,A,Members,{T},M[T,P],"returns the symbols for all members of "<>ToString[T]<>"."];
    Members[T] = M[T,P];
    Usage[Append,A,Members,{_T},Members[T],"calls Members["<>ToString[T]<>"]."];
    Members[x_T] = Members[T];

    (* get the index for all members *)
    m = P /. {Pattern -> Function[{s, p}, s[Sequence @@ p]]};
    i = Drop[#, -1] & /@ Cases[Position[m, _?(! MatchQ[#, Alternatives @@ k] &)], {__, 0}];
    MapThread[(
      Usage[Append,A,Pos,{T,#1},#2,"returns the position for the member "<>ToString[#1]<>" in the data-portion of a "<>ToString[T]<>"-object."];
      Pos[T,#1] = #2
      )&,{Members[T],i}];

(*
    Print["M ",Members[T]];
    Print["m ",m];
    Print["i ",i];
    Print["p ",m[[Sequence@@#,0]]&/@i];
*)

    Usage[Append,A,Part,{_T,_Symbol},_,"returns the value for a member in a "<>ToString[T]<>"-object."];
    T /: Part[x_T, s_Symbol] := Data[x][[Sequence @@ Pos[T,s]]];
    Usage[Append,A,Part,{_T,_Symbol,__Integer},_,"returns an element in the value for a member in a "<>ToString[T]<>"-object."];
    T /: Part[x_T, s_Symbol, n__Integer] := Part[x,s][[n]];
    Usage[Append,A,ReplacePart,{_T, _, _Symbol},_T,"changes the value for a member in a "<>ToString[T]<>"-object."];
    T /: ReplacePart[x_T, d_, s_Symbol] := Data[x, d, s, Pos[T,s]];

    Scan[(
      Usage[Append,A,#,{_T},_,"returns the value for the member "<>ToString[#]<>" in a "<>ToString[T]<>"-object."];
      T /: #[x_T] := x[[#]];
      )&,
      Members[T]
      ];

    Usage[Append,A,ContainerQ,{T},False,"todo"];
    ContainerQ[T] = False;

    Usage[Append,A,DataToRules,{_T},{___?OptionQ},"todo"];
    T /: DataToRules[x_T] := (# -> #[x]) & /@ Members[T];

    Usage[Append,A,OrderedQ,{_T, _T, {__}},(True|False),"todo"];
    T /: OrderedQ[x1_T, x2_T, s : {__}] :=
      Module[{f = s, y},
        While[0 < Length[f],
          y = f[[1]];
          If[OrderedQ[{y[x1], y[x2]}//N],
            If[y[x1] =!= y[x2],
              Return[True]
              ],
            Return[False]
            ];
          (*If[! OrderedQ[{y[x1], y[x2]}], Return[False]];*)
          f = Drop[f, 1];
          ];
        True
        ];

    DefineCommon[A,T];

    If[D =!= Null,
      Module[{d=T[D]},
        Usage[Append,A,T,{},_T,"todo"];
        T[] := T[D];
        Options[T] = Join[Options[T],(#->#[d])& /@ Members[T]];
        ];
      ];
    
    T /: TestSuite[T] = Join[TestSuite[T],{
      TestCase[ContainerQ[T],False],
      TestCase[Struct[T],P],
      TestCase[DataQ[T][Data[T[]]],True],
      TestCase[Opts[T[Data[T[]], test->42]],{test->42}],
      TestCase[Data[T[Data[T[]], test->42]],Data[T[]]]
      }];
    
(*
    Print["END: ",T];
*)
    ];

DefineContainer[A_Symbol,T_Symbol, ET_Symbol] :=
  Module[{t,et,i},
    Usage[Append,A,Members,{T},Members[ET],"todo"];
    Members[T] = Members[ET];
    
    Usage[Append,A,ElementType,{T},ET,"todo"];
    ElementType[T] = ET;
    Usage[Append,A,ElementType,{_T},ET,"todo"];
    T /: ElementType[x_T] := ET;
    Usage[Append,A,DataQ,{T},_Function,"todo"];
    DataQ[T] = MatchQ[#, {(_?(DataQ[ET])|{_?OptionQ,_?(DataQ[ET])})...}]&;

    (* outgoing and incoming element's *)
    Pack[T] = Function[{container,element},If[MatchQ[element,{{__?OptionQ},_?(DataQ[ET])}],ET[element[[2]], Sequence @@ element[[1]]],ET[element]]];
    UnPack[T] = Function[{element,opts},If[Opts[element]==={},Data[element],{Opts[element],Data[element]}]];
    UnPackOpts[T] = Function[{elements,opts},opts];

    (* constructors, container from element *)
    Usage[Append,A,T,{{__ET}},_T,"from a list of "<>ToString[ET]<>" create a "<>ToString[T]<>""];
    T[{y__?(ObjectTypeQ[ET])},opts___?OptionQ] := Function[o,T[(UnPack[T][#,o])& /@ {y}, Sequence @@ o]][UnPackOpts[T][{y},{opts}]];
    Usage[Append,A,T,{_ET},_T,"from a  "<>ToString[ET]<>" create a "<>ToString[T]<>""];
    T[y_?(ObjectTypeQ[ET]),opts___?OptionQ] := T[{y}, opts];

    (* constructor, element from container *)
    Usage[Append,A,ET,{_T},{___ET},"get all "<>ToString[ET]<>" from a "<>ToString[T]<>""];
    T /: ET[x_T] := (Pack[T][x,#]& /@ Data[x]);
    i=1;
    For[t = ET,ContainerQ[t]===True,t=et,
      et = ElementType[t];
      Usage[Append,A,et,{_T},Nest[{#...}&, {___et},i],"get all "<>ToString[et]<>" from a "<>ToString[T]<>""];
      T /: et[x_T] := et /@ x;
      i++;
      ];

    (* get member data *)
    i=1;
    For[t = ET,ContainerQ[t]===True,t=et,
      et = ElementType[t];
      i++
      ];
    Scan[(
      Usage[Append,A,#,{_T},Nest[{#...}&, {___},i-1],"get all "<>ToString[#]<>" from a "<>ToString[T]<>""];
      T /: #[x_T] := # /@ ET[x]
      )&,Members[T]
      ];

    Usage[Append,A,DataToRules,{_T},{___?OptionQ},"get all data as rules from a "<>ToString[T]<>""];
    T /: DataToRules[x_T] := (# -> #[x]) & /@ Members[T];

    (* T /: EmptyQ[x_T] := Length[x]==0; *)

    (* handy list-manipulation-functions *)
    Usage[Append,A,Append,{_T, _ET},_T,"append a new "<>ToString[ET]<>" to a "<>ToString[T]<>""];
    T /: Append[x_T, y_ET] := T[Append[ET[x],y], Sequence @@ Opts[x]];
    Usage[Append,A,Delete,{_T, __},_T,"delete "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Delete[x_T, n__] := T[Delete[Data[x],n], Sequence @@ Opts[x]];
    Usage[Append,A,Drop,{_T, _},_T,"drop "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Drop[x_T, n_] := T[Drop[Data[x],n], Sequence @@ Opts[x]];
    Usage[Append,A,Extract,{_T, _Integer},_ET,"extract an "<>ToString[ET]<>" from a "<>ToString[T]<>""];
    T /: Extract[x_T, n_Integer] := Part[x,n];
    Usage[Append,A,First,{_T},_ET,"get the first "<>ToString[ET]<>" from a "<>ToString[T]<>""];
    T /: First[x_T] := Part[x,1];
    Usage[Append,A,Insert,{_T, _ET, _Integer},_T,"insert an "<>ToString[ET]<>" into a "<>ToString[T]<>""];
    T /: Insert[x_T, y_ET, n_Integer] := T[Insert[ET[x],y,n], Sequence @@ Opts[x]];
    Usage[Append,A,Last,{_T},_ET,"get the last "<>ToString[ET]<>" from a "<>ToString[T]<>""];
    T /: Last[x_T] := Part[x,-1];
    Usage[Append,A,Length,{_T},_Integer,"get the number of "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: Length[x_T] := Length[Data[x]];
    Usage[Append,A,Map,{_Function, _T},(_T|_List),"apply a function on all "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: Map[f_, x_T] := Module[{r=Map[f, ET[x]]},If[MatchQ[r,{___ET}],T[r,Sequence@@Opts[x]],r]];
    Usage[Append,A,MapIndexed,{_Function, _T},(_T|_List),"apply a function on all "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: MapIndexed[f_, x_T] := Module[{r=MapIndexed[f, ET[x]]},If[MatchQ[r,{___ET}],T[r,Sequence@@Opts[x]],r]];
    Usage[Append,A,Most,{_T},_T,"get most "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Most[x_T] := T[Most[Data[x]], Sequence @@ Opts[x]];
    Usage[Append,A,Part,{_T,_Integer},_ET,"get a "<>ToString[ET]<>" from a "<>ToString[T]<>""];
    T /: Part[x_T, n_Integer] := Pack[T][x,#]&[Part[Data[x],n]] /; n!=0;
    Usage[Append,A,Part,{_T,_Integer,__Integer},_,"get an element from an "<>ToString[ET]<>" in a "<>ToString[T]<>""];
    T /: Part[x_T, n_Integer, m__Integer] := Part[x,n][[m]] /; n!=0;
    Usage[Append,A,Prepend,{_T, _ET},_T,"prepend a new "<>ToString[ET]<>" to a "<>ToString[T]<>""];
    T /: Prepend[x_T, y_ET] := T[Prepend[ET[x],y], Sequence @@ Opts[x]];
    Usage[Append,A,ReplacePart,{_T, _ET, _Integer},_T,"replace an "<>ToString[ET]<>" in a "<>ToString[T]<>""];
    T /: ReplacePart[x_T, y_ET, n_Integer] := T[ReplacePart[ET[x],y,n], Sequence @@ Opts[x]] /; n!=0;
    Usage[Append,A,Reverse,{_T},_T,"reverse a "<>ToString[T]<>""];
    T /: Reverse[x_T] := T[Reverse[ET[x]], Sequence @@ Opts[x]];
    Usage[Append,A,Rest,{_T},_T,"get rest "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Rest[x_T] := T[Rest[Data[x]], Sequence @@ Opts[x]];
    Usage[Append,A,Scan,{_Function,_T},Null,"scan the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: Scan[f_, x_T] := Scan[f, ET[x]];
    Usage[Append,A,Select,{_T,_Function},_T,"select "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Select[x_T, f_] := T[Select[ET[x], f], Sequence @@ Opts[x]];
    Usage[Append,A,Take,{_T,_},_T,"take "<>ToString[ET]<>"s from a "<>ToString[T]<>""];
    T /: Take[x_T, n_] := T[Take[Data[x],n], Sequence @@ Opts[x]];


    (* extended list-manipulation-functions *)
    Usage[Append,A,Map,{_Function, _T, _Symbol},_T,"apply a function to a member in the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    If[ContainerQ[ET],
      T /: Map[f_, x_T, s_Symbol] := Map[Map[f,#,s]&,x],
      T /: Map[f_, x_T, s_Symbol] := Map[ReplacePart[#,f[#[[s]]],s]&,x]
      ];
    Usage[Append,A,MapIndexed,{_Function, _T, _Symbol},_T,"apply a function to a member in the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    If[ContainerQ[ET],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[Function[{y,i},MapIndexed[f[#,Join[i,#2]]&,y,s]],x],
      T /: MapIndexed[f_, x_T, s_Symbol] := MapIndexed[ReplacePart[#,f[#[[s]],#2],s]&,x]
      ];
    Usage[Append,A,Sort,{_T, {__Symbol}},_T,"sort the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    Usage[Append,A,Sort,{_T, _Symbol},_T,"sort the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    If[ContainerQ[ET],
      T /: Sort[x_T, s:{__Symbol}] := Sort[#,s]& /@ x;
      T /: Sort[x_T, s_Symbol] := Sort[#,s]& /@ x,
      T /: Sort[x_T, s:{__Symbol}] := T[Sort[ET[x],OrderedQ[#1,#2,s]&], Sequence @@ Opts[x]];
      T /: Sort[x_T, s_Symbol] := T[Sort[ET[x],OrderedQ[#1, #2,{s}]&], Sequence @@ Opts[x]]
      ];

    Usage[Append,A,ReplacePart,{_T, _, _Symbol},_T,"replace the member(s) in the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: ReplacePart[x_T, y_, s_Symbol] :=
      If[ListQ[y] && Length[y]==Length[x],
        MapIndexed[ReplacePart[#,y[[#2[[1]]]],s]&,x],
        Map[ReplacePart[#,y,s]&,x]
        ];
    Usage[Append,A,ReplacePart,{_T, _, __Integer, _Symbol},_T,"replace the member(s) in the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: ReplacePart[x_T, y_, n__Integer, s_Symbol] :=
      ReplacePart[x,ReplacePart[x[[{n}[[1]]]],y,Sequence@@Drop[{n},1],s],{n}[[1]]];
    
    Usage[Append,A,Part,{_T, _Symbol},_,"get a member from the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: Part[x_T, s_Symbol] := #[[s]]&/@ x;
    Usage[Append,A,Part,{_T, _Integer, ___Integer, _Symbol},_ET,"get a member from the "<>ToString[ET]<>"s in a "<>ToString[T]<>""];
    T /: Part[x_T, n_Integer, m___Integer, s_Symbol] := Part[x,n][[m,s]] /; n!=0;

    Usage[Append,A,ContainerQ,{T},True,"todo"];
    ContainerQ[T] = True;

    DefineCommon[A,T];

    T /: TestSuite[T] = Join[TestSuite[T],{
      TestCase[ContainerQ[T],True],
      TestCase[ElementType[T],ET]
      }];
    
    Usage[Append,A,Tidy,{T},_Function,"returns a Tidy function for "<>ToString[T]<>"-objects which calls Tidy for each "<>ToString[ET]<>"-object."];
    Tidy[T] = (Tidy /@ #)&;

    T /: DataDeep[T,d_] := DataDeep[ET,If[MatchQ[#,{_?OptionQ,_}],#[[2]],#]]& /@ d;
    DataDeep[T] = DataDeep[T,Data[#]]&;
  ];

Usage[Append,ObjectType,ObjectType,{},{___Symbol},"The list of ObjectType's"];
ObjectType[] = {}

ObjectType /: TestRun[ObjectType,opts___?OptionQ] := And @@ (TestRun[#,opts]& /@ ObjectType[])

End[]

Protect[
  ];

EndPackage[]
