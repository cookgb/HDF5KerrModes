(* ::Package:: *)

(* ::Title:: *)
(*Plotting routines for HDF5 KerrModes data sets*)


(* ::Section::Closed:: *)
(*Begin HDF5KerrModes Package*)


BeginPackage["HDF5KerrModes`"]


Unprotect[HDF5KerrModesDebug];
HDF5KerrModesDebug=True; (* Set this to True to allow reloading of Package with changes *)
If[HDF5KerrModesDebug,Unprotect["HDF5KerrModesDebug`*"];Unprotect["HDF5KerrModesDebug`Private`*"]];
Protect[HDF5KerrModesDebug];


(* ::Section::Closed:: *)
(*Documentation of External Functions*)


(* ::Subsection::Closed:: *)
(*Data Routines*)


HDF5KerrModesDir::usage=
"HDF5KerrModesDir[dir] "<>
"Set the directory containing the HDF5 Kerr Modes datafiles."


Read\[Omega]KerrQNM::usage=
"Read\[Omega]KerrQNM[{l,m,n},range] "<>
"Read in the QNM mode frequency data.\n"<>
"Read\[Omega]KerrQNM[l,m,n,range] "<>
"Read in the QNM mode frequency data."


Read\[Omega]KerrTTM::usage=
"Read\[Omega]KerrTTM[LR,{l,m,n},range] "<>
"Read in the TTM-LR mode frequency data.\n"<>
"Read\[Omega]KerrTTM[LR,l,m,n,range] "<>
"Read in the TTM-LR mode frequency data."


ReadAlmKerrQNM::usage=
"ReadAlmKerrQNM[{l,m,n},range] "<>
"Read in the QNM separation constant data.\n"<>
"ReadAlmKerrQNM[l,m,n,range] "<>
"Read in the QNM separation constant data."


ReadAlmKerrTTM::usage=
"ReadAlmKerrTTM[LR,{l,m,n},range] "<>
"Read in the TTM-LR separation constant data.\n"<>
"ReadAlmKerrTTM[LR,l,m,n,range] "<>
"Read in the TTM-LR separation constant data."


ReadSpectralCoefsQNM::usage=
"ReadSpectralCoefsQNM[{l,m,n},range] "<>
"Read in the QNM spectral coefficients data.\n"<>
"ReadSpectralCoefsQNM[l,m,n,range] "<>
"Read in the QNM spectral coefficients data."


ReadSpectralCoefsTTM::usage=
"ReadSpectralCoefsTTM[LR,{l,m,n},range] "<>
"Read in the TTM-LR spectral coefficients data.\n"<>
"ReadSpectralCoefsTTM[LR,l,m,n,range] "<>
"Read in the TTM-LR spectral coefficients data."


(* ::Subsection::Closed:: *)
(*Plotting Routines*)


QNMPlot\[Omega]::usage=
"QNMPlot\[Omega][{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex frequency plot of a set of QNMs."


TTMLPlot\[Omega]::usage=
"TTMLPlot\[Omega][{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex frequency plot of a set of TTMLs."


TTMRPlot\[Omega]::usage=
"TTMRPlot\[Omega][{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex frequency plot of a set of TTMRs."


QNMPlotAlm::usage=
"QNMPlotAlm[{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex separation constant plot of a set of QNMs."


TTMLPlotAlm::usage=
"TTMLPlotAlm[{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex separation constant plot of a set of TTMLs."


TTMRPlotAlm::usage=
"TTMLPlotAlm[{{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(m\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(1\)]\)},{\!\(\*SubscriptBox[\(l\), \(2\)]\),\!\(\*SubscriptBox[\(m\), \(2\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)},...}] "<>
"Complex separation constant plot of a set of TTMRs."


QNMPlotAll\[Omega]Tones::usage=
"QNMPlotAll\[Omega]Tones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex frequency plot of a set of QNMs with the same value of m."


TTMLPlotAll\[Omega]Tones::usage=
"TTMLPlotAll\[Omega]Tones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex frequency plot of a set of TTMLs with the same value of m."


TTMRPlotAll\[Omega]Tones::usage=
"TTMRPlotAll\[Omega]Tones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex frequency plot of a set of TTMRs with the same value of m."


QNMPlotAllAlmTones::usage=
"QNMPlotAllAlmTones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex separation constant plot of a set of QNMs with the same value of m."


TTMLPlotAllAlmTones::usage=
"TTMLPlotAllAlmTones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex separation constant plot of a set of TTMLs with the same value of m."


TTMRPlotAllAlmTones::usage=
"TTMRPlotAllAlmTones[m,{\!\(\*SubscriptBox[\(l\), \(1\)]\),\!\(\*SubscriptBox[\(l\), \(2\)]\),...}] "<>
"Complex separation constant plot of a set of TTMRs with the same value of m."


QNMPlotmTones::usage=
"QNMPlotmTones[m] "<>
"Plots for the complex frequency and separation constant of a set of QNMs with the same value of m."


TTMLPlotmTones::usage=
"TTMLPlotmTones[m] "<>
"Plots for the complex frequency and separation constant of a set of TTMLs with the same value of m."


TTMRPlotmTones::usage=
"TTMRPlotmTones[m] "<>
"Plots for the complex frequency and separation constant of a set of TTMRs with the same value of m."


SWSFlist::usage=
"SWSFlist[m,s,SWdat] "<>
"Evaluate the spin-weighted spheroidal function for the given set of expansion coefficients."


PlotQNMSWSF::usage=
"PlotQNMSWSF[l,m,n] "<>
"Plot the SWSF for the specified QNM over the entire sequence.\n"<>
"PlotQNMSWSF[l,m,n,range] "<>
"Plot the SWSF for the specified QNM over the specified range of the sequence."


PlotTTMSWSF::usage=
"PlotQNMSWSF[LR,l,m,n] "<>
"Plot the SWSF for the specified TTM over the entire sequence.\n"<>
"PlotQNMSWSF[LR,l,m,n,range] "<>
"Plot the SWSF for the specified TTM over the specified range of the sequence."


PhaseDifferenceQNM::usage=
"PhaseDifferenceQNM[{l,m,n}] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceQNM[{l,m,n},range] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceQNM[l,m,n] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceQNM[l,m,n,range] "<>
"Compute the phase difference."


PhaseDifferenceTTM::usage=
"PhaseDifferenceTTM[LR,{l,m,n}] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceTTM[LR,{l,m,n},range] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceTTM[LR,l,m,n] "<>
"Compute the phase difference.\n"<>
"PhaseDifferenceTTM[LR,l,m,n,range] "<>
"Compute the phase difference."


QNMSpectralCoef::usage=
"QNMSpectralCoef[l,m,n] "<>
"Plot the magnitudes of the spectral coefficients for the specified QNM."


QNMSpectralCoefRe::usage=
"QNMSpectralCoef[l,m,n] "<>
"Plot the real part of the spectral coefficients for the specified QNM."


QNMSpectralCoefIm::usage=
"QNMSpectralCoef[l,m,n] "<>
"Plot the imaginary part of the spectral coefficients for the specified QNM."


TTMSpectralCoef::usage=
"TTMSpectralCoef[LR,l,m,n] "<>
"Plot the magnitudes of the spectral coefficients for the specified TTM."


TTMSpectralCoefRe::usage=
"TTMSpectralCoef[LR,l,m,n] "<>
"Plot the real part of the spectral coefficients for the specified TTM."


TTMSpectralCoefIm::usage=
"TTMSpectralCoef[LR,l,m,n] "<>
"Plot the imaginary part of the spectral coefficients for the specified TTM."


(* ::Subsection::Closed:: *)
(*Reserved Globals*)


Protect[PlotSchwarzschild,SchwarzschildStyle,SchwarzschildOnly,ModePlotRange,OvertoneList,ModeMaxl,
		OTskip,OTmultiple,MarkerSize,LineThickness,TruncateLongSequences,ZeroTol,MaxFail,
        ModReal,PhaseChoice,Loga];


Protect[naf,\[Epsilon]af,\[Alpha]1,\[Alpha]2,\[Alpha]3,\[Alpha]4,\[Alpha]5,\[Beta]1,\[Beta]2,\[Beta]3,\[Beta]4,\[Beta]5,\[Beta]6,\[Beta]7,\[Beta]8]


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Data Routines*)


(* ::Subsection::Closed:: *)
(*Set HDF5 data directory*)


HDF5KERRMODESDIR="";
HDF5KerrModesDir[dir_]:=HDF5KERRMODESDIR=dir;


(* ::Subsection::Closed:: *)
(*Low Level routines to read data from HDF file*)


ReadKerrMode::badtype="Unknown mode type: `1`.";
ReadKerrMode::badovertone="Improper overtone index: `1`.";
ReadKerrMode::badmindex="Improper m index: `1`.";
ReadKerrMode::badlindex="Improper l index: `1`.";
ReadKerrMode[type_,mode_List]:=
Module[{nname,mnamea,mname,modename},
	If[!(type=="QNM" || type=="TTML" || type=="TTMR"),Message[ReadKerrModes::badtype,type];Abort[]];
	If[Head[mode[[3]]]==List,
		If[Length[mode[[3]]]!=2 &&Not[IntegerQ[mode[[3]]]],Message[ReadKerrModes::badovertone,mode[[3]]];Abort[]];
		nname=If[mode[[3,1]]<10,"0"<>ToString[mode[[3,1]]],ToString[mode[[3,1]]]],
		Null[],
		nname=If[mode[[3]]<10,"0"<>ToString[mode[[3]]],ToString[mode[[3]]]]
	];
	If[Not[IntegerQ[mode[[2]]]],Message[ReadKerrModes::badmindex,mode[[2]]];Abort[]];
	mnamea=If[Abs[mode[[2]]]<10,"0"<>ToString[Abs[mode[[2]]]],ToString[Abs[mode[[2]]]]];
	mname=If[mode[[2]]<0,"-"<>mnamea,"+"<>mnamea];
	If[Not[IntegerQ[mode[[1]]]],Message[ReadKerrModes::badlindex,mode[[1]]];Abort[]];
	modename = StringReplace[ToString[mode]," "->""];
	Switch[type,
		"QNM",Import[HDF5KERRMODESDIR<>"KerrQNM_"<>nname<>".h5",{"HDF5","Datasets",{"/n"<>nname<>"/m"<>mname<>"/"<>modename}}],
		"TTML",Import[HDF5KERRMODESDIR<>"KerrTTML_"<>nname<>".h5",{"HDF5","Datasets",{"/n"<>nname<>"/m"<>mname<>"/"<>modename}}],
		"TTMR",Import[HDF5KERRMODESDIR<>"KerrTTMR_"<>nname<>".h5",{"HDF5","Datasets",{"/n"<>nname<>"/m"<>mname<>"/"<>modename}}]
	]
]
ReadKerrMode[l_Integer,m_Integer,n_Integer|n_List]:=ReadKerrMode[{l,m,n}]


(* ::Subsection::Closed:: *)
(*Routines to read \[Omega], Alm, Spectral Coefficients*)


Options[Read\[Omega]KerrQNM]={TruncateLongSequences->True};
Read\[Omega]KerrQNM[qnm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,Nelems,full,amod,short,a,lr=range,trunc=OptionValue[TruncateLongSequences]},
	Off[Import::dataset];
	rawdat=ReadKerrMode["QNM",qnm];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	If[lr=={1,-1} && trunc,
		If[qnm=={2,0,{11,1}},lr={1,20000}];If[qnm=={2,0,{12,1}},lr={1,22000}];If[qnm=={2,0,{13,1}},lr={1,35000}];
		If[qnm=={2,0,{14,1}},lr={1,35000}];If[qnm=={2,0,{15,1}},lr={1,35000}];If[qnm=={2,0,{16,1}},lr={1,25000}];
		If[qnm=={2,0,{17,1}},lr={1,25000}];If[qnm=={2,0,{18,1}},lr={1,25000}];If[qnm=={2,0,{19,1}},lr={1,25000}];
		If[qnm=={2,0,{20,1}},lr={1,25000}];If[qnm=={2,0,{21,1}},lr={1,25000}];If[qnm=={2,0,{22,1}},lr={1,25000}];
		If[qnm=={2,0,{23,1}},lr={1,25000}];If[qnm=={2,0,{24,1}},lr={1,25000}];If[qnm=={2,0,{25,1}},lr={1,25000}];
		If[qnm=={2,0,{26,1}},lr={1,25000}];If[qnm=={3,0,19},lr={1,20000}];If[qnm=={3,0,20},lr={1,20000}];
		If[qnm=={3,0,21},lr={1,22000}];If[qnm=={3,0,22},lr={1,35000}];If[qnm=={3,0,23},lr={1,35000}];
		If[qnm=={3,0,24},lr={1,25000}];If[qnm=={3,0,25},lr={1,25000}];If[qnm=={3,0,26},lr={1,25000}];
		If[qnm=={3,0,27},lr={1,25000}];If[qnm=={3,0,28},lr={1,25000}];If[qnm=={3,0,29},lr={1,25000}];
		If[qnm=={3,0,30},lr={1,25000}];If[qnm=={3,0,31},lr={1,25000}];If[qnm=={4,0,26},lr={1,22000}];
		If[qnm=={4,0,27},lr={1,25000}];If[qnm=={4,0,28},lr={1,35000}];If[qnm=={4,0,29},lr={1,35000}];
		If[qnm=={4,0,30},lr={1,25000}];If[qnm=={4,0,31},lr={1,25000}];
	];
	full=Function[x,{x[[1]],-x[[2]]}]/@Take[rawdat,lr,{2,3}];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
Read\[Omega]KerrQNM[l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=Read\[Omega]KerrQNM[{l,m,n},range,opts]


Options[ReadAlmKerrQNM]={TruncateLongSequences->True};
ReadAlmKerrQNM[qnm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,Nelems,full,amod,short,a,lr=range,trunc=OptionValue[TruncateLongSequences]},
	Off[Import::dataset];
	rawdat=ReadKerrMode["QNM",qnm];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	If[lr=={1,-1} && trunc,
		If[qnm=={2,0,{11,1}},lr={1,20000}];If[qnm=={2,0,{12,1}},lr={1,22000}];If[qnm=={2,0,{13,1}},lr={1,35000}];
		If[qnm=={2,0,{14,1}},lr={1,35000}];If[qnm=={2,0,{15,1}},lr={1,35000}];If[qnm=={2,0,{16,1}},lr={1,25000}];
		If[qnm=={2,0,{17,1}},lr={1,25000}];If[qnm=={2,0,{18,1}},lr={1,25000}];If[qnm=={2,0,{19,1}},lr={1,25000}];
		If[qnm=={2,0,{20,1}},lr={1,25000}];If[qnm=={2,0,{21,1}},lr={1,25000}];If[qnm=={2,0,{22,1}},lr={1,25000}];
		If[qnm=={2,0,{23,1}},lr={1,25000}];If[qnm=={2,0,{24,1}},lr={1,25000}];If[qnm=={2,0,{25,1}},lr={1,25000}];
		If[qnm=={2,0,{26,1}},lr={1,25000}];If[qnm=={3,0,19},lr={1,20000}];If[qnm=={3,0,20},lr={1,20000}];
		If[qnm=={3,0,21},lr={1,22000}];If[qnm=={3,0,22},lr={1,35000}];If[qnm=={3,0,23},lr={1,35000}];
		If[qnm=={3,0,24},lr={1,25000}];If[qnm=={3,0,25},lr={1,25000}];If[qnm=={3,0,26},lr={1,25000}];
		If[qnm=={3,0,27},lr={1,25000}];If[qnm=={3,0,28},lr={1,25000}];If[qnm=={3,0,29},lr={1,25000}];
		If[qnm=={3,0,30},lr={1,25000}];If[qnm=={3,0,31},lr={1,25000}];If[qnm=={4,0,26},lr={1,22000}];
		If[qnm=={4,0,27},lr={1,25000}];If[qnm=={4,0,28},lr={1,35000}];If[qnm=={4,0,29},lr={1,35000}];
		If[qnm=={4,0,30},lr={1,25000}];If[qnm=={4,0,31},lr={1,25000}];
	];
	full=Take[rawdat,lr,{4,5}];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
ReadAlmKerrQNM[l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=ReadAlmKerrQNM[{l,m,n},range,opts]


Options[ReadSpectralCoefsQNM]={TruncateLongSequences->True,PhaseChoice->"SL-C"};
ReadSpectralCoefsQNM[qnm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,full,amod,short,a,lr=range,trunc=OptionValue[TruncateLongSequences],phase,index,pc=OptionValue[PhaseChoice]},
	Off[Import::dataset];
	rawdat=ReadKerrMode["QNM",qnm];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	If[lr=={1,-1} && trunc,
		If[qnm=={2,0,{11,1}},lr={1,20000}];If[qnm=={2,0,{12,1}},lr={1,22000}];If[qnm=={2,0,{13,1}},lr={1,35000}];
		If[qnm=={2,0,{14,1}},lr={1,35000}];If[qnm=={2,0,{15,1}},lr={1,35000}];If[qnm=={2,0,{16,1}},lr={1,25000}];
		If[qnm=={2,0,{17,1}},lr={1,25000}];If[qnm=={2,0,{18,1}},lr={1,25000}];If[qnm=={2,0,{19,1}},lr={1,25000}];
		If[qnm=={2,0,{20,1}},lr={1,25000}];If[qnm=={2,0,{21,1}},lr={1,25000}];If[qnm=={2,0,{22,1}},lr={1,25000}];
		If[qnm=={2,0,{23,1}},lr={1,25000}];If[qnm=={2,0,{24,1}},lr={1,25000}];If[qnm=={2,0,{25,1}},lr={1,25000}];
		If[qnm=={2,0,{26,1}},lr={1,25000}];If[qnm=={3,0,19},lr={1,20000}];If[qnm=={3,0,20},lr={1,20000}];
		If[qnm=={3,0,21},lr={1,22000}];If[qnm=={3,0,22},lr={1,35000}];If[qnm=={3,0,23},lr={1,35000}];
		If[qnm=={3,0,24},lr={1,25000}];If[qnm=={3,0,25},lr={1,25000}];If[qnm=={3,0,26},lr={1,25000}];
		If[qnm=={3,0,27},lr={1,25000}];If[qnm=={3,0,28},lr={1,25000}];If[qnm=={3,0,29},lr={1,25000}];
		If[qnm=={3,0,30},lr={1,25000}];If[qnm=={3,0,31},lr={1,25000}];If[qnm=={4,0,26},lr={1,22000}];
		If[qnm=={4,0,27},lr={1,25000}];If[qnm=={4,0,28},lr={1,35000}];If[qnm=={4,0,29},lr={1,35000}];
		If[qnm=={4,0,30},lr={1,25000}];If[qnm=={4,0,31},lr={1,25000}];
	];
	(* Extract just the expansion coefficients and convert to complex numbers *)
	full = Complex[#[[1]],#[[2]]]&/@ Partition[#,2]&/@ Drop[Take[rawdat,lr],0,8];
	(* Remove trailing coefficients that are zero *)
	full = Drop[#,-LengthWhile[Reverse[#],Abs[#]==0&]]&/@ full;
	Switch[pc,
	(* Continuous Spherical Limit *) "SL-C",
		Null[],
	(* Indexed Spherical Limit *) "SL-Ind",
		phase = (#[[1]]+I #[[2]])&/@ Take[rawdat,lr,{7,8}];
		full*=phase,
	(* Cook-Zalutskiy Spherical Limit *) "CZ-SL",
		index=qnm[[1]]-Max[Abs[qnm[[2]]],2]+1;
		phase=Exp[-I Arg[#[[index]]]]&/@ full;
		full*=phase,
	(* Cook-Zalutskiy Indexed *) "CZ-Ind",
		index = (IntegerPart[#]+1)&/@ Flatten[Take[rawdat,lr,{6}],1];
		phase=Exp[-I Arg[#[[2,#[[1]]]]]]&/@ Transpose[{index,full}];
		full*=phase,
	_,Print["Unknown Phase Choice: Abort"];Abort[]
	];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
ReadSpectralCoefsQNM[l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=ReadSpectralCoefsQNM[{l,m,n},range,opts]


Read\[Omega]KerrTTM[LR_,ttm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,Nelems,full,amod,short,a,lr=range},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	Off[Import::dataset];
	rawdat=If[LR=="L",ReadKerrMode["TTML",ttm],ReadKerrMode["TTMR",ttm]];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	full=Function[x,{x[[1]],-x[[2]]}]/@Take[rawdat,lr,{2,3}];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
Read\[Omega]KerrTTM[LR_,l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=Read\[Omega]KerrTTM[LR,{l,m,n},range,opts]


ReadAlmKerrTTM[LR_,ttm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,Nelems,full,amod,short,a,lr=range},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	Off[Import::dataset];
	rawdat=If[LR=="L",ReadKerrMode["TTML",ttm],ReadKerrMode["TTMR",ttm]];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	full=Take[rawdat,lr,{4,5}];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
ReadAlmKerrTTM[LR_,l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=ReadAlmKerrTTM[LR,{l,m,n},range,opts]


Options[ReadSpectralCoefsTTM]={PhaseChoice->"SL-C"};
ReadSpectralCoefsTTM[LR_,ttm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,full,amod,short,a,lr=range,phase,index,pc=OptionValue[PhaseChoice]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	Off[Import::dataset];
	rawdat=If[LR=="L",ReadKerrMode["TTML",ttm],ReadKerrMode["TTMR",ttm]];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	(* Extract just the expansion coefficients and convert to complex numbers *)
	full = Complex[#[[1]],#[[2]]]&/@ Partition[#,2]&/@ Drop[Take[rawdat,lr],0,8];
	(* Remove trailing coefficients that are zero *)
	full = Drop[#,-LengthWhile[Reverse[#],Abs[#]==0&]]&/@ full;
	Switch[pc,
	(* Continuous Spherical Limit *) "SL-C",
		Null[],
	(* Indexed Spherical Limit *) "SL-Ind",
		phase = (#[[1]]+I #[[2]])&/@ Take[rawdat,lr,{7,8}];
		full*=phase,
	(* Cook-Zalutskiy Spherical Limit *) "CZ-SL",
		index=ttm[[1]]-Max[Abs[ttm[[2]]],2]+1;
		phase=Exp[-I Arg[#[[index]]]]&/@ full;
		full*=phase,
	(* Cook-Zalutskiy Indexed *) "CZ-Ind",
		index = (IntegerPart[#]+1)&/@ Flatten[Take[rawdat,lr,{6}],1];
		phase=Exp[-I Arg[#[[2,#[[1]]]]]]&/@ Transpose[{index,full}];
		full*=phase,
	_,Print["Unknown Phase Choice: Abort"];Abort[]
	];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=full[[Flatten[amod]]];
	{full,short,a,a[[Flatten[amod]]]}
]
ReadSpectralCoefsTTM[LR_,l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=ReadSpectralCoefsTTM[LR,{l,m,n},range,opts]


(* ::Section::Closed:: *)
(*Plotting Routines*)


(* ::Subsection::Closed:: *)
(*Routines to Plot \[Omega] and Alm for specific lists of modes {l, m, n}*)


Options[QNMPlot\[Omega]]=Union[{ModePlotRange->{1,-1},PlotSchwarzschild->True,SchwarzschildStyle->{{Gray,Dashed}},SchwarzschildOnly->False},
						Options[Read\[Omega]KerrQNM],Options[ListLinePlot],Options[ListPlot]];
SetOptions[QNMPlot\[Omega],AxesLabel->Automatic];
QNMPlot\[Omega][qnmlist_List,opts:OptionsPattern[]]:=
Module[{alllists,onlyovertones,plotlist,
		range=OptionValue[ModePlotRange],alabel=OptionValue[AxesLabel],
		pos,qnml=qnmlist,schplot,plotsch=OptionValue[PlotSchwarzschild],onlysch=OptionValue[SchwarzschildOnly]},
	SetOptions[ListLinePlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All];
	SetOptions[ListPlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All,PlotMarkers->{Automatic,Small}];
	alllists=Read\[Omega]KerrQNM[#,range,FilterRules[{opts},Options[Read\[Omega]KerrQNM]]]& /@ qnmlist;
	(* onlyovertones makes sure all qnm in alllists have same value of l and m *)
	onlyovertones=Length[DeleteCases[qnmlist,{l_,m_,n_}/;l==qnmlist[[1,1]]&&m==qnmlist[[1,2]]]]==0;
	If[MemberQ[alllists,$Failed],
		Print["No data for QNMs :",qnmlist[[Flatten[Position[alllists,$Failed],1]]]];Return[$Failed];
	];
	plotlist= {ListLinePlot[Part[#,1] & /@ alllists,FilterRules[{opts},FilterRules[Options[ListLinePlot],Except[{PlotMarkers,Joined}]]]]};
	AppendTo[plotlist,ListPlot[Part[#,2] & /@ alllists,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]]];
	If[onlysch,
		schplot=Part[#,2,1] & /@ alllists,
		If[plotsch&&onlyovertones && range[[1]]==1,
			axismodes=Transpose[{{{2,_,8},{0,2}},{{3,_,40},{0,10}}}];
			schplot=Part[#,2,1] & /@ alllists;
			pos=Position[qnml,{_,_,{_,1}}];
			For[i=Length[pos],i>0,--i,qnml=Drop[qnml,pos[[i]]];schplot=Drop[schplot,pos[[i]]]];
			qnml = qnml/.{l_,m_,{n_,0}}->{l,m,n};
			For[i=1,i<=Length[axismodes[[1]]],++i,
				pos=Position[qnml,axismodes[[1,i]]];
				If[Length[pos]>=1,
					schplot=ReplacePart[schplot,#->axismodes[[2,i]]]&/@Position[qnml,axismodes[[1,i]]]
				];
			];
			AppendTo[plotlist,ListLinePlot[schplot,PlotStyle->OptionValue[SchwarzschildStyle]]];
		];
	];
	Clear[alllists];
	If[alabel==Automatic,
		alabel = {Style["Re(\!\(\*OverscriptBox[\(\[Omega]\), \(_\)]\))",16],Style["-Im(\!\(\*OverscriptBox[\(\[Omega]\), \(_\)]\))",16]};
	];
	If[onlysch,
		ListPlot[schplot,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]],
		Show[plotlist,AxesLabel->alabel,PlotRange->OptionValue[PlotRange]]
	]
]


Options[QNMPlotAlm]=Union[{ModePlotRange->{1,-1}},Options[Read\[Omega]KerrQNM],Options[ListLinePlot],Options[ListPlot]];
SetOptions[QNMPlotAlm,AxesLabel->Automatic];
QNMPlotAlm[qnmlist_List,opts:OptionsPattern[]]:=
Module[{alllists,plotlist,range=OptionValue[ModePlotRange],alabel=OptionValue[AxesLabel]},
	SetOptions[ListLinePlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All];
	SetOptions[ListPlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All,PlotMarkers->{Automatic,Small}];
	alllists=ReadAlmKerrQNM[#,range,FilterRules[{opts},Options[ReadAlmKerrQNM]]]& /@ qnmlist;
	If[MemberQ[alllists,$Failed],
		Print["No data for QNMs :",qnmlist[[Flatten[Position[alllists,$Failed],1]]]];Return[$Failed];
	];
	plotlist= {ListLinePlot[Part[#,1] & /@ alllists,FilterRules[{opts},FilterRules[Options[ListLinePlot],Except[{PlotMarkers,Joined}]]]]};
	AppendTo[plotlist,ListPlot[Part[#,2] & /@ alllists,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]]];
	Clear[alllists];
	If[alabel==Automatic,
		alabel={Style[Re["\!\(\*SubscriptBox[\(A\), \(lm\)]\)"],16],Style[Im["\!\(\*SubscriptBox[\(A\), \(lm\)]\)"],16]};
	];
	Show[plotlist,AxesLabel->alabel,PlotRange->OptionValue[PlotRange]]
]


Options[TTMPlot\[Omega]]=Union[{ModePlotRange->{1,-1},PlotSchwarzschild->False,SchwarzschildStyle->{{Gray,Dashed}},SchwarzschildOnly->False},
						Options[ListLinePlot],Options[ListPlot]];
SetOptions[TTMPlot\[Omega],AxesLabel->Automatic];
TTMPlot\[Omega][LR_,ttmlist_List,opts:OptionsPattern[]]:=
Module[{alllists,onlyovertones,plotlist,
		range=OptionValue[ModePlotRange],alabel=OptionValue[AxesLabel],
		pos,ttml=ttmlist,schplot,plotsch=OptionValue[PlotSchwarzschild],onlysch=OptionValue[SchwarzschildOnly]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	SetOptions[ListLinePlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All];
	SetOptions[ListPlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All,PlotMarkers->{Automatic,Small}];
	alllists=Read\[Omega]KerrTTM[LR,#,range]& /@ ttmlist;
	(* onlyovertones makes sure all ttm in alllists have same value of l and m *)
	onlyovertones=Length[DeleteCases[ttmlist,{l_,m_,n_}/;l==ttmlist[[1,1]]&&m==ttmlist[[1,2]]]]==0;
	If[MemberQ[alllists,$Failed],
		Print["No data for TTMs :",ttmlist[[Flatten[Position[alllists,$Failed],1]]]];Return[$Failed];
	];
	plotlist= {ListLinePlot[Part[#,1] & /@ alllists,FilterRules[{opts},FilterRules[Options[ListLinePlot],Except[{PlotMarkers,Joined}]]]]};
	AppendTo[plotlist,ListPlot[Part[#,2] & /@ alllists,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]]];
	If[onlysch,
		schplot=Part[#,2,1] & /@ alllists,
		If[plotsch&&onlyovertones && range[[1]]==1,
			axismodes=Transpose[{{{2,_,8},{0,2}},{{3,_,40},{0,10}}}];
			schplot=Part[#,2,1] & /@ alllists;
			pos=Position[ttml,{_,_,{_,1}}];
			For[i=Length[pos],i>0,--i,ttml=Drop[ttml,pos[[i]]];schplot=Drop[schplot,pos[[i]]]];
			ttml = ttml/.{l_,m_,{n_,0}}->{l,m,n};
			For[i=1,i<=Length[axismodes[[1]]],++i,
				pos=Position[ttml,axismodes[[1,i]]];
				If[Length[pos]>=1,
					schplot=ReplacePart[schplot,#->axismodes[[2,i]]]&/@Position[ttml,axismodes[[1,i]]]
				];
			];
			AppendTo[plotlist,ListLinePlot[schplot,PlotStyle->OptionValue[SchwarzschildStyle]]];
		];
	];
	Clear[alllists];
	If[alabel==Automatic,
		alabel = {Style["Re(\!\(\*OverscriptBox[\(\[Omega]\), \(_\)]\))",16],Style["-Im(\!\(\*OverscriptBox[\(\[Omega]\), \(_\)]\))",16]};
	];
	If[onlysch,
		ListPlot[schplot,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]],
		Show[plotlist,AxesLabel->alabel,PlotRange->OptionValue[PlotRange]]
	]
]


Options[TTMRPlot\[Omega]]=Options[TTMPlot\[Omega]];
TTMRPlot\[Omega][ttmlist_List,opts:OptionsPattern[]]:=TTMPlot\[Omega]["R",ttmlist,opts]


Options[TTMLPlot\[Omega]]=Options[TTMPlot\[Omega]];
TTMLPlot\[Omega][ttmlist_List,opts:OptionsPattern[]]:=TTMPlot\[Omega]["L",ttmlist,opts]


Options[TTMPlotAlm]=Union[{ModePlotRange->{1,-1}},Options[ListLinePlot],Options[ListPlot]];
SetOptions[TTMPlotAlm,AxesLabel->Automatic];
TTMPlotAlm[LR_,ttmlist_List,opts:OptionsPattern[]]:=
Module[{alllists,plotlist,range=OptionValue[ModePlotRange],alabel=OptionValue[AxesLabel]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	SetOptions[ListLinePlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All];
	SetOptions[ListPlot,ImageSize->800,TicksStyle->Directive[14],PlotRange->All,PlotMarkers->{Automatic,Small}];
	alllists=ReadAlmKerrTTM[LR,#,range]& /@ ttmlist;
	If[MemberQ[alllists,$Failed],
		Print["No data for TTMs :",ttmlist[[Flatten[Position[alllists,$Failed],1]]]];Return[$Failed];
	];
	plotlist= {ListLinePlot[Part[#,1] & /@ alllists,FilterRules[{opts},FilterRules[Options[ListLinePlot],Except[{PlotMarkers,Joined}]]]]};
	AppendTo[plotlist,ListPlot[Part[#,2] & /@ alllists,FilterRules[FilterRules[{opts},Options[ListPlot]],Except[{Joined,PlotLegends}]]]];
	Clear[alllists];
	If[alabel==Automatic,
		alabel={Style[Re["\!\(\*SubscriptBox[\(A\), \(lm\)]\)"],16],Style[Im["\!\(\*SubscriptBox[\(A\), \(lm\)]\)"],16]};
	];
	Show[plotlist,AxesLabel->alabel,PlotRange->OptionValue[PlotRange]]
]


Options[TTMRPlotAlm]=Options[TTMPlotAlm];
TTMRPlotAlm[ttmlist_List,opts:OptionsPattern[]]:=TTMPlotAlm["R",ttmlist,opts]


Options[TTMLPlotAlm]=Options[TTMPlotAlm];
TTMLPlotAlm[ttmlist_List,opts:OptionsPattern[]]:=TTMPlotAlm["L",ttmlist,opts]


(* ::Subsection::Closed:: *)
(*Routines to Plot \[Omega] and Alm for sets of modes for a specific "m"*)


(* ::Subsubsection::Closed:: *)
(*Lists of known Overtone Multiplets*)


QNMOTlist=Join[Table[{2,-2,n},{n,13,16}],Table[{3,-2,n},{n,26,29}],{{2,1,8},{2,2,8}},Table[{2,0,n},{n,8,26}],Table[{2,1,n},{n,34,39}]];
QNMOTSlist={{2,-2,15},{3,-2,28}}; (* Special cases of "single" multiplets *)


QNMOTMultipletQ[qnm_]:=MemberQ[HDF5KerrModes`Private`QNMOTlist,qnm]


QNMOTSingletQ[qnm_]:=MemberQ[HDF5KerrModes`Private`QNMOTSlist,qnm]


TTMOTlist=Flatten[Table[{l,0,n},{n,0,1},{l,2,8}],1];


TTMMOTMultipletQ[ttm_]:=MemberQ[HDF5KerrModes`Private`TTMOTlist,ttm]


(* ::Subsubsection::Closed:: *)
(*Ploting routines*)


Options[QNMPlotAll\[Omega]Tones]=Union[{OvertoneList->Range[0,15],MarkerSize->10,LineThickness->Automatic},Options[QNMPlot\[Omega]]];
SetOptions[QNMPlotAll\[Omega]Tones,PlotMarkers->Automatic];
QNMPlotAll\[Omega]Tones[m_Integer,llist_List,opts:OptionsPattern[]]:=
Module[{lmin,lvals=llist,nlist=OptionValue[OvertoneList],qnms,i,j,qnmot,label,plots={},colors,marks,pstyle=OptionValue[PlotStyle],pmarks=OptionValue[PlotMarkers],msize=OptionValue[MarkerSize]},
	lmin = Max[2,Abs[m]];
	lvals=DeleteCases[lvals,l_/;l<lmin];
	label=OptionValue[PlotLabel];
	If[label==Automatic,
		label = Style["m="<>ToString[m]<>" : (l="<>ToString[lvals]<>", n="<>ToString[nlist]<>")",20];
	];
	For[i=1,i<=Length[lvals],++i,
		qnms=Table[{lvals[[i]],m,nlist[[j]]},{j,1,Length[nlist]}];
		colors=Range[1,Length[qnms]];
		marks=Range[1,Length[qnms]];
		For[j=Length[qnms],j>0,--j,
			qnmot=qnms[[j]];
			If[MemberQ[HDF5KerrModes`Private`QNMOTlist,qnmot],
				If[MemberQ[HDF5KerrModes`Private`QNMOTSlist,qnmot],
					qnms=ReplacePart[qnms,j->{qnmot[[1]],qnmot[[2]],{qnmot[[3]],0}}],
					qnms=ReplacePart[qnms,j->{qnmot[[1]],qnmot[[2]],{qnmot[[3]],1}}];
					qnms=Insert[qnms,{qnmot[[1]],qnmot[[2]],{qnmot[[3]],0}},j];
					colors=Insert[colors,colors[[j]],j];
					marks=Insert[marks,marks[[j]],j];
				];
			];
		];
		If[OptionValue[LineThickness]==Automatic,
			If[OptionValue[PlotStyle]==Automatic,pstyle=ColorData[1,#]& /@colors],
			Null,
			If[OptionValue[PlotStyle]==Automatic,pstyle={ColorData[1,#],Thickness[OptionValue[LineThickness]]}& /@colors]
		];
		If[OptionValue[PlotMarkers]==Automatic,
			pmarks={{Global`\[FilledCircle],Global`\[FilledSmallSquare],Global`\[FilledDiamond],Global`\[FilledUpTriangle],Global`\[FilledDownTriangle],Global`\[EmptyCircle],Global`\[EmptySquare],Global`\[EmptyDiamond],Global`\[EmptyUpTriangle],Global`\[EmptyDownTriangle]}[[Mod[#-1,10]+1]],msize}& /@marks
		];
		AppendTo[plots,QNMPlot\[Omega][qnms,PlotStyle->pstyle,PlotMarkers->pmarks,FilterRules[FilterRules[{opts},Options[QNMPlot\[Omega]]],Except[{PlotStyle,PlotMarkers}]]]];
	];
	Show[plots,PlotLabel->label,PlotRange->OptionValue[PlotRange]]
]


Options[QNMPlotAllAlmTones]=Union[{OvertoneList->Range[0,15],MarkerSize->10,LineThickness->Automatic},Options[QNMPlotAlm]];
SetOptions[QNMPlotAllAlmTones,PlotMarkers->Automatic];
QNMPlotAllAlmTones[m_Integer,llist_List,opts:OptionsPattern[]]:=
Module[{lmin,lvals=llist,nlist=OptionValue[OvertoneList],qnms,i,j,qnmot,label,plots={},colors,marks,pstyle=OptionValue[PlotStyle],pmarks=OptionValue[PlotMarkers],msize=OptionValue[MarkerSize]},
	lmin = Max[2,Abs[m]];
	lvals=DeleteCases[lvals,l_/;l<lmin];
	label=OptionValue[PlotLabel];
	If[label==Automatic,
		label = Style["m="<>ToString[m]<>" : (l="<>ToString[lvals]<>", n="<>ToString[nlist]<>")",20];
	];
	For[i=1,i<=Length[lvals],++i,
		qnms=Table[{lvals[[i]],m,nlist[[j]]},{j,1,Length[nlist]}];
		colors=Range[1,Length[qnms]];
		marks=Range[1,Length[qnms]];
		For[j=Length[qnms],j>0,--j,
			qnmot=qnms[[j]];
			If[MemberQ[HDF5KerrModes`Private`QNMOTlist,qnmot],
				If[MemberQ[HDF5KerrModes`Private`QNMOTSlist,qnmot],
					qnms=ReplacePart[qnms,j->{qnmot[[1]],qnmot[[2]],{qnmot[[3]],0}}],
					qnms=ReplacePart[qnms,j->{qnmot[[1]],qnmot[[2]],{qnmot[[3]],1}}];
					qnms=Insert[qnms,{qnmot[[1]],qnmot[[2]],{qnmot[[3]],0}},j];
					colors=Insert[colors,colors[[j]],j];
					marks=Insert[marks,marks[[j]],j];
				];
			];
		];
		If[OptionValue[LineThickness]==Automatic,
			If[OptionValue[PlotStyle]==Automatic,pstyle=ColorData[1,#]& /@colors],
			Null,
			If[OptionValue[PlotStyle]==Automatic,pstyle={ColorData[1,#],Thickness[OptionValue[LineThickness]]}& /@colors]
		];
		If[OptionValue[PlotMarkers]==Automatic,
			pmarks={{Global`\[FilledCircle],Global`\[FilledSmallSquare],Global`\[FilledDiamond],Global`\[FilledUpTriangle],Global`\[FilledDownTriangle],Global`\[EmptyCircle],Global`\[EmptySquare],Global`\[EmptyDiamond],Global`\[EmptyUpTriangle],Global`\[EmptyDownTriangle]}[[Mod[#-1,10]+1]],msize}& /@marks
		];
		AppendTo[plots,QNMPlotAlm[qnms,PlotStyle->pstyle,PlotMarkers->pmarks,FilterRules[FilterRules[{opts},Options[QNMPlot\[Omega]]],Except[{PlotStyle,PlotMarkers}]]]];
	];
	Show[plots,PlotLabel->label,PlotRange->OptionValue[PlotRange]]
]


Options[TTMPlotAll\[Omega]Tones]=Union[{OvertoneList->Range[0,2],MarkerSize->10,LineThickness->Automatic},Options[TTMPlot\[Omega]]];
SetOptions[TTMPlotAll\[Omega]Tones,PlotMarkers->Automatic];
TTMPlotAll\[Omega]Tones[LR_,m_Integer,llist_List,opts:OptionsPattern[]]:=
Module[{lmin,lvals=llist,nlist=OptionValue[OvertoneList],ttms,i,j,ttmot,label,plots={},colors,marks,pstyle=OptionValue[PlotStyle],pmarks=OptionValue[PlotMarkers],msize=OptionValue[MarkerSize]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	lmin = Max[2,Abs[m]];
	lvals=DeleteCases[lvals,l_/;l<lmin];
	label=OptionValue[PlotLabel];
	If[label==Automatic,
		label = Style["m="<>ToString[m]<>" : (l="<>ToString[lvals]<>", n="<>ToString[nlist]<>")",20];
	];
	For[i=1,i<=Length[lvals],++i,
		ttms=Table[{lvals[[i]],m,nlist[[j]]},{j,1,Length[nlist]}];
		colors=Range[1,Length[ttms]];
		marks=Range[1,Length[ttms]];
		For[j=Length[ttms],j>0,--j,
			ttmot=ttms[[j]];
			If[MemberQ[HDF5KerrModes`Private`TTMOTlist,ttmot],
				ttms=ReplacePart[ttms,j->{ttmot[[1]],ttmot[[2]],{ttmot[[3]],1}}];
				ttms=Insert[ttms,{ttmot[[1]],ttmot[[2]],{ttmot[[3]],0}},j];
				colors=Insert[colors,colors[[j]],j];
				marks=Insert[marks,marks[[j]],j];
			];
		];
		If[OptionValue[LineThickness]==Automatic,
			If[OptionValue[PlotStyle]==Automatic,pstyle=ColorData[1,#]& /@colors],
			Null,
			If[OptionValue[PlotStyle]==Automatic,pstyle={ColorData[1,#],Thickness[OptionValue[LineThickness]]}& /@colors]
		];
		If[OptionValue[PlotMarkers]==Automatic,
			pmarks={{Global`\[FilledCircle],Global`\[FilledSmallSquare],Global`\[FilledDiamond],Global`\[FilledUpTriangle],Global`\[FilledDownTriangle],Global`\[EmptyCircle],Global`\[EmptySquare],Global`\[EmptyDiamond],Global`\[EmptyUpTriangle],Global`\[EmptyDownTriangle]}[[Mod[#-1,10]+1]],msize}& /@marks
		];
		AppendTo[plots,TTMPlot\[Omega][LR,ttms,PlotStyle->pstyle,PlotMarkers->pmarks,FilterRules[FilterRules[{opts},Options[TTMPlot\[Omega]]],Except[{PlotStyle,PlotMarkers}]]]];
	];
	Show[plots,PlotLabel->label,PlotRange->OptionValue[PlotRange]]
]


Options[TTMRPlotAll\[Omega]Tones]=Options[TTMPlotAll\[Omega]Tones];
TTMRPlotAll\[Omega]Tones[m_Integer,llist_List,opts:OptionsPattern[]]:=TTMPlotAll\[Omega]Tones["R",m,llist,opts]


Options[TTMLPlotAll\[Omega]Tones]=Options[TTMPlotAll\[Omega]Tones];
TTMLPlotAll\[Omega]Tones[m_Integer,llist_List,opts:OptionsPattern[]]:=TTMPlotAll\[Omega]Tones["L",m,llist,opts]


Options[TTMPlotAllAlmTones]=Union[{OvertoneList->Range[0,2],MarkerSize->10,LineThickness->Automatic},Options[TTMPlotAlm]];
SetOptions[TTMPlotAllAlmTones,PlotMarkers->Automatic];
TTMPlotAllAlmTones[LR_,m_Integer,llist_List,opts:OptionsPattern[]]:=
Module[{lmin,lvals=llist,nlist=OptionValue[OvertoneList],ttms,i,j,ttmot,label,plots={},colors,marks,pstyle=OptionValue[PlotStyle],pmarks=OptionValue[PlotMarkers],msize=OptionValue[MarkerSize]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	lmin = Max[2,Abs[m]];
	lvals=DeleteCases[lvals,l_/;l<lmin];
	label=OptionValue[PlotLabel];
	If[label==Automatic,
		label = Style["m="<>ToString[m]<>" : (l="<>ToString[lvals]<>", n="<>ToString[nlist]<>")",20];
	];
	For[i=1,i<=Length[lvals],++i,
		ttms=Table[{lvals[[i]],m,nlist[[j]]},{j,1,Length[nlist]}];
		colors=Range[1,Length[ttms]];
		marks=Range[1,Length[ttms]];
		For[j=Length[ttms],j>0,--j,
			ttmot=ttms[[j]];
			If[MemberQ[HDF5KerrModes`Private`TTMOTlist,ttmot],
				ttms=ReplacePart[ttms,j->{ttmot[[1]],ttmot[[2]],{ttmot[[3]],1}}];
				ttms=Insert[ttms,{ttmot[[1]],ttmot[[2]],{ttmot[[3]],0}},j];
				colors=Insert[colors,colors[[j]],j];
				marks=Insert[marks,marks[[j]],j];
			];
		];
		If[OptionValue[LineThickness]==Automatic,
			If[OptionValue[PlotStyle]==Automatic,pstyle=ColorData[1,#]& /@colors],
			Null,
			If[OptionValue[PlotStyle]==Automatic,pstyle={ColorData[1,#],Thickness[OptionValue[LineThickness]]}& /@colors]
		];
		If[OptionValue[PlotMarkers]==Automatic,
			pmarks={{Global`\[FilledCircle],Global`\[FilledSmallSquare],Global`\[FilledDiamond],Global`\[FilledUpTriangle],Global`\[FilledDownTriangle],Global`\[EmptyCircle],Global`\[EmptySquare],Global`\[EmptyDiamond],Global`\[EmptyUpTriangle],Global`\[EmptyDownTriangle]}[[Mod[#-1,10]+1]],msize}& /@marks
		];
		AppendTo[plots,TTMPlotAlm[LR,ttms,PlotStyle->pstyle,PlotMarkers->pmarks,FilterRules[FilterRules[{opts},Options[TTMPlot\[Omega]]],Except[{PlotStyle,PlotMarkers}]]]];
	];
	Show[plots,PlotLabel->label,PlotRange->OptionValue[PlotRange]]
]


Options[TTMRPlotAllAlmTones]=Options[TTMPlotAllAlmTones];
TTMRPlotAllAlmTones[m_Integer,llist_List,opts:OptionsPattern[]]:=TTMPlotAllAlmTones["R",m,llist,opts]


Options[TTMLPlotAllAlmTones]=Options[TTMPlotAllAlmTones];
TTMLPlotAllAlmTones[m_Integer,llist_List,opts:OptionsPattern[]]:=TTMPlotAllAlmTones["R",m,llist,opts]


(* ::Subsubsection::Closed:: *)
(*Routine to plot all modes for specified "m"*)


Options[QNMPlotmTones]=Union[{ModeMaxl->16},Options[QNMPlotAll\[Omega]Tones]];
QNMPlotmTones[m_,opts:OptionsPattern[]]:=
Module[{lmax=OptionValue[ModeMaxl]},
	Flatten[Table[{QNMPlotAll\[Omega]Tones[m,{l},FilterRules[{opts},Options[QNMPlotAll\[Omega]Tones]]],QNMPlotAllAlmTones[m,{l},FilterRules[{opts},Options[QNMPlotAll\[Omega]Tones]]]},{l,Max[2,Abs[m]],lmax}]]
]


Options[TTMPlotmTones]=Union[{ModeMaxl->8},Options[TTMPlotAll\[Omega]Tones]];
TTMPlotmTones[LR_,m_,opts:OptionsPattern[]]:=
Module[{lmax=OptionValue[ModeMaxl]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	Flatten[Table[{TTMPlotAll\[Omega]Tones[LR,m,{l},FilterRules[{opts},Options[TTMPlotAll\[Omega]Tones]]],TTMPlotAllAlmTones[LR,m,{l},FilterRules[{opts},Options[TTMPlotAll\[Omega]Tones]]]},{l,Max[2,Abs[m]],lmax}]]
]


Options[TTMRPlotmTones]=Options[TTMPlotmTones];
TTMRPlotmTones[m_Integer,opts:OptionsPattern[]]:=TTMPlotmTones["R",m,opts]


Options[TTMLPlotmTones]=Options[TTMPlotmTones];
TTMLPlotmTones[m_Integer,opts:OptionsPattern[]]:=TTMPlotmTones["L",m,opts]


(* ::Subsection::Closed:: *)
(*Routines to Plot Slm(x,c) along sequences where c=a\[Omega]lmn*)


Options[SWSFlist]={PlotPoints->100};
SWSFlist[m_/;IntegerQ[2m],s_/;IntegerQ[2s],SWdat_List,opts:OptionsPattern[]]:=
Module[{NCmax,NC,x,theta,Ntheta,lmin,Matdlx,SWSF,hoint=1,npoints=OptionValue[PlotPoints]},
	If[!IntegerQ[m],hoint=-I];
	NCmax=Max[Length[#]&/@SWdat];
	x=Table[x,{x,-1,1,1/(Floor[npoints/2])}];
	theta=ArcCos[#]&/@x;
	Ntheta=Length[theta];
	lmin=Max[Abs[m],Abs[s]];
	Matdlx = ParallelTable[N[Sqrt[j-1+lmin+1/2]WignerD[{j-1+lmin,m,-s},0,theta[[k]],0]],{k,1,Ntheta},{j,1,NCmax},DistributedContexts->{"HDF5KerrModes`Private`"}];
	SWSF={};
	Do[NC=Length[SWdat[[ind]]];
		AppendTo[SWSF ,hoint (-1)^m  Take[Matdlx,All,NC] . SWdat[[ind]]];
	,{ind,Length[SWdat]}];
	{x,SWSF}
]


Options[PlotQNMSWSF]=Union[{PlotPoints->100},Options[ReadSpectralCoefsQNM],Options[ListLinePlot],Options[ListPlot]];
SetOptions[PlotQNMSWSF,PlotRange->Automatic];
PlotQNMSWSF[l_,m_,n_,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{\[Omega]data,scdata,swdat,minmax,npoints=OptionValue[PlotPoints],prange=OptionValue[PlotRange]},
	\[Omega]data=Complex[#[[1]],-#[[2]]]&/@ Read\[Omega]KerrQNM[l,m,n,range,FilterRules[{opts},Options[Read\[Omega]KerrQNM]]][[1]];
	scdata=ReadSpectralCoefsQNM[l,m,n,range,FilterRules[{opts},Options[ReadSpectralCoefsQNM]]];
	swdat=SWSFlist[m,-2,scdata[[1]],PlotPoints->npoints];
	If[prange==Automatic,prange={{-1.02,1.02},1.06 MinMax[Flatten[Join[Re[swdat[[2]]],Im[swdat[[2]]]]]]}];
	Parallelize[ListLinePlot[{Transpose[{swdat[[1]],Re[#[[1]]]}],Transpose[{swdat[[1]],Im[#[[1]]]}]},PlotRange->prange,ReleaseHold[FilterRules[{opts},Options[ListLinePlot]]]]&/@ Transpose[{swdat[[2]],scdata[[3]],\[Omega]data}],DistributedContexts->{"Global`","HDF5KerrModes`Private`"}]
]


Options[PhaseDifferenceQNM]={PhaseChoice->"SL-C"};
PhaseDifferenceQNM[qnm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,full,amod,short,a,lr=range,phase,index,pc=OptionValue[PhaseChoice]},
	Off[Import::dataset];
	rawdat=ReadKerrMode["QNM",qnm];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	(* Extract just the expansion coefficients and convert to complex numbers *)
	full = Complex[#[[1]],#[[2]]]&/@ Partition[#,2]&/@ Drop[Take[rawdat,lr],0,8];
	(* Remove trailing coefficients that are zero *)
	full = Drop[#,-LengthWhile[Reverse[#],Abs[#]==0&]]&/@ full;
	Switch[pc,
	(* Continuous Spherical Limit *) "SL-C",
		phase=Table[1,Length[Flatten[Take[rawdat,lr,1]]]],
	(* Indexed Spherical Limit *) "SL-Ind",
		phase = (#[[1]]+I #[[2]])&/@ Take[rawdat,lr,{7,8}],
	(* Cook-Zalutskiy Spherical Limit *) "CZ-SL",
		index=qnm[[1]]-Max[Abs[qnm[[2]]],2]+1;
		phase=Exp[-I Arg[#[[index]]]]&/@ full,
	(* Cook-Zalutskiy Indexed *) "CZ-Ind",
		index = (IntegerPart[#]+1)&/@ Flatten[Take[rawdat,lr,{6}],1];
		phase=Exp[-I Arg[#[[2,#[[1]]]]]]&/@ Transpose[{index,full}],
	_,Print["Unknown Phase Choice: Abort"];Abort[]
	];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=phase[[Flatten[amod]]];
	{phase,short,a,a[[Flatten[amod]]]}
]
PhaseDifferenceQNM[l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=PhaseDifferenceQNM[{l,m,n},range,opts]


Options[PlotTTMSWSF]=Union[{PlotPoints->100},Options[ReadSpectralCoefsTTM],Options[ListLinePlot],Options[ListPlot]];
SetOptions[PlotTTMSWSF,PlotRange->Automatic];
PlotTTMSWSF[LR_,l_,m_,n_,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{s=-2,\[Omega]data,scdata,swdat,minmax,npoints=OptionValue[PlotPoints],prange=OptionValue[PlotRange]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	If[LR=="R",s=2];
	\[Omega]data=Complex[#[[1]],-#[[2]]]&/@ Read\[Omega]KerrTTM[LR,l,m,n,range,FilterRules[{opts},Options[Read\[Omega]KerrTTM]]][[1]];
	scdata=ReadSpectralCoefsTTM[LR,l,m,n,range,FilterRules[{opts},Options[ReadSpectralCoefsTTM]]];
	swdat=SWSFlist[m,s,scdata[[1]],PlotPoints->npoints];
	If[prange==Automatic,prange={{-1.02,1.02},1.06 MinMax[Flatten[Join[Re[swdat[[2]]],Im[swdat[[2]]]]]]}];
	Parallelize[ListLinePlot[{Transpose[{swdat[[1]],Re[#[[1]]]}],Transpose[{swdat[[1]],Im[#[[1]]]}]},PlotRange->prange,ReleaseHold[FilterRules[{opts},Options[ListLinePlot]]]]&/@ Transpose[{swdat[[2]],scdata[[3]],\[Omega]data}],DistributedContexts->{"Global`","HDF5KerrModes`Private`"}]
]


Options[PhaseDifferenceTTM]={PhaseChoice->"SL-C"};
PhaseDifferenceTTM[LR_,ttm_List,range_List:{1,-1},opts:OptionsPattern[]]:=
Module[{rawdat,full,amod,short,a,lr=range,phase,index,pc=OptionValue[PhaseChoice]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	Off[Import::dataset];
	rawdat=If[LR=="L",ReadKerrMode["TTML",ttm],ReadKerrMode["TTMR",ttm]];
	On[Import::dataset];
	If[rawdat==$Failed,Return[$Failed]];
	(* Extract just the expansion coefficients and convert to complex numbers *)
	full = Complex[#[[1]],#[[2]]]&/@ Partition[#,2]&/@ Drop[Take[rawdat,lr],0,8];
	(* Remove trailing coefficients that are zero *)
	full = Drop[#,-LengthWhile[Reverse[#],Abs[#]==0&]]&/@ full;
	Switch[pc,
	(* Continuous Spherical Limit *) "SL-C",
		phase=Table[1,Length[Flatten[Take[rawdat,lr,1]]]],
	(* Indexed Spherical Limit *) "SL-Ind",
		phase = (#[[1]]+I #[[2]])&/@ Take[rawdat,lr,{7,8}],
	(* Cook-Zalutskiy Spherical Limit *) "CZ-SL",
		index=ttm[[1]]-Max[Abs[ttm[[2]]],2]+1;
		phase=Exp[-I Arg[#[[index]]]]&/@ full,
	(* Cook-Zalutskiy Indexed *) "CZ-Ind",
		index = (IntegerPart[#]+1)&/@ Flatten[Take[rawdat,lr,{6}],1];
		phase=Exp[-I Arg[#[[2,#[[1]]]]]]&/@ Transpose[{index,full}],
	_,Print["Unknown Phase Choice: Abort"];Abort[]
	];
	a=Flatten[Take[rawdat,lr,1]];
	amod = Position[Mod[#,1/20]&/@Function[x,x/10^16]/@ Function[x,IntegerPart[10^16 x]]/@a,0];
	If[1-a[[-1]]<10^(-6),AppendTo[amod,Length[full]]];
	short=phase[[Flatten[amod]]];
	{phase,short,a,a[[Flatten[amod]]]}
]
PhaseDifferenceTTM[LR_,l_Integer,m_Integer,n_Integer|n_List,range_List:{1,-1},opts:OptionsPattern[]]:=PhaseDifferenceTTM[LR,{l,m,n},range,opts]


(* ::Subsection::Closed:: *)
(*Routines for Spectral Coefficients*)


Options[QNMSpectralCoef]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[QNMSpectralCoef,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
QNMSpectralCoef[l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsQNM[l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Log10[Abs[llist[[i,Lind+1]]]],-20]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	If[range==Automatic,range={-15,0}];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


Options[QNMSpectralCoefRe]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[QNMSpectralCoefRe,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
QNMSpectralCoefRe[l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsQNM[l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Re[llist[[i,Lind+1]]],0]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


Options[QNMSpectralCoefIm]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[QNMSpectralCoefIm,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
QNMSpectralCoefIm[l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsQNM[l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Im[llist[[i,Lind+1]]],0]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


Options[TTMSpectralCoef]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[TTMSpectralCoef,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
TTMSpectralCoef[LR_,l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsTTM[LR,l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Log10[Abs[llist[[i,Lind+1]]]],-20]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	If[range==Automatic,range={-15,0}];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


Options[TTMSpectralCoefRe]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[TTMSpectralCoefRe,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
TTMSpectralCoefRe[LR_,l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsTTM[LR,l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Re[llist[[i,Lind+1]]],0]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


Options[TTMSpectralCoefIm]=Union[{Loga->False},Options[ListPointPlot3D]];
SetOptions[TTMSpectralCoefIm,PlotLabel->Automatic,AxesLabel->Automatic,PlotRange->Automatic];
TTMSpectralCoefIm[LR_,l_,m_,n_,opts:OptionsPattern[]]:=
Module[{rawdat,Na,Nl,lmin,lmax,lind,Lind,amin,amax,alist,llist,llen,plotline,plotlist={},label=OptionValue[PlotLabel],alabel=OptionValue[AxesLabel],range=OptionValue[PlotRange],loga=OptionValue[Loga]},
	If[!(LR=="L" || LR=="R"),Print["Must choose either L or R type TTM"];Abort[]];
	SetOptions[ListPointPlot3D,TicksStyle->Directive[14]];
	rawdat=ReadSpectralCoefsTTM[LR,l,m,n];
	alist=rawdat[[3]];
	llist=rawdat[[1]];
	Na=Length[alist];
	Nl=Max[Length[#]&/@llist];
	lmin=Max[Abs[m],2];
	lmax=Nl+lmin-1;
	llen=Length[#]&/@ llist;
	For[Lind=0,Lind<Nl,++Lind,
		lind=Lind+lmin;
		amin=LengthWhile[llen,#<=Lind&]+1;
		amax=Na-LengthWhile[Reverse[llen],#<Lind&];
		plotline=Table[{lind,If[loga,Log10[alist[[i]]],alist[[i]]],If[Lind+1<=llen[[i]],Im[llist[[i,Lind+1]]],0]},{i,amin,amax}];
		AppendTo[plotlist,plotline];
	];
	If[label==Automatic,label=Style["l="<>ToString[l]<>" m="<>ToString[m]<> " n="<>ToString[n],16]];
	If[loga,
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["Log(\!\(\*OverscriptBox[\(a\), \(_\)]\))",16],""}],
		If[alabel==Automatic,alabel={Style["\!\(\*SuperscriptBox[\(\[ScriptL]\), \(\[Prime]\)]\)",20],Style["\!\(\*OverscriptBox[\(a\), \(_\)]\)",16],""}]
	];
	ListPointPlot3D[plotlist,PlotRange->range,AxesLabel->alabel,PlotLabel->label,FilterRules[FilterRules[{opts},Options[ListPointPlot3D]],Except[{PlotRange,AxesLabel,PlotLabel}]]]
]


End[] (* `Private` *)


EndPackage[]
