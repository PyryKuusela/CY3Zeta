(* ::Package:: *)

BeginPackage["CY3Zeta`"];


ClearAll["CY3Zeta`*"];
ClearAll["CY3Zeta`Private`*"];


zSetNParameters::usage = "\!\(\*
StyleBox[\"zSetNParams\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]
Sets the number of complex structure parameters for which the following computations are to be performed.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\) is the number of complex structure parameters (=\!\(\*SuperscriptBox[\(h\), \(1, 2\)]\)) of the manifold X.";


zSetNMax::usage = "\!\(\*
StyleBox[\"zSetNMax\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NMax\",\nFontWeight->\"Bold\"]\)]
Sets the maximal order to which the power series are evaluated during the following computations.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"NMax\",\nFontWeight->\"Bold\"]\) is the maximal order to which the power series are to be evaluated.";


zSetY::usage = "\!\(\*
StyleBox[\"zSetY\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"YRules\",\nFontWeight->\"Bold\"]\)]
Sets values of the triple intersection numbers \!\(\*SubscriptBox[\(Y\), \(ijk\)]\) of the mirror manifold of X. The \!\(\*SubscriptBox[\(Y\), \(ijk\)]\) are assumed to be symmetric.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"YRules\",\nFontWeight->\"Bold\"]\) is a list of rules (in the form \!\(\*
StyleBox[\"Y\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"j\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"k\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Bold\"]\)->\!\(\*
StyleBox[\"yval\",\nFontWeight->\"Bold\"]\)) giving the values of the independent \!\(\*SubscriptBox[\(Y\), \(ijk\)]\).";


zSetYhat::usage = "\!\(\*
StyleBox[\"zSetYhat\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"YhatRules\",\nFontWeight->\"Bold\"]\)]
Sets values of the \[OpenCurlyQuote]inverse\[CloseCurlyQuote] triple intersection numbers \!\(\*SuperscriptBox[\(\:0176\), \(ijk\)]\) of the mirror manifold of X. The \!\(\*SuperscriptBox[\(\:0176\), \(ijk\)]\) are assumed to be symmetric in the first two indices. To modify this behaviour, set \!\(\*
StyleBox[\"$zYhatSymmetryRules\",\nFontWeight->\"Bold\"]\)={}.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"YhatRules\",\nFontWeight->\"Bold\"]\) is a list of rules (in the form \!\(\*StyleBox[\"Yhat\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"j\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"k\",\nFontWeight->\"Bold\"]\)]->\!\(\*
StyleBox[\"yval\",\nFontWeight->\"Bold\"]\)) giving the values of the independent \!\(\*SuperscriptBox[\(\:0176\), \(ijk\)]\).";


zSetConifoldLocus::usage = "\!\(\*
StyleBox[\"zSetConifoldLocus\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"\[CapitalDelta]\",\nFontWeight->\"Bold\"]\)]
Specifies the polynomial \!\(\*
StyleBox[\"\:2206\",\nFontWeight->\"Bold\"]\) defining the conifold locus of the manifold X. The complex structure moduli space coordinates are \!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)].
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"\[CapitalDelta]\",\nFontWeight->\"Bold\"]\) is a polynomial whose vanishing locus is the conifold locus of X.";


zSetOtherSingularLocus::usage = "\!\(\*
StyleBox[\"zSetOtherSingularLocus\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"\[ScriptCapitalY]\",\nFontWeight->\"Bold\"]\)]
Specifies the polynomial \!\(\*
StyleBox[\"\[ScriptCapitalY]\",\nFontWeight->\"Bold\"]\) defining moduli space locus of the manifold X, where X is has a singularity not of conifold or large complex structure type.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"\[ScriptCapitalY]\",\nFontWeight->\"Bold\"]\) is a polynomial whose vanishing locus is the singular locus of X, without the conifold and large complex structure singularities.";


zc::usage = "\!\(\*
StyleBox[\"zc\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"s\",\nFontWeight->\"Bold\"]\),{},{\!\(\*
StyleBox[\"a1\",\nFontWeight->\"Bold\"]\),...,\!\(\*
StyleBox[\"am\",\nFontWeight->\"Bold\"]\)}]
Gives the coefficient of \!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(1\)], SubscriptBox[\(a\), \(1\)]]\)...\!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(m\)], SubscriptBox[\(a\), \(m\)]]\) in f or \!\(\*OverscriptBox[\(f\), \(~\)]\), depending on whether \!\(\*
StyleBox[\"s\",\nFontWeight->\"Bold\"]\) equals 0 or 3.
\!\(\*
StyleBox[\"zc\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"s\",\nFontWeight->\"Bold\"]\),{\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)},{\!\(\*
StyleBox[\"a1\",\nFontWeight->\"Bold\"]\),...,\!\(\*
StyleBox[\"am\",\nFontWeight->\"Bold\"]\)}]
Gives the coefficient of \!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(1\)], SubscriptBox[\(a\), \(1\)]]\)...\!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(m\)], SubscriptBox[\(a\), \(m\)]]\) in \!\(\*SuperscriptBox[\(f\), \(i\)]\) or \!\(\*SuperscriptBox[OverscriptBox[\(f\), \(~\)], \(i\)]\), depending on whether \!\(\*
StyleBox[\"s\",\nFontWeight->\"Bold\"]\) equals 1 or 2.";


zPeriodsFromFundamental::usage = "\!\(\*
StyleBox[\"zPeriodsFromFundamental\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"fundPeriodCoeff\",\nFontWeight->\"Bold\"]\),{\!\(\*
StyleBox[\"k1\",\nFontWeight->\"Bold\"]\),...,\!\(\*
StyleBox[\"km\",\nFontWeight->\"Bold\"]\)}]
Computes the periods \!\(\*SuperscriptBox[\(\[CurlyPi]\), \(i\)]\), \!\(\*SubscriptBox[\(\[CurlyPi]\), \(i\)]\) and \!\(\*SubscriptBox[\(\[CurlyPi]\), \(9\)]\) from the fundamental period coefficients using the expansion for the periods in terms of the \[Epsilon]-matrices.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"fundPeriodCoeff\",\nFontWeight->\"Bold\"]\) is the coefficient of \!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(1\)], SubscriptBox[\(k\), \(1\)]]\)...\!\(\*SuperscriptBox[SubscriptBox[\(\[CurlyPhi]\), \(m\)], SubscriptBox[\(k\), \(m\)]]\) of the fundamental period \!\(\*SuperscriptBox[\(\[CurlyPi]\), \(0\)]\).
\!\(\*
StyleBox[\"k1\",\nFontWeight->\"Bold\"]\), ..., \!\(\*
StyleBox[\"km\",\nFontWeight->\"Bold\"]\) are the variables \!\(\*SubscriptBox[\(k\), \(1\)]\),...,\!\(\*SubscriptBox[\(k\), \(m\)]\) that give the powers of \!\(\*SubscriptBox[\(\[CurlyPhi]\), \(i\)]\) corresponding to the coefficient \!\(\*
StyleBox[\"fundPeriodCoeff\",\nFontWeight->\"Bold\"]\).
\!\(\*
StyleBox[\"Warning\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\":\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"This\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"function\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"can\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"be\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"very\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"slow\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";


\[Omega]t::usage = "\!\(\*
StyleBox[\"\[Omega]t\",\nFontWeight->\"Bold\"]\)[]
Gives the period vector the logarithm-free period vector \!\(\*OverscriptBox[\(\*SubscriptBox[\(\[CurlyTheta]\), \(0\)] \[Omega]\), \(~\)]\).";
\[Theta]\[Omega]t::usage = "\!\(\*
StyleBox[\"\[Theta]\[Omega]t\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)]
Gives the period vector the logarithm-free period vector \!\(\*OverscriptBox[\(\*SubscriptBox[\(\[CurlyTheta]\), \(i\)] \[Omega]\), \(~\)]\) involving the first derivatives.";
\[Theta]2\[Omega]t::usage = "\!\(\*
StyleBox[\"\[Theta]2\[Omega]t\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)]
Gives the period vector the logarithm-free period vector \!\(\*OverscriptBox[\(\*SuperscriptBox[\(\[CurlyTheta]\), \(i\)] \[Omega]\), \(~\)]\) involving the second derivatives.";
\[Theta]3\[Omega]t::usage = "\!\(\*
StyleBox[\"\[Theta]3\[Omega]t\",\nFontWeight->\"Bold\"]\)[]
Gives the period vector the logarithm-free period vector \!\(\*OverscriptBox[\(\*SuperscriptBox[\(\[CurlyTheta]\), \(0\)] \[Omega]\), \(~\)]\) involving the third derivatives.";


zSetDirectory::usage = "\!\(\*
StyleBox[\"zSetDirectory\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"path\",\nFontWeight->\"Bold\"]\)]
Specifies the directory where the text files containing the period coefficients are stored
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"path\",\nFontWeight->\"Bold\"]\) is a string containing the path of the directory relative to the \!\(\*
StyleBox[\"Directory\",\nFontWeight->\"Bold\"]\)[].";


zPeriodsToFile::usage="\!\(\*
StyleBox[\"zPeriodsToFile\",\nFontWeight->\"Bold\"]\)[]
Saves the logarithm-free period vectors stored into the variables \!\(\*
StyleBox[\"\[Omega]t\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\), \!\(\*
StyleBox[\"\[Theta]\[Omega]t\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\), \!\(\*
StyleBox[\"\[Theta]2\[Omega]t\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\), and \!\(\*
StyleBox[\"\[Theta]3\[Omega]t\",\nFontWeight->\"Bold\"]\)[] to \!\(\*
StyleBox[\".\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"txt\",\nFontWeight->\"Bold\"]\) files named \!\(\*
StyleBox[\"wtilde_Coeffs\",\nFontWeight->\"Bold\"]\), \!\(\*
StyleBox[\"thwtilde_Coeffs\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"_i\",\nFontWeight->\"Bold\"]\), \!\(\*
StyleBox[\"th2wtilde_Coeffs\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"_i\",\nFontWeight->\"Bold\"]\), and \!\(\*
StyleBox[\"th3wtilde_Coeffs\",\nFontWeight->\"Bold\"]\), where \!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\) ranges from \!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\) to \!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\). The files are located in the directory specified by \!\(\*
StyleBox[\"zSetDirectory\",\nFontWeight->\"Bold\"]\).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"None\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";


zPeriodsFromFile::usage="\!\(\*
StyleBox[\"zPeriodsToFile\",\nFontWeight->\"Bold\"]\)[]
Reads the series expressions for the logarithm-free periods from \!\(\*
StyleBox[\".\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"txt\",\nFontWeight->\"Bold\"]\) files to which they have been saved and stores the expressions to the variables \!\(\*
StyleBox[\"\[Omega]t\",\nFontWeight->\"Bold\"]\)[],...,\!\(\*
StyleBox[\"\[Theta]3\[Omega]t\",\nFontWeight->\"Bold\"]\)[] representing these vectors.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"None\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)";


zFindW::usage="\!\(\*
StyleBox[\"zFindW\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"deg1\",\nFontWeight->\"Bold\"]\),...,\!\(\*
StyleBox[\"degm\",\nFontWeight->\"Bold\"]\)},\!\(\*
StyleBox[\"NMax\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"NumDeg\",\nFontWeight->\"Bold\"]\)]
Looks for the rational matrix W by evaluating it as a series and solving for the denominator using a generic ansatz.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"deg1\",\nFontWeight->\"Bold\"]\),...,\!\(\*
StyleBox[\"degm\",\nFontWeight->\"Bold\"]\) is a list giving the degree of the ansatz for the denominator of W in the coordinates \!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)].
\!\(\*
StyleBox[\"NMax\",\nFontWeight->\"Bold\"]\) is number of terms to which the series used for finding the denominator should be computed.
\!\(\*
StyleBox[\"NumDeg\",\nFontWeight->\"Bold\"]\) is number of terms to which the series used for finding the numerator should be computed.";


zW::usage="\!\(\*StyleBox[\"zW\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"Gives\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"matrix\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"W\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"defined\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"by\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"E\",\nFontWeight->\"Plain\"]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)\!\(\*SuperscriptBox[\()\), \(T\)]\)\[Sigma]E(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)).";


z\[Sigma]::usage="\!\(\*
StyleBox[\"z\[Sigma]\",\nFontWeight->\"Bold\"]\)
Gives the matrix \[Sigma] representing the wedge product.";


zSetW::usage="\!\(\*
StyleBox[\"zSetW\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"WMat\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\)
Specifies the the matrix W.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"WMat\",\nFontWeight->\"Bold\"]\) is the matrix W.";


zComputeEMatrices::usage="\!\(\*
StyleBox[\"zComputeEMatrices\",\nFontWeight->\"Bold\"]\)[]
Computes the matrices \!\(\*OverscriptBox[\(E\), \(~\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) and \!\(\*SuperscriptBox[\(\!\(\*OverscriptBox[\(E\), \(~\)]\)\), \(-1\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) and stores them in internal variables.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
None.";


zFindU0Constants::usage="\!\(\*
StyleBox[\"zFindU0Constants\",\nFontWeight->\"Bold\"]\)[]
Computes coefficients \!\(\*SubscriptBox[\(\[Alpha]\), \(i\)]\) and \[Gamma]hat which appear in \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"0\",\nFontWeight->\"Bold\"]\)).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"0\",\nFontWeight->\"Bold\"]\)) is computed.
\!\(\*
StyleBox[\"acc\",\nFontWeight->\"Bold\"]\) is the p-adic target accuracy to which the function aims to compute the constants. However, a lower accuracy solution may be returned, if higher-accuracy solution is not found.
\!\(\*
StyleBox[\"maxdeg\",\nFontWeight->\"Bold\"]\) is the maximum degree of the series \!\(\*SubscriptBox[\(P\), \(n\)]\)(\!\(\*SuperscriptBox[
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"], \(p\)]\))\!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) for the series to be considered terminating. If the degree of \!\(\*SubscriptBox[\(P\), \(n\)]\)(\!\(\*SuperscriptBox[
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"], \(p\)]\))U(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) is higher than \!\(\*
StyleBox[\"maxdeg\",\nFontWeight->\"Bold\"]\) for a particular set of constants \!\(\*SubscriptBox[\(\[Alpha]\), \(i\)]\) and \[Gamma]hat, then the series is considered non-terminating and the values of \!\(\*SubscriptBox[\(\[Alpha]\), \(i\)]\) and \[Gamma]hat are considered not to give a solution.";


zUSeries::usage="\!\(\*
StyleBox[\"zUSeries\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)]
Gives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) as a matrix of Taylor series.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix is computed.";


zURational::usage="\!\(\*
StyleBox[\"zURational\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]\)]
Gives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) as a matrix of rational functions in the coordinates \!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)].
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix is computed.
\!\(\*
StyleBox[\"padiacc\",\nFontWeight->\"Bold\"]\) is the p-adic accuracy to which the matrix is computed, i.e. the coefficients in the series appearing in the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) are treated mod \!\(\*SuperscriptBox[\(p\), 
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]]\) when computing the rational functions.";


zUNumeric::usage="\!\(\*
StyleBox[\"zUNumeric\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]},\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]\)]
Gives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) as a matrix of integers, given to the specified p-adic accuracy.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)] is a list of integers at whose Teichm\[UDoubleDot]ller representatives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) is evaluated.
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix is computed.
\!\(\*
StyleBox[\"padiacc\",\nFontWeight->\"Bold\"]\) is the p-adic accuracy to which the matrix is computed, i.e. the entries of the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) are treated mod \!\(\*SuperscriptBox[\(p\), 
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]]\) when computing the rational functions.";


zR::usage="\!\(\*
StyleBox[\"zR\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]},\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]\)]
Gives the characteristic polynomial R(X,T) of the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[1],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)] is a list of integers at whose Teichm\[UDoubleDot]ller representatives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) is evaluated.
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix is computed.
\!\(\*
StyleBox[\"padiacc\",\nFontWeight->\"Bold\"]\) is the p-adic accuracy to which the matrix is computed, i.e. the entries of the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) are treated mod \!\(\*SuperscriptBox[\(p\), 
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]]\).";


zRCoefficient::usage="\!\(\*
StyleBox[\"zRCoefficient\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\),{\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]},\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]\)]
Gives coefficient of \!\(\*SuperscriptBox[\(T\), \(i\)]\) in the characteristic polynomial R(X,T) of the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"i\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)is a power of T whose coefficient in the polynomial R(X,T) is to be computed.
\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[1],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)] is a list of integers at whose Teichm\[UDoubleDot]ller representatives the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) is evaluated.
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p for which the matrix is computed.
\!\(\*
StyleBox[\"padiacc\",\nFontWeight->\"Bold\"]\) is the p-adic accuracy to which the matrix is computed, i.e. the entries of the matrix \!\(\*SubscriptBox[\(U\), \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) are treated mod \!\(\*SuperscriptBox[\(p\), 
StyleBox[\"padicacc\",\nFontWeight->\"Bold\"]]\).";


zSingularityType::usage="\!\(\*
StyleBox[\"zSingularityType\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]},\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)]
Gives the list of singularity types of the manifold X/\!\(\*SubscriptBox[\(F\), \(p\)]\) corresponding to the point (Teich(\!\(\*SubscriptBox[\(\[CurlyPhi]\), \(1\)]\)),...,Teich(\!\(\*SubscriptBox[\(\[CurlyPhi]\), \(m\)]\))) in the complex structure moduli space.
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)
\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[1],...,\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)] is a list of integer coordinates in the moduli space, specifying the manifold X.
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\) is the prime p giving the number of elements in the finite field \!\(\*SubscriptBox[\(F\), \(p\)]\) over which X is considered to be defined.";


zUDenominator::usage="\!\(\*
StyleBox[\"zUDenominator\",\nFontWeight->\"Bold\"]\)[{\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)]},\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)]
Gives the denominator \!\(\*SubscriptBox[\(P\), \(n\)]\)(\!\(\*SuperscriptBox[
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"], 
StyleBox[\"p\",\nFontWeight->\"Plain\"]]\)) at \!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"=\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"(\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"...\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"[\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"]\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\")\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"matrix\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*SubscriptBox[
StyleBox[\"U\",\nFontWeight->\"Plain\"], \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"1\",\nFontWeight->\"Bold\"]\)],...\!\(\*
StyleBox[\"\[Phi]\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"NParams\",\nFontWeight->\"Bold\"]\)] are the coordinates at which the denominator is to be evaluated.
\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"prime\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"specifying\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"matrix\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*SubscriptBox[
StyleBox[\"U\",\nFontWeight->\"Plain\"], \(p\)]\)(\!\(\*
StyleBox[\"\[CurlyPhi]\",\nFontWeight->\"Bold\"]\)) whose denominator is given.";


pzeta3::usage="
\!\(\*
StyleBox[\"pzeta3\",\nFontWeight->\"Bold\"]\)[\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\),\!\(\*
StyleBox[\"acc\",\nFontWeight->\"Bold\"]\)]
Gives the \!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)-adic zeta function \!\(\*SubscriptBox[\(\[Zeta]\), 
StyleBox[\"p\",\nFontWeight->\"Bold\"]]\)(3) to \!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)-adic accuracy \!\(\*
StyleBox[\"acc\",\nFontWeight->\"Bold\"]\).
\!\(\*
StyleBox[\"Arguments\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"prime\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"for\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"which\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"zeta\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"function\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"computed\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"\\\\n\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"acc\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"p\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"-\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"adic\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"accuracy\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"to\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"which\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"the\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"function\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"is\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"computed\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)"


YY;Y;\[Lambda];T;\[Phi];\[Alpha];\[Gamma]hat;


$zYhatSymmetryRules={Yhat[$i_,$j_,$k_]:>Yhat@@Flatten[{Sort[{$i,$j}],$k}]};


Begin["`Private`"];


(* ::Subsection:: *)
(*Settings*)


zSetNParameters[NParameters_Integer]:=(
$zNParams=NParameters;
z\[Sigma]=(2\[Pi]*I)^-3*{{0,0,0,-1},{0,0,IdentityMatrix[$zNParams],0},{0,-IdentityMatrix[$zNParams],0,0},{1,0,0,0}}//ArrayFlatten;
$zSumRanges=Table[{$i[$$i],0,$zNMax-Sum[$i[$$j],{$$j,1,$$i-1}]},{$$i,1,$zNParams}];

zRCoefficient[2+2*$zNParams,{\[Phi]vals__},p_,acc_]:=p^(3*(1+$zNParams));
)


zSetNMax[NMax_Integer]:=($zNMax=NMax;)


$zYSymmetryRules={Y[i_,j_,k_]:>Y@@Sort[{i,j,k}]};
$zYhatSymmetryRules={Yhat[i_,j_,k_]:>Yhat@@Flatten[{Sort[{i,j}],k}]};

zSetY[YValueRules_List]:=($zYValueRules=(YValueRules/.$zYSymmetryRules))
zSetYhat[YhatValueRules_List]:=($zYhatValueRules=(YhatValueRules/.$zYhatSymmetryRules))


YY[i_,j_,k_]:=Y[i,j,k]/.$zYSymmetryRules/.$zYValueRules
YYhat[i_,j_,k_]:=Yhat[i,j,k]/.$zYhatSymmetryRules/.$zYhatValueRules


zSetConifoldLocus[\[CapitalDelta]_]:=($zDenom\[CapitalDelta]=\[CapitalDelta]);
zSetOtherSingularLocus[Y_]:=($zDenomY=Y);


(* ::Subsection:: *)
(*Periods*)


Clear[$zfs]
$zfs[i_,{a___}]:=$zfs[{a}]=Sum[zc[i,{a},Table[$i[$$i],{$$i,1,$zNParams}]]*Times@@(Table[\[Phi][$$i],{$$i,1,$zNParams}]^Table[$i[$$i],{$$i,1,$zNParams}])*\[Lambda]^Sum[$i[$$i],{$$i,1,$zNParams}],##]&@@$zSumRanges
$zfs[i_,{a___},{\[Theta]___}]:=$zfs[i,{a},{\[Theta]}]=Block[{DFactor=Evaluate[Times@@(Table[$i[$$i],{$$i,1,$zNParams}]^(Length/@Gather[Sort[Join[{\[Theta]},Table[$$i,{$$i,1,$zNParams}]]]]-1))],coordFactor=Times@@(Table[\[Phi][$$i],{$$i,1,$zNParams}]^Table[$i[$$i],{$$i,1,$zNParams}])*\[Lambda]^Sum[$i[$$i],{$$i,1,$zNParams}]},Sum[zc[i,{a},Table[$i[$$i],{$$i,1,$zNParams}]]*DFactor*coordFactor,##]&@@$zSumRanges]


Clear[\[Omega]t,\[Theta]\[Omega]t,\[Theta]2\[Omega]t,\[Theta]3\[Omega]t]
\[Omega]t[]:=\[Omega]t[]=ExpandAll[Flatten[{$zfs[0,{},{}],Table[$zfs[1,{$$$i},{}],{$$$i,1,$zNParams}],Table[$zfs[2,{$$$i},{}],{$$$i,1,$zNParams}],$zfs[3,{},{}]}]]
\[Theta]\[Omega]t[j_]:=\[Theta]\[Omega]t[j]=ExpandAll[Flatten[{$zfs[0,{},{j}],Table[KroneckerDelta[$$$i,j]*$zfs[0,{},{}]+$zfs[1,{$$$i},{j}],{$$$i,1,$zNParams}],Table[$zfs[2,{$$$i},{j}]+2*Sum[YY[$$$i,j,$a]/2!*$zfs[1,{$a},{}],{$a,1,$zNParams}],{$$$i,1,$zNParams}],$zfs[3,{},{j}]+$zfs[2,{j},{}]}]]
\[Theta]2\[Omega]t[j_]:=\[Theta]2\[Omega]t[j]=ExpandAll[Flatten[{Sum[YYhat[$\[Alpha],$\[Beta],j]*$zfs[0,{},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}],Table[2*Sum[YYhat[$\[Alpha],$$$i,j]*$zfs[0,{},{$\[Alpha]}],{$\[Alpha],1,$zNParams}]+Sum[YYhat[$\[Alpha],$\[Beta],j]*$zfs[1,{$$$i},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}],{$$$i,1,$zNParams}],Table[Sum[YYhat[$\[Alpha],$\[Beta],j]*$zfs[2,{$$$i},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}]+2*Sum[YY[$$$i,$a,$b]/2!*YYhat[$a,$\[Alpha],j]*2*$zfs[1,{$b},{$\[Alpha]}],{$a,1,$zNParams},{$b,1,$zNParams},{$\[Alpha],1,$zNParams}]+KroneckerDelta[$$$i,j]*$zfs[0,{},{}],{$$$i,1,$zNParams}],Sum[YYhat[$\[Alpha],$\[Beta],j]*$zfs[3,{},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}]+2*Sum[YYhat[$a,$\[Alpha],j]*$zfs[2,{$a},{$\[Alpha]}],{$a,1,$zNParams},{$\[Alpha],1,$zNParams}]+$zfs[1,{j},{}]}]]
\[Theta]3\[Omega]t[]:=\[Theta]3\[Omega]t[]=ExpandAll[Flatten[{1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$\[Gamma]]*$zfs[0,{},{$\[Alpha],$\[Beta],$\[Gamma]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$\[Gamma],1,$zNParams}],Table[1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$$$i]*$zfs[0,{},{$\[Alpha],$\[Beta]}]+2*YYhat[$\[Alpha],$$$i,$\[Beta]]*$zfs[0,{},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}]+1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$\[Gamma]]*$zfs[1,{$$$i},{$\[Alpha],$\[Beta],$\[Gamma]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$\[Gamma],1,$zNParams}],{$$$i,1,$zNParams}],Table[1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$\[Gamma]]*$zfs[2,{$$$i},{$\[Alpha],$\[Beta],$\[Gamma]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$\[Gamma],1,$zNParams}]+1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$j]*YY[$$$i,$j,$k]*$zfs[1,{$k},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$j,1,$zNParams},{$k,1,$zNParams}]+2/$zNParams*Sum[YYhat[$j,$\[Alpha],$\[Beta]]*YY[$$$i,$j,$k]*$zfs[1,{$k},{$\[Alpha],$\[Beta]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$j,1,$zNParams},{$k,1,$zNParams}]+1/$zNParams*$zfs[0,{},{$$$i}]+2/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$\[Gamma]]*YY[$$$i,$\[Beta],$\[Gamma]]*$zfs[0,{},{$\[Alpha]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$\[Gamma],1,$zNParams}],{$$$i,1,$zNParams}],1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$\[Gamma]]*$zfs[3,{},{$\[Alpha],$\[Beta],$\[Gamma]}],{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams},{$\[Gamma],1,$zNParams}]+1/$zNParams*Sum[2!*YYhat[$$$i,$\[Alpha],$\[Gamma]]*$zfs[2,{$$$i},{$\[Alpha],$\[Gamma]}],{$$$i,1,$zNParams},{$\[Alpha],1,$zNParams},{$\[Gamma],1,$zNParams}]+1/$zNParams*Sum[YYhat[$\[Alpha],$\[Beta],$$$i]*$zfs[2,{$$$i},{$\[Alpha],$\[Beta]}],{$$$i,1,$zNParams},{$\[Alpha],1,$zNParams},{$\[Beta],1,$zNParams}]+1/$zNParams*Sum[$zfs[1,{$k},{$k}],{$k,1,$zNParams}]+2/$zNParams*Sum[YY[$$$i,$j,$k]*YYhat[$$$i,$\[Alpha],$j]*$zfs[1,{$k},{$\[Alpha]}],{$$$i,1,$zNParams},{$j,1,$zNParams},{$k,1,$zNParams},{$\[Alpha],1,$zNParams}]+$zfs[0,{}]}]]



zPeriodsFromFundamental[fundCoeffs_,{vars__}]:=Block[{},
Clear[zc];
(*\[CurlyPi]^0*)
With[{args={0,{},Table[Pattern[Evaluate[ToExpression["$k"<>ToString[$i]]],Blank[]],{$i,1,$zNParams}]},form=zc[0,{},Table[ToExpression["$k"<>ToString[$i]],{$i,1,$zNParams}]],body=fundCoeffs/.Table[{vars}[[$i]]->ToExpression["$k"<>ToString[$i]],{$i,1,$zNParams}]},Evaluate[zc@@args]:=form=body];
(*\[CurlyPi]^i*)
Table[
With[{args={1,{$j},Table[Pattern[Evaluate[ToExpression["$k"<>ToString[$i]]],Blank[]],{$i,1,$zNParams}]},form=zc[1,{$j},Table[ToExpression["$k"<>ToString[$i]],{$i,1,$zNParams}]],body=FullSimplify[D[zc[0,{},Table[ToExpression["$k"<>ToString[$i]]+\[Delta][$i],{$i,1,$zNParams}]]/zc[0,{},Table[\[Delta][$i],{i,1,$zNParams}]],\[Delta][$j]]/.\[Delta][i_]:>0]},Evaluate[zc@@args]:=form=body],
{$j,1,$zNParams}];
(*Subscript[\[CurlyPi], i]*)
Table[
With[{args={2,{$j},Table[Pattern[Evaluate[ToExpression["$k"<>ToString[$i]]],Blank[]],{$i,1,$zNParams}]},form=zc[2,{$j},Table[ToExpression["$k"<>ToString[$i]],{$i,1,$zNParams}]],body=FullSimplify[FunctionExpand[Sum[YY[$j,$a,$b]/2!*D[zc[0,{},Table[ToExpression["$k"<>ToString[$i]]+\[Delta][$i],{$i,1,$zNParams}]]/zc[0,{},Table[\[Delta][$i],{$i,1,$zNParams}]],\[Delta][$a],\[Delta][$b]]/.\[Delta][i_]:>0,{$a,1,$zNParams},{$b,1,$zNParams}]]]},Evaluate[zc@@args]:=form=FullSimplify[body]],
{$j,1,$zNParams}];
(*Subscript[\[CurlyPi], 0]*)
With[{args={3,{},Table[Pattern[Evaluate[ToExpression["$k"<>ToString[$i]]],Blank[]],{$i,1,$zNParams}]},form=zc[3,{},Table[ToExpression["$k"<>ToString[$i]],{$i,1,$zNParams}]],body=FullSimplify[FunctionExpand[Sum[YY[$a,$b,$c]/3!*D[zc[0,{},Table[ToExpression["$k"<>ToString[$i]]+\[Delta][$i],{$i,1,$zNParams}]]/zc[0,{},Table[\[Delta][$i],{$i,1,$zNParams}]],\[Delta][$a],\[Delta][$b],\[Delta][$c]]/.\[Delta][i_]:>0,{$a,1,$zNParams},{$b,1,$zNParams},{$c,1,$zNParams}]]]},Evaluate[zc@@args]:=form=FullSimplify[body]]
]


(* ::Subsection:: *)
(*Write/Read to/from a file*)


zSetDirectory[directory_]:=$zfolderName=directory

zPeriodsToFile[]:=Block[{strIn,$$i},
strIn=OpenWrite[$zfolderName<>"/wtilde_Coeffs.txt"];
Write[strIn,CoefficientList[Normal[\[Omega]t[]]/.\[Lambda]->1,Table[\[Phi][$$j],{$$j,1,$zNParams}],Table[$zNMax+1,$zNParams]]];
Close[strIn];
Clear[strIn];

For[$$i=1,$$i<=$zNParams,$$i++,
strIn=OpenWrite[$zfolderName<>"/thwtilde_Coeffs_"<>ToString[$$i]<>".txt"];
Write[strIn,CoefficientList[Normal[\[Theta]\[Omega]t[$$i]]/.\[Lambda]->1,Table[\[Phi][$$j],{$$j,1,$zNParams}],Table[$zNMax+1,$zNParams]]];
Close[strIn];
Clear[strIn]
];

For[$$i=1,$$i<=$zNParams,$$i++,
strIn=OpenWrite[$zfolderName<>"/th2wtilde_Coeffs_"<>ToString[$$i]<>".txt"];
Write[strIn,CoefficientList[Normal[\[Theta]2\[Omega]t[$$i]]/.\[Lambda]->1,Table[\[Phi][$$j],{$$j,1,$zNParams}],Table[$zNMax+1,$zNParams]]];
Close[strIn];
Clear[strIn]
];

strIn=OpenWrite[$zfolderName<>"/th3wtilde_Coeffs.txt"];
Write[strIn,CoefficientList[Normal[\[Theta]3\[Omega]t[]]/.\[Lambda]->1,Table[\[Phi][$$j],{$$j,1,$zNParams}],Table[$zNMax+1,$zNParams]]];
Close[strIn];
Clear[strIn];
]

zPeriodsFromFile[]:=Block[{strOut,$$i,coordsTab},
strOut=OpenRead[$zfolderName<>"/wtilde_Coeffs.txt"];
$z\[Omega]tildeTab=Read[strOut,Expression];
Close[strOut];
Clear[strOut];

For[$$i=1,$$i<=$zNParams,$$i++,
strOut=OpenRead[$zfolderName<>"/thwtilde_Coeffs_"<>ToString[$$i]<>".txt"];
$z\[Theta]1\[Omega]tildeTab[$$i]=Read[strOut,Expression];
Close[strOut];
Clear[strOut]
];

For[$$i=1,$$i<=$zNParams,$$i++,
strOut=OpenRead[$zfolderName<>"/th2wtilde_Coeffs_"<>ToString[$$i]<>".txt"];
$z\[Theta]2\[Omega]tildeTab[$$i]=Read[strOut,Expression];
Close[strOut];
Clear[strOut]
];

strOut=OpenRead[$zfolderName<>"/th3wtilde_Coeffs.txt"];
$z\[Theta]3\[Omega]tildeTab=Read[strOut,Expression];
Close[strOut];
Clear[strOut];


coordsTab=Table[Times@@(Table[\[Phi][$$i],{$$i,1,$zNParams}]^Table[$i[$$i],{$$i,1,$zNParams}]),##]&@@Table[{$i[$$i],0,$zNMax},{$$i,1,$zNParams}];

Clear[\[Omega]t,\[Theta]\[Omega]t,\[Theta]2\[Omega]t,\[Theta]3\[Omega]t];
\[Omega]t[]=SeriesData[\[Lambda],0,CoefficientList[#,\[Lambda]],0,$zNMax+1,1]&/@((Total[Flatten[coordsTab*#]]&/@Extract[$z\[Omega]tildeTab,Join[{All},Table[1;;$zNMax+1,{$zNParams}]]])/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]});

For[$$i=1,$$i<=$zNParams,$$i++,
\[Theta]\[Omega]t[$$i]=SeriesData[\[Lambda],0,CoefficientList[#,\[Lambda]],0,$zNMax+1,1]&/@((Total[Flatten[coordsTab*#]]&/@Extract[$z\[Theta]1\[Omega]tildeTab[$$i],Join[{All},Table[1;;$zNMax+1,{$zNParams}]]])/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]});
\[Theta]2\[Omega]t[$$i]=SeriesData[\[Lambda],0,CoefficientList[#,\[Lambda]],0,$zNMax+1,1]&/@((Total[Flatten[coordsTab*#]]&/@Extract[$z\[Theta]2\[Omega]tildeTab[$$i],Join[{All},Table[1;;$zNMax+1,{$zNParams}]]])/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]});
];

\[Theta]3\[Omega]t[]=SeriesData[\[Lambda],0,CoefficientList[#,\[Lambda]],0,$zNMax+1,1]&/@((Total[Flatten[coordsTab*#]]&/@Extract[$z\[Theta]3\[Omega]tildeTab,Join[{All},Table[1;;$zNMax+1,{$zNParams}]]])/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]});
]


(* ::Subsection:: *)
(*W Matrix*)


Clear[$zWDenominator]
$zWDenominator[{nmax__Integer},NMax_Integer]:=$zWDenominator[{nmax},NMax]=Block[{$series\[CurlyPi]\[Theta]3\[CurlyPi],$\[Omega]tshort=\[Omega]t[]+O[\[Lambda]]^(NMax+Max[{nmax}]+1),$\[Theta]3\[Omega]tshort=\[Theta]3\[Omega]t[]+O[\[Lambda]]^(NMax+Max[{nmax}]+1),$sumLimits,$sumRanges,$denomAnsatz,$coordFactor=(Times@@(Table[\[Phi][$$i],{$$i,1,$zNParams}]^Table[$i[$$i],{$$i,1,$zNParams}])),$denomSoln,$denom,$freevars},

$series\[CurlyPi]\[Theta]3\[CurlyPi]=ExpandAll[(2\[Pi]*I)^3*$\[Omega]tshort . z\[Sigma] . $\[Theta]3\[Omega]tshort];

If[Length[{nmax}]==1,
$sumLimits={nmax}[[1]]*Table[1,{$$i,1,$zNParams}],
$sumLimits={nmax}
];

$sumRanges=Table[{$i[$$i],0,$sumLimits[[$$i]]},{$$i,1,$zNParams}];

$denomAnsatz=(Sum[($aa@@Table[$i[$$i],{$$i,1,$zNParams}])*$coordFactor,##]&@@$sumRanges)/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]};

$denomSoln=CoefficientList[#,Table[\[Phi][$$i],{$$i,0,$zNParams}]]&/@CoefficientList[$denomAnsatz*$series\[CurlyPi]\[Theta]3\[CurlyPi],\[Lambda]][[NMax;;NMax+Max[{nmax}]+1]]==0//Solve;

If[$denomSoln=={}||Simplify[($denomAnsatz/.\[Lambda]->1/.$denomSoln[[1]])]===0,
Print["No solution to the given accuracy"];
{},
$denom=$denomAnsatz/.\[Lambda]->1/.$denomSoln[[1]];

$freevars=Complement[Variables[$denom],Table[\[Phi][$$i],{$$i,0,$zNParams}]];
If[$freevars=={},
$denom,
If[Length[$freevars]>1,
Print["Warning: There are free variables - a denominator of lower degree may exist."]
];
$denom/.{$freevars[[1]]->1}/.Table[$$i->1,{$$i,$freevars}]//Simplify//Factor
]
]
]


Clear[zFindW]
zFindW[{denomDegs__Integer},NMax_Integer,NumDeg_Integer]:=zFindW[{denomDegs},NMax,NumDeg]=Block[{$EtildeW=Transpose[Join[{\[Omega]t[]+O[\[Lambda]]^(NumDeg+1)},Table[\[Theta]\[Omega]t[$$i]+O[\[Lambda]]^(NumDeg+1),{$$i,1,$zNParams}],Table[\[Theta]2\[Omega]t[$$i]+O[\[Lambda]]^(NumDeg+1),{$$i,1,$zNParams}],{\[Theta]3\[Omega]t[]+O[\[Lambda]]^(NumDeg+1)}]]},

If[$zWDenominator[{denomDegs},NMax]==={},
{},
zSetW[(1/($zWDenominator[{denomDegs},NMax])^2 Normal[ExpandAll[($zWDenominator[{denomDegs},NMax]/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]})^2*Transpose[$EtildeW] . z\[Sigma] . $EtildeW]]/.\[Lambda]->1)//Factor];
]
]

zSetW[WMatt_List?MatrixQ]:=(
zW=WMatt;
$zDenomW=PolynomialLCM@@Flatten[Denominator[Inverse[zW]//Cancel]];
);


(* ::Subsection:: *)
(*E Matrices*)


zComputeEMatrices[]:=(
$zEt=Transpose[Join[{\[Omega]t[]},Table[\[Theta]\[Omega]t[$$i],{$$i,1,$zNParams}],Table[\[Theta]2\[Omega]t[$$i],{$$i,1,$zNParams}],{\[Theta]3\[Omega]t[]}]];
$zEtShort=$zEt+O[\[Lambda]]^Ceiling[($zNMax+1)/7];
$zEtInvShort=Normal[ExpandAll[Transpose[z\[Sigma] . $zEtShort . (Simplify[Inverse[zW]]/.{\[Phi][i_]:>\[Lambda]*\[Phi][i]})]+O[\[Lambda]]^Ceiling[($zNMax+1)/7]]];
)


(* ::Subsection:: *)
(*Cohomology Matrices*)


z\[Epsilon][i_]:=(Join[{Table[0,{a,1,2*$zNParams+2}]},{{0,0*IdentityMatrix[$zNParams],0*IdentityMatrix[$zNParams],0},{0,Table[YY[i,a,b],{a,1,$zNParams},{b,1,$zNParams}],0*IdentityMatrix[$zNParams],0}}//ArrayFlatten,{Flatten[{0,Table[0,{a,1,$zNParams}],Table[KroneckerDelta[i,a],{a,1,$zNParams}],0}]}]+Transpose[Join[{Flatten[{0,Table[KroneckerDelta[i,a],{a,1,$zNParams}],Table[0,{a,1,$zNParams}],0}]},Table[0,{a,1,2*$zNParams+2-1},{b,1,2*$zNParams+2}]]])

z\[Mu][i_]:=Join[Table[0,{a,1,2*$zNParams+2-1},{b,1,2*$zNParams+2}],{Flatten[{0,Table[KroneckerDelta[i,a],{a,1,$zNParams}],Table[0,{a,1,$zNParams}],0}]}]+Transpose[Join[{Flatten[{0,Table[0,{a,1,$zNParams}],Table[KroneckerDelta[i,a],{a,1,$zNParams}],0}]},Table[0,{a,1,2*$zNParams+2-1},{b,1,2*$zNParams+2}]]]

z\[Eta][]:=Table[KroneckerDelta[i,2$zNParams+2]*KroneckerDelta[j,1],{i,1,2$zNParams+2},{j,1,2$zNParams+2}]



Clear[zU0]
zU0[p_]:=DiagonalMatrix[Flatten[{1,Table[p,$zNParams],Table[p^2,$zNParams],p^3}]] . (IdentityMatrix[2+2*$zNParams]+Sum[Subscript[\[Alpha], i][p]*z\[Epsilon][i],{i,1,$zNParams}]+Sum[YY[i,j,k]/2 Subscript[\[Alpha], i][p]*Subscript[\[Alpha], j][p]*z\[Mu][k],{i,1,$zNParams},{j,1,$zNParams},{k,1,$zNParams}]+\[Gamma]hat[p]*z\[Eta][])


(* ::Subsection:: *)
(*Denominators*)


zSetConifoldLocus[\[CapitalDelta]_]:=($zDenom\[CapitalDelta]=\[CapitalDelta]);
zSetOtherSingularLocus[Y_]:=($zDenomY=Y);

zUDenominator[pp_,padicacc_/;padicacc>=4]:=$zDenomW*$zDenomY^2*($zDenomY^2*$zDenom\[CapitalDelta])^(padicacc-4)/.{\[Phi][i_]:>\[Lambda]^pp*\[Phi][i]^pp}
zUDenominator[pp_,padicacc_/;padicacc<4]:=$zDenomW*$zDenomY^2/.{\[Phi][i_]:>\[Lambda]^pp*\[Phi][i]^pp}


zSingularityType[{\[Phi]vals__Integer},pp_]:={If[Mod[$zDenomW/.\[Phi][i_]:>{\[Phi]vals}[[i]],pp]==0,"apparent",Nothing],If[Mod[$zDenomY/.\[Phi][i_]:>{\[Phi]vals}[[i]],pp]==0,"other",Nothing],If[Mod[$zDenom\[CapitalDelta]/.\[Phi][i_]:>{\[Phi]vals}[[i]],pp]==0,"conifold",Nothing]}


(* ::Subsection:: *)
(*p-adic utilities*)


pTeich[\[Phi]_,0,p_,acc_]=1;
pTeich[\[Phi]_,1,p_,acc_]:=pTeich[\[Phi],1,p,acc]=PowerMod[\[Phi], p^(acc-1),p^acc];
pTeich[\[Phi]_,m_/;m>1,p_,acc_]:=pTeich[Mod[\[Phi]^Mod[m,p-1],p],1,p,acc]

$zTeichrule[p_]:={\[Phi][i_]^$n_:>\[Phi][i]^Mod[$n,p-1]}


pExpand[x_,p_]:=Block[{digs=pDigits[x,p]},

digs[[1]] . Table[\[ScriptP]^n,{n,digs[[2]],digs[[2]]+Length[digs[[1]]]-1}]
]

pExpandSymb[s_,p_,ord_,acc_]:=Sum[s[i]*\[ScriptP]^i,{i,ord,ord+acc-1}]


pOrd[x_Rational,p_]:=IntegerExponent[Numerator[x],p]-IntegerExponent[Denominator[x],p]
pOrd[x_Integer,p_]:=IntegerExponent[x,p]
pDigits[0,p_]:={{0},0}
pDigits[x_Rational,p_]:={$pdigits[p^-pOrd[x,p]*x,p],pOrd[x,p]}
pDigits[x_Integer,p_]:={$pdigits[p^-pOrd[x,p]*x,p],pOrd[x,p]}

$pdigits[num_,p_]:=
(* returns the p-adic digits of an integer *)
Drop[
Mod[
FixedPointList[Floor[#/p]&,num],
        p],
         -2];


pRationalTopAdic[rat_,p_,acc_]:=
(* converts a rational number to the form integer/p^n *)
Block[{numer,denom,a,b},
If[rat===0,0,
numer=Numerator[rat];
denom=Denominator[rat];
a=pOrd[numer,p];
b=pOrd[denom,p];
Mod[numer/p^a PowerMod[denom/p^b,-1,p^acc],p^acc]p^(a-b)
   ]
            ];

SetAttributes[pRationalTopAdic, Listable]

pRationalTopAdicRound[rat_,p_,lowacc_]:=
Block[{numer,denom,a,b},
If[rat===0,0,
numer=Numerator[rat];
denom=Denominator[rat];
a=pOrd[numer,p];
b=pOrd[denom,p];
If[a-b>=lowacc,Return[0]];
Mod[numer/p^a PowerMod[denom/p^b,-1,p^lowacc] p^a,p^lowacc]p^-b
   ]
            ];
SetAttributes[pRationalTopAdicRound, Listable]


(* APPROXIMATE upper limit for the sum for the p-adic Gamma function *)
$nmaxc[ac_,p_]:=

Block[{c,red},
c=ac;
red=ac/p;
While[Floor[red]>0, c+=Floor[red];red/=p];
c]; 

$dw[n_,p_]:=Sum[1/(((n-r)p)! r! p^r),{r,0,n}]

Gammap1[p_,acc_]:=pRationalTopAdic[ Sum[$dw[(m+1),p] p^m  m!,{m,0,$nmaxc[acc,p]}], p,acc]

Gammap2[p_,acc_]:=pRationalTopAdic[ 2Sum[$dw[(m+2),p] p^m  (m+1)! HarmonicNumber[m+1],{m,0,$nmaxc[acc,p]}], p, acc]

Gammap3[p_,acc_]:=
pRationalTopAdic[ 3Sum[$dw[(m+3),p] p^m  (m+2)!( HarmonicNumber[m+2]^2-HarmonicNumber[m+2,2]),{m,0,$nmaxc[acc,p]}],p,acc]

h3[p_,acc_]:=pRationalTopAdic[120(Gammap3[p,acc]-Gammap1[p,acc]^3),p,acc+2]

pzeta3[p_,acc_]:=Mod[pRationalTopAdic[-(h3[p,acc+2]/(2*120)),p,acc+2],p^acc]



(*This is just usual mod, but makes some numbers negative rather than very large*)
cmod[num_,md_]:=Mod[num,md, -(md-1)/2]

(*This implements the relation \[CurlyPhi]^p=\[CurlyPhi], which is satisfied when \[CurlyPhi] is a Teichm\[UDoubleDot]ller representative*)
teichrule[p_]:={\[CurlyPhi]^pow_:>\[CurlyPhi]^Mod[pow,p-1],u^pow_:>u^Mod[pow,p-1],w^pow_:>w^Mod[pow,p-1]}


(* ::Subsection:: *)
(*Finding the coefficients \[Alpha] and \[Gamma]hat*)


$zComputeEMatricesLine[]:=(
$zEtildeLine=(Normal[$zEt]/.\[Lambda]->1/.\[Phi][i_]:>\[CurlyPhi])+O[\[CurlyPhi]]^($zNMax+1);
$zWinvLine=Simplify[Cancel[Inverse[zW]]/.{\[Phi][i_]:>\[CurlyPhi]}];
$zEtildeInverseLine=Transpose[z\[Sigma] . $zEtildeLine . $zWinvLine]+O[\[CurlyPhi]]^Ceiling[($zNMax+1)/7];
)


Clear[$zULine]
$zULine[p_]:=$zULine[p]=($zComputeEMatricesLine[];
(Normal[$zEtildeInverseLine]/.{\[CurlyPhi]->\[CurlyPhi]^p}) . (zU0[p]/.Subscript[\[Alpha], i_][p]:>Subscript[\[Alpha], i]/.\[Gamma]hat[p]->\[Gamma]hat) . $zEtildeLine+O[\[CurlyPhi]]^($zNMax+1));


Clear[$zU\[Alpha]Coeffs]
$zU\[Alpha]Coeffs[pp_,padicacc_]:=$zU\[Alpha]Coeffs[pp,padicacc]=CoefficientList[Collect[Normal[(zUDenominator[pp,padicacc]/.\[Lambda]->1/.{\[Phi][i_]:>\[CurlyPhi]})*Tr[$zULine[pp]]],{\[CurlyPhi],\[Gamma]hat,Subscript[\[Alpha], i_]},pRationalTopAdicRound[#,pp,padicacc]&],\[CurlyPhi]]

Clear[$zU\[Alpha]CoeffspExpansion]
$zU\[Alpha]CoeffspExpansion[pp_,padicacc_]:=$zU\[Alpha]CoeffspExpansion[pp,padicacc]=Collect[$zU\[Alpha]Coeffs[pp,padicacc],{Subscript[\[Alpha], i_],\[Gamma]hat},pExpand[#,pp]&]/.{Subscript[\[Alpha], i_]:>pExpandSymb[Subscript[\[Alpha], i],pp,0,padicacc],\[Gamma]hat->pExpandSymb[\[Gamma]hat,pp,0,padicacc]}


Clear[$zU\[Alpha]pExpansionEquations]
$zU\[Alpha]pExpansionEquations[pp_,padicacc_,maxord_]:=$zU\[Alpha]pExpansionEquations[pp,padicacc,maxord]=Table[Coefficient[\[ScriptP]*$zU\[Alpha]CoeffspExpansion[pp,padicacc][[maxord+1;;$zNMax]],\[ScriptP],$n],{$n,0,padicacc}]


Clear[$zFindFreeVariables]
$zFindFreeVariables[solution_]:=Variables[solution[[All,2]]]



Clear[$zNextSolution,$zFixFreeVariables]
$zNextSolution[pp_,padicacc_,maxord_,numEq_,previousSoln_,carry_]:=Block[{newSolns,newCarry},

newSolns=Solve[Collect[$zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousSoln,{Subscript[\[Alpha], i_][n_],\[Gamma]hat[n_]},Mod[#,pp]&]==0,Modulus->pp];
If[newSolns=={},previousSoln,

If[Length[$zFindFreeVariables[newSolns[[1]]]]>0,
(*If there are free variables*)
newCarry=Expand[($zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousSoln)/pp];

If[padicacc==numEq-1,(*If this is the last equation, just display the result*)
Flatten[{previousSoln}]
,
$zFixFreeVariables[pp,padicacc,maxord,numEq+1,previousSoln,newSolns,newCarry,$zFindFreeVariables[newSolns[[1]]]]
]

,
(*If there are no free variables*)
newCarry=Expand[($zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousSoln/.newSolns[[1]])/pp];

If[padicacc==numEq-1,(*If this is the last equation, just display the result*)
Flatten[{previousSoln,newSolns[[1]]}]
,
$zNextSolution[pp,padicacc,maxord,numEq+1,Flatten[{previousSoln,newSolns[[1]]}],newCarry]
]

]

]

]

$zFixFreeVariables[pp_,padicacc_,maxord_,numEq_,previousFullSoln_,variableRelation_,carry_,freeVars_]:=Block[{ansatz,newSolns,newCarry,solution={},$k=0,cases,ranges=Table[{$i[$j],0,pp-1},{$j,1,Length[freeVars]}]},
cases=Flatten[Table[Table[freeVars[[$j]]->$i[$j],{$j,1,Length[freeVars]}],##]&@@ranges,2];



While[solution=={}&&$k<Length[cases],
$k++;
ansatz=Flatten[Join[variableRelation/.cases[[$k]],{cases[[$k]]}]]/.{(x_->y_Integer):>(x->Mod[y,pp])};

solution=Solve[Collect[$zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousFullSoln/.ansatz,{Subscript[\[Alpha], i_][n_],\[Gamma]hat[n_]},Mod[#,pp]&]==0,Modulus->pp];
];

If[solution=={},
(*If we do not have a solution, we might as well give the relation we know*)
Flatten[{previousFullSoln,variableRelation}]
,

If[Length[$zFindFreeVariables[solution[[1]]]]>0,
(*If there are free variables again*)
newCarry=Expand[($zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousFullSoln/.ansatz)/pp];

If[padicacc==numEq-1,(*If this is the last equation, just display the result*)
Flatten[{previousFullSoln,ansatz}]
,
(*There may be completely fixed variables in solution*)
$zFixFreeVariables[pp,padicacc,maxord,numEq+1,Flatten[{previousFullSoln,ansatz}],solution,newCarry,$zFindFreeVariables[solution[[1]]]]
]

,
(*If there are no free variables*)
newCarry=Expand[($zU\[Alpha]pExpansionEquations[pp,padicacc,maxord][[numEq]]+carry/.previousFullSoln/.ansatz/.solution[[1]])/pp];

If[padicacc==numEq-1,(*If this is the last equation, just display the result*)
Flatten[{previousFullSoln,ansatz,solution[[1]]}]
,
$zNextSolution[pp,padicacc,maxord,numEq+1,Flatten[{previousFullSoln,ansatz,solution[[1]]}],newCarry]
]

]
]
]

zFindU0Constants[pp_,padicacc_,maxord_]:=Join[Table[Subscript[\[Alpha], $i]->pExpandSymb[Subscript[\[Alpha], $i],pp,0,padicacc],{$i,1,$zNParams}],{\[Gamma]hat->pExpandSymb[\[Gamma]hat,pp,0,padicacc-3]}]/.$zNextSolution[pp,padicacc,maxord,1,{},Table[0,{$i,1,$zNMax-maxord}]]/.\[ScriptP]->pp


(* ::Subsection:: *)
(*The matrix U*)


Clear[zUSeries]
zUSeries[pp_]:=zUSeries[pp]=((($zEtInvShort/.{\[Phi][i_]:>\[Phi][i]^pp,\[Lambda]->\[Lambda]^pp})+O[\[Lambda]]^($zNMax+1)) . zU0[pp] . ($zEt+O[\[Lambda]]^($zNMax+1)))


Clear[zURational]
zURational[pp_,padicacc_]:=zURational[pp,padicacc]=1/(Expand[zUDenominator[pp,padicacc]]/.\[Lambda]->1)*Expand[Collect[(zUDenominator[pp,padicacc]+O[\[Lambda]]^($zNMax+1))*(zUSeries[pp]),{\[Phi][i_],\[Lambda]},pRationalTopAdicRound[#,pp,padicacc]&]/.\[Lambda]->1]


Clear[zUNumeric]
zUNumeric[{\[Phi]vals__Integer},p_,acc_]:=zUNumeric[{\[Phi]vals},p,acc]=pRationalTopAdicRound[#,p,acc]&/@(zURational[p,acc]/.\[Lambda]->1/.$zTeichrule[p]/.{\[Phi][i_]:>pTeich[{\[Phi]vals}[[i]],1,p,20+acc]})


Clear[$zRCoefficientList]
$zRCoefficientList[{\[Phi]vals__},p_,acc_]:=$zRCoefficientList[{\[Phi]vals},p,acc]=CoefficientList[Det[IdentityMatrix[2+2*$zNParams]-T*zUNumeric[{\[Phi]vals},p,acc]],T]


zRCoefficient[i_/;i<=1+$zNParams,{\[Phi]vals__},p_,acc_]:=cmod[$zRCoefficientList[{\[Phi]vals},p,acc][[i+1]],p^Ceiling[Log[p,Binomial[2+2*$zNParams,i]*p^((3*i)/2)]]]
zRCoefficient[i_/;1+$zNParams<i<2+2*$zNParams,{\[Phi]vals__},p_,acc_]:=p^(3(i-$zNParams-1))*zRCoefficient[2*(1+$zNParams)-i,{\[Phi]vals},p,acc]


Clear[zR]
zR[{\[Phi]vals__Integer},p_,acc_]:=zR[{\[Phi]vals},p,acc]=1+Sum[zRCoefficient[$i,{\[Phi]vals},p,acc]*T^$i,{$i,1,2+2*$zNParams}]


End[];


EndPackage[];
