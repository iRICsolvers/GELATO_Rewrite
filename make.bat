@echo off

call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2022

rem ----------------------------------------------------------------------
rem ifort compile with debug options
rem ----------------------------------------------------------------------
set FC=ifx
set FLAGS_RELEASE= /O2 /QxHost /heap-arrays:0 /Qopenmp /nostandard-realloc-lhs /MD /c /Qdiag-disable:10448
set FLAGS_DEBUG= /Qopenmp /nostandard-realloc-lhs /MD /traceback /check all /heap-arrays /c /Qdiag-disable:10448

%FC% .\src\iric.f90 %FLAGS_RELEASE%
%FC% .\src\timer_m.f90 %FLAGS_RELEASE%
%FC% .\src\Cell2node_m.f90 %FLAGS_RELEASE%
%FC% .\src\common.f90 %FLAGS_RELEASE%
%FC% .\src\grid.f90 %FLAGS_RELEASE%
%FC% .\src\result.f90 %FLAGS_RELEASE%
%FC% .\src\trace.f90 %FLAGS_RELEASE%
%FC% .\src\fish.f90 %FLAGS_RELEASE%
%FC% .\src\landscape_poly.f90 %FLAGS_RELEASE%
%FC% .\src\GELATO.f90 %FLAGS_RELEASE%

rem ----------------------------------------------------------------------
rem Link
rem ----------------------------------------------------------------------
%FC% *.obj .\lib\iriclib.lib -o ".\install\GELATO.exe"

rem ----------------------------------------------------------------------
rem Cleanup
rem ----------------------------------------------------------------------
del *.obj
del *.mod
