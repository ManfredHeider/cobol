000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0620.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0620.CPv  $
000140*
000150*   Rev 2.1   Apr 15 2002 08:37:14   T13EX27
000160*AKI00278 DB-Änderung für Fixpack 15
000170*
000180*   Rev 2.0   Apr 03 2002 13:48:52   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-PERIPHERIE-GESAMT.
000230   10 EA-NI-PERIPHERIE-TAB.
000240     20 EA-NI-PERIPHERIE-ELE  PIC X
000250            OCCURS 5 TIMES.
000260   10 EA-NI-PERIPHERIE
000270           REDEFINES EA-NI-PERIPHERIE-TAB.
000280     20 EA-NI-PER-PC-NR         PIC X.
000290     20 EA-NI-PER-LFDNR         PIC X.
000300     20 EA-NI-PER-HDWID         PIC X.
000310     20 EA-NI-PER-INSTALLIERT   PIC X.
000320     20 EA-NI-PER-SERIENNR      PIC X.
000330   10 EA-DCL-PERIPHERIE.
000340     20 EA-PER-PC-NR            PIC --------9 .
000350     20 EA-PER-LFDNR            PIC -----9    .
000360     20 EA-PER-HDWID            PIC -----9    .
000370     20 EA-PER-INSTALLIERT      PIC X(10).
000380     20 EA-PER-SERIENNR         PIC X(20).
000390******************************************************************
