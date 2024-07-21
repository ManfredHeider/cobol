000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0625.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0625.CPv  $
000140*
000150*   Rev 2.1   Apr 15 2002 08:37:22   T13EX27
000160*AKI00278 DB-Änderung für Fixpack 15
000170*
000180*   Rev 2.0   Apr 03 2002 13:49:16   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-RECHNUNGSKORREKTUR-GESAMT.
000230   10 EA-NI-RECHNUNGSKORREKTUR-TAB.
000240     20 EA-NI-RECHNUNGSKORREKTUR-ELE PIC X
000250            OCCURS 5 TIMES.
000260   10 EA-NI-RECHNUNGSKORREKTUR
000270           REDEFINES EA-NI-RECHNUNGSKORREKTUR-TAB.
000280     20 EA-NI-RKOVBID              PIC X.
000290     20 EA-NI-RKOVBROLLE           PIC X.
000300     20 EA-NI-RKOMONAT             PIC X.
000310     20 EA-NI-RKOBUCHUNGSKZ        PIC X.
000320     20 EA-NI-RKOBETRAG            PIC X.
000330   10 EA-DCL-RECHNUNGSKORREKTUR.
000340     20 EA-RKOVBID              PIC X(13).
000350     20 EA-RKOVBROLLE           PIC X(1).
000360     20 EA-RKOMONAT             PIC X(6).
000370     20 EA-RKOBUCHUNGSKZ        PIC X(1).
000380     20 EA-RKOBETRAG            PIC ---------9.99    .
000390******************************************************************
