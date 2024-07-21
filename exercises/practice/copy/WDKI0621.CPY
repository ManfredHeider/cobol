000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0621.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0621.CPv  $
000140*
000150*   Rev 2.1   Apr 15 2002 08:37:16   T13EX27
000160*AKI00278 DB-Änderung für Fixpack 15
000170*
000180*   Rev 2.0   Apr 03 2002 13:48:56   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-RECHNUNGSART-GESAMT.
000230   10 EA-NI-RECHNUNGSART-TAB.
000240     20 EA-NI-RECHNUNGSART-ELE  PIC X
000250            OCCURS 2 TIMES.
000260   10 EA-NI-RECHNUNGSART
000270           REDEFINES EA-NI-RECHNUNGSART-TAB.
000280     20 EA-NI-RECHNUNGSARTID    PIC X.
000290     20 EA-NI-RECHNUNGSARTTEXT  PIC X.
000300   10 EA-DCL-RECHNUNGSART.
000310     20 EA-RECHNUNGSARTID       PIC -----9    .
000320     20 EA-RECHNUNGSARTTEXT     PIC X(25).
000330******************************************************************
