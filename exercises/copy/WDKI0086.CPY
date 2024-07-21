000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0086.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0086.CPv  $
000140*
000150*   Rev 2.1   Jun 03 2002 09:32:20   T13EX27
000160*AKI00289 DB-Änderung für neue VP-Analyse
000170*
000180*   Rev 2.0   May 17 2002 12:04:44   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-SONSTIGESEINKOMMEN-GESAMT.
000230   10 EA-NI-SONSTIGESEINKOMMEN-TAB.
000240     20 EA-NI-SONSTIGESEINKOMMEN-ELE PIC X
000250            OCCURS 7 TIMES.
000260   10 EA-NI-SONSTIGESEINKOMMEN
000270           REDEFINES EA-NI-SONSTIGESEINKOMMEN-TAB.
000280     20 EA-NI-SEINKOPERSONENID     PIC X.
000290     20 EA-NI-SEINKOPERSROLLE      PIC X.
000300     20 EA-NI-SEINKOLAUFENDENNUM   PIC X.
000310     20 EA-NI-SEINKOBRUTTOEINKO    PIC X.
000320     20 EA-NI-SEINKONETTOEINKO     PIC X.
000330     20 EA-NI-SEINKOANZAHLMONATE   PIC X.
000340     20 EA-NI-SEINKOART            PIC X.
000350   10 EA-DCL-SONSTIGESEINKOMMEN.
000360     20 EA-SEINKOPERSONENID     PIC X(13).
000370     20 EA-SEINKOPERSROLLE      PIC X(1).
000380     20 EA-SEINKOLAUFENDENNUM   PIC -----9    .
000390     20 EA-SEINKOBRUTTOEINKO    PIC ---------9.99    .
000400     20 EA-SEINKONETTOEINKO     PIC ---------9.99    .
000410     20 EA-SEINKOANZAHLMONATE   PIC -----9    .
000420     20 EA-SEINKOART            PIC X(30).
000430******************************************************************
