000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0090.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0090.CPv  $
000140*
000150*   Rev 2.1   Jun 03 2002 09:32:26   T13EX27
000160*AKI00289 DB-Änderung für neue VP-Analyse
000170*
000180*   Rev 2.0   May 17 2002 12:05:04   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-WPANLEGER-GESAMT.
000230   10 EA-NI-WPANLEGERTYP-TAB.
000240     20 EA-NI-WPANLEGERTYP-ELE PIC X
000250            OCCURS 2 TIMES.
000260   10 EA-NI-WPANLEGERTYP
000270           REDEFINES EA-NI-WPANLEGERTYP-TAB.
000280     20 EA-NI-WPATID               PIC X.
000290     20 EA-NI-WPATTEXT             PIC X.
000300   10 EA-DCL-WPANLEGERTYP.
000310     20 EA-WPATID               PIC -----9    .
000320     20 EA-WPATTEXT             PIC X(30).
000330******************************************************************
