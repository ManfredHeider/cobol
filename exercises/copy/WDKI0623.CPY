000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0623.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0623.CPv  $
000140*
000150*   Rev 2.1   Apr 15 2002 08:37:20   T13EX27
000160*AKI00278 DB-Änderung für Fixpack 15
000170*
000180*   Rev 2.0   Apr 03 2002 13:49:06   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-PCTYP-GESAMT.
000230   10 EA-NI-PCTYP-TAB.
000240     20 EA-NI-PCTYP-ELE           PIC X
000250            OCCURS 4 TIMES.
000260   10 EA-NI-PCTYP
000270           REDEFINES EA-NI-PCTYP-TAB.
000280     20 EA-NI-PCTYPID              PIC X.
000290     20 EA-NI-PCTYPTEXT            PIC X.
000300     20 EA-NI-PCTYPMIETE           PIC X.
000310     20 EA-NI-PCTYPVERSICHERUNG    PIC X.
000320   10 EA-DCL-PCTYP.
000330     20 EA-PCTYPID              PIC -----9    .
000340     20 EA-PCTYPTEXT            PIC X(25).
000350     20 EA-PCTYPMIETE           PIC ------9.99       .
000360     20 EA-PCTYPVERSICHERUNG    PIC ------9.99       .
000370******************************************************************
