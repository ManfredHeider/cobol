000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0088.CPY  $
000110*    $Revision:   2.1  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0088.CPv  $
000140*
000150*   Rev 2.1   Jun 03 2002 09:32:24   T13EX27
000160*AKI00289 DB-Änderung für neue VP-Analyse
000170*
000180*   Rev 2.0   May 17 2002 12:04:54   p13sp16
000190*Archiverstellung
000200*
000210*---------------------------------------------------------------*
000220  05 EA-WPGESCHAEFTE-GESAMT.
000230   10 EA-NI-WPGESCHAEFTE-TAB.
000240     20 EA-NI-WPGESCHAEFTE-ELE   PIC X
000250            OCCURS 2 TIMES.
000260   10 EA-NI-WPGESCHAEFTE
000270           REDEFINES EA-NI-WPGESCHAEFTE-TAB.
000280     20 EA-NI-WPGID                PIC X.
000290     20 EA-NI-WPGTEXT              PIC X.
000300   10 EA-DCL-WPGESCHAEFTE.
000310     20 EA-WPGID                PIC -----9    .
000320     20 EA-WPGTEXT              PIC X(70).
000330******************************************************************
