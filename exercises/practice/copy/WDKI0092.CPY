000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0092.CPY  $
000110*    $Revision:   2.3  $
000120*
000130*    $Log:   L:/mf/CPY/WDKI0092.CPv  $
000140*
000150*   Rev 2.3   Oct 17 2005 08:56:44   t13ex27
000160*AKI00521: DB-Änderung für Fix 80 EA-Programme und WDKI's
000170*
000180*   Rev 2.2   Apr 07 2003 10:12:42   t13ex27
000190*AKI00347 - DB-Änderung Fix 38
000200*
000210*   Rev 2.1   Jun 03 2002 09:32:30   T13EX27
000220*AKI00289 DB-Änderung für neue VP-Analyse
000230*
000240*   Rev 2.0   May 23 2002 10:33:28   p13sp16
000250*Archiverstellung
000260*
000270*---------------------------------------------------------------*
000280  05 EA-EINKOMMENRV-GESAMT.
000290   10 EA-NI-EINKOMMENRV-TAB.
000300     20 EA-NI-EINKOMMENRV-ELE PIC X OCCURS 10 TIMES.
000310   10 EA-NI-EINKOMMENRV
000320           REDEFINES EA-NI-EINKOMMENRV-TAB.
000330     20 EA-NI-EKRVPERSONENID           PIC X.
000340     20 EA-NI-EKRVPERSONENROLLE        PIC X.
000350     20 EA-NI-EKRVFREIWILLRENT         PIC X.
000360     20 EA-NI-EKRVGESRENTENJAHR        PIC X.
000370     20 EA-NI-EKRVVERSNUMMER           PIC X.
000380     20 EA-NI-EKRVBERUFVERSJAHR        PIC X.
000390     20 EA-NI-EKRVALTERSRENTE          PIC X.
000400     20 EA-NI-EKRVALTERSRENTEWKZ       PIC X.
000410     20 EA-NI-EKRVEMRENTE              PIC X.
000420     20 EA-NI-EKRVEMRENTEWKZ           PIC X.
000430   10 EA-DCL-EINKOMMENRV.
000440     20 EA-EKRVPERSONENID              PIC X(13).
000450     20 EA-EKRVPERSONENROLLE           PIC X(1).
000460     20 EA-EKRVFREIWILLRENT            PIC -----9.
000470     20 EA-EKRVGESRENTENJAHR           PIC -----9.
000480     20 EA-EKRVVERSNUMMER              PIC X(12).
000490     20 EA-EKRVBERUFVERSJAHR           PIC -----9.
000500     20 EA-EKRVALTERSRENTE             PIC ---------9.99.
000510     20 EA-EKRVALTERSRENTEWKZ          PIC X(1).
000520     20 EA-EKRVEMRENTE                 PIC ---------9.99.
000530     20 EA-EKRVEMRENTEWKZ              PIC X(1).
000540*****************************************************************
