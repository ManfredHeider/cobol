000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0087.CPY  $
000110*    $Revision:   2.3  $
000120*
000130*    $Log:   L:/mf/CPY/WDKI0087.CPv  $
000140*
000150*   Rev 2.3   Feb 16 2004 09:49:26   t13ex27
000160*AKI00403 - DB-Änderung für Fix 57
000170*
000180*   Rev 2.2   Apr 07 2003 10:12:40   t13ex27
000190*AKI00347 - DB-Änderung Fix 38
000200*
000210*   Rev 2.1   Jun 03 2002 09:32:22   T13EX27
000220*AKI00289 DB-Änderung für neue VP-Analyse
000230*
000240*   Rev 2.0   May 17 2002 12:04:50   p13sp16
000250*Archiverstellung
000260*
000270*---------------------------------------------------------------*
000280  05 EA-WPHG-GESAMT.
000290   10 EA-NI-WPHG-TAB.
000300     20 EA-NI-WPHG-ELE PIC X OCCURS 9 TIMES.
000310   10 EA-NI-WPHG
000320           REDEFINES EA-NI-WPHG-TAB.
000330     20 EA-NI-WPHGPERSONENID           PIC X.
000340     20 EA-NI-WPHGPERSONENROLLE        PIC X.
000350     20 EA-NI-WPHGLAUFENDENNUM         PIC X.
000360     20 EA-NI-WPHGWERTPAPGESCH         PIC X.
000370     20 EA-NI-WPHGUMFANG               PIC X.
000380     20 EA-NI-WPHGANLEGERTYP           PIC X.
000390     20 EA-NI-WPHGANLAGEDAUER          PIC X.
000400     20 EA-NI-WPHGMAXRISKLASSE         PIC X.
000410     20 EA-NI-WPHGGELDAUSKREDIT        PIC X.
000420   10 EA-DCL-WPHG.
000430     20 EA-WPHGPERSONENID              PIC X(13).
000440     20 EA-WPHGPERSONENROLLE           PIC X(1).
000450     20 EA-WPHGLAUFENDENNUM            PIC -----9.
000460     20 EA-WPHGWERTPAPGESCH            PIC -----9.
000470     20 EA-WPHGUMFANG                  PIC -----9.
000480     20 EA-WPHGANLEGERTYP              PIC -----9.
000490     20 EA-WPHGANLAGEDAUER             PIC -----9.
000500     20 EA-WPHGMAXRISKLASSE            PIC -----9.
000510     20 EA-WPHGGELDAUSKREDIT           PIC X(1).
000520*****************************************************************
