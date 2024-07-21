000010*---------------------------------------------------------------*
000020*    Standard für alle Objekte unter PVCS
000030*            Datum   :  Januar 2001
000040*            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
000050*---------------------------------------------------------------*
000060*
000070*---------------------------------------------------------------*
000080*    Copyright 2001 ATLAS Dienstleistungs GmbH
000090*---------------------------------------------------------------*
000100*    $Workfile:   WDKI0085.CPY  $
000110*    $Revision:   2.2  $
000120*
000130*    $Log:   L:/mf/cpy/WDKI0085.CPv  $
000140*
000150*   Rev 2.2   Apr 07 2003 10:12:38   t13ex27
000160*AKI00347 - DB-Änderung Fix 38
000170*
000180*   Rev 2.1   Jun 03 2002 09:32:20   T13EX27
000190*AKI00289 DB-Änderung für neue VP-Analyse
000200*
000210*   Rev 2.0   May 17 2002 12:04:40   p13sp16
000220*Archiverstellung
000230*
000240*---------------------------------------------------------------*
000250  05 EA-EIGENTUMMIETE-GESAMT.
000260   10 EA-NI-EIGENTUMMIETE-TAB.
000270     20 EA-NI-EIGENTUMMIETE-ELE PIC X
000280            OCCURS 14 TIMES.
000290   10 EA-NI-EIGENTUMMIETE
000300           REDEFINES EA-NI-EIGENTUMMIETE-TAB.
000310     20 EA-NI-EMPERSONENID             PIC X.
000320     20 EA-NI-EMPERSONENROLLE          PIC X.
000330     20 EA-NI-EMLAUFENDENUMMER         PIC X.
000340     20 EA-NI-EMTYPE                   PIC X.
000350     20 EA-NI-EMWOHNFLAECHE            PIC X.
000360     20 EA-NI-EMMIETE                  PIC X.
000370     20 EA-NI-EMNEBENKOSTEN            PIC X.
000380     20 EA-NI-EMMIETFINKOSTEN          PIC X.
000390     20 EA-NI-EMWERTVORHANDEN          PIC X.
000400     20 EA-NI-EMRESTSCHULD             PIC X.
000410     20 EA-NI-EMKAUFWUNSCH             PIC X.
000420     20 EA-NI-EMWERTZUKAUFEN           PIC X.
000430     20 EA-NI-EMWERTGRUNDSTUECK        PIC X.
000440     20 EA-NI-EMKAUFBAUDATUM           PIC X.
000450   10 EA-DCL-EIGENTUMMIETE.
000460     20 EA-EMPERSONENID                PIC X(13).
000470     20 EA-EMPERSONENROLLE             PIC X(1).
000480     20 EA-EMLAUFENDENUMMER            PIC -----9.
000490     20 EA-EMTYPE                      PIC -----9.
000500     20 EA-EMWOHNFLAECHE               PIC ----9.99.
000510     20 EA-EMMIETE                     PIC ---------9.99.
000520     20 EA-EMNEBENKOSTEN               PIC ---------9.99.
000530     20 EA-EMMIETFINKOSTEN             PIC ---------9.99.
000540     20 EA-EMWERTVORHANDEN             PIC ---------9.99.
000550     20 EA-EMRESTSCHULD                PIC ---------9.99.
000560     20 EA-EMKAUFWUNSCH                PIC X(3).
000570     20 EA-EMWERTZUKAUFEN              PIC ---------9.99.
000580     20 EA-EMWERTGRUNDSTUECK           PIC ---------9.99.
000590     20 EA-EMKAUFBAUDATUM              PIC X(010).
000600*****************************************************************

