      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0080.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0080.CPv  $
      *
      *   Rev 2.2   Jun 03 2002 09:32:16   T13EX27
      *AKI00289 DB-Änderung für neue VP-Analyse
      *
      *   Rev 2.1   Apr 16 1999 10:06:58   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:24   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HAUSHALT-GESAMT.
         10 EA-NI-HAUSHALT-TAB.
           20 EA-NI-HAUSHALT-ELE PIC X
210502*           OCCURS 3 TIMES.
210502            OCCURS 4 TIMES.
         10 EA-NI-HAUSHALT
                 REDEFINES EA-NI-HAUSHALT-TAB.
           20 EA-NI-HAUSHALTID               PIC X.
           20 EA-NI-HHBETREUERID             PIC X.
           20 EA-NI-HHBETREUERROLLE          PIC X.
210502     20 EA-NI-HHKUNDENAKTEVER          PIC X.
         10 EA-DCL-HAUSHALT.
           20 EA-HAUSHALTID                  PIC X(13).
           20 EA-HHBETREUERID                PIC X(13).
           20 EA-HHBETREUERROLLE             PIC X(01).
210502     20 EA-HHKUNDENAKTEVER             PIC X(01).
      ******************************************************************
