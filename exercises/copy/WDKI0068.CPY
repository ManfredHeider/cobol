      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0068.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:46   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-MITARBEITER-GESAMT.
         10 EA-NI-MITARBEITER-TAB.
           20 EA-NI-MITARBEITER-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-MITARBEITER
                 REDEFINES EA-NI-MITARBEITER-TAB.
           20 EA-NI-MITARBPERSONENID         PIC X.
           20 EA-NI-MITARBPERSROLLE          PIC X.
           20 EA-NI-MITARBVORGESPERSID       PIC X.
           20 EA-NI-MITARBVORGESROLLE        PIC X.
           20 EA-NI-MITARBEITERNR            PIC X.
         10 EA-DCL-MITARBEITER.
           20 EA-MITARBPERSONENID            PIC X(13).
           20 EA-MITARBPERSROLLE             PIC X(01).
           20 EA-MITARBVORGESPERSID          PIC X(13).
           20 EA-MITARBVORGESROLLE           PIC X(01).
           20 EA-MITARBEITERNR               PIC --------9 .
      ******************************************************************
