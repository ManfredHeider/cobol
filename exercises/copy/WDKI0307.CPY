      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0307.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/WDKI0307.CPv  $
      *
      *   Rev 2.2   Oct 15 2002 08:15:48   t13ex27
      *AKI00308 DB-Änderung Fixpack 26
      *
      *   Rev 2.1   22 Oct 2001 17:23:38   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:28   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FOND-GESAMT.
         10 EA-NI-FOND-TAB.
      *    10 NI-FOND-ELE PIC S9(04) COMP OCCURS 6 TIMES.
           20 EA-NI-FOND-ELE PIC X
300902*           OCCURS 7 TIMES.
300902            OCCURS 8 TIMES.
         10 EA-NI-FOND
                 REDEFINES EA-NI-FOND-TAB.
           20 EA-NI-FONDID                   PIC X.
           20 EA-NI-FONDDVAGPRODSCHL         PIC X.
           20 EA-NI-FONDGESNR                PIC X.
           20 EA-NI-FONDWERTPAPKNR           PIC X.
           20 EA-NI-FONDWERTPAPKZ            PIC X.
           20 EA-NI-FONDBEZEICHNUNG          PIC X.
           20 EA-NI-FONART                   PIC X.
300902     20 EA-NI-FONDISIN                 PIC X.
         10 EA-DCL-FOND.
           20 EA-FONDID                      PIC -----9    .
           20 EA-FONDDVAGPRODSCHL            PIC -----9    .
           20 EA-FONDGESNR                   PIC -----9    .
           20 EA-FONDWERTPAPKNR              PIC X(6).
           20 EA-FONDWERTPAPKZ               PIC X(01).
           20 EA-FONDBEZEICHNUNG             PIC X(40).
           20 EA-FONDART                     PIC X(20).
300902     20 EA-FONDISIN                    PIC X(12).
      ******************************************************************