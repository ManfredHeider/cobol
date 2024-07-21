      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0809.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0809.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:33:02   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:20   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-IIMROW-GESAMT.
         10 EA-NI-IIMROW-TAB.
           20 EA-NI-IIMROW-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-IIMROW
                 REDEFINES EA-NI-IIMROW-TAB.
           20 EA-NI-IIMID                    PIC X.
           20 EA-NI-IIMMODIFICATIONID        PIC X.
           20 EA-NI-IIMSUBSCRIBERS           PIC X.
           20 EA-NI-IIMSSUBTYPE              PIC X.
         10 EA-DCL-IIMROW.
           20 EA-IIMID                       PIC --------9 .
           20 EA-IIMMODIFICATIONID           PIC --------9 .
           20 EA-IIMSUBSCRIBERS.
              49 EA-IIMSUBSCRIBERS-LEN       PIC -----9    .
              49 EA-IIMSUBSCRIBERS-TEXT      PIC X(254).
           20 EA-IIMSSUBTYPE.
              49 EA-IIMSSUBTYPE-LEN          PIC -----9    .
              49 EA-IIMSSUBTYPE-TEXT         PIC X(254).
      ******************************************************************
