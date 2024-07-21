      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0803.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0803.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:32:14   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-REPLLOG-GESAMT.
         10 EA-NI-REPLLOG-TAB.
           20 EA-NI-REPLLOG-ELE PIC X
                  OCCURS 9 TIMES.
         10 EA-NI-REPLLOG
                 REDEFINES EA-NI-REPLLOG-TAB.
           20 EA-NI-LOGMARKER                PIC X.
           20 EA-NI-LFDNR                    PIC X.
           20 EA-NI-LOGID                    PIC X.
           20 EA-NI-TRANSACTIONID            PIC X.
           20 EA-NI-INFOTEXT                 PIC X.
           20 EA-NI-CLASSNAME                PIC X.
           20 EA-NI-KEYS                     PIC X.
           20 EA-NI-DATASTRING1              PIC X.
           20 EA-NI-DATASTRING2              PIC X.
         10 EA-DCL-REPLLOG.
           20 EA-LOGMARKER                   PIC X(26).
           20 EA-LFDNR                       PIC --------9 .
           20 EA-LOGID                       PIC --------9 .
           20 EA-TRANSACTIONID               PIC --------9 .
           20 EA-INFOTEXT.
              49 EA-INFOTEXT-LEN             PIC -----9    .
              49 EA-INFOTEXT-TEXT            PIC X(40).
           20 EA-CLASSNAME.
              49 EA-CLASSNAME-LEN            PIC -----9    .
              49 EA-CLASSNAME-TEXT           PIC X(40).
           20 EA-KEYS.
              49 EA-KEYS-LEN                 PIC -----9    .
              49 EA-KEYS-TEXT                PIC X(128).
           20 EA-DATASTRING1.
              49 EA-DATASTRING1-LEN          PIC -----9    .
              49 EA-DATASTRING1-TEXT         PIC X(128).
           20 EA-DATASTRING2.
              49 EA-DATASTRING2-LEN          PIC -----9    .
              49 EA-DATASTRING2-TEXT         PIC X(128).
      ******************************************************************
