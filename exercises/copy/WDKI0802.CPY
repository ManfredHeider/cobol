      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0802.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0802.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:24   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-HISTORYVALUETAB-GESAMT.
         10 EA-NI-HISTORYVALUETAB-TAB.
           20 EA-NI-HISTORYVALUETAB-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-HISTORYVALUETAB
                 REDEFINES EA-NI-HISTORYVALUETAB-TAB.
           20 EA-NI-HID                      PIC X.
           20 EA-NI-SEQNUMBER                PIC X.
           20 EA-NI-ATTRIBUT                 PIC X.
           20 EA-NI-OLDVALUE                 PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-HISTORYVALUETAB.
           20 EA-HID                         PIC X(13).
           20 EA-SEQNUMBER                   PIC --------9 .
           20 EA-ATTRIBUT.
              49 EA-ATTRIBUT-LEN             PIC -----9    .
              49 EA-ATTRIBUT-TEXT            PIC X(128).
           20 EA-OLDVALUE.
              49 EA-OLDVALUE-LEN             PIC -----9    .
              49 EA-OLDVALUE-TEXT            PIC X(128).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
