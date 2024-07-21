      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0201.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0201.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:07:00   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:24   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUSGABE-GESAMT.
         10 EA-NI-AUSGABE-TAB.
           20 EA-NI-AUSGABE-ELE PIC X
                  OCCURS 9 TIMES.
         10 EA-NI-AUSGABE
                 REDEFINES EA-NI-AUSGABE-TAB.
           20 EA-NI-JAHR                     PIC X.
           20 EA-NI-HEFTNUMMER               PIC X.
           20 EA-NI-AUSGABENJAHR             PIC X.
           20 EA-NI-LOSGROESSE               PIC X.
           20 EA-NI-PREISEIGEN               PIC X.
           20 EA-NI-PREISKUNDEN              PIC X.
           20 EA-NI-BESTELLENDE              PIC X.
           20 EA-NI-THEMEN                   PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUSGABE.
           20 EA-JAHR                        PIC -----9    .
           20 EA-HEFTNUMMER                  PIC -----9    .
           20 EA-AUSGABENJAHR                PIC -----9    .
           20 EA-LOSGROESSE                  PIC -----9    .
           20 EA-PREISEIGEN                  PIC ---9.99           .
           20 EA-PREISKUNDEN                 PIC ---9.99           .
           20 EA-BESTELLENDE                 PIC X(10).
           20 EA-THEMEN.
              49 EA-THEMEN-LEN               PIC -----9    .
              49 EA-THEMEN-TEXT              PIC X(1000).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
