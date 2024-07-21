      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0066.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0066.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:42   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:18   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ZUKUENFTEINNAHMEN-GESAMT.
         10 EA-NI-ZUKUENFTEINNAHMEN-TAB.
           20 EA-NI-ZUKUENFTEINNAHMEN-ELE PIC X
                  OCCURS 8 TIMES.
         10 EA-NI-ZUKUENFTEINNAHMEN
                 REDEFINES EA-NI-ZUKUENFTEINNAHMEN-TAB.
           20 EA-NI-ZEPERSONENID             PIC X.
           20 EA-NI-ZEPERSONENROLLE          PIC X.
           20 EA-NI-ZELAUFENDENUMMER         PIC X.
           20 EA-NI-ZEJAHR                   PIC X.
           20 EA-NI-ZETEXT                   PIC X.
           20 EA-NI-ZEBETRAG                 PIC X.
           20 EA-NI-ZEBETRAGWKZ              PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-ZUKUENFTEINNAHMEN.
           20 EA-ZEPERSONENID                PIC X(13).
           20 EA-ZEPERSONENROLLE             PIC X(01).
           20 EA-ZELAUFENDENUMMER            PIC --------9 .
           20 EA-ZEJAHR                      PIC X(10).
           20 EA-ZETEXT                      PIC X(30).
           20 EA-ZEBETRAG                    PIC ---------9.99    .
           20 EA-ZEBETRAGWKZ                 PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
