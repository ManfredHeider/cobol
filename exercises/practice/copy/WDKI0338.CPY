      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0338.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0338.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:10:20   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:38   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-UMSATZ-GESAMT.
         10 EA-NI-UMSATZ-TAB.
           20 EA-NI-UMSATZ-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-UMSATZ
                 REDEFINES EA-NI-UMSATZ-TAB.
           20 EA-NI-UMSVERTRAGSID            PIC X.
           20 EA-NI-UMSGUTHABPRODLFDNR       PIC X.
           20 EA-NI-UMSLAUFENDENR            PIC X.
           20 EA-NI-UMSBUCHUNGSDATUM         PIC X.
           20 EA-NI-UMSUMSATZBETRAG          PIC X.
           20 EA-NI-UMSUMSBETRWKZ            PIC X.
           20 EA-NI-UMSTEXTID                PIC X.
           20 EA-NI-UMSZAHLUNGSWEISEID       PIC X.
           20 EA-NI-UMSTEXT                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-UMSATZ.
           20 EA-UMSVERTRAGSID               PIC X(13).
           20 EA-UMSGUTHABPRODLFDNR          PIC -----9    .
           20 EA-UMSLAUFENDENR               PIC -----9    .
           20 EA-UMSBUCHUNGSDATUM            PIC X(10).
           20 EA-UMSUMSATZBETRAG             PIC ---------9.99    .
           20 EA-UMSUMSBETRWKZ               PIC X(01).
           20 EA-UMSTEXTID                   PIC -----9    .
           20 EA-UMSZAHLUNGSWEISEID          PIC -----9    .
           20 EA-UMSTEXT.
              49 EA-UMSTEXT-LEN              PIC -----9    .
              49 EA-UMSTEXT-TEXT             PIC X(20).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
