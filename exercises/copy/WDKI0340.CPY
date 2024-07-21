      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0340.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0340.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:10:26   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:38   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERBTILGUNG-GESAMT.
         10 EA-NI-VERBTILGUNG-TAB.
           20 EA-NI-VERBTILGUNG-ELE PIC X
                  OCCURS 11 TIMES.
         10 EA-NI-VERBTILGUNG
                 REDEFINES EA-NI-VERBTILGUNG-TAB.
           20 EA-NI-VTLVERTRAGSID            PIC X.
           20 EA-NI-VTLGUTHABPRODLFDNR       PIC X.
           20 EA-NI-VTLVERTRTEILLFDNR        PIC X.
           20 EA-NI-VTLLFDNR                 PIC X.
           20 EA-NI-VTLTSONDERTILGUNG        PIC X.
           20 EA-NI-VTLSONDERTBEZ            PIC X.
           20 EA-NI-VTLZEITPUNKT             PIC X.
           20 EA-NI-VTLBETRAG                PIC X.
           20 EA-NI-VTLBETRAGDWKZ            PIC X.
           20 EA-NI-VTLTILGUNGDURCH          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERBTILGUNG.
           20 EA-VTLVERTRAGSID               PIC X(13).
           20 EA-VTLGUTHABPRODLFDNR          PIC -----9    .
           20 EA-VTLVERTRTEILLFDNR           PIC -----9    .
           20 EA-VTLLFDNR                    PIC -----9    .
           20 EA-VTLTSONDERTILGUNG           PIC X(01).
           20 EA-VTLSONDERTBEZ               PIC X(20).
           20 EA-VTLZEITPUNKT                PIC X(6).
           20 EA-VTLBETRAG                   PIC ---------9.99    .
           20 EA-VTLBETRAGDWKZ               PIC X(01).
           20 EA-VTLTILGUNGDURCH             PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
