      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0362.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0362.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:08   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:48   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERTRAGSARTBEZ-GESAMT.
         10 EA-NI-VERTRAGSARTBEZ-TAB.
           20 EA-NI-VERTRAGSARTBEZ-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-VERTRAGSARTBEZ
                 REDEFINES EA-NI-VERTRAGSARTBEZ-TAB.
           20 EA-NI-VRTVERTRAGSID            PIC X.
           20 EA-NI-VRTVERSTARLFDNR          PIC X.
           20 EA-NI-VRTVERTRAGSARTID         PIC X.
           20 EA-NI-VRTLFDNR                 PIC X.
           20 EA-NI-VRTVERTRAGWAGNISKZ       PIC X.
           20 EA-NI-VRTVERTRAGSART           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERTRAGSARTBEZ.
           20 EA-VRTVERTRAGSID               PIC X(13).
           20 EA-VRTVERSTARLFDNR             PIC -----9    .
           20 EA-VRTVERTRAGSARTID            PIC -----9    .
           20 EA-VRTLFDNR                    PIC -----9    .
           20 EA-VRTVERTRAGWAGNISKZ          PIC X(01).
           20 EA-VRTVERTRAGSART.
              49 EA-VRTVERTRAGSART-LEN       PIC -----9    .
              49 EA-VRTVERTRAGSART-TEXT      PIC X(20).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
