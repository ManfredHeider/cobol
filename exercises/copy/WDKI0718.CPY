      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0718.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0718.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:14:12   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SELLAUFBEARBEITUNG-GESAMT.
         10 EA-NI-SELLAUFBEARBEITUNG-TAB.
           20 EA-NI-SELLAUFBEARBEITUNG-ELE PIC X
                  OCCURS 12 TIMES.
         10 EA-NI-SELLAUFBEARBEITUNG
                 REDEFINES EA-NI-SELLAUFBEARBEITUNG-TAB.
           20 EA-NI-SLAID                    PIC X.
           20 EA-NI-SLABENUTZTANFRAGID       PIC X.
           20 EA-NI-SLAERSTELLPID            PIC X.
           20 EA-NI-SLAERSTELLPROLLE         PIC X.
           20 EA-NI-SLAERSTELLDATUM          PIC X.
           20 EA-NI-SLAERSTELLZEIT           PIC X.
           20 EA-NI-SLASTARTZEIT             PIC X.
           20 EA-NI-SLAFERTIGSTELLZEIT       PIC X.
           20 EA-NI-SLATREFFERDATEI          PIC X.
           20 EA-NI-SLADATEIGESENDET         PIC X.
           20 EA-NI-SLADATEIEMPFANGEN        PIC X.
           20 EA-NI-SLAANZAHLTREFFER         PIC X.
         10 EA-DCL-SELLAUFBEARBEITUNG.
           20 EA-SLAID                       PIC X(13).
           20 EA-SLABENUTZTANFRAGID          PIC X(13).
           20 EA-SLAERSTELLPID               PIC X(13).
           20 EA-SLAERSTELLPROLLE            PIC X(1).
           20 EA-SLAERSTELLDATUM             PIC X(10).
           20 EA-SLAERSTELLZEIT              PIC X(08).
           20 EA-SLASTARTZEIT                PIC X(08).
           20 EA-SLAFERTIGSTELLZEIT          PIC X(08).
           20 EA-SLATREFFERDATEI.
              49 EA-SLATREFFERDATEI-LEN      PIC -----9    .
              49 EA-SLATREFFERDATEI-TEXT     PIC X(40).
           20 EA-SLADATEIGESENDET            PIC X(1).
           20 EA-SLADATEIEMPFANGEN           PIC X(1).
           20 EA-SLAANZAHLTREFFER            PIC --------9 .
      ******************************************************************
