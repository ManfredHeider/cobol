      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0333.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0333.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:25:24   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:36   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PRODUKT-GESAMT.
         10 EA-NI-PRODUKT-TAB.
           20 EA-NI-PRODUKT-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-PRODUKT
                 REDEFINES EA-NI-PRODUKT-TAB.
           20 EA-NI-PRODUKTID                PIC X.
           20 EA-NI-PRODUKTSPARTEID          PIC X.
           20 EA-NI-HAUPTPRODUKTID           PIC X.
           20 EA-NI-PRODUKTTEXT              PIC X.
         10 EA-DCL-PRODUKT.
           20 EA-PRODUKTID                   PIC -----9    .
           20 EA-PRODUKTSPARTEID             PIC -----9    .
           20 EA-HAUPTPRODUKTID              PIC -----9    .
           20 EA-PRODUKTTEXT                 PIC X(72).
      ******************************************************************
