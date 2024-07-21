      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0371.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0371.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:18   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:52   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-NBPRODUKT-GESAMT.
         10 EA-NI-NBPRODUKT-TAB.
           20 EA-NI-NBPRODUKT-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-NBPRODUKT
                 REDEFINES EA-NI-NBPRODUKT-TAB.
           20 EA-NI-PRODUKT                  PIC X.
           20 EA-NI-PRODUKTMENUETEXT         PIC X.
           20 EA-NI-PRODUKTKLASSE            PIC X.
         10 EA-DCL-NBPRODUKT.
           20 EA-PRODUKT                     PIC -----9    .
           20 EA-PRODUKTMENUETEXT            PIC X(35).
           20 EA-PRODUKTKLASSE               PIC X(72).
      ******************************************************************
