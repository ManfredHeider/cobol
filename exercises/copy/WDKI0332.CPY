      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0332.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0332.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:25:16   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:36   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PARTNERGESELLSCHAF-GESAMT.
         10 EA-NI-PARTNERGESELLSCHAF-TAB.
           20 EA-NI-PARTNERGESELLSCHAF-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-PARTNERGESELLSCHAF
                 REDEFINES EA-NI-PARTNERGESELLSCHAF-TAB.
           20 EA-NI-PARTGESELLSCHAFTID       PIC X.
           20 EA-NI-PGESGRUPPE               PIC X.
           20 EA-NI-PARTGESELLSCHNAME        PIC X.
         10 EA-DCL-PARTNERGESELLSCHAF.
           20 EA-PARTGESELLSCHAFTID          PIC -----9    .
           20 EA-PGESGRUPPE                  PIC -----9    .
           20 EA-PARTGESELLSCHNAME           PIC X(25).
      ******************************************************************
