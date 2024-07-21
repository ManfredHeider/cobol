      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0374.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0374.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:27:42   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:54   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-BEIHILFEBEZ-GESAMT.
         10 EA-NI-BEIHILFEBEZ-TAB.
           20 EA-NI-BEIHILFEBEZ-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-BEIHILFEBEZ
                 REDEFINES EA-NI-BEIHILFEBEZ-TAB.
           20 EA-NI-BEIHILFESTATUSID         PIC X.
           20 EA-NI-BEIHILFEVORSCHRID        PIC X.
           20 EA-NI-BHBEZSATZAMBULANT        PIC X.
           20 EA-NI-BHBEZSATZSTATION         PIC X.
           20 EA-NI-BHBEZSATZZAHN            PIC X.
         10 EA-DCL-BEIHILFEBEZ.
           20 EA-BEIHILFESTATUSID            PIC -----9    .
           20 EA-BEIHILFEVORSCHRID           PIC -----9    .
           20 EA-BHBEZSATZAMBULANT           PIC ---9.99          .
           20 EA-BHBEZSATZSTATION            PIC ---9.99          .
           20 EA-BHBEZSATZZAHN               PIC ---9.99          .
      ******************************************************************
