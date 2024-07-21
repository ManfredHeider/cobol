      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0001.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 15:19:52   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabelle
      *
      *   Rev 2.0   Apr 15 1999 13:31:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-ADRESSART-GESAMT.
         10 EA-NI-ADRESSART-TAB.
           20 EA-NI-ADRESSART-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-ADRESSART
                 REDEFINES EA-NI-ADRESSART-TAB.
           20 EA-NI-ADRESSARTID              PIC X.
           20 EA-NI-ADRESSARTKZ              PIC X.
           20 EA-NI-ADRESSARTTEXT            PIC X.
         10 EA-DCL-ADRESSART.
           20 EA-ADRESSARTID                 PIC -----9    .
           20 EA-ADRESSARTKZ                 PIC X(01).
           20 EA-ADRESSARTTEXT               PIC X(30).
      ******************************************************************