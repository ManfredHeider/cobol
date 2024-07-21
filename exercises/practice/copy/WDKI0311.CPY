      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0311.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0311.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:23:54   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:28   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GLASBERECHNET-GESAMT.
         10 EA-NI-GLASBERECHNET-TAB.
           20 EA-NI-GLASBERECHNET-ELE PIC X
                  OCCURS 2 TIMES.
         10 EA-NI-GLASBERECHNET
                 REDEFINES EA-NI-GLASBERECHNET-TAB.
           20 EA-NI-GLASBERECHNETID          PIC X.
           20 EA-NI-GLASBERECHTEXT           PIC X.
         10 EA-DCL-GLASBERECHNET.
           20 EA-GLASBERECHNETID             PIC -----9    .
           20 EA-GLASBERECHTEXT              PIC X(25).
      ******************************************************************
