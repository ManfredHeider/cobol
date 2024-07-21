      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0502.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0502.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:04   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-EXTERNERSERVICE-GESAMT.
         10 EA-NI-EXTERNERSERVICE-TAB.
           20 EA-NI-EXTERNERSERVICE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-EXTERNERSERVICE
                 REDEFINES EA-NI-EXTERNERSERVICE-TAB.
           20 EA-NI-EXTSERVICEID             PIC X.
           20 EA-NI-INTERNERSERVICEID        PIC X.
           20 EA-NI-ESNAME                   PIC X.
         10 EA-DCL-EXTERNERSERVICE.
           20 EA-EXTSERVICEID                PIC -----9    .
           20 EA-INTERNERSERVICEID           PIC -----9    .
           20 EA-ESNAME.
              49 EA-ESNAME-LEN               PIC -----9    .
              49 EA-ESNAME-TEXT              PIC X(50).
      ******************************************************************
