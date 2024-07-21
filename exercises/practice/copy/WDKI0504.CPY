      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0504.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/WDKI0504.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 17:29:12   t13ex27
      *Umstellung von OS2 auf W2000
      *Support-Tabellen
      *
      *   Rev 2.0   Apr 15 1999 13:32:58   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-INTERNERSERVICE-GESAMT.
         10 EA-NI-INTERNERSERVICE-TAB.
           20 EA-NI-INTERNERSERVICE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-INTERNERSERVICE
                 REDEFINES EA-NI-INTERNERSERVICE-TAB.
           20 EA-NI-INTSERVICEID             PIC X.
           20 EA-NI-ISNAME                   PIC X.
           20 EA-NI-SERVICENAME              PIC X.
         10 EA-DCL-INTERNERSERVICE.
           20 EA-INTSERVICEID                PIC -----9    .
           20 EA-ISNAME.
              49 EA-ISNAME-LEN               PIC -----9    .
              49 EA-ISNAME-TEXT              PIC X(50).
           20 EA-SERVICENAME.
              49 EA-SERVICENAME-LEN          PIC -----9    .
              49 EA-SERVICENAME-TEXT         PIC X(50).
      ******************************************************************
