      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0606.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0606.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:36   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUTGRUPPEBEZIEHUNG-GESAMT.
         10 EA-NI-AUTGRUPPEBEZIEHUNG-TAB.
           20 EA-NI-AUTGRUPPEBEZIEHUNG-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-AUTGRUPPEBEZIEHUNG
                 REDEFINES EA-NI-AUTGRUPPEBEZIEHUNG-TAB.
           20 EA-NI-AGBAUTNAME               PIC X.
           20 EA-NI-AGBGRUPPENNAME           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUTGRUPPEBEZIEHUNG.
           20 EA-AGBAUTNAME                  PIC X(13).
           20 EA-AGBGRUPPENNAME              PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
