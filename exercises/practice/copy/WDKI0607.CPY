      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0607.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0607.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:38   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-GRUPPEARBEITSSTBEZ-GESAMT.
         10 EA-NI-GRUPPEARBEITSSTBEZ-TAB.
           20 EA-NI-GRUPPEARBEITSSTBEZ-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-GRUPPEARBEITSSTBEZ
                 REDEFINES EA-NI-GRUPPEARBEITSSTBEZ-TAB.
           20 EA-NI-GABARBEITSTATIONID       PIC X.
           20 EA-NI-GABGRUPPENNAME           PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-GRUPPEARBEITSSTBEZ.
           20 EA-GABARBEITSTATIONID          PIC --------9 .
           20 EA-GABGRUPPENNAME              PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
