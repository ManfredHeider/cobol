      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0604.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0604.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:32   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUTKATEGORIE-GESAMT.
         10 EA-NI-AUTKATEGORIE-TAB.
           20 EA-NI-AUTKATEGORIE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-AUTKATEGORIE
                 REDEFINES EA-NI-AUTKATEGORIE-TAB.
           20 EA-NI-AKAID                    PIC X.
           20 EA-NI-AKANAME                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUTKATEGORIE.
           20 EA-AKAID                       PIC X(13).
           20 EA-AKANAME                     PIC X(30).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
