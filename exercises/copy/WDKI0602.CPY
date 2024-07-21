      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0602.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0602.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:26   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:00   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUTBENUTZERBEZ-GESAMT.
         10 EA-NI-AUTBENUTZERBEZ-TAB.
           20 EA-NI-AUTBENUTZERBEZ-ELE PIC X
                  OCCURS 4 TIMES.
         10 EA-NI-AUTBENUTZERBEZ
                 REDEFINES EA-NI-AUTBENUTZERBEZ-TAB.
           20 EA-NI-ABBPERSONENID            PIC X.
           20 EA-NI-ABBPERSONENROLLE         PIC X.
           20 EA-NI-ABBAUTNAME               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUTBENUTZERBEZ.
           20 EA-ABBPERSONENID               PIC X(13).
           20 EA-ABBPERSONENROLLE            PIC X(01).
           20 EA-ABBAUTNAME                  PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
