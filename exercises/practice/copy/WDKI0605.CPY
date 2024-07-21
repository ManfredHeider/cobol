      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0605.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0605.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:34   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-AUTGRUPPE-GESAMT.
         10 EA-NI-AUTGRUPPE-TAB.
           20 EA-NI-AUTGRUPPE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-AUTGRUPPE
                 REDEFINES EA-NI-AUTGRUPPE-TAB.
           20 EA-NI-AGRID                    PIC X.
           20 EA-NI-AGRNAME                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-AUTGRUPPE.
           20 EA-AGRID                       PIC X(13).
           20 EA-AGRNAME                     PIC X(64).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
