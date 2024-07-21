      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0616.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0616.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:13:48   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:33:06   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-PCLINIEAUTGRUPPE-GESAMT.
         10 EA-NI-PCLINIEAUTGRUPPE-TAB.
           20 EA-NI-PCLINIEAUTGRUPPE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-PCLINIEAUTGRUPPE
                 REDEFINES EA-NI-PCLINIEAUTGRUPPE-TAB.
           20 EA-NI-PCLINIE                  PIC X.
           20 EA-NI-AUTGRUPPE                PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-PCLINIEAUTGRUPPE.
           20 EA-PCLINIE                     PIC X(2).
           20 EA-AUTGRUPPE                   PIC X(13).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
