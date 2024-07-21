      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0382.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0382.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:20   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:56   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-FINDATGRUPPE-GESAMT.
         10 EA-NI-FINDATGRUPPE-TAB.
           20 EA-NI-FINDATGRUPPE-ELE PIC X
                  OCCURS 3 TIMES.
         10 EA-NI-FINDATGRUPPE
                 REDEFINES EA-NI-FINDATGRUPPE-TAB.
           20 EA-NI-FGRGRUPPEID              PIC X.
           20 EA-NI-FGRBESCHREIBUNG          PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-FINDATGRUPPE.
           20 EA-FGRGRUPPEID                 PIC X(13).
           20 EA-FGRBESCHREIBUNG             PIC X(50).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
