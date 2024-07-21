      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0370.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:11:12   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:52   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-WOHNBAUPRAEMIEJAHR-GESAMT.
         10 EA-NI-WOHNBAUPRAEMIEJAHR-TAB.
           20 EA-NI-WOHNBAUPRAEMIEJAHR-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-WOHNBAUPRAEMIEJAHR
                 REDEFINES EA-NI-WOHNBAUPRAEMIEJAHR-TAB.
           20 EA-NI-WBPVERTRAGSID            PIC X.
           20 EA-NI-WBPGUTHPRODLFDNR         PIC X.
           20 EA-NI-WBPLAUFENDENUMMER        PIC X.
           20 EA-NI-WBPJAHR                  PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-WOHNBAUPRAEMIEJAHR.
           20 EA-WBPVERTRAGSID               PIC X(13).
           20 EA-WBPGUTHPRODLFDNR            PIC -----9    .
           20 EA-WBPLAUFENDENUMMER           PIC -----9    .
           20 EA-WBPJAHR                     PIC X(4).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
