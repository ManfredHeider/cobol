      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0348.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0348.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:10:40   I13PC19
      * 
      *
      *   Rev 2.0   Apr 15 1999 13:32:42   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-VERSICHERTERTARIF-GESAMT.
         10 EA-NI-VERSICHERTERTARIF-TAB.
           20 EA-NI-VERSICHERTERTARIF-ELE PIC X
                  OCCURS 7 TIMES.
         10 EA-NI-VERSICHERTERTARIF
                 REDEFINES EA-NI-VERSICHERTERTARIF-TAB.
           20 EA-NI-VETVERTRAGSID            PIC X.
           20 EA-NI-VETVERSTARLFDNR          PIC X.
           20 EA-NI-VETPRODUKTID             PIC X.
           20 EA-NI-VETVERSSUMME             PIC X.
           20 EA-NI-VETVERSSUMMEWKZ          PIC X.
           20 EA-NI-VETSUBTYPE               PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-VERSICHERTERTARIF.
           20 EA-VETVERTRAGSID               PIC X(13).
           20 EA-VETVERSTARLFDNR             PIC -----9    .
           20 EA-VETPRODUKTID                PIC -----9    .
           20 EA-VETVERSSUMME                PIC ---------9.99    .
           20 EA-VETVERSSUMMEWKZ             PIC X(01).
           20 EA-VETSUBTYPE                  PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
