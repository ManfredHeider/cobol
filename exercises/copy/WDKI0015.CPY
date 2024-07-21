      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0015.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0015.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:04:20   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:02   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-DATENFELDDEF-GESAMT.
         10 EA-NI-DATENFELDDEF-TAB.
           20 EA-NI-DATENFELDDEF-ELE PIC X
                  OCCURS 5 TIMES.
         10 EA-NI-DATENFELDDEF
                 REDEFINES EA-NI-DATENFELDDEF-TAB.
           20 EA-NI-DATFDEFVBID              PIC X.
           20 EA-NI-DATFDEFVBROLLE           PIC X.
           20 EA-NI-DATFDEFLFDNUMMER         PIC X.
           20 EA-NI-DATFDEFBEZEICHNUNG       PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-DATENFELDDEF.
           20 EA-DATFDEFVBID                 PIC X(13).
           20 EA-DATFDEFVBROLLE              PIC X(01).
           20 EA-DATFDEFLFDNUMMER            PIC --------9 .
           20 EA-DATFDEFBEZEICHNUNG          PIC X(15).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
