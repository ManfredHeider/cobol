      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WDKI0051.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WDKI0051.CPv  $
      *
      *   Rev 2.1   Apr 16 1999 10:06:30   I13PC19
      *AKI00027
      *Produktionsübergabe
      *
      *   Rev 2.0   Apr 15 1999 13:32:14   P13SP99
      *Neuerstellung
      *
      *---------------------------------------------------------------*
        05 EA-SPARBETRAG-GESAMT.
         10 EA-NI-SPARBETRAG-TAB.
           20 EA-NI-SPARBETRAG-ELE PIC X
                  OCCURS 10 TIMES.
         10 EA-NI-SPARBETRAG
                 REDEFINES EA-NI-SPARBETRAG-TAB.
           20 EA-NI-SPBETRPERSONENID         PIC X.
           20 EA-NI-SPBETRPERSROLLE          PIC X.
           20 EA-NI-SPBETRMONATLICH          PIC X.
           20 EA-NI-SPBETRMONATLICHWKZ       PIC X.
           20 EA-NI-SPBETRERHOEHBARUM        PIC X.
           20 EA-NI-SPBETRERHBUMWKZ          PIC X.
           20 EA-NI-SPBETRERHBARUMABJ        PIC X.
           20 EA-NI-SPBETRZUSGELBETRAG       PIC X.
           20 EA-NI-SPBETRZUSGBETRWKZ        PIC X.
           20 EA-NI-LETZTEAENDERUNG          PIC X.
         10 EA-DCL-SPARBETRAG.
           20 EA-SPBETRPERSONENID            PIC X(13).
           20 EA-SPBETRPERSROLLE             PIC X(01).
           20 EA-SPBETRMONATLICH             PIC ---------9.99    .
           20 EA-SPBETRMONATLICHWKZ          PIC X(01).
           20 EA-SPBETRERHOEHBARUM           PIC ---------9.99    .
           20 EA-SPBETRERHBUMWKZ             PIC X(01).
           20 EA-SPBETRERHBARUMABJ           PIC X(10).
           20 EA-SPBETRZUSGELBETRAG          PIC ---------9.99    .
           20 EA-SPBETRZUSGBETRWKZ           PIC X(01).
           20 EA-LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
