      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0301.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0301.CPv  $
      *
      *   Rev 2.1   Aug 20 2018 08:06:12   t13ae31
      *AKI01619: SUH KSDS VSAM in DB2 BMG OZP
      *
      *   Rev 1.1   09 May 1998 13:28:22   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:56:14   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE G10KSDSSUHBMG TABLE
CCC004*    ( G10SUHBMGVNR                   CHAR(15) NOT NULL,
CCC004*      G10SUHBMGKZ                    CHAR(1) NOT NULL,
CCC004*      G10SUHBMGTEXT                  CHAR(30) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G10KSDSSUHBMG               *
      ******************************************************************
       01  G10KSDSSUHBMG-GESAMT.
        05 NI-G10KSDSSUHBMG-TAB.
           10 NI-G10KSDSSUHBMG-ELE PIC S9(04) COMP OCCURS 3 TIMES.
        05 NI-G10KSDSSUHBMG REDEFINES NI-G10KSDSSUHBMG-TAB.
           10 NI-G10SUHBMGVNR       PIC S9(4) COMP.
           10 NI-G10SUHBMGKZ        PIC S9(4) COMP.
           10 NI-G10SUHBMGTEXT      PIC S9(4) COMP.
        05 DCL-G10KSDSSUHBMG.
           10 G10SUHBMGVNR         PIC X(15).
           10 G10SUHBMGKZ          PIC X(1).
           10 G10SUHBMGTEXT        PIC X(30).
      ************************** ENDE-DCLGEN *************************

