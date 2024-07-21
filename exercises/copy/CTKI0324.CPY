      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0324.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0324.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:02   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:46   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZUNFALL  TABLE
CCC004*    ( KFUVERTRAGSID CHAR (13) NOT NULL,
CCC004*      KFUVERSTARLFDNR SMALLINT NOT NULL,
CCC004*      KFUUNFALLART CHAR NOT NULL,
CCC004*      KFUBEITRAG DECIMAL (11, 2),
CCC004*      KFUBEITRAGWKZ CHAR NOT NULL,
CCC004*      KFUINVALIDSUMME DECIMAL (11, 2),
CCC004*      KFUINVSUMMEWKZ CHAR NOT NULL,
CCC004*      KFUPROGRESSINVPROZ DECIMAL (5, 2),
CCC004*      KFUTODSUMME DECIMAL (11, 2),
CCC004*      KFUTODSUMMEWKZ CHAR NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZUNFALL-GESAMT.
        05  NI-KFZUNFALL-TAB.
           10 NI-KFZUNFALL-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-KFZUNFALL  REDEFINES  NI-KFZUNFALL-TAB.
           10 NI-KFUVERTRAGSID            PIC S9(4) COMP.
           10 NI-KFUVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-KFUUNFALLART             PIC S9(4) COMP.
           10 NI-KFUBEITRAG               PIC S9(4) COMP.
           10 NI-KFUBEITRAGWKZ            PIC S9(4) COMP.
           10 NI-KFUINVALIDSUMME          PIC S9(4) COMP.
           10 NI-KFUINVSUMMEWKZ           PIC S9(4) COMP.
           10 NI-KFUPROGRESSINVPROZ       PIC S9(4) COMP.
           10 NI-KFUTODSUMME              PIC S9(4) COMP.
           10 NI-KFUTODSUMMEWKZ           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-KFZUNFALL.
           10 KFUVERTRAGSID               PIC X(13).
           10 KFUVERSTARLFDNR             PIC S9(4) COMP.
           10 KFUUNFALLART                PIC X(01).
           10 KFUBEITRAG                  PIC S9(9)V9(2) COMP-3.
           10 KFUBEITRAGWKZ               PIC X(01).
           10 KFUINVALIDSUMME             PIC S9(9)V9(2) COMP-3.
           10 KFUINVSUMMEWKZ              PIC X(01).
           10 KFUPROGRESSINVPROZ          PIC S9(3)V9(2) COMP-3.
           10 KFUTODSUMME                 PIC S9(9)V9(2) COMP-3.
           10 KFUTODSUMMEWKZ              PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
