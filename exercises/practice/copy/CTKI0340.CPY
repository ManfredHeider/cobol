      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0340.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0340.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:28   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:58:48   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERBTILGUNG  TABLE
CCC004*    ( VTLVERTRAGSID CHAR (13) NOT NULL,
CCC004*      VTLGUTHABPRODLFDNR SMALLINT NOT NULL,
CCC004*      VTLVERTRTEILLFDNR SMALLINT NOT NULL,
CCC004*      VTLLFDNR SMALLINT NOT NULL,
CCC004*      VTLTSONDERTILGUNG CHAR NOT NULL,
CCC004*      VTLSONDERTBEZ CHAR(20),
CCC004*      VTLZEITPUNKT CHAR(6),
CCC004*      VTLBETRAG DECIMAL (11, 2),
CCC004*      VTLBETRAGDWKZ CHAR NOT NULL,
CCC004*      VTLTILGUNGDURCH CHAR NOT NULL,
CCC004*      LETZTEAENDERUNG TIMESTAMP
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERBTILGUNG-GESAMT.
        05  NI-VERBTILGUNG-TAB.
           10 NI-VERBTILGUNG-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05  NI-VERBTILGUNG  REDEFINES  NI-VERBTILGUNG-TAB.
           10 NI-VTLVERTRAGSID            PIC S9(4) COMP.
           10 NI-VTLGUTHABPRODLFDNR       PIC S9(4) COMP.
           10 NI-VTLVERTRTEILLFDNR        PIC S9(4) COMP.
           10 NI-VTLLFDNR                 PIC S9(4) COMP.
           10 NI-VTLTSONDERTILGUNG        PIC S9(4) COMP.
           10 NI-VTLSONDERTBEZ            PIC S9(4) COMP.
           10 NI-VTLZEITPUNKT             PIC S9(4) COMP.
           10 NI-VTLBETRAG                PIC S9(4) COMP.
           10 NI-VTLBETRAGDWKZ            PIC S9(4) COMP.
           10 NI-VTLTILGUNGDURCH          PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
        05  DCL-VERBTILGUNG.
           10 VTLVERTRAGSID               PIC X(13).
           10 VTLGUTHABPRODLFDNR          PIC S9(4) COMP.
           10 VTLVERTRTEILLFDNR           PIC S9(4) COMP.
           10 VTLLFDNR                    PIC S9(4) COMP.
           10 VTLTSONDERTILGUNG           PIC X(01).
           10 VTLSONDERTBEZ               PIC X(20).
           10 VTLZEITPUNKT                PIC X(6).
           10 VTLBETRAG                   PIC S9(9)V9(2) COMP-3.
           10 VTLBETRAGDWKZ               PIC X(01).
           10 VTLTILGUNGDURCH             PIC X(01).
           10 LETZTEAENDERUNG             PIC X(26).
      ******************************************************************
