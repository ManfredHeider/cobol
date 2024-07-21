      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0323.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTKI0323.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:50:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:00   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:57:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  KFZMOPEDVERS  TABLE
CCC004*    ( KFMVERTRAGSID CHAR (13) NOT NULL,
CCC004*      KFMVERSTARLFDNR SMALLINT NOT NULL,
CCC004*      KFMBEITRSATZHAFTPF DECIMAL (5, 2),
CCC004*      KFMDECKSCHLHPFLID CHAR (2) NOT NULL,
CCC004*      KFMDECKUMFANG VARCHAR (30),
CCC004*      KFMHAFTPFLBEITRAG DECIMAL (11, 2),
CCC004*      KFMHPFBEITRAGWKZ CHAR NOT NULL,
CCC004*      KFMHAFTPFLREGIOKL CHAR (3),
CCC004*      KFMHAFTPFLSCHADFID SMALLINT,
CCC004*      KFMTARBEZEICHNUNG CHAR(15)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  KFZMOPEDVERS-GESAMT.
        05  NI-KFZMOPEDVERS-TAB.
           10 NI-KFZMOPEDVERS-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05  NI-KFZMOPEDVERS  REDEFINES  NI-KFZMOPEDVERS-TAB.
           10 NI-KFMVERTRAGSID            PIC S9(4) COMP.
           10 NI-KFMVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-KFMBEITRSATZHAFTPF       PIC S9(4) COMP.
           10 NI-KFMDECKSCHLHPFLID        PIC S9(4) COMP.
           10 NI-KFMDECKUMFANG            PIC S9(4) COMP.
           10 NI-KFMHAFTPFLBEITRAG        PIC S9(4) COMP.
           10 NI-KFMHPFBEITRAGWKZ         PIC S9(4) COMP.
           10 NI-KFMHAFTPFLREGIOKL        PIC S9(4) COMP.
           10 NI-KFMHAFTPFLSCHADFID       PIC S9(4) COMP.
           10 NI-KFMTARBEZEICHNUNG        PIC S9(4) COMP.
        05  DCL-KFZMOPEDVERS.
           10 KFMVERTRAGSID               PIC X(13).
           10 KFMVERSTARLFDNR             PIC S9(4) COMP.
           10 KFMBEITRSATZHAFTPF          PIC S9(3)V9(2) COMP-3.
           10 KFMDECKSCHLHPFLID           PIC X(2).
           10 KFMDECKUMFANG.
              49 KFMDECKUMFANG-LEN        PIC S9(4) COMP.
              49 KFMDECKUMFANG-TEXT       PIC X(30).
           10 KFMHAFTPFLBEITRAG           PIC S9(9)V9(2) COMP-3.
           10 KFMHPFBEITRAGWKZ            PIC X(01).
           10 KFMHAFTPFLREGIOKL           PIC X(3).
           10 KFMHAFTPFLSCHADFID          PIC S9(4) COMP.
           10 KFMTARBEZEICHNUNG           PIC X(15).
      ******************************************************************
