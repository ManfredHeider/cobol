      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0098.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0098.CPv  $
      *
      *   Rev 2.3   Dec 03 2003 09:30:58   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *   Rev 2.2   Oct 23 2003 16:22:22   t13ex27
      *AKI00373 - DB-Änderung für Fixpack 51
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE NEUKUNDE                                          *
      ******************************************************************
CCC005*    EXEC SQL DECLARE NEUKUNDE TABLE
CCC004*    ( NKDPERSONENID                  CHAR(13) NOT NULL,
CCC004*      NKDLFDNR                       SMALLINT NOT NULL,
CCC004*      NKDKUNDENNUMMER                INTEGER NOT NULL,
CCC004*      NKDVBNR                        INTEGER NOT NULL,
CCC004*      NKDDIREKTION                   SMALLINT NOT NULL,
CCC004*      NKDPARTGESELLSCHID             SMALLINT NOT NULL,
CCC004*      NKDVERTRAGSNUMMER              CHAR(15) NOT NULL,
CCC004*      NKDTAGESDATUM                  DATE NOT NULL,
CCC004*      NKDKNDSTATUSIDALT              CHAR(1) NOT NULL,
CCC004*      NKDKNDSTATUSIDNEU              CHAR(1) NOT NULL,
CCC004*      NKDBWTUNGSKZDVAG               CHAR(2) NOT NULL,
CCC004*      NKDBWTUNGSKZVB                 CHAR(2),
CCC004*      NKDKOMMISARISCHKZ              CHAR(1),
CCC004*      NKDPGMNAME                     CHAR(8) NOT NULL,
CCC004*      NKDPRODUKT                     CHAR(10),
CCC004*      NKDEREIGNISKZ                  SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      NKDKUZUPERSONENID              CHAR(13)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE NEUKUNDE                           *
      ******************************************************************
       01  NEUKUNDE-GESAMT.
        05 NI-NEUKUNDE-TAB.
221003*    10 NI-NEUKUNDE-ELE    PIC S9(4)  COMP OCCURS 17 TIMES.
221003     10 NI-NEUKUNDE-ELE    PIC S9(4)  COMP OCCURS 18 TIMES.
        05 NI-NEUKUNDE-TAB REDEFINES NI-NEUKUNDE-TAB.
           10 NI-NKDPERSONENID      PIC S9(4)  COMP.
           10 NI-NKDLFDNR           PIC S9(4)  COMP.
           10 NI-NKDKUNDENNUMMER    PIC S9(4)  COMP.
           10 NI-NKDVBNR            PIC S9(4)  COMP.
           10 NI-NKDDIREKTION       PIC S9(4)  COMP.
           10 NI-NKDPARTGESELLSCHID PIC S9(4)  COMP.
           10 NI-NKDVERTRAGSNUMMER  PIC S9(4)  COMP.
           10 NI-NKDTAGESDATUM      PIC S9(4)  COMP.
           10 NI-NKDKNDSTATUSIDALT  PIC S9(4)  COMP.
           10 NI-NKDKNDSTATUSIDNEU  PIC S9(4)  COMP.
           10 NI-NKDBWTUNGSKZDVAG   PIC S9(4)  COMP.
           10 NI-NKDBWTUNGSKZVB     PIC S9(4)  COMP.
           10 NI-NKDKOMMISARISCHKZ  PIC S9(4)  COMP.
           10 NI-NKDPGMNAME         PIC S9(4)  COMP.
           10 NI-NKDPRODUKT         PIC S9(4)  COMP.
           10 NI-NKDEREIGNISKZ      PIC S9(4)  COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4)  COMP.
221003     10 NI-NKDKUZUPERSONENID  PIC S9(4)  COMP.
        05 DCL-NEUKUNDE.
           10 NKDPERSONENID        PIC X(13).
           10 NKDLFDNR             PIC S9(4)  COMP.
           10 NKDKUNDENNUMMER      PIC S9(9)  COMP.
           10 NKDVBNR              PIC S9(9)  COMP.
           10 NKDDIREKTION         PIC S9(4)  COMP.
           10 NKDPARTGESELLSCHID   PIC S9(4)  COMP.
           10 NKDVERTRAGSNUMMER    PIC X(15).
           10 NKDTAGESDATUM        PIC X(10).
           10 NKDKNDSTATUSIDALT    PIC X(1).
           10 NKDKNDSTATUSIDNEU    PIC X(1).
           10 NKDBWTUNGSKZDVAG     PIC X(2).
           10 NKDBWTUNGSKZVB       PIC X(2).
           10 NKDKOMMISARISCHKZ    PIC X(1).
           10 NKDPGMNAME           PIC X(8).
           10 NKDPRODUKT           PIC X(10).
           10 NKDEREIGNISKZ        PIC S9(4)  COMP.
           10 LETZTEAENDERUNG      PIC X(26).
221003     10 NKDKUZUPERSONENID    PIC X(13).
      ******************************************************************
