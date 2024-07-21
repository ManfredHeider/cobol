      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0393.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CTKI0393.CPv  $
      *
      *   Rev 2.1   Oct 15 2002 15:35:44   t13ex27
      *AKI00308 DB-Änderung für Fixpack 27
      *
      *   Rev 2.0   15 May 2001 13:47:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE T13KIS                                            *
      ******************************************************************
CCC005*    EXEC SQL DECLARE MISCHFOND TABLE
CCC004*    ( MISCHFONDID                    SMALLINT NOT NULL,
CCC004*      MISCHAKTIEN                    DECIMAL(3, 2) NOT NULL,
CCC004*      MISCHRENTEN                    DECIMAL(3, 2) NOT NULL,
CCC004*      MISCHIMMO                      DECIMAL(3, 2) NOT NULL,
CCC004*      MISCHGELD                      DECIMAL(3, 2) NOT NULL,
CCC004*      MISCHSONST                     DECIMAL(3, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE MISCHFOND                          *
      ******************************************************************
       01  MISCHFOND-GESAMT.
        05 NI-MISCHFOND-TAB.
           10 NI-MISCHFOND-ELE        PIC S9(4) COMP OCCURS 6 TIMES.
        05 NI-MISCHFOND               REDEFINES NI-MISCHFOND-TAB.
           10 NI-MISCHFONDID       PIC S9(4) USAGE COMP.
           10 NI-MISCHAKTIEN       PIC S9(4) USAGE COMP.
           10 NI-MISCHRENTEN       PIC S9(4) USAGE COMP.
           10 NI-MISCHIMMO         PIC S9(4) USAGE COMP.
           10 NI-MISCHGELD         PIC S9(4) USAGE COMP.
           10 NI-MISCHSONST        PIC S9(4) USAGE COMP.
        05 DCL-MISCHFOND.
           10 MISCHFONDID          PIC S9(4) USAGE COMP.
           10 MISCHAKTIEN          PIC S9(1)V9(2) USAGE COMP-3.
           10 MISCHRENTEN          PIC S9(1)V9(2) USAGE COMP-3.
           10 MISCHIMMO            PIC S9(1)V9(2) USAGE COMP-3.
           10 MISCHGELD            PIC S9(1)V9(2) USAGE COMP-3.
           10 MISCHSONST           PIC S9(1)V9(2) USAGE COMP-3.
