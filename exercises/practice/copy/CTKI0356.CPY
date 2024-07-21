      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0356.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0356.CPv  $
      *
      *   Rev 2.2   Sep 26 2005 13:54:26   t13ex27
      *AKI00521: DB-Änderung für Fix 80 CTKI's
      *
      *   Rev 2.1   Apr 10 2000 14:24:02   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:56   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VUMUMFANGID IST           !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSUMFANGBEZ  TABLE
CCC004*    ( VUMVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VUMVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      VUMUMFANGID                    INTEGER NOT NULL,
CCC004*      VUMUMFANGLFDNR                 SMALLINT NOT NULL,
CCC004*      VUMUMFANGTEXT                  VARCHAR(20) NOT NULL,
CCC004*      VUMUMFANGPROZ                  DECIMAL(5, 2),
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VUMUMFEGR                      INTEGER,
CCC004*      VUMEGREH                       CHAR(1)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSUMFANGBEZ-GESAMT.
        05  NI-VERSUMFANGBEZ-TAB.
           10 NI-VERSUMFANGBEZ-ELE PIC S9(04) COMP OCCURS 9 TIMES.
        05  NI-VERSUMFANGBEZ  REDEFINES  NI-VERSUMFANGBEZ-TAB.
           10 NI-VUMVERTRAGSID            PIC S9(4) COMP.
           10 NI-VUMVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-VUMUMFANGID              PIC S9(4) COMP.
           10 NI-VUMUMFANGLFDNR           PIC S9(4) COMP.
           10 NI-VUMUMFANGTEXT            PIC S9(4) COMP.
           10 NI-VUMUMFANGPROZ            PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG          PIC S9(4) COMP.
           10 NI-VUMUMFEGR                PIC S9(4) COMP.
           10 NI-VUMEGREH                 PIC S9(4) COMP.
        05  DCL-VERSUMFANGBEZ.
           10 VUMVERTRAGSID               PIC X(13).
           10 VUMVERSTARLFDNR             PIC S9(4) COMP.
           10 VUMUMFANGID                 PIC S9(9) COMP.
           10 VUMUMFANGLFDNR              PIC S9(4) COMP.
           10 VUMUMFANGTEXT.
              49 VUMUMFANGTEXT-LEN        PIC S9(4) COMP.
              49 VUMUMFANGTEXT-TEXT       PIC X(20).
           10 VUMUMFANGPROZ               PIC S9(3)V9(2) COMP-3.
           10 LETZTEAENDERUNG             PIC X(26).
           10 VUMUMFEGR                   PIC S9(9) COMP.
           10 VUMEGREH                    PIC X(1).
      ******************************************************************
