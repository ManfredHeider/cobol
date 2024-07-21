      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0034.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTAT0034.CPv  $
      *
      *   Rev 2.3   Aug 23 2019 10:27:52   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.2   Jul 07 2015 11:14:12   t13ex27
      *AAT00049: Fehlerverfahren mit Prozentkriterien-OZPs
      *
      *   Rev 2.1   02 May 2001 08:56:20   t13ex49
      *AKI00161 - erstmalige Prod.-Übergabe
      *
      *   Rev 2.0   Mar 18 1999 10:44:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00161!19.04.01!    ! Neuerstellung             !P.Pohl    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *****************************************************************
CCC005*    EXEC SQL DECLARE TBAT0034 TABLE
CCC004*    ( FAB_SYSCODE                    CHAR(2) NOT NULL,
CCC004*      FAB_JOBNAME                    CHAR(8) NOT NULL,
CCC004*      FAB_PROGRAMMNAME               CHAR(8) NOT NULL,
CCC004*      FAB_RELEVANZ                   CHAR(1) NOT NULL,
CCC004*      FAB_ANZAHL                     SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      FAB_USERID                     CHAR(7) NOT NULL,
CCC004*      FAB_PROZENT                    DECIMAL(4, 2) NOT NULL,
CCC004*      FAB_GRENZE                     INTEGER,
CCC004*      FAB_FAKTOR                     SMALLINT
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0034                    *
      ******************************************************************
       01  TBAT0034-GESAMT.
        05 NI-TBAT0034-TAB.
           10 NI-TBAT0034-ELE PIC S9(04) COMP OCCURS 10 TIMES.
        05 NI-TBAT0034 REDEFINES NI-TBAT0034-TAB.
           10 NI-FAB-SYSCODE        PIC S9(4) COMP.
           10 NI-FAB-JOBNAME        PIC S9(4) COMP.
           10 NI-FAB-PROGRAMMNAME   PIC S9(4) COMP.
           10 NI-FAB-RELEVANZ       PIC S9(4) COMP.
           10 NI-FAB-ANZAHL         PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-FAB-USERID         PIC S9(4) COMP.
           10 NI-FAB-PROZENT        PIC S9(4) COMP.
           10 NI-FAB-GRENZE         PIC S9(4) COMP.
           10 NI-FAB-FAKTOR         PIC S9(4) COMP.
        05 DCL-TBAT0034.
           10 FAB-SYSCODE          PIC X(2).
           10 FAB-JOBNAME          PIC X(8).
           10 FAB-PROGRAMMNAME     PIC X(8).
           10 FAB-RELEVANZ         PIC X(1).
           10 FAB-ANZAHL           PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 FAB-USERID           PIC X(7).
           10 FAB-PROZENT          PIC S9(2)V9(2) USAGE COMP-3.
           10 FAB-GRENZE           PIC S9(9) USAGE COMP.
           10 FAB-FAKTOR           PIC S9(4) USAGE COMP.
      ************************** ENDE-DCLGEN *************************

