      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0024.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTAT0024.CPv  $
      *
      *   Rev 2.2   24 Jan 2007 13:32:52   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   Feb 27 1999 11:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   06 Feb 1998 10:53:36   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(SYSIBM.SYSCOLUMNS)                                *
      *        LIBRARY(T130005.KI010000.COBCOPY(PPOSCOLU))             *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      *        INDVAR(YES)                                             *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE SYSIBM.SYSCOLUMNS TABLE
CCC004*    ( NAME                           VARCHAR(18) NOT NULL,
CCC004*      TBNAME                         VARCHAR(18) NOT NULL,
CCC004*      TBCREATOR                      CHAR(8) NOT NULL,
CCC004*      COLNO                          SMALLINT NOT NULL,
CCC004*      COLTYPE                        CHAR(8) NOT NULL,
CCC004*      LENGTH                         SMALLINT NOT NULL,
CCC004*      SCALE                          SMALLINT NOT NULL,
CCC004*      NULLS                          CHAR(1) NOT NULL,
CCC004*      COLCARD                        INTEGER NOT NULL,
CCC004*      HIGH2KEY                       CHAR(8) NOT NULL,
CCC004*      LOW2KEY                        CHAR(8) NOT NULL,
CCC004*      UPDATES                        CHAR(1) NOT NULL,
CCC004*      IBMREQD                        CHAR(1) NOT NULL,
CCC004*      REMARKS                        VARCHAR(254) NOT NULL,
CCC004*      DEFAULT                        CHAR(1) NOT NULL,
CCC004*      KEYSEQ                         SMALLINT NOT NULL,
CCC004*      FOREIGNKEY                     CHAR(1) NOT NULL,
CCC004*      FLDPROC                        CHAR(1) NOT NULL,
CCC004*      LABEL                          VARCHAR(30) NOT NULL,
CCC004*      STATSTIME                      TIMESTAMP NOT NULL,
CCC004*      DEFAULTVALUE                   VARCHAR(512) NOT NULL,
CCC004*      COLCARDF                       FLOAT NOT NULL,
CCC004*      COLSTATUS                      CHAR(1) NOT NULL,
CCC004*      LENGTH2                        INTEGER NOT NULL,
CCC004*      DATATYPEID                     INTEGER NOT NULL,
CCC004*      SOURCETYPEID                   INTEGER NOT NULL,
CCC004*      TYPESCHEMA                     CHAR(8) NOT NULL,
CCC004*      TYPENAME                       VARCHAR(18) NOT NULL,
CCC004*      CREATEDTS                      TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SYSIBM.SYSCOLUMNS                  *
      ******************************************************************
       01  DCLSYSCOLUMNS.
           10 NAME.
              49 NAME-LEN          PIC S9(4) USAGE COMP.
              49 NAME-TEXT         PIC X(18).
           10 TBNAME.
              49 TBNAME-LEN        PIC S9(4) USAGE COMP.
              49 TBNAME-TEXT       PIC X(18).
           10 TBCREATOR            PIC X(8).
           10 COLNO                PIC S9(4) USAGE COMP.
           10 COLTYPE              PIC X(8).
           10 SYS-LENGTH           PIC S9(4) USAGE COMP.
           10 SCALE                PIC S9(4) USAGE COMP.
           10 SYS-NULLS            PIC X(1).
           10 COLCARD              PIC S9(9) USAGE COMP.
           10 HIGH2KEY             PIC X(8).
           10 LOW2KEY              PIC X(8).
           10 UPDATES              PIC X(1).
           10 IBMREQD              PIC X(1).
           10 REMARKS.
              49 REMARKS-LEN       PIC S9(4) USAGE COMP.
              49 REMARKS-TEXT      PIC X(254).
           10 DEFAULT              PIC X(1).
           10 KEYSEQ               PIC S9(4) USAGE COMP.
           10 FOREIGNKEY           PIC X(1).
           10 FLDPROC              PIC X(1).
           10 SYS-LABEL.
              49 LABEL-LEN         PIC S9(4) USAGE COMP.
              49 LABEL-TEXT        PIC X(30).
           10 STATSTIME            PIC X(26).
           10 DEFAULTVALUE.
              49 DEFAULTVALUE-LEN  PIC S9(4) USAGE COMP.
              49 DEFAULTVALUE-TEXT  PIC X(512).
           10 COLCARDF             USAGE COMP-2.
           10 COLSTATUS            PIC X(1).
           10 LENGTH2              PIC S9(9) USAGE COMP.
           10 DATATYPEID           PIC S9(9) USAGE COMP.
           10 SOURCETYPEID         PIC S9(9) USAGE COMP.
           10 TYPESCHEMA           PIC X(8).
           10 TYPENAME.
              49 TYPENAME-LEN      PIC S9(4) USAGE COMP.
              49 TYPENAME-TEXT     PIC X(18).
           10 CREATEDTS            PIC X(26).
      ******************************************************************
      * INDICATOR VARIABLE STRUCTURE                                   *
      ******************************************************************
       01  ISYSCOLUMNS.
           10 INDSTRUC           PIC S9(4) USAGE COMP OCCURS 29 TIMES.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 29      *
      ******************************************************************
