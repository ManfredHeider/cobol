      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTDB0001.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTDB0001.CPv  $
      *
      *   Rev 2.2   Dec 02 2015 09:19:24   p13sp14
      *ADB00040: DB2 COBOL Dyn-SQL PGM: Comment Support 
      *
      *   Rev 2.1   18 Sep 2006 17:04:00   p13sp14
      *ADB00040: DB2 COBOL Unload-Programm für CSV Format
      *
      *   Rev 2.0   18 Sep 2006 16:51:34   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(SYSIBM.SYSPARMS)                                  *
      *        LIBRARY(P13SP54.USERTEST.SRCDCL(CTDB0001))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE SYSPARMS TABLE
CCC004*    ( SCHEMA                         VARCHAR(128) NOT NULL,
CCC004*      OWNER                          VARCHAR(128) NOT NULL,
CCC004*      NAME                           VARCHAR(128) NOT NULL,
CCC004*      SPECIFICNAME                   VARCHAR(128) NOT NULL,
CCC004*      ROUTINETYPE                    CHAR(1) NOT NULL,
CCC004*      CAST_FUNCTION                  CHAR(1) NOT NULL,
CCC004*      PARMNAME                       VARCHAR(128) NOT NULL,
CCC004*      ROUTINEID                      INTEGER NOT NULL,
CCC004*      ROWTYPE                        CHAR(1) NOT NULL,
CCC004*      ORDINAL                        SMALLINT NOT NULL,
CCC004*      TYPESCHEMA                     VARCHAR(128) NOT NULL,
CCC004*      TYPENAME                       VARCHAR(128) NOT NULL,
CCC004*      DATATYPEID                     INTEGER NOT NULL,
CCC004*      SOURCETYPEID                   INTEGER NOT NULL,
CCC004*      LOCATOR                        CHAR(1) NOT NULL,
CCC004*      TABLE                          CHAR(1) NOT NULL,
CCC004*      TABLE_COLNO                    SMALLINT NOT NULL,
CCC004*      LENGTH                         INTEGER NOT NULL,
CCC004*      SCALE                          SMALLINT NOT NULL,
CCC004*      SUBTYPE                        CHAR(1) NOT NULL,
CCC004*      CCSID                          INTEGER NOT NULL,
CCC004*      CAST_FUNCTION_ID               INTEGER NOT NULL,
CCC004*      ENCODING_SCHEME                CHAR(1) NOT NULL,
CCC004*      IBMREQD                        CHAR(1) NOT NULL,
CCC004*      VERSION                        VARCHAR(122) NOT NULL,
CCC004*      OWNERTYPE                      CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SYSIBM.SYSPARMS                    *
      ******************************************************************
       01  DCLSYSPARMS.
      *    *************************************************************
           10 SCHEMA.
              49 SCHEMA-LEN        PIC S9(4) USAGE COMP.
              49 SCHEMA-TEXT       PIC X(128).
      *    *************************************************************
           10 OWNER.
              49 OWNER-LEN         PIC S9(4) USAGE COMP.
              49 OWNER-TEXT        PIC X(128).
      *    *************************************************************
           10 NAME.
              49 NAME-LEN          PIC S9(4) USAGE COMP.
              49 NAME-TEXT         PIC X(128).
      *    *************************************************************
           10 SPECIFICNAME.
              49 SPECIFICNAME-LEN
                 PIC S9(4) USAGE COMP.
              49 SPECIFICNAME-TEXT
                 PIC X(128).
      *    *************************************************************
           10 ROUTINETYPE          PIC X(1).
      *    *************************************************************
           10 CAST-FUNCTION        PIC X(1).
      *    *************************************************************
           10 PARMNAME.
              49 PARMNAME-LEN      PIC S9(4) USAGE COMP.
              49 PARMNAME-TEXT     PIC X(128).
      *    *************************************************************
           10 ROUTINEID            PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 ROWTYPE              PIC X(1).
      *    *************************************************************
           10 ORDINAL              PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 TYPESCHEMA.
              49 TYPESCHEMA-LEN    PIC S9(4) USAGE COMP.
              49 TYPESCHEMA-TEXT   PIC X(128).
      *    *************************************************************
           10 TYPENAME.
              49 TYPENAME-LEN      PIC S9(4) USAGE COMP.
              49 TYPENAME-TEXT     PIC X(128).
      *    *************************************************************
           10 DATATYPEID           PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 SOURCETYPEID         PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 LOCATOR              PIC X(1).
      *    *************************************************************
           10 XTABLE               PIC X(1).
      *    *************************************************************
           10 TABLE-COLNO          PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 XLENGTH              PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 SCALE                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 SUBTYPE              PIC X(1).
      *    *************************************************************
           10 CCSID                PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 CAST-FUNCTION-ID     PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 ENCODING-SCHEME      PIC X(1).
      *    *************************************************************
           10 IBMREQD              PIC X(1).
      *    *************************************************************
           10 VERSION.
              49 VERSION-LEN       PIC S9(4) USAGE COMP.
              49 VERSION-TEXT      PIC X(122).
      *    *************************************************************
           10 OWNERTYPE            PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 26      *
      ******************************************************************

