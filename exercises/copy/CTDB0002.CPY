      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTDB0002.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTDB0002.CPv  $
      *
      *   Rev 2.2   Dec 02 2015 09:19:30   p13sp14
      *ADB00040: DB2 COBOL Dyn-SQL PGM: Comment Support 
      *
      *   Rev 2.1   18 Sep 2006 17:04:12   p13sp14
      *ADB00040: DB2 COBOL Unload-Programm für CSV Format
      *
      *   Rev 2.0   18 Sep 2006 16:51:46   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(SYSIBM.SYSROUTINES)                               *
      *        LIBRARY(P13SP54.USERTEST.SRCDCL(CTDB0002))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE SYSROUTINES TABLE
CCC004*    ( SCHEMA                         VARCHAR(128) NOT NULL,
CCC004*      OWNER                          VARCHAR(128) NOT NULL,
CCC004*      NAME                           VARCHAR(128) NOT NULL,
CCC004*      ROUTINETYPE                    CHAR(1) NOT NULL,
CCC004*      CREATEDBY                      VARCHAR(128) NOT NULL,
CCC004*      SPECIFICNAME                   VARCHAR(128) NOT NULL,
CCC004*      ROUTINEID                      INTEGER NOT NULL,
CCC004*      RETURN_TYPE                    INTEGER NOT NULL,
CCC004*      ORIGIN                         CHAR(1) NOT NULL,
CCC004*      FUNCTION_TYPE                  CHAR(1) NOT NULL,
CCC004*      PARM_COUNT                     SMALLINT NOT NULL,
CCC004*      LANGUAGE                       VARCHAR(24) NOT NULL,
CCC004*      COLLID                         VARCHAR(128) NOT NULL,
CCC004*      SOURCESCHEMA                   VARCHAR(128) NOT NULL,
CCC004*      SOURCESPECIFIC                 VARCHAR(128) NOT NULL,
CCC004*      DETERMINISTIC                  CHAR(1) NOT NULL,
CCC004*      EXTERNAL_ACTION                CHAR(1) NOT NULL,
CCC004*      NULL_CALL                      CHAR(1) NOT NULL,
CCC004*      CAST_FUNCTION                  CHAR(1) NOT NULL,
CCC004*      SCRATCHPAD                     CHAR(1) NOT NULL,
CCC004*      SCRATCHPAD_LENGTH              INTEGER NOT NULL,
CCC004*      FINAL_CALL                     CHAR(1) NOT NULL,
CCC004*      PARALLEL                       CHAR(1) NOT NULL,
CCC004*      PARAMETER_STYLE                CHAR(1) NOT NULL,
CCC004*      FENCED                         CHAR(1) NOT NULL,
CCC004*      SQL_DATA_ACCESS                CHAR(1) NOT NULL,
CCC004*      DBINFO                         CHAR(1) NOT NULL,
CCC004*      STAYRESIDENT                   CHAR(1) NOT NULL,
CCC004*      ASUTIME                        INTEGER NOT NULL,
CCC004*      WLM_ENVIRONMENT                VARCHAR(96) NOT NULL,
CCC004*      WLM_ENV_FOR_NESTED             CHAR(1) NOT NULL,
CCC004*      PROGRAM_TYPE                   CHAR(1) NOT NULL,
CCC004*      EXTERNAL_SECURITY              CHAR(1) NOT NULL,
CCC004*      COMMIT_ON_RETURN               CHAR(1) NOT NULL,
CCC004*      RESULT_SETS                    SMALLINT NOT NULL,
CCC004*      LOBCOLUMNS                     SMALLINT NOT NULL,
CCC004*      CREATEDTS                      TIMESTAMP NOT NULL,
CCC004*      ALTEREDTS                      TIMESTAMP NOT NULL,
CCC004*      IBMREQD                        CHAR(1) NOT NULL,
CCC004*      PARM1                          SMALLINT NOT NULL,
CCC004*      PARM2                          SMALLINT NOT NULL,
CCC004*      PARM3                          SMALLINT NOT NULL,
CCC004*      PARM4                          SMALLINT NOT NULL,
CCC004*      PARM5                          SMALLINT NOT NULL,
CCC004*      PARM6                          SMALLINT NOT NULL,
CCC004*      PARM7                          SMALLINT NOT NULL,
CCC004*      PARM8                          SMALLINT NOT NULL,
CCC004*      PARM9                          SMALLINT NOT NULL,
CCC004*      PARM10                         SMALLINT NOT NULL,
CCC004*      PARM11                         SMALLINT NOT NULL,
CCC004*      PARM12                         SMALLINT NOT NULL,
CCC004*      PARM13                         SMALLINT NOT NULL,
CCC004*      PARM14                         SMALLINT NOT NULL,
CCC004*      PARM15                         SMALLINT NOT NULL,
CCC004*      PARM16                         SMALLINT NOT NULL,
CCC004*      PARM17                         SMALLINT NOT NULL,
CCC004*      PARM18                         SMALLINT NOT NULL,
CCC004*      PARM19                         SMALLINT NOT NULL,
CCC004*      PARM20                         SMALLINT NOT NULL,
CCC004*      PARM21                         SMALLINT NOT NULL,
CCC004*      PARM22                         SMALLINT NOT NULL,
CCC004*      PARM23                         SMALLINT NOT NULL,
CCC004*      PARM24                         SMALLINT NOT NULL,
CCC004*      PARM25                         SMALLINT NOT NULL,
CCC004*      PARM26                         SMALLINT NOT NULL,
CCC004*      PARM27                         SMALLINT NOT NULL,
CCC004*      PARM28                         SMALLINT NOT NULL,
CCC004*      PARM29                         SMALLINT NOT NULL,
CCC004*      PARM30                         SMALLINT NOT NULL,
CCC004*      IOS_PER_INVOC                  FLOAT NOT NULL,
CCC004*      INSTS_PER_INVOC                FLOAT NOT NULL,
CCC004*      INITIAL_IOS                    FLOAT NOT NULL,
CCC004*      INITIAL_INSTS                  FLOAT NOT NULL,
CCC004*      CARDINALITY                    FLOAT NOT NULL,
CCC004*      RESULT_COLS                    SMALLINT NOT NULL,
CCC004*      EXTERNAL_NAME                  VARCHAR(762) NOT NULL,
CCC004*      PARM_SIGNATURE                 VARCHAR(150) NOT NULL,
CCC004*      RUNOPTS                        VARCHAR(762) NOT NULL,
CCC004*      REMARKS                        VARCHAR(762) NOT NULL,
CCC004*      JAVA_SIGNATURE                 VARCHAR(3072) NOT NULL,
CCC004*      CLASS                          VARCHAR(384) NOT NULL,
CCC004*      JARSCHEMA                      VARCHAR(128) NOT NULL,
CCC004*      JAR_ID                         VARCHAR(128) NOT NULL,
CCC004*      SPECIAL_REGS                   CHAR(1) NOT NULL,
CCC004*      NUM_DEP_MQTS                   SMALLINT NOT NULL,
CCC004*      MAX_FAILURE                    SMALLINT NOT NULL,
CCC004*      PARAMETER_CCSID                INTEGER NOT NULL,
CCC004*      VERSION                        VARCHAR(122) NOT NULL,
CCC004*      CONTOKEN                       CHAR(8) NOT NULL,
CCC004*      ACTIVE                         CHAR(1) NOT NULL,
CCC004*      DEBUG_MODE                     CHAR(1) NOT NULL,
CCC004*      TEXT_ENVID                     INTEGER NOT NULL,
CCC004*      TEXT_ROWID                      ROWID NOT NULL,
CCC004*      TEXT                           CLOB(2097152) NOT NULL,
CCC004*      OWNERTYPE                      CHAR(1) NOT NULL,
CCC004*      PARAMETER_VARCHARFORM          INTEGER NOT NULL,
CCC004*      RELCREATED                     CHAR(1) NOT NULL,
CCC004*      PACKAGEPATH                    VARCHAR(4096) NOT NULL,
CCC004*      SECURE                         CHAR(1) NOT NULL,
CCC004*      SYSTEM_DEFINED                 CHAR(1) NOT NULL,
CCC004*      INLINE                         CHAR(1) NOT NULL,
CCC004*      PARSETREE                      BLOB(1073741824) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE SYSIBM.SYSROUTINES                 *
      ******************************************************************
       01  DCLSYSROUTINES.
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
           10 ROUTINETYPE          PIC X(1).
      *    *************************************************************
           10 CREATEDBY.
              49 CREATEDBY-LEN     PIC S9(4) USAGE COMP.
              49 CREATEDBY-TEXT    PIC X(128).
      *    *************************************************************
           10 SPECIFICNAME.
              49 SPECIFICNAME-LEN
                 PIC S9(4) USAGE COMP.
              49 SPECIFICNAME-TEXT
                 PIC X(128).
      *    *************************************************************
           10 ROUTINEID            PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 RETURN-TYPE          PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 ORIGIN               PIC X(1).
      *    *************************************************************
           10 FUNCTION-TYPE        PIC X(1).
      *    *************************************************************
           10 PARM-COUNT           PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 LANGUAGE.
              49 LANGUAGE-LEN      PIC S9(4) USAGE COMP.
              49 LANGUAGE-TEXT     PIC X(24).
      *    *************************************************************
           10 COLLID.
              49 COLLID-LEN        PIC S9(4) USAGE COMP.
              49 COLLID-TEXT       PIC X(128).
      *    *************************************************************
           10 SOURCESCHEMA.
              49 SOURCESCHEMA-LEN
                 PIC S9(4) USAGE COMP.
              49 SOURCESCHEMA-TEXT
                 PIC X(128).
      *    *************************************************************
           10 SOURCESPECIFIC.
              49 SOURCESPECIFIC-LEN
                 PIC S9(4) USAGE COMP.
              49 SOURCESPECIFIC-TEXT
                 PIC X(128).
      *    *************************************************************
           10 DETERMINISTIC        PIC X(1).
      *    *************************************************************
           10 EXTERNAL-ACTION      PIC X(1).
      *    *************************************************************
           10 NULL-CALL            PIC X(1).
      *    *************************************************************
           10 CAST-FUNCTION        PIC X(1).
      *    *************************************************************
           10 SCRATCHPAD           PIC X(1).
      *    *************************************************************
           10 SCRATCHPAD-LENGTH    PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 FINAL-CALL           PIC X(1).
      *    *************************************************************
           10 PARALLEL             PIC X(1).
      *    *************************************************************
           10 PARAMETER-STYLE      PIC X(1).
      *    *************************************************************
           10 FENCED               PIC X(1).
      *    *************************************************************
           10 SQL-DATA-ACCESS      PIC X(1).
      *    *************************************************************
           10 DBINFO               PIC X(1).
      *    *************************************************************
           10 STAYRESIDENT         PIC X(1).
      *    *************************************************************
           10 ASUTIME              PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 WLM-ENVIRONMENT.
              49 WLM-ENVIRONMENT-LEN
                 PIC S9(4) USAGE COMP.
              49 WLM-ENVIRONMENT-TEXT
                 PIC X(96).
      *    *************************************************************
           10 WLM-ENV-FOR-NESTED   PIC X(1).
      *    *************************************************************
           10 PROGRAM-TYPE         PIC X(1).
      *    *************************************************************
           10 EXTERNAL-SECURITY    PIC X(1).
      *    *************************************************************
           10 COMMIT-ON-RETURN     PIC X(1).
      *    *************************************************************
           10 RESULT-SETS          PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 LOBCOLUMNS           PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 CREATEDTS            PIC X(26).
      *    *************************************************************
           10 ALTEREDTS            PIC X(26).
      *    *************************************************************
           10 IBMREQD              PIC X(1).
      *    *************************************************************
           10 PARM1                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM2                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM3                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM4                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM5                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM6                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM7                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM8                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM9                PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM10               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM11               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM12               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM13               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM14               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM15               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM16               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM17               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM18               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM19               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM20               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM21               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM22               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM23               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM24               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM25               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM26               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM27               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM28               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM29               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARM30               PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 IOS-PER-INVOC        USAGE COMP-2.
      *    *************************************************************
           10 INSTS-PER-INVOC      USAGE COMP-2.
      *    *************************************************************
           10 INITIAL-IOS          USAGE COMP-2.
      *    *************************************************************
           10 INITIAL-INSTS        USAGE COMP-2.
      *    *************************************************************
           10 CARDINALITY          USAGE COMP-2.
      *    *************************************************************
           10 RESULT-COLS          PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 EXTERNAL-NAME.
              49 EXTERNAL-NAME-LEN
                 PIC S9(4) USAGE COMP.
              49 EXTERNAL-NAME-TEXT
                 PIC X(762).
      *    *************************************************************
           10 PARM-SIGNATURE.
              49 PARM-SIGNATURE-LEN
                 PIC S9(4) USAGE COMP.
              49 PARM-SIGNATURE-TEXT
                 PIC X(150).
      *    *************************************************************
           10 RUNOPTS.
              49 RUNOPTS-LEN       PIC S9(4) USAGE COMP.
              49 RUNOPTS-TEXT      PIC X(762).
      *    *************************************************************
           10 REMARKS.
              49 REMARKS-LEN       PIC S9(4) USAGE COMP.
              49 REMARKS-TEXT      PIC X(762).
      *    *************************************************************
           10 JAVA-SIGNATURE.
              49 JAVA-SIGNATURE-LEN
                 PIC S9(4) USAGE COMP.
              49 JAVA-SIGNATURE-TEXT
                 PIC X(3072).
      *    *************************************************************
           10 XCLASS.
              49 CLASS-LEN         PIC S9(4) USAGE COMP.
              49 CLASS-TEXT        PIC X(384).
      *    *************************************************************
           10 JARSCHEMA.
              49 JARSCHEMA-LEN     PIC S9(4) USAGE COMP.
              49 JARSCHEMA-TEXT    PIC X(128).
      *    *************************************************************
           10 JAR-ID.
              49 JAR-ID-LEN        PIC S9(4) USAGE COMP.
              49 JAR-ID-TEXT       PIC X(128).
      *    *************************************************************
           10 SPECIAL-REGS         PIC X(1).
      *    *************************************************************
           10 NUM-DEP-MQTS         PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 MAX-FAILURE          PIC S9(4) USAGE COMP.
      *    *************************************************************
           10 PARAMETER-CCSID      PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 VERSION.
              49 VERSION-LEN       PIC S9(4) USAGE COMP.
              49 VERSION-TEXT      PIC X(122).
      *    *************************************************************
           10 CONTOKEN             PIC X(8).
      *    *************************************************************
           10 ACTIVE               PIC X(1).
      *    *************************************************************
           10 DEBUG-MODE           PIC X(1).
      *    *************************************************************
           10 TEXT-ENVID           PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 TEXT-ROWID           USAGE SQL TYPE IS ROWID.
      *    *************************************************************
           10 XTEXT                USAGE SQL TYPE IS CLOB-LOCATOR.
      *    *************************************************************
           10 OWNERTYPE            PIC X(1).
      *    *************************************************************
           10 PARAMETER-VARCHARFORM
              PIC S9(9) USAGE COMP.
      *    *************************************************************
           10 RELCREATED           PIC X(1).
      *    *************************************************************
           10 PACKAGEPATH.
              49 PACKAGEPATH-LEN   PIC S9(4) USAGE COMP.
              49 PACKAGEPATH-TEXT
                 PIC X(4096).
      *    *************************************************************
           10 SECURE               PIC X(1).
      *    *************************************************************
           10 SYSTEM-DEFINED       PIC X(1).
      *    *************************************************************
           10 INLINE               PIC X(1).
      *    *************************************************************
           10 PARSETREE            USAGE SQL TYPE IS BLOB-LOCATOR.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 102     *
      ******************************************************************

