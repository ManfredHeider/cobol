      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0019                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0019                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    16.09.2021                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0019 TABLE
CCC004*    ( PAAA_NAME                      CHAR(30)      NOT NULL,
CCC004*      PAAA_PRIO                      SMALLINT      NOT NULL,
CCC004*      PAAA_KEY                       VARCHAR(1000) NOT NULL,
CCC004*      PAAA_LFDNR                     SMALLINT      NOT NULL,
CCC004*      PAAA_ZUSTAND                   SMALLINT      NOT NULL,
CCC004*      PAAA_ZUSATZ_INFO               VARCHAR(1400) NOT NULL,
CCC004*      PAAA_LAST_UPDATE               TIMESTAMP     NOT NULL,
CCC004*      PAAA_KENNUNG                   CHAR(8)       NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0019                           *
      ******************************************************************
        01 TBPA0019-GESAMT.
           05 DCL-TBPA0019.
              10 PAAA-NAME                  PIC  X(030).
              10 PAAA-PRIO                  PIC S9(004) BINARY.
              10 PAAA-KEY.         
                 49 PAAA-KEY-LEN            PIC S9(004) BINARY.
                 49 PAAA-KEY-TEXT           PIC  X(1000).
              10 PAAA-LFDNR                 PIC S9(004) BINARY.    
              10 PAAA-ZUSTAND               PIC S9(004) BINARY.
              10 PAAA-ZUSATZ-INFO.
                 49 PAAA-ZUSATZ-INFO-LEN    PIC S9(004) BINARY.
                 49 PAAA-ZUSATZ-INFO-TEXT   PIC  X(1400).
              10 PAAA-LAST-UPDATE           PIC  X(026).
              10 PAAA-KENNUNG               PIC  X(008).
      *
      *****************************************************************
      *---------------------- ENDE CTPA0019 --------------------------* 
      *****************************************************************
