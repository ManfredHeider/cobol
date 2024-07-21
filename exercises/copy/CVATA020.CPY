      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvata020.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\cvata020.cpv  $
      *
      *   Rev 2.1   Feb 17 2000 17:13:52   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.0   Feb 09 2000 11:10:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVATA020                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIATA020                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                          *
      *    DATUM:    08.02.2000                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(VIATA020)                                         *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVATA020))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIATA020 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      NTZ_ID                         DECIMAL (15, 0) NOT NULL,
CCC004*      KNR_NOTIZ_ART                  SMALLINT        NOT NULL,
CCC004*      KNR_OBJEKT_HKF_TAB             SMALLINT        NOT NULL,
CCC004*      NTZ_PTR_ID                     DECIMAL (15, 0),
CCC004*      NTZ_PTR_ST_NR                  INTEGER        ,
CCC004*      NTZ_PTR_PIN                    INTEGER        ,
CCC004*      NTZ_PTR_VNR                    CHAR (15)      ,
CCC004*      NTZ_LAST_UPDATE                TIMESTAMP       NOT NULL,
CCC004*      NTZ_KENNUNG                    CHAR (8)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA020                           *
      ******************************************************************
       01  VIATA020-GESAMT.
           05  NI-VIATA020-TAB.
               10 NI-VIATA020        PIC S9(04)  COMP  OCCURS 10 TIMES.
           05  NI-VIATA020            REDEFINES   NI-VIATA020-TAB.
010200         10 PGEB-GES                  PIC S9(04)  COMP.
               10 NI-NTZ-ID                 PIC S9(04)  COMP.
               10 NI-KNR-NOTIZ-ART          PIC S9(04)  COMP.
               10 NI-KNR-OBJEKT-HKF-TAB     PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ID             PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ST-NR          PIC S9(04)  COMP.
               10 NI-NTZ-PTR-PIN            PIC S9(04)  COMP.
               10 NI-NTZ-PTR-VNR            PIC S9(04)  COMP.
               10 NI-NTZ-LAST-UPDATE        PIC S9(04)  COMP.
               10 NI-NTZ-KENNUNG            PIC S9(04)  COMP.
           05  DCL-VIATA020.
010200         10 PGEB-GES                  PIC S9(04)  COMP.
               10 NTZ-ID                    PIC S9(15)V COMP-3.
               10 KNR-NOTIZ-ART             PIC S9(4) COMP.
               10 KNR-OBJEKT-HKF-TAB        PIC S9(4) COMP.
               10 NTZ-PTR-ID                PIC S9(15)V COMP-3.
               10 NTZ-PTR-ST-NR             PIC S9(9) COMP.
               10 NTZ-PTR-PIN               PIC S9(9) COMP.
               10 NTZ-PTR-VNR               PIC X(15).
               10 NTZ-LAST-UPDATE           PIC X(26).
               10 NTZ-KENNUNG               PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
