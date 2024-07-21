      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTPA0023.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTPA0023.CPv  $
      *
      *   Rev 2.3   Sep 21 2018 12:20:36   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.2   Apr 11 2016 08:21:42   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.1   Apr 07 2016 10:15:14   T13EX67
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - TBPA0023/MSS
      *
      *   Rev 2.0   Feb 27 1999 14:26:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:47:22   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   04 Jul 1997 08:01:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTPA0023                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBPA0023                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    ......1997                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBPA0023)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTPA0023))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0023 TABLE
CCC004*    (
CCC004*      PGEB_GES                      SMALLINT  NOT NULL,
CCC004*      PROD_PROD_SCHL                SMALLINT  NOT NULL,
CCC004*      KNR_KOND_ART                  SMALLINT  NOT NULL,
CCC004*      KTY_KOND_ART                  SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR                  SMALLINT NOT NULL,
CCC004*      PRKO_GUELTIG                  CHAR (1) NOT NULL
CCC004*                                             WITH DEFAULT,
CCC004*      PRKO_GUELTIG_AB               DATE     NOT NULL,
CCC004*      PRKO_GUELTIG_BIS              DATE     NOT NULL,
CCC004*      PRKO_SELKRIT_PA               CHAR (1) NOT NULL,
CCC004*      PRKO_SELKRIT_LZ               CHAR (1) NOT NULL,
CCC004*      PRKO_PA_VON                   SMALLINT NOT NULL,
CCC004*      PRKO_PA_BIS                   SMALLINT NOT NULL,
CCC004*      PRKO_LZ_VON                   SMALLINT NOT NULL,
CCC004*      PRKO_LZ_BIS                   SMALLINT NOT NULL,
CCC004*      KNR_KOND_REGEL                SMALLINT NOT NULL,
CCC004*      KTY_KOND_REGEL                SMALLINT NOT NULL,
CCC004*      PRKO_SATZ_1                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_2                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_3                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_4                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_5                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_6                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_7                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_8                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_9                   DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_SATZ_10                  DECIMAL (8, 4) NOT NULL,
CCC004*      PRKO_WERT_1                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_2                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_3                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_4                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_5                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_6                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_7                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_8                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_9                   INTEGER        NOT NULL,
CCC004*      PRKO_WERT_10                  INTEGER        NOT NULL,
CCC004*      PRKO_LAST_UPDATE              TIMESTAMP      NOT NULL,
CCC004*      PRKO_KENNUNG                  CHAR (8)       NOT NULL
CCC003*    ) END-EXEC.
 
 
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBPA0023                     *
      ******************************************************************
       01  TBPA0023-GESAMT.
           05  DCL-TBPA0023.
               10 PGEB-GES                       PIC S9(4) COMP.
               10 PROD-PROD-SCHL                 PIC S9(4) COMP.
               10 KNR-KOND-ART                   PIC S9(4) COMP.
               10 KTY-KOND-ART                   PIC S9(4) COMP.
               10 PRKO-KOND-NR                   PIC S9(4) COMP.
               10 PRKO-GUELTIG                   PIC X.
               10 PRKO-GUELTIG-AB                PIC X(10).
               10 PRKO-GUELTIG-BIS               PIC X(10).
               10 PRKO-SELKRIT-PA                PIC X.
               10 PRKO-SELKRIT-LZ                PIC X.
               10 PRKO-PA-VON                    PIC S9(4) COMP.
               10 PRKO-PA-BIS                    PIC S9(4) COMP.
               10 PRKO-LZ-VON                    PIC S9(4) COMP.
               10 PRKO-LZ-BIS                    PIC S9(4) COMP.
               10 KNR-KOND-REGEL                 PIC S9(4) COMP.
               10 KTY-KOND-REGEL                 PIC S9(4) COMP.
               10 PRKO-SATZ-1                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-2                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-3                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-4                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-5                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-6                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-7                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-8                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-9                    PIC S9(4)V9(4) COMP-3.
               10 PRKO-SATZ-10                   PIC S9(4)V9(4) COMP-3.
               10 PRKO-WERT-1                    PIC S9(9) COMP.
               10 PRKO-WERT-2                    PIC S9(9) COMP.
               10 PRKO-WERT-3                    PIC S9(9) COMP.
               10 PRKO-WERT-4                    PIC S9(9) COMP.
               10 PRKO-WERT-5                    PIC S9(9) COMP.
               10 PRKO-WERT-6                    PIC S9(9) COMP.
               10 PRKO-WERT-7                    PIC S9(9) COMP.
               10 PRKO-WERT-8                    PIC S9(9) COMP.
               10 PRKO-WERT-9                    PIC S9(9) COMP.
               10 PRKO-WERT-10                   PIC S9(9) COMP.
               10 PRKO-LAST-UPDATE               PIC X(26).
               10 PRKO-KENNUNG                   PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 30      *
      ******************************************************************
 
 

