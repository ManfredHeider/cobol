      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0020.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/CTAT0020.CPv  $
      *
      *   Rev 1.1   12 Nov 1998 10:34:36   T13EX26
      *AFAT0033 - Neuerstellung
      *
      *   Rev 1.0   12 Nov 1998 08:36:48   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0020                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0020                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    07.11.1997                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(T13AE.TBAT0020)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTAT0020))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0020 TABLE
CCC004*    ( NTZ_ID                         DECIMAL (15, 0) NOT NULL,
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
      * COBOL DECLARATION FOR TABLE T13AE.TBAT0020                     *
      ******************************************************************
       01  TBAT0020-GESAMT.
           05  NI-TBAT0020-TAB.
               10 NI-TBAT0020        PIC S9(04)  COMP   OCCURS 9 TIMES.
           05  NI-TBAT0020            REDEFINES   NI-TBAT0020-TAB.
               10 NI-NTZ-ID                 PIC S9(04)  COMP.
               10 NI-KNR-NOTIZ-ART          PIC S9(04)  COMP.
               10 NI-KNR-OBJEKT-HKF-TAB     PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ID             PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ST-NR          PIC S9(04)  COMP.
               10 NI-NTZ-PTR-PIN            PIC S9(04)  COMP.
               10 NI-NTZ-PTR-VNR            PIC S9(04)  COMP.
               10 NI-NTZ-LAST-UPDATE        PIC S9(04)  COMP.
               10 NI-NTZ-KENNUNG            PIC S9(04)  COMP.
           05  DCL-TBAT0020.
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
