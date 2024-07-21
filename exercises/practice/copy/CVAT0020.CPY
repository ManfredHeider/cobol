      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvat0020.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\cvat0020.cpv  $
      *
      *   Rev 2.1   Jun 29 1999 10:41:54   P13SP20
      *FRZ00028 - Korrektur PVCS-Kopf
      *
      *   Rev 2.0   27 Feb 1999 11:56:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:59:54   T13EX26
      *PEG-Produktionsübergabe
      *
      *   Rev 1.0   16 Jan 1998 09:48:12   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVAT0020                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIAT0020                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    16.01.1998                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * DCLGEN TABLE(VIAT0020)                                         *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVAT0020))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0020 TABLE
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
      * COBOL DECLARATION FOR TABLE VIAT0020                           *
      ******************************************************************
       01  VIAT0020-GESAMT.
           05  NI-VIAT0020-TAB.
               10 NI-VIAT0020        PIC S9(04)  COMP   OCCURS 9 TIMES.
           05  NI-VIAT0020            REDEFINES   NI-VIAT0020-TAB.
               10 NI-NTZ-ID                 PIC S9(04)  COMP.
               10 NI-KNR-NOTIZ-ART          PIC S9(04)  COMP.
               10 NI-KNR-OBJEKT-HKF-TAB     PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ID             PIC S9(04)  COMP.
               10 NI-NTZ-PTR-ST-NR          PIC S9(04)  COMP.
               10 NI-NTZ-PTR-PIN            PIC S9(04)  COMP.
               10 NI-NTZ-PTR-VNR            PIC S9(04)  COMP.
               10 NI-NTZ-LAST-UPDATE        PIC S9(04)  COMP.
               10 NI-NTZ-KENNUNG            PIC S9(04)  COMP.
           05  DCL-VIAT0020.
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
