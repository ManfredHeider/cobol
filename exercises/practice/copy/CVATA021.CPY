      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvata021.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\cvata021.cpv  $
      *
      *   Rev 2.1   Feb 17 2000 17:13:56   T13EX13
      *APA00081 - Pgeb-Ges in SQL-Statements
      *ergaenzt
      *
      *   Rev 2.0   Feb 09 2000 11:10:40   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVATA021                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIATA021                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                          *
      *    DATUM:    08.02.2000                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00081!08.02.00! 01 ! NEU                       ! WILKENS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * DCLGEN TABLE(VIATA021)                                         *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVATA021))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIATA021 TABLE
CCC004*    ( PGEB_GES                       SMALLINT NOT NULL,
CCC004*      NTZ_ID                         DECIMAL (15, 0) NOT NULL,
CCC004*      NTZZ_ZEILEN_NR                 SMALLINT        NOT NULL,
CCC004*      NTZZ_ZEILE                     CHAR (80)       NOT NULL,
CCC004*      NTZZ_LAST_UPDATE               TIMESTAMP       NOT NULL,
CCC004*      NTZZ_KENNUNG                   CHAR (8)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIATA021                           *
      ******************************************************************
       01  VIATA021-GESAMT.
           05  DCL-VIATA021.
010200         10 PGEB-GES                  PIC S9(4) COMP.
               10 NTZ-ID                    PIC S9(15)V COMP-3.
               10 NTZZ-ZEILEN-NR            PIC S9(4) COMP.
               10 NTZZ-ZEILE                PIC  X(80).
               10 NTZZ-LAST-UPDATE          PIC  X(26).
               10 NTZZ-KENNUNG              PIC  X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
