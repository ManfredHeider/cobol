      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0021.CPY  $
      *    $Revision:   1.1  $
      *
      *    $Log:   L:/MF/CPY/CTAT0021.CPv  $
      *
      *   Rev 1.1   12 Nov 1998 10:37:12   T13EX26
      *AFAT0033 - Neuerstellung
      *
      *   Rev 1.0   12 Nov 1998 08:36:56   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0021                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0021                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    07.11.1997                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.11.97! 01 ! NEU                       ! BOHLINGER*
      *         !17.01.98!    ! NTZZ_ZEILE_TEXT UMBENANNT ! AUERBACH *
      *         !        !    ! NTZZ_ZEILE                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * DCLGEN TABLE(T13AE.TBAT0021)                                   *
      *        LIBRARY(T130003.CE010000.SRCDCL(CTAT0021))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0021 TABLE
CCC004*    ( NTZ_ID                         DECIMAL (15, 0) NOT NULL,
CCC004*      NTZZ_ZEILEN_NR                 SMALLINT        NOT NULL,
CCC004*      NTZZ_ZEILE                     CHAR (80)       NOT NULL,
CCC004*      NTZZ_LAST_UPDATE               TIMESTAMP       NOT NULL,
CCC004*      NTZZ_KENNUNG                   CHAR (8)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13AE.TBAT0021                     *
      ******************************************************************
       01  TBAT0021-GESAMT.
           05  DCL-TBAT0021.
               10 NTZ-ID                    PIC S9(15)V COMP-3.
               10 NTZZ-ZEILEN-NR            PIC S9(4) COMP.
               10 NTZZ-ZEILE                PIC  X(80).
               10 NTZZ-LAST-UPDATE          PIC  X(26).
               10 NTZZ-KENNUNG              PIC  X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
