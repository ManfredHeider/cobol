      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVAT0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVAT0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:59:58   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   16 Jan 1998 09:48:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CVAT0021                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON VIAT0021                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    16.01.1998                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!16.01.98! 01 ! NEU                       ! STORCH   *
      *         !17.01.98!    ! NTZZ_ZEILE_TEXT UMBENANNT ! AUERBACH *
      *         !        !    ! NTZZ_ZEILE                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      * DCLGEN TABLE(VIAT0021)                                         *
      *        LIBRARY(T130005.AE010000.SRCDCL(CVAT0021))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIAT0021 TABLE
CCC004*    ( NTZ_ID                         DECIMAL (15, 0) NOT NULL,
CCC004*      NTZZ_ZEILEN_NR                 SMALLINT        NOT NULL,
CCC004*      NTZZ_ZEILE                     CHAR (80)       NOT NULL,
CCC004*      NTZZ_LAST_UPDATE               TIMESTAMP       NOT NULL,
CCC004*      NTZZ_KENNUNG                   CHAR (8)        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0021                           *
      ******************************************************************
       01  VIAT0021-GESAMT.
           05  DCL-VIAT0021.
               10 NTZ-ID                    PIC S9(15)V COMP-3.
               10 NTZZ-ZEILEN-NR            PIC S9(4) COMP.
               10 NTZZ-ZEILE                PIC  X(80).
               10 NTZZ-LAST-UPDATE          PIC  X(26).
               10 NTZZ-KENNUNG              PIC  X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
