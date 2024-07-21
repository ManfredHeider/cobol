      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0026.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTAT0026.CPv  $
      *
      *   Rev 2.1   Apr 06 1999 11:32:50   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Mar 18 1999 10:44:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0026                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0026                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    19.00.1999                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00005!19.03.99!    ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *****************************************************************
CCC005*    EXEC SQL DECLARE TBAT0026 TABLE
CCC004*    ( ESBM_PGNAME                    CHAR (8)        NOT NULL,
CCC004*      ESBM_BILDNR                    SMALLINT        NOT NULL,
CCC004*      ESBM_LFDNR                     SMALLINT        NOT NULL,
CCC004*      ESBM_POS                       SMALLINT        NOT NULL,
CCC004*      ESBM_LAENGE                    SMALLINT        NOT NULL,
CCC004*      ESBM_ANZ_DEZ                   SMALLINT        NOT NULL,
CCC004*      ESBM_1000ER_PUNKT              CHAR (1)        NOT NULL
CCC003*    ) END-EXEC.
      *****************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0026
      *****************************************************************
       01  TBAT0026-GESAMT.
           05  DCL-TBAT0026.
               10 ESBM-PGNAME               PIC  X(8).
               10 ESBM-BILDNR               PIC S9(4) COMP.
               10 ESBM-LFDNR                PIC S9(4) COMP.
               10 ESBM-POS                  PIC S9(4) COMP.
               10 ESBM-LAENGE               PIC S9(4) COMP.
               10 ESBM-ANZ-DEZ              PIC S9(4) COMP.
               10 ESBM-1000ER-PUNKT         PIC  X(1).
      *****************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7
      *****************************************************************
