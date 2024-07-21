      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTAT0027.CPv  $
      *
      *   Rev 2.1   Apr 06 1999 11:32:52   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Mar 18 1999 10:44:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0027                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0027                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                           *
      *    DATUM:    19.00.1999                                       *
      *****************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00005!19.03.99!    ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0027 TABLE
CCC004*    ( ESWT_PGNAME                    CHAR (8)        NOT NULL,
CCC004*      ESWT_BILDNR                    SMALLINT        NOT NULL,
CCC004*      ESWT_LFDNR                     SMALLINT        NOT NULL,
CCC004*      ESWT_POS                       SMALLINT        NOT NULL,
CCC004*      ESWT_LAENGE                    SMALLINT        NOT NULL,
CCC004*      ESWT_STD_WTEXT                 CHAR (20)       NOT NULL,
CCC004*      ESWT_ALT_WTEXT                 CHAR (20)       NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0027                           *
      ******************************************************************
       01  TBAT0027-GESAMT.
           05  DCL-TBAT0027.
               10 ESWT-PGNAME               PIC  X(8).
               10 ESWT-BILDNR               PIC S9(4) COMP.
               10 ESWT-LFDNR                PIC S9(4) COMP.
               10 ESWT-POS                  PIC S9(4) COMP.
               10 ESWT-LAENGE               PIC S9(4) COMP.
               10 ESWT-STD-WTEXT            PIC  X(20).
               10 ESWT-ALT-WTEXT            PIC  X(20).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
