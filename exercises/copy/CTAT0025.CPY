      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0025.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\CTAT0025.CPv  $
      *
      *   Rev 2.1   Apr 06 1999 11:32:48   T13AE34
      *AAT00005-Euro-Swicht
      *
      *   Rev 2.0   Feb 27 1999 11:56:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   06 Feb 1998 10:53:42   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0025                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0025                                        *
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
CCC005*    EXEC SQL DECLARE TBAT0025 TABLE
CCC004*    ( ESWB_PGNAME                    CHAR (8)        NOT NULL,
CCC004*      ESWB_BILDNR                    SMALLINT        NOT NULL,
CCC004*      ESWB_SUCHFOLGE                 SMALLINT        NOT NULL,
CCC004*      ESWB_BEZGROESSE                CHAR (15)       NOT NULL,
CCC004*      ESWB_POS                       SMALLINT        NOT NULL,
CCC004*      ESWB_LAENGE                    SMALLINT        NOT NULL,
CCC004*      ESWB_STD_WAE                   SMALLINT        NOT NULL,
CCC004*      ESWB_ALT_WAE                   SMALLINT        NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0025                           *
      ******************************************************************
       01  TBAT0025-GESAMT.
           05  DCL-TBAT0025.
               10 ESWB-PGNAME               PIC  X(8).
               10 ESWB-BILDNR               PIC S9(4) COMP.
               10 ESWB-SUCHFOLGE            PIC S9(4) COMP.
               10 ESWB-BEZGROESSE           PIC  X(15).
               10 ESWB-POS                  PIC S9(4) COMP.
               10 ESWB-LAENGE               PIC S9(4) COMP.
               10 ESWB-STD-WAE              PIC S9(4) COMP.
               10 ESWB-ALT-WAE              PIC S9(4) COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
