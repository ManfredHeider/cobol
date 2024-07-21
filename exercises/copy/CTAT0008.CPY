      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAT0008.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTAT0008.CPv  $
      *
      *   Rev 2.0   Jun 23 1999 15:44:24   T13EX21
      *APA00012 - Korrektur Bezeichnung
      *
      *   Rev 1.0   Nov 04 1995 11:47:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: CTAT0008                                      *
      *    --------------------------------------------------------   *
      *    DCLGEN VON TBAT0008                                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BÖFF                                             *
      *    DATUM:    04.11.1994                                       *
      *****************************************************************
      *    VERSION   1    10.01.95 16:39:29
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0142!TT.MM.94!    ! NEU                       ! KUHN     *
      *         !04.11.94!    ! ZUFÜGEN DES FELDES        ! BÖFF     *
      *         !        !    ! KNR-BEZ10                 !          *
      *         !10.01.95! 01 ! PRODUKTIONSÜBERGABE       ! GIESE    *
      * AFPA0013!30.05.97! 02 ! KORREKTUR BEZ.            ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBAT0008)                                         *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAT0008 TABLE
CCC004*    ( KTY_NR                         SMALLINT NOT NULL,
CCC004*      KNR_NR                         SMALLINT NOT NULL,
CCC004*      KNR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KNR_BEZ                        CHAR(80) NOT NULL,
CCC004*      KNR_BEZ_KURZ                   CHAR(4) NOT NULL,
CCC004*      KNR_HINDERNIS                  CHAR(1) NOT NULL,
CCC004*      KNR_BEZ10                      CHAR(10) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0008                           *
      ******************************************************************
       01  TBAT0008-GESAMT.
        05  NI-TBAT0008-TAB.
           10  NI-TBAT0008-ELE     PIC S9(04)  COMP   OCCURS  7  TIMES.
        05  NI-TBAT0008            REDEFINES   NI-TBAT0008-TAB.
           10 NI-KTY-NR            PIC S9(04)  COMP.
           10 NI-KNR-NR            PIC S9(04)  COMP.
           10 NI-KNR-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-KNR-BEZ           PIC S9(04)  COMP.
           10 NI-KNR-BEZ-KURZ      PIC S9(04)  COMP.
           10 NI-KNR-HINDERNIS     PIC S9(04)  COMP.
           10 NI-KNR-BEZ10         PIC S9(04)  COMP.
        05  DCL-TBAT0008.
           10 KTY-NR               PIC S9(4) USAGE COMP.
           10 KNR-NR               PIC S9(4) USAGE COMP.
           10 KNR-LAST-UPDATE      PIC X(26).
           10 KNR-BEZ              PIC X(80).
           10 KNR-BEZ-KURZ         PIC X(4).
           10 KNR-HINDERNIS        PIC X(1).
           10 KNR-BEZ10            PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
