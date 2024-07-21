      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0017.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTOK0017.CPv  $
      *
      *   Rev 2.2   Jul 19 2016 11:47:22   t13ex48
      *FKI01156: Wartungsauftrag Abgleich: ZJKI0035
      *
      *   Rev 2.1   24 Apr 2007 09:53:32   t13ex43
      *AKI00641: PGM7 - Anpassung für HOST_KUNDENUEBER
      *
      *   Rev 2.0   16 Apr 2007 14:19:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    ERSTELLT: BERND KLEEMEIER                                  *
      *    DATUM:    16.04.2007                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AKI00641!16.04.07!2.1 ! NEU                       ! KLEEMEIER *
      * --------+--------+----+---------------------------+-----------*
      * FKI01156!19.07.16!2.2 ! Tabellenname statt ALIAS  ! HSE       *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      * DCLGEN TABLE(TCOK0017) HOST_KUNDENUEBER                       *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTOK0017)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
190716*    EXEC SQL DECLARE TCOK0017 TABLE
CCC005*    EXEC SQL DECLARE HOST_KUNDENUEBER TABLE
CCC004*    ( HKUID                       CHAR (13) NOT NULL,
CCC004*      HKUTYP                      SMALLINT  NOT NULL,
CCC004*      HKUERSTELLT                 TIMESTAMP NOT NULL,
CCC004*      HKUERSTELLTVON              CHAR (08) NOT NULL,
CCC004*      HKUSTEUERART                CHAR (01) NOT NULL,
CCC004*      HKUKUNDENID                 CHAR (13),
CCC004*      HKUVERTRAGID                CHAR (13) NOT NULL,
CCC004*      HKUVBNRALT                  INTEGER NOT NULL,
CCC004*      HKUVBNRNEU                  INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TCOK0017
      ******************************************************************
       01  TCOK0017-GESAMT.
        05  NI-TCOK0017-TAB.
           10 NI-TCOK0017-ELE      PIC S9(04) BINARY OCCURS  9 TIMES.
        05  NI-TCOK0017            REDEFINES   NI-TCOK0017-TAB.
           10 NI-HKUID                    PIC S9(04)       BINARY.
           10 NI-HKUTYP                   PIC S9(04)       BINARY.
           10 NI-HKUERSTELLT              PIC S9(04)       BINARY.
           10 NI-HKUERSTELLTVON           PIC S9(04)       BINARY.
           10 NI-HKUSTEUERART             PIC S9(04)       BINARY.
           10 NI-HKUKUNDENID              PIC S9(04)       BINARY.
           10 NI-HKUVERTRAGID             PIC S9(04)       BINARY.
           10 NI-HKUVBNRALT               PIC S9(04)       BINARY.
           10 NI-HKUVBNRNEU               PIC S9(04)       BINARY.
 
        05  DCL-TCOK0017.
           10 HKUID                       PIC  X(13).
           10 HKUTYP                      PIC S9(04)       BINARY.
           10 HKUERSTELLT                 PIC  X(26).
           10 HKUERSTELLTVON              PIC  X(08).
           10 HKUSTEUERART                PIC  X(01).
           10 HKUKUNDENID                 PIC  X(13).
           10 HKUVERTRAGID                PIC  X(13).
           10 HKUVBNRALT                  PIC S9(09)       BINARY.
           10 HKUVBNRNEU                  PIC S9(09)       BINARY.
 
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

