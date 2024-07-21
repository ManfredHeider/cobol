      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0505.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0505.CPv  $
      *
      *   Rev 2.1   May 04 2016 13:38:06   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0505
      *
      *   Rev 2.0   Apr 21 2016 15:22:20   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    21.04.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!21.04.16!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0505) USTSATZ                                *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0505)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0505 TABLE
CCC004*    ( KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_UST_TYP                 SMALLINT NOT NULL
CCC004*    , UST_SATZ                    DECIMAL(06, 3) NOT NULL
CCC004*    , UST_SCHLUESSEL              SMALLINT NOT NULL
CCC004*    , UST_DAT_VON                 DATE NOT NULL
CCC004*    , UST_DAT_BIS                 DATE NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KTY_UST_TYP                 SMALLINT NOT NULL
CCC004*    , UST_LAST_UPDATE             TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0505                           *
      *****************************************************************
 
       01  VIVB0505-GESAMT.
        05 NI-VIVB0505-TAB.
           10 NI-VIVB0505-ELE      PIC S9(04)  BINARY OCCURS   9.
        05 NI-VIVB0505             REDEFINES   NI-VIVB0505-TAB.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-UST-TYP              PIC S9(04)       BINARY.
           10 NI-UST-SATZ                 PIC S9(04)       BINARY.
           10 NI-UST-SCHLUESSEL           PIC S9(04)       BINARY.
           10 NI-UST-DAT-VON              PIC S9(04)       BINARY.
           10 NI-UST-DAT-BIS              PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KTY-UST-TYP              PIC S9(04)       BINARY.
           10 NI-UST-LAST-UPDATE          PIC S9(04)       BINARY.
 
        05 DCL-VIVB0505.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-UST-TYP                 PIC S9(04)       BINARY.
           10 UST-SATZ                    PIC S9(03)V9(03) COMP-3.
           10 UST-SCHLUESSEL              PIC S9(04)       BINARY.
           10 UST-DAT-VON                 PIC  X(10).
           10 UST-DAT-BIS                 PIC  X(10).
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KTY-UST-TYP                 PIC S9(04)       BINARY.
           10 UST-LAST-UPDATE             PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   9                    *
      *****************************************************************

