      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0210.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0210.CPv  $
      *
      *   Rev 2.2   Sep 01 2011 11:47:44   t13ae14
      *ASL00390: Videosonderbonus Anpassung PMRW0018
      *
      *   Rev 2.1   Aug 19 2011 14:11:38   t13ae14
      *ASL00390: Videosonderbonus FTP und Load in die Tabelle VIDEO
      *A, Anpassung PMRW0018
      *
      *   Rev 2.0   Aug 12 2011 11:17:06   t13ae14
      *Archiverstellung
      *
      *****************************************************************
      *    ERSTELLT: K.OBER                                           *
      *    DATUM:    12.08.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00390!12.08.11!2.1 ! NEU                       ! K.OBER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0210) VIDEOA                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0210)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0210 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , VIDEOA_ANSPR_VOM            DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , VIDEOA_LAST_UPDATE          TIMESTAMP NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , VIDEOA_ANSPR_BETRAG         DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT NOT NULL
CCC004*    , KNR_ANSPR_STATUS            SMALLINT NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0210                           *
      *****************************************************************

       01  VIVB0210-GESAMT.
        05 NI-VIVB0210-TAB.
           10 NI-VIVB0210-ELE      PIC S9(04)  BINARY OCCURS  10.
        05 NI-VIVB0210             REDEFINES   NI-VIVB0210-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-VIDEOA-ANSPR-VOM         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.
           10 NI-VIDEOA-LAST-UPDATE       PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-VIDEOA-ANSPR-BETRA       PIC S9(04)       BINARY.
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.

        05 DCL-VIVB0210.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 VIDEOA-ANSPR-VOM            PIC  X(10).
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.
           10 VIDEOA-LAST-UPDATE          PIC  X(26).
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 VIDEOA-ANSPR-BETRA          PIC S9(09)V9(02) COMP-3.
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

      *---------------------------------------------------------------*
