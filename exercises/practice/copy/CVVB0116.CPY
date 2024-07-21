      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0116.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0116.CPv  $
      *
      *   Rev 2.2   Jun 30 2016 11:54:30   t13ex71
      *AVB01400: Generierung Lese-OZP, Schreibmodul, etc. für Tabel
      *le TBVB0116 (BPR)
      *
      *   Rev 2.1   Feb 20 2009 08:30:18   t13ex43
      *AAS00812: PGM - D-Chancenprofil
      *
      *   Rev 2.0   Feb 19 2009 09:35:10   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    07.06.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01400!07.06.16!2.1 ! NEU                       ! B.PRETSCH *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0116) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0116)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0116 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , RSTAT_DAT_VON               TIMESTAMP NOT NULL
CCC004*    , KNR_RSTAT_FIBU              SMALLINT NOT NULL
CCC004*    , KTY_RSTAT_FIBU              SMALLINT NOT NULL
CCC004*    , KNR_RSTAT_DAISY             SMALLINT NOT NULL
CCC004*    , KTY_RSTAT_DAISY             SMALLINT NOT NULL
CCC004*    , RSTAT_DAT_BIS               DATE NOT NULL
CCC004*    , RSTAT_LAST_UPDATE           TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0116                           *
      *****************************************************************
 
       01  VIVB0116-GESAMT.
        05 NI-VIVB0116-TAB.
           10 NI-VIVB0116-ELE      PIC S9(04)  BINARY OCCURS   8.
        05 NI-VIVB0116             REDEFINES   NI-VIVB0116-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-RSTAT-DAT-VON            PIC S9(04)       BINARY.
           10 NI-KNR-RSTAT-FIBU           PIC S9(04)       BINARY.
           10 NI-KTY-RSTAT-FIBU           PIC S9(04)       BINARY.
           10 NI-KNR-RSTAT-DAISY          PIC S9(04)       BINARY.
           10 NI-KTY-RSTAT-DAISY          PIC S9(04)       BINARY.
           10 NI-RSTAT-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-RSTAT-LAST-UPDATE        PIC S9(04)       BINARY.
 
        05 DCL-VIVB0116.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 RSTAT-DAT-VON               PIC  X(26).
           10 KNR-RSTAT-FIBU              PIC S9(04)       BINARY.
           10 KTY-RSTAT-FIBU              PIC S9(04)       BINARY.
           10 KNR-RSTAT-DAISY             PIC S9(04)       BINARY.
           10 KTY-RSTAT-DAISY             PIC S9(04)       BINARY.
           10 RSTAT-DAT-BIS               PIC  X(10).
           10 RSTAT-LAST-UPDATE           PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************

