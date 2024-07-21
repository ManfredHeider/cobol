      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0125.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CVVB0125.CPv  $
      *
      *   Rev 2.2   Apr 28 2015 12:57:42   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.1   Mar 13 2015 10:00:28   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   Mar 13 2015 08:58:36   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    27.04.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * AVB01347!27.04.15!2.1 ! NEU                       ! J.HERING  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0125) USTAT                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0125)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0125 TABLE
CCC004*    ( ST_NR                       INTEGER NOT NULL
CCC004*    , USTAT_DAT_VON               TIMESTAMP NOT NULL
CCC004*    , KNR_USTAT_TYP               SMALLINT NOT NULL
CCC004*    , KTY_USTAT_TYP               SMALLINT NOT NULL
CCC004*    , USTAT_LAST_UPD              TIMESTAMP NOT NULL
CCC004*    , USTAT_DAT_BIS               DATE NOT NULL
CCC004*    , USTAT_AEND_DURCH            CHAR(08) NOT NULL
CCC004*    , ZUGANG                      TIMESTAMP NOT NULL
CCC004*    , VON                         TIMESTAMP NOT NULL
CCC004*    , BIS                         TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0125                           *
      *****************************************************************
 
       01  VIVB0125-GESAMT.
        05 NI-VIVB0125-TAB.
           10 NI-VIVB0125-ELE      PIC S9(04)  BINARY OCCURS  10.
        05 NI-VIVB0125             REDEFINES   NI-VIVB0125-TAB.
           10 NI-ST-NR                    PIC S9(04)       BINARY.
           10 NI-USTAT-DAT-VON            PIC S9(04)       BINARY.
           10 NI-KNR-USTAT-TYP            PIC S9(04)       BINARY.
           10 NI-KTY-USTAT-TYP            PIC S9(04)       BINARY.
           10 NI-USTAT-LAST-UPD           PIC S9(04)       BINARY.
           10 NI-USTAT-DAT-BIS            PIC S9(04)       BINARY.
           10 NI-USTAT-AEND-DURCH         PIC S9(04)       BINARY.
           10 NI-ZUGANG                   PIC S9(04)       BINARY.
           10 NI-VON                      PIC S9(04)       BINARY.
           10 NI-BIS                      PIC S9(04)       BINARY.
 
        05 DCL-VIVB0125.
           10 ST-NR                       PIC S9(09)       BINARY.
           10 USTAT-DAT-VON               PIC  X(26).
           10 KNR-USTAT-TYP               PIC S9(04)       BINARY.
           10 KTY-USTAT-TYP               PIC S9(04)       BINARY.
           10 USTAT-LAST-UPD              PIC  X(26).
           10 USTAT-DAT-BIS               PIC  X(10).
           10 USTAT-AEND-DURCH            PIC  X(08).
           10 ZUGANG                      PIC  X(26).
           10 VON                         PIC  X(26).
           10 BIS                         PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    *
      *****************************************************************

