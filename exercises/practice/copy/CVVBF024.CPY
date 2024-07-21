      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBF024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBF024.CPv  $
      *
      *   Rev 2.1   Nov 24 2011 08:43:24   t13ex43
      *ASL00405: PGM - VASP im VB32
      *
      *   Rev 2.0   Nov 17 2011 11:18:42   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    18.11.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00405!18.11.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBF024) VSWZ                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBF024)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBF024 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , VSWZ_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , VSWZ_ZUSAGE_DAT             DATE NOT NULL
CCC004*    , VSWZ_A_IG_DAT               DATE
CCC004*    , VSWZ_A_GESAMT               DECIMAL(11, 2) NOT NULL
CCC004*    , VSWZ_G_DAT_VON              DATE
CCC004*    , VSWZ_G_STZ_JAHR             DATE
CCC004*    , VSWZ_G_VERS_SUM             DECIMAL(11, 2) NOT NULL
CCC004*    , VSWZ_G_STZ_ANZ              SMALLINT NOT NULL
CCC004*    , VSWZ_G_VERS_NR              DECIMAL(15, 0) NOT NULL
CCC004*    , VSWZ_Z_VERS_SUM             DECIMAL(11, 2) NOT NULL
CCC004*    , VSWZ_Z_VERS_NR              DECIMAL(15, 0) NOT NULL
CCC004*    , VSWZ_Z_DAT_VON              DATE
CCC004*    , KTY_A_BERECHTIG             SMALLINT NOT NULL
CCC004*    , KNR_A_BERECHTIG             SMALLINT NOT NULL
CCC004*    , KTY_G_BERECHTIG             SMALLINT NOT NULL
CCC004*    , KNR_G_BERECHTIG             SMALLINT NOT NULL
CCC004*    , VSWZ_DBI_DAT                DATE
CCC004*    , VSWZ_DBI_KTO_NR             CHAR(09)
CCC004*    , VSWZ_G_RV_VERS_NR           DECIMAL(15, 0)
CCC004*    , VSWZ_G_RV_RENTE             DECIMAL(11, 2)
CCC004*    , VSWZ_G_RV_ABF               DECIMAL(11, 2)
CCC004*    , VSWZ_G_RV_BURENTE           DECIMAL(11, 2)
CCC004*    , VSWZ_Z_RV_VERS_NR           DECIMAL(15, 0)
CCC004*    , VSWZ_Z_RV_RENTE             DECIMAL(11, 2)
CCC004*    , VSWZ_Z_RV_ABF               DECIMAL(11, 2)
CCC004*    , VSWZ_Z_RV_BURENTE           DECIMAL(11, 2)
CCC004*    , KTY_Z_BERECHTIG             SMALLINT NOT NULL
CCC004*    , KNR_Z_BERECHTIG             SMALLINT NOT NULL
CCC004*    , VSWZ_G_VERSB_LV             DECIMAL(11, 2)
CCC004*    , VSWZ_G_VERSB_RV             DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VERSB_LV             DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VERSB_RV             DECIMAL(11, 2)
CCC004*    , VSWZ_AGI_KTO_NR             CHAR(10)
CCC004*    , VSWZ_AGI_KTO_NR2            CHAR(10)
CCC004*    , VSWZ_G_VA_VERS_NR           DECIMAL(15, 0)
CCC004*    , VSWZ_G_VA_VERSB             DECIMAL(11, 2)
CCC004*    , VSWZ_G_VA_VSUM              DECIMAL(11, 2)
CCC004*    , VSWZ_G_VA_RENTE             DECIMAL(11, 2)
CCC004*    , VSWZ_G_VA_BURENTE           DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VA_VERS_NR           DECIMAL(15, 0)
CCC004*    , VSWZ_Z_VA_VERSB             DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VA_VSUM              DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VA_RENTE             DECIMAL(11, 2)
CCC004*    , VSWZ_Z_VA_BURENTE           DECIMAL(11, 2)
CCC004*    , VSWZ_A_VA_VERS_NR           DECIMAL(15, 0)
CCC004*    , VSWZ_A_VA_VERSB             DECIMAL(11, 2)
CCC004*    , VSWZ_A_VA_VSUM              DECIMAL(11, 2)
CCC004*    , VSWZ_A_VA_RENTE             DECIMAL(11, 2)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBF024                           *
      *****************************************************************

       01  VIVBF024-GESAMT.
        05 NI-VIVBF024-TAB.
           10 NI-VIVBF024-ELE      PIC S9(04)  BINARY OCCURS  49.
        05 NI-VIVBF024             REDEFINES   NI-VIVBF024-TAB.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-VSWZ-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-VSWZ-ZUSAGE-DAT          PIC S9(04)       BINARY.
           10 NI-VSWZ-A-IG-DAT            PIC S9(04)       BINARY.
           10 NI-VSWZ-A-GESAMT            PIC S9(04)       BINARY.
           10 NI-VSWZ-G-DAT-VON           PIC S9(04)       BINARY.
           10 NI-VSWZ-G-STZ-JAHR          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VERS-SUM          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-STZ-ANZ           PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VERS-NR           PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VERS-SUM          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VERS-NR           PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-DAT-VON           PIC S9(04)       BINARY.
           10 NI-KTY-A-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-KNR-A-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-KTY-G-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-KNR-G-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-VSWZ-DBI-DAT             PIC S9(04)       BINARY.
           10 NI-VSWZ-DBI-KTO-NR          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-RV-VERS-NR        PIC S9(04)       BINARY.
           10 NI-VSWZ-G-RV-RENTE          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-RV-ABF            PIC S9(04)       BINARY.
           10 NI-VSWZ-G-RV-BURENTE        PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-RV-VERS-NR        PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-RV-RENTE          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-RV-ABF            PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-RV-BURENTE        PIC S9(04)       BINARY.
           10 NI-KTY-Z-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-KNR-Z-BERECHTIG          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VERSB-LV          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VERSB-RV          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VERSB-LV          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VERSB-RV          PIC S9(04)       BINARY.
           10 NI-VSWZ-AGI-KTO-NR          PIC S9(04)       BINARY.
           10 NI-VSWZ-AGI-KTO-NR2         PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VA-VERS-NR        PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VA-VERSB          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VA-VSUM           PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VA-RENTE          PIC S9(04)       BINARY.
           10 NI-VSWZ-G-VA-BURENTE        PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VA-VERS-NR        PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VA-VERSB          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VA-VSUM           PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VA-RENTE          PIC S9(04)       BINARY.
           10 NI-VSWZ-Z-VA-BURENTE        PIC S9(04)       BINARY.
           10 NI-VSWZ-A-VA-VERS-NR        PIC S9(04)       BINARY.
           10 NI-VSWZ-A-VA-VERSB          PIC S9(04)       BINARY.
           10 NI-VSWZ-A-VA-VSUM           PIC S9(04)       BINARY.
           10 NI-VSWZ-A-VA-RENTE          PIC S9(04)       BINARY.

        05 DCL-VIVBF024.
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 VSWZ-LAST-UPDATE            PIC  X(26).
           10 VSWZ-ZUSAGE-DAT             PIC  X(10).
           10 VSWZ-A-IG-DAT               PIC  X(10).
           10 VSWZ-A-GESAMT               PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-DAT-VON              PIC  X(10).
           10 VSWZ-G-STZ-JAHR             PIC  X(10).
           10 VSWZ-G-VERS-SUM             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-STZ-ANZ              PIC S9(04)       BINARY.
           10 VSWZ-G-VERS-NR              PIC S9(15)       COMP-3.
           10 VSWZ-Z-VERS-SUM             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VERS-NR              PIC S9(15)       COMP-3.
           10 VSWZ-Z-DAT-VON              PIC  X(10).
           10 KTY-A-BERECHTIG             PIC S9(04)       BINARY.
           10 KNR-A-BERECHTIG             PIC S9(04)       BINARY.
           10 KTY-G-BERECHTIG             PIC S9(04)       BINARY.
           10 KNR-G-BERECHTIG             PIC S9(04)       BINARY.
           10 VSWZ-DBI-DAT                PIC  X(10).
           10 VSWZ-DBI-KTO-NR             PIC  X(09).
           10 VSWZ-G-RV-VERS-NR           PIC S9(15)       COMP-3.
           10 VSWZ-G-RV-RENTE             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-RV-ABF               PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-RV-BURENTE           PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-RV-VERS-NR           PIC S9(15)       COMP-3.
           10 VSWZ-Z-RV-RENTE             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-RV-ABF               PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-RV-BURENTE           PIC S9(09)V9(02) COMP-3.
           10 KTY-Z-BERECHTIG             PIC S9(04)       BINARY.
           10 KNR-Z-BERECHTIG             PIC S9(04)       BINARY.
           10 VSWZ-G-VERSB-LV             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-VERSB-RV             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VERSB-LV             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VERSB-RV             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-AGI-KTO-NR             PIC  X(10).
           10 VSWZ-AGI-KTO-NR2            PIC  X(10).
           10 VSWZ-G-VA-VERS-NR           PIC S9(15)       COMP-3.
           10 VSWZ-G-VA-VERSB             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-VA-VSUM              PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-VA-RENTE             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-G-VA-BURENTE           PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VA-VERS-NR           PIC S9(15)       COMP-3.
           10 VSWZ-Z-VA-VERSB             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VA-VSUM              PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VA-RENTE             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-Z-VA-BURENTE           PIC S9(09)V9(02) COMP-3.
           10 VSWZ-A-VA-VERS-NR           PIC S9(15)       COMP-3.
           10 VSWZ-A-VA-VERSB             PIC S9(09)V9(02) COMP-3.
           10 VSWZ-A-VA-VSUM              PIC S9(09)V9(02) COMP-3.
           10 VSWZ-A-VA-RENTE             PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  49                    *
      *****************************************************************
