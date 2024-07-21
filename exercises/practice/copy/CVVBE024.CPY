      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.02.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBE024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBE024.CPv  $
      *
      *   Rev 2.1   Feb 19 2009 12:49:36   t13ex43
      *ASL00326: PGM - Dialogänderung VB 32
      *
      *   Rev 2.0   Feb 10 2009 09:39:38   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * DCLGEN TABLE(VIVBE024)                                        *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBE024))             *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS  *
      *****************************************************************
CCC005*    EXEC SQL DECLARE VIVBE024 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      VSWZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      VSWZ_ZUSAGE_DAT                DATE NOT NULL,
CCC004*      VSWZ_A_IG_DAT                  DATE,
CCC004*      VSWZ_A_GESAMT                  DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_G_DAT_VON                 DATE,
CCC004*      VSWZ_G_STZ_JAHR                DATE,
CCC004*      VSWZ_G_VERS_SUM                DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_G_STZ_ANZ                 SMALLINT NOT NULL,
CCC004*      VSWZ_G_VERS_NR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VSWZ_Z_VERS_SUM                DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_Z_VERS_NR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VSWZ_Z_DAT_VON                 DATE,
CCC004*      KTY_A_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KNR_A_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KTY_G_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KNR_G_BERECHTIG                SMALLINT NOT NULL,
CCC004*      VSWZ_DBI_DAT                   DATE,
CCC004*      VSWZ_DBI_KTO_NR                CHAR(9),
CCC004*      VSWZ_G_RV_VERS_NR              DECIMAL(15, 0),
CCC004*      VSWZ_G_RV_RENTE                DECIMAL(11, 2),
CCC004*      VSWZ_G_RV_ABF                  DECIMAL(11, 2),
CCC004*      VSWZ_G_RV_BURENTE              DECIMAL(11, 2),
CCC004*      VSWZ_Z_RV_VERS_NR              DECIMAL(15, 0),
CCC004*      VSWZ_Z_RV_RENTE                DECIMAL(11, 2),
CCC004*      VSWZ_Z_RV_ABF                  DECIMAL(11, 2),
CCC004*      VSWZ_Z_RV_BURENTE              DECIMAL(11, 2),
CCC004*      KTY_Z_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KNR_Z_BERECHTIG                SMALLINT NOT NULL,
CCC004*      VSWZ_G_VERSB_LV                DECIMAL(11, 2),
CCC004*      VSWZ_G_VERSB_RV                DECIMAL(11, 2),
CCC004*      VSWZ_Z_VERSB_LV                DECIMAL(11, 2),
CCC004*      VSWZ_Z_VERSB_RV                DECIMAL(11, 2),
CCC004*      VSWZ_AGI_KTO_NR                CHAR(10),
CCC004*      VSWZ_AGI_KTO_NR2               CHAR(10)
CCC003*    ) END-EXEC.
      *****************************************************************
      * COBOL DECLARATION FOR TABLE VIVBE024                          *
      *****************************************************************
       01  VIVBE024-GESAMT.
        05  NI-VIVBE024-TAB.
           10  NI-VIVBE024-ELE     PIC S9(04)  COMP   OCCURS  35 TIMES.
        05  NI-VIVBE024            REDEFINES   NI-VIVBE024-TAB.
           10 NI-MA-ST-NR           PIC S9(04)  COMP.
           10 NI-VSWZ-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-VSWZ-ZUSAGE-DAT    PIC S9(04)  COMP.
           10 NI-VSWZ-A-IG-DAT      PIC S9(04)  COMP.
           10 NI-VSWZ-A-GESAMT      PIC S9(04)  COMP.
           10 NI-VSWZ-G-DAT-VON     PIC S9(04)  COMP.
           10 NI-VSWZ-G-STZ-JAHR    PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERS-SUM    PIC S9(04)  COMP.
           10 NI-VSWZ-G-STZ-ANZ     PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERS-NR     PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERS-SUM    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERS-NR     PIC S9(04)  COMP.
           10 NI-VSWZ-Z-DAT-VON     PIC S9(04)  COMP.
           10 NI-KTY-A-BERECHTIG    PIC S9(04)  COMP.
           10 NI-KNR-A-BERECHTIG    PIC S9(04)  COMP.
           10 NI-KTY-G-BERECHTIG    PIC S9(04)  COMP.
           10 NI-KNR-G-BERECHTIG    PIC S9(04)  COMP.
           10 NI-VSWZ-DBI-DAT       PIC S9(04)  COMP.
           10 NI-VSWZ-DBI-KTO-NR    PIC S9(04)  COMP.
           10 NI-VSWZ-G-RV-VERS-NR  PIC S9(04)  COMP.
           10 NI-VSWZ-G-RV-RENTE    PIC S9(04)  COMP.
           10 NI-VSWZ-G-RV-ABF      PIC S9(04)  COMP.
           10 NI-VSWZ-G-RV-BURENTE  PIC S9(04)  COMP.
           10 NI-VSWZ-Z-RV-VERS-NR  PIC S9(04)  COMP.
           10 NI-VSWZ-Z-RV-RENTE    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-RV-ABF      PIC S9(04)  COMP.
           10 NI-VSWZ-Z-RV-BURENTE  PIC S9(04)  COMP.
           10 NI-KTY-Z-BERECHTIG    PIC S9(04)  COMP.
           10 NI-KNR-Z-BERECHTIG    PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERSB-LV    PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERSB-RV    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERSB-LV    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERSB-RV    PIC S9(04)  COMP.
           10 NI-VSWZ-AGI-KTO-NR    PIC S9(04)  COMP.
           10 NI-VSWZ-AGI-KTO-NR2   PIC S9(04)  COMP.
        05  DCL-VIVBE024.
           10 MA-ST-NR          PIC S9(9)     COMP       VALUE ZERO.
           10 VSWZ-LAST-UPDATE  PIC X(26)                VALUE SPACE.
           10 VSWZ-ZUSAGE-DAT   PIC X(10)                VALUE SPACE.
           10 VSWZ-A-IG-DAT     PIC X(10)                VALUE SPACE.
           10 VSWZ-A-GESAMT     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-G-DAT-VON    PIC X(10)                VALUE SPACE.
           10 VSWZ-G-STZ-JAHR   PIC X(10)                VALUE SPACE.
           10 VSWZ-G-VERS-SUM   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-G-STZ-ANZ    PIC S9(4)     COMP       VALUE ZERO.
           10 VSWZ-G-VERS-NR    PIC S9(15)    COMP-3     VALUE ZERO.
           10 VSWZ-Z-VERS-SUM   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-VERS-NR    PIC S9(15)    COMP-3     VALUE ZERO.
           10 VSWZ-Z-DAT-VON    PIC X(10)                VALUE SPACE.
           10 KTY-A-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 KNR-A-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 KTY-G-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 KNR-G-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 VSWZ-DBI-DAT      PIC X(10)                VALUE SPACE.
           10 VSWZ-DBI-KTO-NR   PIC X(09)                VALUE SPACE.
           10 VSWZ-G-RV-VERS-NR PIC S9(15)    COMP-3     VALUE ZERO.
           10 VSWZ-G-RV-RENTE   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-G-RV-ABF     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-G-RV-BURENTE PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-RV-VERS-NR PIC S9(15)    COMP-3     VALUE ZERO.
           10 VSWZ-Z-RV-RENTE   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-RV-ABF     PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-RV-BURENTE PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 KTY-Z-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 KNR-Z-BERECHTIG   PIC S9(4)     COMP       VALUE ZERO.
           10 VSWZ-G-VERSB-LV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-G-VERSB-RV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-VERSB-LV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-Z-VERSB-RV   PIC S9(09)V99 COMP-3     VALUE ZERO.
           10 VSWZ-AGI-KTO-NR   PIC X(10)                VALUE SPACE.
           10 VSWZ-AGI-KTO-NR2  PIC X(10)                VALUE SPACE.
      *****************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35     *
      *****************************************************************
