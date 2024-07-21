      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.02.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBC024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBC024.CPv  $
      *
      *   Rev 2.1   11 Apr 2007 16:03:50   t13ae30
      *ASL00247: Verlängerung der Zusatzleistungen und VSW
      *
      *   Rev 2.0   Feb 21 2007 15:08:44   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBC024)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC024))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBC024 TABLE
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
CCC004*      VSWZ_Z_VERSB_RV                DECIMAL(11, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBC024                           *
      ******************************************************************
       01  VIVBC024-GESAMT.
        05  NI-VIVBC024-TAB.
           10  NI-VIVBC024-ELE     PIC S9(04)  COMP   OCCURS  33 TIMES.
        05  NI-VIVBC024            REDEFINES   NI-VIVBC024-TAB.
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
        05  DCL-VIVBC024.
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
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 33      *
      ******************************************************************
