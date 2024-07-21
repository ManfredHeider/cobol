      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0022.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTAN0022.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:14:28   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:59:02   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:07:08   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 09:35:02   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0022                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0022 DB              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01211!08.06.15!2.1  !NEU                        !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
 
CCC005*    EXEC SQL DECLARE TBAN0022 TABLE
CCC004*    ( DB_HVANTNUM                    CHAR(9) NOT NULL,
CCC004*      DB_PIN                         INTEGER NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      DB_VB_GES                      SMALLINT NOT NULL,
CCC004*      DB_VDABVERM                    INTEGER NOT NULL,
CCC004*      DB_VDABVERMPZ                  CHAR(1) NOT NULL,
CCC004*      DB_AFO_34C_ZEICHN              INTEGER NOT NULL,
CCC004*      DB_VDVMNR                      INTEGER NOT NULL,
CCC004*      DB_VDVMNRPZ                    CHAR(1) NOT NULL,
CCC004*      DB_VDVMZUNA                    CHAR(26) NOT NULL,
CCC004*      DB_VDVMVORNA                   CHAR(26) NOT NULL,
CCC004*      DB_VDDIREKT                    SMALLINT NOT NULL,
CCC004*      DB_PDVNZUNA                    CHAR(26) NOT NULL,
CCC004*      DB_PDVNVORNA                   CHAR(26) NOT NULL,
CCC004*      DB_PDVNPLZ                     CHAR(6) NOT NULL,
CCC004*      DB_PDVNORT                     CHAR(30) NOT NULL,
CCC004*      DB_PDVNLKZ                     CHAR(3) NOT NULL,
CCC004*      DB_PROD1                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD1                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE1                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT1                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD1                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD2                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD2                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE2                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT2                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD2                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD3                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD3                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE3                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT3                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD3                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD4                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD4                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE4                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT4                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD4                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD5                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD5                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE5                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT5                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD5                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD6                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD6                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE6                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT6                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD6                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD7                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD7                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE7                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT7                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD7                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD8                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD8                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE8                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT8                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD8                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_PROD9                       SMALLINT NOT NULL,
CCC004*      DB_BETRAG_PROD9                DECIMAL(11, 2) NOT NULL,
CCC004*      DB_ZAHLWEISE9                  SMALLINT NOT NULL,
CCC004*      DB_LAUFZEIT9                   SMALLINT NOT NULL,
CCC004*      DB_EINH_PROD9                  DECIMAL(7, 1) NOT NULL,
CCC004*      DB_VDABLEIN                    DECIMAL(7, 1) NOT NULL,
CCC004*      DB_DYNAMIK                     DECIMAL(6, 3) NOT NULL,
CCC004*      DB_HAND                        CHAR(1) NOT NULL,
CCC004*      DB_ANTRAG_DATUM                DATE NOT NULL,
CCC004*      DB_KD_BEST_ID                  DECIMAL(18, 0) NOT NULL,
CCC004*      DB_ESIGN                       CHAR(1) NOT NULL,
CCC004*      DB_VERSION                     SMALLINT NOT NULL,
CCC004*      DB_ERSTELLUNG                  TIMESTAMP NOT NULL,
CCC004*      DB_KENNUNG_ERST                CHAR(8) NOT NULL,
CCC004*      DB_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      DB_KENNUNG_AEND                CHAR(8) NOT NULL,
CCC004*      DB_XGDV                        VARCHAR(20000) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0022                           *
      ******************************************************************
       01  DCLTBAN0022.
           10 DB-HVANTNUM          PIC X(9).
           10 DB-PIN               PIC S9(9) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 DB-VB-GES            PIC S9(4) USAGE COMP.
           10 DB-VDABVERM          PIC S9(9) USAGE COMP.
           10 DB-VDABVERMPZ        PIC X(1).
           10 DB-AFO-34C-ZEICHN    PIC S9(9) USAGE COMP.
           10 DB-VDVMNR            PIC S9(9) USAGE COMP.
           10 DB-VDVMNRPZ          PIC X(1).
           10 DB-VDVMZUNA          PIC X(26).
           10 DB-VDVMVORNA         PIC X(26).
           10 DB-VDDIREKT          PIC S9(4) USAGE COMP.
           10 DB-PDVNZUNA          PIC X(26).
           10 DB-PDVNVORNA         PIC X(26).
           10 DB-PDVNPLZ           PIC X(6).
           10 DB-PDVNORT           PIC X(30).
           10 DB-PDVNLKZ           PIC X(3).
           10 DB-PROD1             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD1      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE1        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT1         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD1        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD2             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD2      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE2        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT2         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD2        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD3             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD3      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE3        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT3         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD3        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD4             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD4      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE4        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT4         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD4        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD5             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD5      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE5        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT5         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD5        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD6             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD6      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE6        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT6         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD6        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD7             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD7      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE7        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT7         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD7        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD8             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD8      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE8        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT8         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD8        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-PROD9             PIC S9(4) USAGE COMP.
           10 DB-BETRAG-PROD9      PIC S9(9)V9(2) USAGE COMP-3.
           10 DB-ZAHLWEISE9        PIC S9(4) USAGE COMP.
           10 DB-LAUFZEIT9         PIC S9(4) USAGE COMP.
           10 DB-EINH-PROD9        PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-VDABLEIN          PIC S9(6)V9(1) USAGE COMP-3.
           10 DB-DYNAMIK           PIC S9(3)V9(3) USAGE COMP-3.
           10 DB-HAND              PIC X(1).
           10 DB-ANTRAG-DATUM      PIC X(10).
           10 DB-KD-BEST-ID        PIC S9(18)V USAGE COMP-3.
           10 DB-ESIGN             PIC X(1).
           10 DB-VERSION           PIC S9(4) USAGE COMP.
           10 DB-ERSTELLUNG        PIC X(26).
           10 DB-KENNUNG-ERST      PIC X(8).
           10 DB-LAST-UPDATE       PIC X(26).
           10 DB-KENNUNG-AEND      PIC X(8).
           10 DB-XGDV.
              49 DB-XGDV-LEN       PIC S9(4) USAGE COMP.
              49 DB-XGDV-TEXT      PIC X(20000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 74      *
      ******************************************************************
 


