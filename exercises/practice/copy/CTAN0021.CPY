      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0021.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTAN0021.CPv  $
      *
      *   Rev 2.3   Feb 25 2016 10:14:18   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" + 
      *Elektra 
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.2   Feb 12 2016 07:58:54   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.1   Aug 26 2015 12:07:00   t13ae34
      *AAN01211: Elektra - AGI, CKV, Deutsche Bank
      *
      *   Rev 2.0   Jun 08 2015 09:34:14   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0021                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0021 AGI              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01211!08.06.15!2.1  !NEU                        !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0021 TABLE
CCC004*    ( AGI_HVANTNUM                   CHAR(9) NOT NULL,
CCC004*      AGI_PIN                        INTEGER NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      AGI_VB_GES                     SMALLINT NOT NULL,
CCC004*      AGI_VDABVERM                   INTEGER NOT NULL,
CCC004*      AGI_VDABVERMPZ                 CHAR(1) NOT NULL,
CCC004*      AGI_AFO_34C_ZEICHN             INTEGER NOT NULL,
CCC004*      AGI_VDVMNR                     INTEGER NOT NULL,
CCC004*      AGI_VDVMNRPZ                   CHAR(1) NOT NULL,
CCC004*      AGI_VDVMZUNA                   CHAR(26) NOT NULL,
CCC004*      AGI_VDVMVORNA                  CHAR(26) NOT NULL,
CCC004*      AGI_VDDIREKT                   SMALLINT NOT NULL,
CCC004*      AGI_PDVNZUNA                   CHAR(26) NOT NULL,
CCC004*      AGI_PDVNVORNA                  CHAR(26) NOT NULL,
CCC004*      AGI_PDVNPLZ                    CHAR(6) NOT NULL,
CCC004*      AGI_PDVNORT                    CHAR(30) NOT NULL,
CCC004*      AGI_PDVNLKZ                    CHAR(3) NOT NULL,
CCC004*      AGI_PROD1                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD1               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE1                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT1                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD1                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD2                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD2               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE2                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT2                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD2                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD3                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD3               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE3                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT3                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD3                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD4                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD4               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE4                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT4                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD4                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD5                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD5               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE5                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT5                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD5                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD6                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD6               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE6                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT6                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD6                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD7                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD7               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE7                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT7                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD7                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD8                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD8               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE8                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT8                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD8                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_PROD9                      SMALLINT NOT NULL,
CCC004*      AGI_BETRAG_PROD9               DECIMAL(11, 2) NOT NULL,
CCC004*      AGI_ZAHLWEISE9                 SMALLINT NOT NULL,
CCC004*      AGI_LAUFZEIT9                  SMALLINT NOT NULL,
CCC004*      AGI_EINH_PROD9                 DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_VDABLEIN                   DECIMAL(7, 1) NOT NULL,
CCC004*      AGI_DYNAMIK                    DECIMAL(6, 3) NOT NULL,
CCC004*      AGI_HAND                       CHAR(1) NOT NULL,
CCC004*      AGI_ANTRAG_DATUM               DATE NOT NULL,
CCC004*      AGI_KD_BEST_ID                 DECIMAL(18, 0) NOT NULL,
CCC004*      AGI_ESIGN                      CHAR(1) NOT NULL,
CCC004*      AGI_VERSION                    SMALLINT NOT NULL,
CCC004*      AGI_ERSTELLUNG                 TIMESTAMP NOT NULL,
CCC004*      AGI_KENNUNG_ERST               CHAR(8) NOT NULL,
CCC004*      AGI_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      AGI_KENNUNG_AEND               CHAR(8) NOT NULL,
CCC004*      AGI_XGDV                       VARCHAR(20000) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0021                           *
      ******************************************************************
       01  DCLTBAN0021.
           10 AGI-HVANTNUM         PIC X(9).
           10 AGI-PIN              PIC S9(9) USAGE COMP.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 AGI-VB-GES           PIC S9(4) USAGE COMP.
           10 AGI-VDABVERM         PIC S9(9) USAGE COMP.
           10 AGI-VDABVERMPZ       PIC X(1).
           10 AGI-AFO-34C-ZEICHN   PIC S9(9) USAGE COMP.
           10 AGI-VDVMNR           PIC S9(9) USAGE COMP.
           10 AGI-VDVMNRPZ         PIC X(1).
           10 AGI-VDVMZUNA         PIC X(26).
           10 AGI-VDVMVORNA        PIC X(26).
           10 AGI-VDDIREKT         PIC S9(4) USAGE COMP.
           10 AGI-PDVNZUNA         PIC X(26).
           10 AGI-PDVNVORNA        PIC X(26).
           10 AGI-PDVNPLZ          PIC X(6).
           10 AGI-PDVNORT          PIC X(30).
           10 AGI-PDVNLKZ          PIC X(3).
           10 AGI-PROD1            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD1     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE1       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT1        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD1       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD2            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD2     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE2       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT2        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD2       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD3            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD3     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE3       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT3        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD3       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD4            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD4     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE4       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT4        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD4       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD5            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD5     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE5       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT5        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD5       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD6            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD6     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE6       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT6        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD6       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD7            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD7     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE7       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT7        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD7       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD8            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD8     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE8       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT8        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD8       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-PROD9            PIC S9(4) USAGE COMP.
           10 AGI-BETRAG-PROD9     PIC S9(9)V9(2) USAGE COMP-3.
           10 AGI-ZAHLWEISE9       PIC S9(4) USAGE COMP.
           10 AGI-LAUFZEIT9        PIC S9(4) USAGE COMP.
           10 AGI-EINH-PROD9       PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-VDABLEIN         PIC S9(6)V9(1) USAGE COMP-3.
           10 AGI-DYNAMIK          PIC S9(3)V9(3) USAGE COMP-3.
           10 AGI-HAND             PIC X(1).
           10 AGI-ANTRAG-DATUM     PIC X(10).
           10 AGI-KD-BEST-ID       PIC S9(18)V USAGE COMP-3.
           10 AGI-ESIGN            PIC X(1).
           10 AGI-VERSION          PIC S9(4) USAGE COMP.
           10 AGI-ERSTELLUNG       PIC X(26).
           10 AGI-KENNUNG-ERST     PIC X(8).
           10 AGI-LAST-UPDATE      PIC X(26).
           10 AGI-KENNUNG-AEND     PIC X(8).
           10 AGI-XGDV.
              49 AGI-XGDV-LEN      PIC S9(4) USAGE COMP.
              49 AGI-XGDV-TEXT     PIC X(20000).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 74      *
      ******************************************************************
 


