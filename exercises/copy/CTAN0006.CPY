      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0006.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/CTAN0006.CPv  $
      *
      *   Rev 2.6   May 28 2015 09:55:44   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.5   Apr 23 2015 13:26:06   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.4   23 Nov 2007 10:38:28   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 48 DWS
      *
      *   Rev 2.3   01 Sep 2006 09:50:06   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.2   31 Aug 2006 09:44:24   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.1   Aug 06 2004 08:37:08   t13ae23
      *AAN00455: Elektronischer Antrag DWS - Programme -
      *
      *   Rev 2.0   Aug 26 2003 15:40:44   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0006                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0006 DWS              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1  !NEU                        !RAHNER   *
      * AAN00684!31.08.06!2.2  !KUNDENBESTANDS-ID FÜR AVS  !RAHNER   *
      * AAN00730!30.11.07!2.3  !ALLFINANZ                  !RAHNER   *
      * AAN01211!09.03.15!2.5  !ESIGN                      !HEIDER   *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0006 TABLE
CCC004*    (
CCC004*       DWS_HVANTNUM                   CHAR (9),
CCC004*       DWS_PIN                        INT,
CCC004*       DWS_VDABVERM                   INT,
CCC004*       DWS_LAST_UPDATE                TIMESTAMP,
CCC004*       DWS_KENNUNG_AEND               CHAR (8),
CCC004*       PGEB_GES                       SMALLINT,
CCC004*       DWS_VDABVERMPZ                 CHAR (1),
CCC004*       DWS_VDVMNR                     INT,
CCC004*       DWS_VDVMNRPZ                   CHAR (1),
CCC004*       DWS_VDVMZUNA                   CHAR (26),
CCC004*       DWS_VDVMVORNA                  CHAR (26),
CCC004*       DWS_VDDIREKT                   SMALLINT,
CCC004*       DWS_PDVNZUNA                   CHAR (26),
CCC004*       DWS_PDVNVORNA                  CHAR (26),
CCC004*       DWS_PDVNPLZ                    CHAR (6),
CCC004*       DWS_PDVNORT                    CHAR (30),
CCC004*       DWS_PDVNLKZ                    CHAR (3),
CCC004*       DWS_PROD1                      SMALLINT,
CCC004*       DWS_SUMME_PROD1                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD1                 DECIMAL (7, 1),
CCC004*       DWS_PROD2                      SMALLINT,
CCC004*       DWS_SUMME_PROD2                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD2                 DECIMAL (7, 1),
CCC004*       DWS_PROD3                      SMALLINT,
CCC004*       DWS_SUMME_PROD3                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD3                 DECIMAL (7, 1),
CCC004*       DWS_PROD4                      SMALLINT,
CCC004*       DWS_SUMME_PROD4                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD4                 DECIMAL (7, 1),
CCC004*       DWS_PROD5                      SMALLINT,
CCC004*       DWS_SUMME_PROD5                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD5                 DECIMAL (7, 1),
CCC004*       DWS_PROD6                      SMALLINT,
CCC004*       DWS_SUMME_PROD6                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD6                 DECIMAL (7, 1),
CCC004*       DWS_PROD7                      SMALLINT,
CCC004*       DWS_SUMME_PROD7                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD7                 DECIMAL (7, 1),
CCC004*       DWS_PROD8                      SMALLINT,
CCC004*       DWS_SUMME_PROD8                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD8                 DECIMAL (7, 1),
CCC004*       DWS_PROD9                      SMALLINT,
CCC004*       DWS_SUMME_PROD9                DECIMAL (11, 2),
CCC004*       DWS_EINH_PROD9                 DECIMAL (7, 1),
CCC004*       DWS_VDABLEIN                   DECIMAL (7, 1),
CCC004*       DWS_HAND                       CHAR (1),
CCC004*       DWS_AFO_34C_ZEICHN               INT,
CCC004*       DWS_ANTRAG_DATUM               DATE,
CCC004*       DWS_ERSTELLUNG                 TIMESTAMP,
CCC004*       DWS_KENNUNG_ERST               CHAR (8),
CCC004*       DWS_KD_BEST_ID                 DECIMAL (18, 0),
CCC004*       DWS_VB_GES                     SMALLINT,
CCC004*       DWS_ESIGN                      CHAR (1),
CCC004*       DWS_XGDV                       VARCHAR(20000) NOT NULL,
CCC004*       DWS_ZAHLWEISE1                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT1                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE2                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT2                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE3                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT3                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE4                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT4                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE5                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT5                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE6                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT6                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE7                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT7                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE8                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT8                  SMALLINT NOT NULL,
CCC004*       DWS_ZAHLWEISE9                 SMALLINT NOT NULL,
CCC004*       DWS_LAUFZEIT9                  SMALLINT NOT NULL
CCC004*    )
CCC003*    END-EXEC.
 
       01  TBAN0006-GESAMT.
           05 DCL-TBAN0006.
              10 DWS-HVANTNUM         PIC X(9).
              10 DWS-PIN              PIC S9(9) USAGE COMP.
              10 DWS-VDABVERM         PIC S9(9) USAGE COMP.
              10 DWS-LAST-UPDATE      PIC X(26).
              10 DWS-KENNUNG-AEND     PIC X(8).
              10 PGEB-GES             PIC S9(4) USAGE COMP.
              10 DWS-VDABVERMPZ       PIC X(1).
              10 DWS-VDVMNR           PIC S9(9) USAGE COMP.
              10 DWS-VDVMNRPZ         PIC X(1).
              10 DWS-VDVMZUNA         PIC X(26).
              10 DWS-VDVMVORNA        PIC X(26).
              10 DWS-VDDIREKT         PIC S9(4) USAGE COMP.
              10 DWS-PDVNZUNA         PIC X(26).
              10 DWS-PDVNVORNA        PIC X(26).
              10 DWS-PDVNPLZ          PIC X(6).
              10 DWS-PDVNORT          PIC X(30).
              10 DWS-PDVNLKZ          PIC X(3).
              10 DWS-PROD1            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD1      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD1       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD2            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD2      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD2       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD3            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD3      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD3       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD4            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD4      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD4       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD5            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD5      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD5       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD6            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD6      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD6       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD7            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD7      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD7       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD8            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD8      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD8       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-PROD9            PIC S9(4) USAGE COMP.
              10 DWS-SUMME-PROD9      PIC S9(9)V9(2) USAGE COMP-3.
              10 DWS-EINH-PROD9       PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-VDABLEIN         PIC S9(6)V9(1) USAGE COMP-3.
              10 DWS-HAND             PIC X(1).
              10 DWS-AFO-34C-ZEICHN   PIC S9(9) USAGE COMP.
              10 DWS-ANTRAG-DATUM     PIC X(10).
              10 DWS-VERSION          PIC S9(4) USAGE COMP.
              10 DWS-ERSTELLUNG       PIC X(26).
              10 DWS-KENNUNG-ERST     PIC X(8).
              10 DWS-KD-BEST-ID       PIC S9(18)V USAGE COMP-3.
              10 DWS-VB-GES           PIC S9(4) USAGE COMP.
310806        10 DWS-ESIGN            PIC X(1).
301107        10 DWS-XGDV.
090315           49 DWS-XGDV-LEN      PIC S9(4) USAGE COMP.
090315           49 DWS-XGDV-TEXT     PIC X(20000).
090315        10 DWS-ZAHLWEISE1       PIC S9(4) USAGE COMP.
090315        10 DWS-LAUFZEIT1        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE2       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT2        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE3       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT3        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE4       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT4        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE5       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT5        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE6       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT6        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE7       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT7        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE8       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT8        PIC S9(4) USAGE COMP.
              10 DWS-ZAHLWEISE9       PIC S9(4) USAGE COMP.
              10 DWS-LAUFZEIT9        PIC S9(4) USAGE COMP.
 
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 73      *
      ******************************************************************
 

