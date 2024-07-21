      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0003.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/CTAN0003.CPv  $
      *
      *   Rev 2.14   May 28 2015 09:55:20   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.13   Apr 23 2015 13:25:42   t13ae34
      *AAN01211: Elektronischer Antrag - Änderungen eingereichtes G
      *eschäft - Einpflegen in Vorschlagsdaten
      *
      *   Rev 2.12   Oct 24 2013 11:02:12   t13ae23
      *AAN01073: Projekt SEPA - Realisierung AN Deutschland
      *AMV +
      *ADVC (Dialog)
      *
      *   Rev 2.11   Jun 19 2009 09:16:28   t13ae23
      *AAN00884: Tarifänderung Kfz
      *
      *   Rev 2.10   Mar 24 2009 10:56:56   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.9   Mar 11 2009 16:12:46   t13ae23
      *AAN00843: Umstellung XGDV-Daten (Antragsdatensätze)
      *
      *   Rev 2.8   May 28 2008 08:50:24   t13ae34
      *AAN00801: POA-Antrag
      *
      *   Rev 2.7   23 Nov 2007 10:17:42   t13ae23
      *AAN00730: VRA - Kennzeichen Vertriebs-Ges in elektr. Daten G
      *es. 10 AM-Vers -
      *
      *   Rev 2.6   01 Sep 2006 09:49:36   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.5   31 Aug 2006 09:43:58   T13AE23
      *AAN00684: elektronische Anträge aus Online-KI
      *
      *   Rev 2.4   Nov 26 2004 15:29:52   t13ae23
      *AAN00529: POA AMVers. Optimierung Quote
      *
      *   Rev 2.3   Sep 30 2002 14:54:30   t13ae34
      *AAN00397-POA Antragsdatum
      *
      *   Rev 2.2   Jun 03 2002 18:10:54   t13ae34
      *AAN00357 - POA-AMV: 10 statt 3 Vorverträge
      *
      *   Rev 2.1   Apr 23 2002 16:59:42   t13ae34
      *AAN00357-POA-AMV
      *
      *   Rev 2.0   Feb 27 1999 11:05:12   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   09 Sep 1997 09:13:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0003                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0003 POAA             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN00357!18.03.02!2.1  !NEU                        !HEIDER   *
      * AAN00357!24.05.02!2.2  !VORVERTRÄGE: 10 STATT 3    !HEIDER   *
      * AAN00397!30.09.02!2.3  !POA_ANTRAG_DATUM           !HEIDER   *
      * AAN00529!26.11.04!2.4  !OPTIMIERUNG POA-QUOTE      !RAHNER   *
      * AAN00684!31.08.06!2.5  !KUNDENBESTANDS-ID FÜR AVS  !RAHNER   *
      * AAN00730!30.11.07!2.6  !ALLFINANZ                  !RAHNER   *
      * AAN00801!27.05.08!2.7  !KENNZ. WIDERSPRUCH         !HEIDER   *
      * AAN00843!11.03.09!2.8  !VERGRÖßERN XGDV-FELD POA   !RAHNER   *
      * AAN00884!17.06.09!2.11 !- KZ FÜR TARIFÄNDERUNG     !         *
      *         !        !     !- FELDER FÜR DIREKT POA    !RAHNER   *
      * AAN01073!24.10.13!2.12 !SEPA                       !RAHNER   *
      * AAN01211!04.03.15!2.13 !ESIGN                      !HEIDER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
CCC005*    EXEC SQL DECLARE TBAN0003 TABLE
CCC004*    (
CCC004*       POA_PIN                        INT,
CCC004*       POA_HVANTNUM                   CHAR (9),
CCC004*       POA_HVANTNUM_LFD               CHAR (1),
CCC004*       POA_VDABVERM                   INT,
CCC004*       POA_LAST_UPDATE                TIMESTAMP,
CCC004*       POA_KENNUNG_AEND               CHAR (8),
CCC004*       PGEB_GES                       SMALLINT,
CCC004*       POA_VDABVERMPZ                 CHAR (1),
CCC004*       POA_VDVMNR                     INT,
CCC004*       POA_VDVMNRPZ                   CHAR (1),
CCC004*       POA_VDVMZUNA                   CHAR (26),
CCC004*       POA_VDVMVORNA                  CHAR (26),
CCC004*       POA_VDDIREKT                   SMALLINT,
CCC004*       POA_PDVNZUNA                   CHAR (26),
CCC004*       POA_PDVNVORNA                  CHAR (26),
CCC004*       POA_PDVNPLZ                    CHAR (6),
CCC004*       POA_PDVNORT                    CHAR (30),
CCC004*       POA_PDVNLKZ                    CHAR (3),
CCC004*       POA_VBEGINN                    DATE,
CCC004*       POA_VABLAUF                    DATE,
CCC004*       POA_KNR_ZAHLW                  SMALLINT,
CCC004*       POA_KTY_ZAHLW                  SMALLINT,
CCC004*       POA_VDPRODKEY1                 SMALLINT,
CCC004*       POA_VDABLEIN1                  DECIMAL (7, 1),
CCC004*       POA_BEITRAGBR1                 DECIMAL (11, 2),
CCC004*       POA_BEITRAGNE1                 DECIMAL (11, 2),
CCC004*       POA_VDPRODKEY2                 SMALLINT,
CCC004*       POA_VDABLEIN2                  DECIMAL (7, 1),
CCC004*       POA_BEITRAGBR2                 DECIMAL (11, 2),
CCC004*       POA_BEITRAGNE2                 DECIMAL (11, 2),
CCC004*       POA_VDPRODKEY3                 SMALLINT,
CCC004*       POA_VDABLEIN3                  DECIMAL (7, 1),
CCC004*       POA_BEITRAGBR3                 DECIMAL (11, 2),
CCC004*       POA_BEITRAGNE3                 DECIMAL (11, 2),
CCC004*       POA_VDPRODKEY4                 SMALLINT,
CCC004*       POA_VDABLEIN4                  DECIMAL (7, 1),
CCC004*       POA_BEITRAGBR4                 DECIMAL (11, 2),
CCC004*       POA_BEITRAGNE4                 DECIMAL (11, 2),
CCC004*       POA_VDPRODKEY5                 SMALLINT,
CCC004*       POA_VDABLEIN5                  DECIMAL (7, 1),
CCC004*       POA_BEITRAGBR5                 DECIMAL (11, 2),
CCC004*       POA_BEITRAGNE5                 DECIMAL (11, 2),
CCC004*       POA_KTY_ZWEIG                  SMALLINT,
CCC004*       POA_VOR_VNR1                   CHAR (14),
CCC004*       POA_KNR_ZWEIG1                 SMALLINT,
CCC004*       POA_VOR_VNR2                   CHAR (14),
CCC004*       POA_KNR_ZWEIG2                 SMALLINT,
CCC004*       POA_VOR_VNR3                   CHAR (14),
CCC004*       POA_KNR_ZWEIG3                 SMALLINT,
CCC004*       POA_VOR_VNR4                   CHAR (14),
CCC004*       POA_KNR_ZWEIG4                 SMALLINT,
CCC004*       POA_VOR_VNR5                   CHAR (14),
CCC004*       POA_KNR_ZWEIG5                 SMALLINT,
CCC004*       POA_VOR_VNR6                   CHAR (14),
CCC004*       POA_KNR_ZWEIG6                 SMALLINT,
CCC004*       POA_VOR_VNR7                   CHAR (14),
CCC004*       POA_KNR_ZWEIG7                 SMALLINT,
CCC004*       POA_VOR_VNR8                   CHAR (14),
CCC004*       POA_KNR_ZWEIG8                 SMALLINT,
CCC004*       POA_VOR_VNR9                   CHAR (14),
CCC004*       POA_KNR_ZWEIG9                 SMALLINT,
CCC004*       POA_VOR_VNR10                  CHAR (14),
CCC004*       POA_KNR_ZWEIG10                SMALLINT,
CCC004*       POA_HAND                       CHAR (1),
CCC004*       POA_ERSTELLUNG                 TIMESTAMP,
CCC004*       POA_KENNUNG_ERST               CHAR (8),
110309*       POA_XGDV                       VARCHAR (7793),
CCC004*       POA_XGDV                       VARCHAR (25000),
CCC004*       POA_ANTRAG_DATUM               DATE,
CCC004*       POA_VBEGINN_E                  DATE,
CCC004*       POA_VABLAUF_E                  DATE,
CCC004*       POA_KNR_ZAHLW_E                SMALLINT,
CCC004*       POA_KTY_ZAHLW_E                SMALLINT,
CCC004*       POA_PDVN_BLZ                   INT,
CCC004*       POA_PDVN_BLZ_E                 INT,
CCC004*       POA_PDVN_KONTONR               INT,
CCC004*       POA_PDVN_KONTONR_E             INT,
CCC004*       POA_PDVN_BEGINKM               INT,
CCC004*       POA_PDVN_BEGINKM_E             INT,
CCC004*       POA_PDVN_FGESTNR               CHAR (17),
CCC004*       POA_PDVN_FGESTNR_E             CHAR (17),
CCC004*       POA_PDVN_AMTLKZ                CHAR (11),
CCC004*       POA_PDVN_AMTLKZ_E              CHAR (11),
CCC004*       POA_PDVN_JAHRKM                INT,
CCC004*       POA_PDVN_JAHRKM_E              INT,
CCC004*       POA_KD_BEST_ID                 DECIMAL (18, 0),
CCC004*       POA_VB_GES                     SMALLINT,
CCC004*       POA_WIDERSPR                   CHAR (1),
CCC004*       POA_WIDERSPR_E                 CHAR (1),
CCC004*       POA_PIN_PZ                     CHAR (2),
CCC004*       POA_BARCODE                    CHAR (10),
CCC004*       POA_SCHNELLANTR                CHAR (1),
CCC004*       POA_GESCH_VORG                 CHAR (2),
CCC004*       POA_PDVN_IBAN                  CHAR (34),
CCC004*       POA_PDVN_IBAN_E                CHAR (34),
CCC004*       POA_PDVN_BIC                   CHAR (11),
CCC004*       POA_PDVN_BIC_E                 CHAR (11),
CCC004*       POA_ESIGN                      CHAR(1) NOT NULL
CCC004*    )
CCC003*    END-EXEC.
 
 
       01  TBAN0003-GESAMT.
           05 DCL-TBAN0003.
              10 POA-PIN                        PIC S9(9) COMP.
              10 POA-HVANTNUM                   PIC X(9).
              10 POA-HVANTNUM-LFD               PIC X.
              10 POA-VDABVERM                   PIC S9(9) COMP.
              10 POA-LAST-UPDATE                PIC X(26).
              10 POA-KENNUNG-AEND               PIC X(8).
              10 PGEB-GES                       PIC S9(4) COMP.
              10 POA-VDABVERMPZ                 PIC X.
              10 POA-VDVMNR                     PIC S9(9) COMP.
              10 POA-VDVMNRPZ                   PIC X.
              10 POA-VDVMZUNA                   PIC X(26).
              10 POA-VDVMVORNA                  PIC X(26).
              10 POA-VDDIREKT                   PIC S9(4) COMP.
              10 POA-PDVNZUNA                   PIC X(26).
              10 POA-PDVNVORNA                  PIC X(26).
              10 POA-PDVNPLZ                    PIC X(6).
              10 POA-PDVNORT                    PIC X(30).
              10 POA-PDVNLKZ                    PIC X(3).
              10 POA-VBEGINN                    PIC X(10).
              10 POA-VABLAUF                    PIC X(10).
              10 POA-KNR-ZAHLW                  PIC S9(4) COMP.
              10 POA-KTY-ZAHLW                  PIC S9(4) COMP.
              10 POA-VDPRODKEY1                 PIC S9(4) COMP.
              10 POA-VDABLEIN1                  PIC S9(6)V9(1) COMP-3.
              10 POA-BEITRAGBR1                 PIC S9(9)V9(2) COMP-3.
              10 POA-BEITRAGNE1                 PIC S9(9)V9(2) COMP-3.
              10 POA-VDPRODKEY2                 PIC S9(4) COMP.
              10 POA-VDABLEIN2                  PIC S9(6)V9(1) COMP-3.
              10 POA-BEITRAGBR2                 PIC S9(9)V9(2) COMP-3.
              10 POA-BEITRAGNE2                 PIC S9(9)V9(2) COMP-3.
              10 POA-VDPRODKEY3                 PIC S9(4) COMP.
              10 POA-VDABLEIN3                  PIC S9(6)V9(1) COMP-3.
              10 POA-BEITRAGBR3                 PIC S9(9)V9(2) COMP-3.
              10 POA-BEITRAGNE3                 PIC S9(9)V9(2) COMP-3.
              10 POA-VDPRODKEY4                 PIC S9(4) COMP.
              10 POA-VDABLEIN4                  PIC S9(6)V9(1) COMP-3.
              10 POA-BEITRAGBR4                 PIC S9(9)V9(2) COMP-3.
              10 POA-BEITRAGNE4                 PIC S9(9)V9(2) COMP-3.
              10 POA-VDPRODKEY5                 PIC S9(4) COMP.
              10 POA-VDABLEIN5                  PIC S9(6)V9(1) COMP-3.
              10 POA-BEITRAGBR5                 PIC S9(9)V9(2) COMP-3.
              10 POA-BEITRAGNE5                 PIC S9(9)V9(2) COMP-3.
              10 POA-KTY-ZWEIG                  PIC S9(4) COMP.
              10 POA-VOR-VNR1                   PIC X(14).
              10 POA-KNR-ZWEIG1                 PIC S9(4) COMP.
              10 POA-VOR-VNR2                   PIC X(14).
              10 POA-KNR-ZWEIG2                 PIC S9(4) COMP.
              10 POA-VOR-VNR3                   PIC X(14).
              10 POA-KNR-ZWEIG3                 PIC S9(4) COMP.
              10 POA-VOR-VNR4                   PIC X(14).
              10 POA-KNR-ZWEIG4                 PIC S9(4) COMP.
              10 POA-VOR-VNR5                   PIC X(14).
              10 POA-KNR-ZWEIG5                 PIC S9(4) COMP.
              10 POA-VOR-VNR6                   PIC X(14).
              10 POA-KNR-ZWEIG6                 PIC S9(4) COMP.
              10 POA-VOR-VNR7                   PIC X(14).
              10 POA-KNR-ZWEIG7                 PIC S9(4) COMP.
              10 POA-VOR-VNR8                   PIC X(14).
              10 POA-KNR-ZWEIG8                 PIC S9(4) COMP.
              10 POA-VOR-VNR9                   PIC X(14).
              10 POA-KNR-ZWEIG9                 PIC S9(4) COMP.
              10 POA-VOR-VNR10                  PIC X(14).
              10 POA-KNR-ZWEIG10                PIC S9(4) COMP.
              10 POA-HAND                       PIC X.
              10 POA-ERSTELLUNG                 PIC X(26).
              10 POA-KENNUNG-ERST               PIC X(8).
              10 POA-XGDV.
110309           49 POA-XGDV-LEN                PIC S9(4) USAGE COMP.
110309           49 POA-XGDV-TEXT               PIC X(25000).
300902        10 POA-ANTRAG-DATUM               PIC X(10).
261104        10 POA-VBEGINN-E                  PIC X(10).
261104        10 POA-VABLAUF-E                  PIC X(10).
261104        10 POA-KNR-ZAHLW-E                PIC S9(4) COMP.
261104        10 POA-KTY-ZAHLW-E                PIC S9(4) COMP.
261104        10 POA-PDVN-BLZ                   PIC S9(08) COMP.
261104        10 POA-PDVN-BLZ-E                 PIC S9(08) COMP.
261104        10 POA-PDVN-KONTONR               PIC S9(010) COMP-3.
261104        10 POA-PDVN-KONTONR-E             PIC S9(010) COMP-3.
261104        10 POA-PDVN-BEGINKM               PIC S9(06) COMP.
261104        10 POA-PDVN-BEGINKM-E             PIC S9(06) COMP.
261104        10 POA-PDVN-FGESTNR               PIC X(17).
261104        10 POA-PDVN-FGESTNR-E             PIC X(17).
261104        10 POA-PDVN-AMTLKZ                PIC X(11).
261104        10 POA-PDVN-AMTLKZ-E              PIC X(11).
261104        10 POA-PDVN-JAHRKM                PIC S9(06) COMP.
261104        10 POA-PDVN-JAHRKM-E              PIC S9(06) COMP.
310806        10 POA-KD-BEST-ID                 PIC S9(18)V COMP-3.
301107        10 POA-VB-GES                     PIC S9(4)  COMP.
260508        10 POA-WIDERSPR                   PIC X.
260508        10 POA-WIDERSPR-E                 PIC X.
170609        10 POA-PIN-PZ                     PIC X(2).
170609        10 POA-BARCODE                    PIC X(10).
170609        10 POA-SCHNELLANTR                PIC X(1).
170609        10 POA-GESCH-VORG                 PIC X(2).
241013        10 POA-PDVN-IBAN                  PIC X(34).
241013        10 POA-PDVN-IBAN-E                PIC X(34).
241013        10 POA-PDVN-BIC                   PIC X(11).
241013        10 POA-PDVN-BIC-E                 PIC X(11).
040315        10 POA-ESIGN                      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 97      *
      ******************************************************************
 

