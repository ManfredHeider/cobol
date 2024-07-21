      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTAN0020.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/CTAN0020.CPv  $
      *
      *   Rev 2.8   Nov 22 2018 16:16:22   t13ae23
      *AAN01370: DV - Neues Produkt DV ABRECHNUNG PLUS
      *
      *   Rev 2.7   Feb 25 2016 10:10:46   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)" +
      *Elektra
      *- AGI, CKV - Deutsche Bank -
      *
      *   Rev 2.6   Feb 22 2016 14:22:28   t13ae23
      *AAN01240: Neue Feld "Fimenname (für DV-Antrag)"
      *
      *   Rev 2.5   Aug 12 2015 14:51:56   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov. - Telefon u.a verlängern -
      *
      *   Rev 2.4   Jul 07 2015 15:56:14   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.3   Jun 22 2015 15:45:34   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.2   Jun 10 2015 17:39:40   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.1   Jun 10 2015 14:26:20   t13ae23
      *AAN01198: Deutsche Verrechnungsstelle Mittelstand  u. USt.pf
      *l. Prov.
      *
      *   Rev 2.0   Jan 13 2015 13:47:20   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: CTAN0020                                     *
      *    FUNKTION   : DCLGEN FÜR TABELLE TBAN0020 DVM              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AAN01198!22.06.15!2.1  !NEU                        !RAHNER   *
      * AAN01198!07.07.15!2.4  !NEU FELD EINR.GEBÜHR       !RAHNER   *
      * AAN01198!11.08.15!2.5  !VERGRÖßERN FELDER          !RAHNER   *
      * AAN01240!22.02.16!2.6  !FIRMA-NAME-LANG            !RAHNER   *
      * AAN01370!22.11.18!2.8  !NEUE FELDER F. NEUE PROD.  !RAHNER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(TBAN0020)                                         *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      *        DBCSDELIM(NO)                                           *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBAN0020 TABLE
CCC004*    ( DVM_HVANTNUM                   CHAR(9) NOT NULL,
CCC004*      DVM_VNR                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      DVM_ABVERM                     INTEGER NOT NULL,
CCC004*      DVM_ABVERMPZ                   CHAR(1) NOT NULL,
CCC004*      DVM_ABVERM_E                   INTEGER NOT NULL,
CCC004*      DVM_ABVERMPZ_E                 CHAR(1) NOT NULL,
CCC004*      DVM_VMNR                       INTEGER NOT NULL,
CCC004*      DVM_VMNRPZ                     CHAR(1) NOT NULL,
CCC004*      DVM_VMZUNA                     CHAR(30) NOT NULL,
CCC004*      DVM_VMVORNA                    CHAR(30) NOT NULL,
CCC004*      DVM_VMNR_E                     INTEGER NOT NULL,
CCC004*      DVM_VMNRPZ_E                   CHAR(1) NOT NULL,
CCC004*      DVM_VMZUNA_E                   CHAR(30) NOT NULL,
CCC004*      DVM_VMVORNA_E                  CHAR(30) NOT NULL,
CCC004*      DVM_DIR                        SMALLINT NOT NULL,
CCC004*      DVM_VB_GES                     SMALLINT NOT NULL,
CCC004*      DVM_VERSION                    SMALLINT NOT NULL,
CCC004*      DVM_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      DVM_KENNUNG_AEND               CHAR(8) NOT NULL,
CCC004*      DVM_ERSTELLUNG                 TIMESTAMP NOT NULL,
CCC004*      DVM_KENNUNG_ERST               CHAR(8) NOT NULL,
CCC004*      DVM_FA_RF                      SMALLINT NOT NULL,
CCC004*      DVM_FA_RF_E                    SMALLINT NOT NULL,
CCC004*      DVM_ANR_FA                     SMALLINT NOT NULL,
CCC004*      DVM_FA_N1                      CHAR(30) NOT NULL,
CCC004*      DVM_FA_N1_E                    CHAR(30) NOT NULL,
CCC004*      DVM_FA_N2                      CHAR(30) NOT NULL,
CCC004*      DVM_FA_N2_E                    CHAR(30) NOT NULL,
CCC004*      DVM_FA_STR                     CHAR(45) NOT NULL,
CCC004*      DVM_FA_STR_E                   CHAR(45) NOT NULL,
CCC004*      DVM_FA_PLZ                     CHAR(15) NOT NULL,
CCC004*      DVM_FA_PLZ_E                   CHAR(15) NOT NULL,
CCC004*      DVM_FA_ORT                     CHAR(50) NOT NULL,
CCC004*      DVM_FA_ORT_E                   CHAR(50) NOT NULL,
CCC004*      DVM_FA_POSTF                   CHAR(10) NOT NULL,
CCC004*      DVM_FA_POSTF_E                 CHAR(10) NOT NULL,
CCC004*      DVM_FA_PLZ_POSTF               CHAR(5) NOT NULL,
CCC004*      DVM_FA_PLZ_POSTF_E             CHAR(5) NOT NULL,
CCC004*      DVM_FA_ORT_POSTF               CHAR(40) NOT NULL,
CCC004*      DVM_FA_ORT_POSTF_E             CHAR(40) NOT NULL,
CCC004*      DVM_FA_TELE                    CHAR(30) NOT NULL,
CCC004*      DVM_FA_TELE_E                  CHAR(30) NOT NULL,
CCC004*      DVM_FA_FAX                     CHAR(30) NOT NULL,
CCC004*      DVM_FA_FAX_E                   CHAR(30) NOT NULL,
CCC004*      DVM_FA_MOBIL                   CHAR(30) NOT NULL,
CCC004*      DVM_FA_MOBIL_E                 CHAR(30) NOT NULL,
CCC004*      DVM_FA_EMAIL                   CHAR(50) NOT NULL,
CCC004*      DVM_FA_EMAIL_E                 CHAR(50) NOT NULL,
CCC004*      DVM_FA_ANSP_TITEL              SMALLINT NOT NULL,
CCC004*      DVM_FA_ANSP_ANR                SMALLINT NOT NULL,
CCC004*      DVM_FA_ANSP_VN                 CHAR(30) NOT NULL,
CCC004*      DVM_FA_ANSP_ZN                 CHAR(30) NOT NULL,
CCC004*      DVM_FA_ANSP_GEB                INTEGER NOT NULL,
CCC004*      DVM_FA_ANSP_FUNK               CHAR(50) NOT NULL,
CCC004*      DVM_FA_KZ_VST                  CHAR(1) NOT NULL,
CCC004*      DVM_FA_KZ_VST_E                CHAR(1) NOT NULL,
CCC004*      DVM_FA_ST_NR                   CHAR(15) NOT NULL,
CCC004*      DVM_FA_ST_NR_E                 CHAR(15) NOT NULL,
CCC004*      DVM_FA_HREG_AMTG               CHAR(50) NOT NULL,
CCC004*      DVM_FA_HREG_AMTG_E             CHAR(50) NOT NULL,
CCC004*      DVM_FA_HREG_NR                 CHAR(30) NOT NULL,
CCC004*      DVM_FA_HREG_NR_E               CHAR(30) NOT NULL,
CCC004*      DVM_FA_INH                     CHAR(100) NOT NULL,
CCC004*      DVM_FA_INH_E                   CHAR(100) NOT NULL,
CCC004*      DVM_FA_BR                      SMALLINT NOT NULL,
CCC004*      DVM_FA_GR_DAT                  INTEGER NOT NULL,
CCC004*      DVM_KZ_FAC_OPT                 CHAR(1) NOT NULL,
CCC004*      DVM_KZ_FAC_OPT_E               CHAR(1) NOT NULL,
CCC004*      DVM_UMSATZ_JAHR                DECIMAL(9, 0) NOT NULL,
CCC004*      DVM_UMSATZ_JAHR_E              DECIMAL(9, 0) NOT NULL,
CCC004*      DVM_KZ_GARANT                  CHAR(1) NOT NULL,
CCC004*      DVM_KZ_GARANT_E                CHAR(1) NOT NULL,
CCC004*      DVM_KZ_GW_GES                  CHAR(1) NOT NULL,
CCC004*      DVM_KZ_GW_GES_E                CHAR(1) NOT NULL,
CCC004*      DVM_KZ_N_ERKL                  CHAR(1) NOT NULL,
CCC004*      DVM_KZ_N_ERKL_E                CHAR(1) NOT NULL,
CCC004*      DVM_Z_ZIEL_TAGE                SMALLINT NOT NULL,
CCC004*      DVM_PS_GR_KUNDE                SMALLINT NOT NULL,
CCC004*      DVM_PS_PRIVAT_K                SMALLINT NOT NULL,
CCC004*      DVM_FAC_ANKH                   DECIMAL(9, 2) NOT NULL,
CCC004*      DVM_VBEGINN                    INTEGER NOT NULL,
CCC004*      DVM_VBEGINN_E                  INTEGER NOT NULL,
CCC004*      DVM_PROD_PROD_SCHL1            SMALLINT NOT NULL,
CCC004*      DVM_ABEINH_1                   DECIMAL(7, 2) NOT NULL,
CCC004*      DVM_ABEITRAG_1                 DECIMAL(7, 2) NOT NULL,
CCC004*      DVM_PROD_PROD_SCHL2            SMALLINT NOT NULL,
CCC004*      DVM_ABEINH_2                   DECIMAL(7, 2) NOT NULL,
CCC004*      DVM_ABEITRAG_2                 DECIMAL(7, 2) NOT NULL,
CCC004*      DVM_KIH_VN                     CHAR(30) NOT NULL,
CCC004*      DVM_KIH_VN_E                   CHAR(30) NOT NULL,
CCC004*      DVM_KIH_ZN                     CHAR(30) NOT NULL,
CCC004*      DVM_KIH_ZN_E                   CHAR(30) NOT NULL,
CCC004*      DVM_KIH_ABW_STR                CHAR(45) NOT NULL,
CCC004*      DVM_KIH_ABW_STR_E              CHAR(45) NOT NULL,
CCC004*      DVM_KIH_ABW_PLZ                CHAR(15) NOT NULL,
CCC004*      DVM_KIH_ABW_PLZ_E              CHAR(15) NOT NULL,
CCC004*      DVM_KIH_ABW_ORT                CHAR(50) NOT NULL,
CCC004*      DVM_KIH_ABW_ORT_E              CHAR(50) NOT NULL,
CCC004*      DVM_GELDINST                   CHAR(58) NOT NULL,
CCC004*      DVM_GELDINST_E                 CHAR(58) NOT NULL,
CCC004*      DVM_BIC                        CHAR(11) NOT NULL,
CCC004*      DVM_BIC_E                      CHAR(11) NOT NULL,
CCC004*      DVM_IBAN                       CHAR(34) NOT NULL,
CCC004*      DVM_IBAN_E                     CHAR(34) NOT NULL,
CCC004*      DVM_SEPA_M_BEG                 INTEGER NOT NULL,
CCC004*      DVM_SEPA_M_BEG_E               INTEGER NOT NULL,
CCC004*      DVM_HAND                       CHAR(1) NOT NULL,
CCC004*      DVM_AN_DAT                     INTEGER NOT NULL,
CCC004*      DVM_KD_BEST_ID                 DECIMAL(18, 0) NOT NULL,
CCC004*      DVM_ESIGN                      CHAR(1) NOT NULL,
CCC004*      DVM_XGDV                       VARCHAR(20000) NOT NULL,
CCC004*      DVM_E_GEBUEHR                  CHAR(1) NOT NULL,
CCC004*      DVM_E_GEBUEHR_E                CHAR(1) NOT NULL,
CCC004*      DVM_FIRMA_LANG                 CHAR(70) NOT NULL,
CCC004*      DVM_FIRMA_LANG_E               CHAR(70) NOT NULL,
CCC004*      DVM_BUERGEL_ARCHIV_ID          DECIMAL(16, 0) NOT NULL,
CCC004*      DVM_BUERGEL_INDEX              INTEGER NOT NULL,
CCC004*      DVM_DDM_KEY                    DECIMAL(10, 0) NOT NULL,
CCC004*      DVM_WZ_CODE                    CHAR(10) NOT NULL

CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAN0020                           *
      ******************************************************************
       01  DCLTBAN0020.
           10 DVM-HVANTNUM         PIC X(9).
           10 DVM-VNR              PIC S9(15)V COMP-3.
           10 PGEB-GES             PIC S9(4) COMP.
           10 DVM-ABVERM           PIC S9(9) COMP.
           10 DVM-ABVERMPZ         PIC X(1).
           10 DVM-ABVERM-E         PIC S9(9) COMP.
           10 DVM-ABVERMPZ-E       PIC X(1).
           10 DVM-VMNR             PIC S9(9) COMP.
           10 DVM-VMNRPZ           PIC X(1).
           10 DVM-VMZUNA           PIC X(30).
           10 DVM-VMVORNA          PIC X(30).
           10 DVM-VMNR-E           PIC S9(9) COMP.
           10 DVM-VMNRPZ-E         PIC X(1).
           10 DVM-VMZUNA-E         PIC X(30).
           10 DVM-VMVORNA-E        PIC X(30).
           10 DVM-DIR              PIC S9(4) COMP.
           10 DVM-VB-GES           PIC S9(4) COMP.
           10 DVM-VERSION          PIC S9(4) COMP.
           10 DVM-LAST-UPDATE      PIC X(26).
           10 DVM-KENNUNG-AEND     PIC X(8).
           10 DVM-ERSTELLUNG       PIC X(26).
           10 DVM-KENNUNG-ERST     PIC X(8).
           10 DVM-FA-RF            PIC S9(4) COMP.
           10 DVM-FA-RF-E          PIC S9(4) COMP.
           10 DVM-ANR-FA           PIC S9(4) COMP.
           10 DVM-FA-N1            PIC X(30).
           10 DVM-FA-N1-E          PIC X(30).
           10 DVM-FA-N2            PIC X(30).
           10 DVM-FA-N2-E          PIC X(30).
           10 DVM-FA-STR           PIC X(45).
           10 DVM-FA-STR-E         PIC X(45).
           10 DVM-FA-PLZ           PIC X(15).
           10 DVM-FA-PLZ-E         PIC X(15).
           10 DVM-FA-ORT           PIC X(50).
           10 DVM-FA-ORT-E         PIC X(50).
           10 DVM-FA-POSTF         PIC X(10).
           10 DVM-FA-POSTF-E       PIC X(10).
           10 DVM-FA-PLZ-POSTF     PIC X(5).
           10 DVM-FA-PLZ-POSTF-E   PIC X(5).
           10 DVM-FA-ORT-POSTF     PIC X(40).
           10 DVM-FA-ORT-POSTF-E   PIC X(40).
           10 DVM-FA-TELE          PIC X(30).
           10 DVM-FA-TELE-E        PIC X(30).
           10 DVM-FA-FAX           PIC X(30).
           10 DVM-FA-FAX-E         PIC X(30).
           10 DVM-FA-MOBIL         PIC X(30).
           10 DVM-FA-MOBIL-E       PIC X(30).
           10 DVM-FA-EMAIL         PIC X(50).
           10 DVM-FA-EMAIL-E       PIC X(50).
           10 DVM-FA-ANSP-TITEL    PIC S9(4) COMP.
           10 DVM-FA-ANSP-ANR      PIC S9(4) COMP.
           10 DVM-FA-ANSP-VN       PIC X(30).
           10 DVM-FA-ANSP-ZN       PIC X(30).
           10 DVM-FA-ANSP-GEB      PIC S9(9) COMP.
           10 DVM-FA-ANSP-FUNK     PIC X(50).
           10 DVM-FA-KZ-VST        PIC X(1).
           10 DVM-FA-KZ-VST-E      PIC X(1).
           10 DVM-FA-ST-NR         PIC X(15).
           10 DVM-FA-ST-NR-E       PIC X(15).
           10 DVM-FA-HREG-AMTG     PIC X(50).
           10 DVM-FA-HREG-AMTG-E   PIC X(50).
           10 DVM-FA-HREG-NR       PIC X(30).
           10 DVM-FA-HREG-NR-E     PIC X(30).
           10 DVM-FA-INH           PIC X(100).
           10 DVM-FA-INH-E         PIC X(100).
           10 DVM-FA-BR            PIC S9(4) COMP.
           10 DVM-FA-GR-DAT        PIC S9(9) COMP.
           10 DVM-KZ-FAC-OPT       PIC X(1).
           10 DVM-KZ-FAC-OPT-E     PIC X(1).
           10 DVM-UMSATZ-JAHR      PIC S9(9)V COMP-3.
           10 DVM-UMSATZ-JAHR-E    PIC S9(9)V COMP-3.
           10 DVM-KZ-GARANT        PIC X(1).
           10 DVM-KZ-GARANT-E      PIC X(1).
           10 DVM-KZ-GW-GES        PIC X(1).
           10 DVM-KZ-GW-GES-E      PIC X(1).
           10 DVM-KZ-N-ERKL        PIC X(1).
           10 DVM-KZ-N-ERKL-E      PIC X(1).
           10 DVM-Z-ZIEL-TAGE      PIC S9(4) COMP.
           10 DVM-PS-GR-KUNDE      PIC S9(4) COMP.
           10 DVM-PS-PRIVAT-K      PIC S9(4) COMP.
           10 DVM-FAC-ANKH         PIC S9(7)V9(2) COMP-3.
           10 DVM-VBEGINN          PIC S9(9) COMP.
           10 DVM-VBEGINN-E        PIC S9(9) COMP.
           10 DVM-PROD-PROD-SCHL1  PIC S9(4) COMP.
           10 DVM-ABEINH-1         PIC S9(5)V9(2) COMP-3.
           10 DVM-ABEITRAG-1       PIC S9(5)V9(2) COMP-3.
           10 DVM-PROD-PROD-SCHL2  PIC S9(4) COMP.
           10 DVM-ABEINH-2         PIC S9(5)V9(2) COMP-3.
           10 DVM-ABEITRAG-2       PIC S9(5)V9(2) COMP-3.
           10 DVM-KIH-VN           PIC X(30).
           10 DVM-KIH-VN-E         PIC X(30).
           10 DVM-KIH-ZN           PIC X(30).
           10 DVM-KIH-ZN-E         PIC X(30).
           10 DVM-KIH-ABW-STR      PIC X(45).
           10 DVM-KIH-ABW-STR-E    PIC X(45).
           10 DVM-KIH-ABW-PLZ      PIC X(15).
           10 DVM-KIH-ABW-PLZ-E    PIC X(15).
           10 DVM-KIH-ABW-ORT      PIC X(50).
           10 DVM-KIH-ABW-ORT-E    PIC X(50).
           10 DVM-GELDINST         PIC X(58).
           10 DVM-GELDINST-E       PIC X(58).
           10 DVM-BIC              PIC X(11).
           10 DVM-BIC-E            PIC X(11).
           10 DVM-IBAN             PIC X(34).
           10 DVM-IBAN-E           PIC X(34).
           10 DVM-SEPA-M-BEG       PIC S9(9) COMP.
           10 DVM-SEPA-M-BEG-E     PIC S9(9) COMP.
           10 DVM-HAND             PIC X(1).
           10 DVM-AN-DAT           PIC S9(9) COMP.
           10 DVM-KD-BEST-ID       PIC S9(18)V COMP-3.
           10 DVM-ESIGN            PIC X(1).
           10 DVM-XGDV.
              49 DVM-XGDV-LEN      PIC S9(4) COMP.
              49 DVM-XGDV-TEXT     PIC X(20000).
           10 DVM-E-GEBUEHR        PIC X(1).
           10 DVM-E-GEBUEHR-E      PIC X(1).
           10 DVM-FIRMA-LANG       PIC X(70).
           10 DVM-FIRMA-LANG-E     PIC X(70).
           10 DVM-BUERGEL-ARCHIV-ID  PIC S9(16) COMP-3.
           10 DVM-BUERGEL-INDEX      PIC S9(4)  COMP.
           10 DVM-DDM-KEY            PIC S9(10) COMP-3.
           10 DVM-WZ-CODE            PIC X(10).

      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 120     *
      ******************************************************************

