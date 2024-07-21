      ******************************************************************
      * DCLGEN TABLE(T13DIS.TBPA0129)                                  *
      *        LIBRARY(T13AE37.USERTEST.SRCDCL(CTPA0129))              *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE T13AES.TBPA0129 TABLE
CCC004*    ( PDPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PDTA_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PVER_VNR                       CHAR(15) NOT NULL,
CCC004*      PAMS_ABR_SORT_NR               CHAR(15) NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PMPT_FAELLIG_VON               DATE NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PVER_VERSION                   SMALLINT NOT NULL,
CCC004*      PVER_KUNDE_NAME                CHAR(25) NOT NULL,
CCC004*      PVER_KUNDE_VNAME               CHAR(20) NOT NULL,
CCC004*      PMBE_RFOLGE_NR                 SMALLINT NOT NULL,
CCC004*      PMPT_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPT_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPT_GSMT_PROV                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_PPFL_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PVPT_VT_PRIO                   SMALLINT NOT NULL,
CCC004*      PMPE_PIN                       INTEGER NOT NULL,
CCC004*      PDPT_PRODART                   SMALLINT NOT NULL,
CCC004*      PDPT_DGV                       SMALLINT NOT NULL,
CCC004*      PMPT_EINH                      DECIMAL(8, 2) NOT NULL,
CCC004*      KNR_BERCH_WAEHRUNG             SMALLINT NOT NULL,
CCC004*      KNR_KZ_EH_MAX                  SMALLINT NOT NULL,
CCC004*      KNR_BUCHUNGS_STAND             SMALLINT NOT NULL,
CCC004*      PEST_AVERM                     INTEGER NOT NULL,
CCC004*      PEST_VM_GESCHFT_KZ             CHAR(1) NOT NULL,
CCC004*      PDPT_FIRST_INSERT              TIMESTAMP NOT NULL,
CCC004*      KNR_MANDANT                    SMALLINT NOT NULL,
CCC004*      PAMS_INSERT                    TIMESTAMP NOT NULL,
CCC004*      MAX_INDEX_PE                   SMALLINT NOT NULL,
CCC004*      PE_STRUKTUR_DATEN              VARCHAR(1600) NOT NULL,
CCC004*      KNR_TARIF_KZ                   SMALLINT NOT NULL,
CCC004*      PVER_VP_NAME                   CHAR(25) NOT NULL,
CCC004*      PVER_VP_VNAME                  CHAR(20) NOT NULL,
CCC004*      PMPR_HAFTZ_VON                 DATE NOT NULL,
CCC004*      PMPR_HAFTZ_BIS                 DATE NOT NULL,
CCC004*      PMPR_PPFL_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PG_SUMME                  DECIMAL(11, 2) NOT NULL,
CCC004*      KNR_DYN_KZ_1                   SMALLINT NOT NULL,
CCC004*      KNR_DYN_KZ_2                   SMALLINT NOT NULL,
CCC004*      PMBE_POL_DAT                   DATE NOT NULL,
CCC004*      PMPR_PRO_RATA_Z                SMALLINT NOT NULL,
CCC004*      PMPR_PRO_RATA_N                SMALLINT NOT NULL,
CCC004*      PMBE_PROV_SCHL                 CHAR(5) NOT NULL,
CCC004*      KNR_RAB_ST                     SMALLINT NOT NULL,
CCC004*      PMPR_RWERT_SUMME               DECIMAL(11, 2) NOT NULL,
CCC004*      PMBE_SAM_NR                    INTEGER NOT NULL,
CCC004*      PMBE_BESVV                     DECIMAL(1, 0) NOT NULL,
CCC004*      PMPR_PGV                       SMALLINT NOT NULL,
CCC004*      PVPT_BUZTAR                    SMALLINT NOT NULL,
CCC004*      PMBE_INKST_DAT                 DATE NOT NULL,
CCC004*      PVER_VN_STRASSE                CHAR(35) NOT NULL,
CCC004*      PVER_VN_PLZ                    CHAR(5) NOT NULL,
CCC004*      PVER_VN_ORT                    CHAR(35) NOT NULL,
CCC004*      PMPR_PG_PRODART                SMALLINT NOT NULL,
CCC004*      KNR_ZAHLWEISE                  SMALLINT NOT NULL,
CCC004*      KNR_BEW_HRKFT                  SMALLINT NOT NULL,
CCC004*      PMPR_BEW_SCHL                  INTEGER NOT NULL,
CCC004*      PMBE_VVG_KZ                    CHAR(1) NOT NULL,
CCC004*      PVER_URSPR_POL_DAT             DATE NOT NULL,
CCC004*      PMBE_UNR                       SMALLINT NOT NULL,
CCC004*      PMBE_SCHICHT_NR                SMALLINT NOT NULL,
CCC004*      PMPR_PG_TARIF                  CHAR(15) NOT NULL,
CCC004*      PAMS_VERTRAGSDAUER             SMALLINT NOT NULL,
CCC004*      PAMS_KZ_KORR_BEWEG             CHAR(1) NOT NULL,
CCC004*      PMPT_PG_UST_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PMPT_PG_UST_SATZ               DECIMAL(6, 3) NOT NULL,
CCC004*      PMBE_PG_RECH_NR                CHAR(30) NOT NULL,
CCC004*      PMPT_FAELLIG_BIS               DATE NOT NULL,
CCC004*      PDPT_UST_JAHR                  SMALLINT NOT NULL,
CCC004*      PAMS_UST_JAHR_KZ               CHAR(1) NOT NULL,
CCC004*      PMPT_VSP_KOMBI_NR              INTEGER NOT NULL,
CCC004*      PMPT_VSP_KOMBI_BZ              CHAR(3) NOT NULL,
CCC004*      PMPT_VSP_FN_BER                CHAR(1) NOT NULL,
CCC004*      PMPT_VSP_FN_REL                CHAR(1) NOT NULL,
CCC004*      PMPT_VSP_FN_FP_KZ              CHAR(1) NOT NULL,
CCC004*      PEST_AVERM_KARR                INTEGER NOT NULL,
CCC004*      PMPR_NBB_SUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_AUW_SUMME                 DECIMAL(11, 2) NOT NULL 
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE T13DIS.TBPA0129                    *
      ******************************************************************
       01  TBPA0129-GESAMT.
         05 DCL-TBPA0129.
           10 PDPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PDTA-ID              PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PVER-VNR             PIC X(15).
           10 PAMS-ABR-SORT-NR     PIC X(15).
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PMPT-FAELLIG-VON     PIC X(10).
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PVER-VERSION         PIC S9(4) USAGE COMP.
           10 PVER-KUNDE-NAME      PIC X(25).
           10 PVER-KUNDE-VNAME     PIC X(20).
           10 PMBE-RFOLGE-NR       PIC S9(4) USAGE COMP.
           10 PMPT-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPT-VERSION         PIC S9(4) USAGE COMP.
           10 PMPT-GSMT-PROV       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PVPT-VT-PRIO         PIC S9(4) USAGE COMP.
           10 PMPE-PIN             PIC S9(9) USAGE COMP.
           10 PDPT-PRODART         PIC S9(4) USAGE COMP.
           10 PDPT-DGV             PIC S9(4) USAGE COMP.
           10 PMPT-EINH            PIC S9(6)V9(2) USAGE COMP-3.
           10 KNR-BERCH-WAEHRUNG   PIC S9(4) USAGE COMP.
           10 KNR-KZ-EH-MAX        PIC S9(4) USAGE COMP.
           10 KNR-BUCHUNGS-STAND   PIC S9(4) USAGE COMP.
           10 PEST-AVERM           PIC S9(9) USAGE COMP.
           10 PEST-VM-GESCHFT-KZ   PIC X(1).
           10 PDPT-FIRST-INSERT    PIC X(26).
           10 KNR-MANDANT          PIC S9(4) USAGE COMP.
           10 PAMS-INSERT          PIC X(26).
           10 MAX-INDEX-PE         PIC S9(4) USAGE COMP.
           10 PE-STRUKTUR-DATEN.
              49 PE-STRUKTUR-DATEN-LEN
                 PIC S9(4) USAGE COMP.
              49 PE-STRUKTUR-DATEN-TEXT
                 PIC X(1600).
           10 KNR-TARIF-KZ         PIC S9(4) USAGE COMP.
           10 PVER-VP-NAME         PIC X(25).
           10 PVER-VP-VNAME        PIC X(20).
           10 PMPR-HAFTZ-VON       PIC X(10).
           10 PMPR-HAFTZ-BIS       PIC X(10).
           10 PMPR-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PG-SUMME        PIC S9(9)V9(2) USAGE COMP-3.
           10 KNR-DYN-KZ-1         PIC S9(4) USAGE COMP.
           10 KNR-DYN-KZ-2         PIC S9(4) USAGE COMP.
           10 PMBE-POL-DAT         PIC X(10).
           10 PMPR-PRO-RATA-Z      PIC S9(4) USAGE COMP.
           10 PMPR-PRO-RATA-N      PIC S9(4) USAGE COMP.
           10 PMBE-PROV-SCHL       PIC X(5).
           10 KNR-RAB-ST           PIC S9(4) USAGE COMP.
           10 PMPR-RWERT-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PMBE-SAM-NR          PIC S9(9) USAGE COMP.
           10 PMBE-BESVV           PIC S9(1)V USAGE COMP-3.
           10 PMPR-PGV             PIC S9(4) USAGE COMP.
           10 PVPT-BUZTAR          PIC S9(4) USAGE COMP.
           10 PMBE-INKST-DAT       PIC X(10).
           10 PVER-VN-STRASSE      PIC X(35).
           10 PVER-VN-PLZ          PIC X(5).
           10 PVER-VN-ORT          PIC X(35).
           10 PMPR-PG-PRODART      PIC S9(4) USAGE COMP.
           10 KNR-ZAHLWEISE        PIC S9(4) USAGE COMP.
           10 KNR-BEW-HRKFT        PIC S9(4) USAGE COMP.
           10 PMPR-BEW-SCHL        PIC S9(9) USAGE COMP.
           10 PMBE-VVG-KZ          PIC X(1).
           10 PVER-URSPR-POL-DAT   PIC X(10).
           10 PMBE-UNR             PIC S9(4) USAGE COMP.
           10 PMBE-SCHICHT-NR      PIC S9(4) USAGE COMP.
           10 PMPR-PG-TARIF        PIC X(15).
           10 PAMS-VERTRAGSDAUER   PIC S9(4) USAGE COMP.
           10 PAMS-KZ-KORR-BEWEG   PIC X(1).
           10 PMPT-PG-UST-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPT-PG-UST-SATZ     PIC S9(3)V9(3) USAGE COMP-3.
           10 PMBE-PG-RECH-NR      PIC X(30).
           10 PMPT-FAELLIG-BIS     PIC X(10).
290116     10 PDPT-UST-JAHR        PIC S9(4) BINARY.
290116     10 PAMS-UST-JAHR-KZ     PIC X(1).
300117     10 PMPT-VSP-KOMBI-NR    PIC S9(05) USAGE COMP.
300117     10 PMPT-VSP-KOMBI-BZ    PIC X(3).
300117     10 PMPT-VSP-FN-BER      PIC X(1).
300117     10 PMPT-VSP-FN-REL      PIC X(1).
300117     10 PMPT-VSP-FN-FP-KZ    PIC X(1).
           10 PEST-AVERM-KARR      PIC S9(9) USAGE COMP.
140921     10 PMPR-NBB-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
140921     10 PMPR-AUW-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 69      *
      ******************************************************************
 
 
 


