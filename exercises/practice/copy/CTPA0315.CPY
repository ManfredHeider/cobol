      ******************************************************************
      *AENDERUNGEN:                                                    *
      *AUFTRAG ! DATUM  !VER ! AENDERUNG                 !NAME         *
      *----------------------------------------------------------------*
      *        !12.05.22!    ! BISHER KEIN LOGBUCH       !SCHEIDEL     *
      *DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        !SCHEIDEL     *
      *        !        !    !                           !             *
      *        !        !    !                           !             *
      *        !        !    !                           !             *
      *ENDE-LOGBUCH                                                    *
      *----------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0315)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0315))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0315 TABLE
CCC004*    ( PMPR_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPR_VERSION                   SMALLINT NOT NULL,
CCC004*      PMPR_V_GUELTIG_AB              TIMESTAMP NOT NULL,
CCC004*      PMPR_V_GUELTIG_BIS             DATE NOT NULL,
CCC004*      PMPR_V_KENNUNG                 CHAR(8) NOT NULL,
CCC004*      PMBE_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMBE_VERSION                   SMALLINT NOT NULL,
CCC004*      PRINTE                         DECIMAL(15, 0) NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PMPR_PGV                       SMALLINT NOT NULL,
CCC004*      PMPR_PPFL_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PPFL_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PG_BEITRAG                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PG_SUMME                  DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PG_PROV_STZ               DECIMAL(8, 4) NOT NULL,
CCC004*      PMPR_PG_GSMT_PROV              DECIMAL(11, 2) NOT NULL,
CCC004*      PRKO_KOND_NR_GSP               SMALLINT NOT NULL,
CCC004*      PRKO_KOND_NR_AP_FP             SMALLINT NOT NULL,
CCC004*      PMPR_PG_DATEN                  CHAR(30) NOT NULL,
CCC004*      PMPR_VSTAT                     CHAR(4) NOT NULL,
CCC004*      PMPR_VSTAT_UNR                 SMALLINT NOT NULL,
CCC004*      PMPR_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMPR_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PMPR_PG_PRODART                SMALLINT NOT NULL,
CCC004*      KNR_DYN_KZ_1                   SMALLINT NOT NULL,
CCC004*      KNR_DYN_KZ_2                   SMALLINT NOT NULL,
CCC004*      KNR_BEW_HRKFT                  SMALLINT NOT NULL,
CCC004*      PMPR_BEW_SCHL                  INTEGER NOT NULL,
CCC004*      PMPR_PRO_RATA_Z                SMALLINT NOT NULL,
CCC004*      PMPR_PRO_RATA_N                SMALLINT NOT NULL,
CCC004*      PMPR_BWERT_SUMME               DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_RWERT_SUMME               DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_BZ_DAUER                  SMALLINT NOT NULL,
CCC004*      PMPR_BEITRAG_JAHR              DECIMAL(9, 2) NOT NULL,
CCC004*      PMPR_NEUST_KZ                  CHAR(1) NOT NULL,
CCC004*      PMPR_TARIF_SCHLGRP             CHAR(15) NOT NULL,
CCC004*      KNR_TARIF_KZ                   SMALLINT NOT NULL,
CCC004*      PMPR_TARIF_GRP                 SMALLINT NOT NULL,
CCC004*      PMPR_DAUER_GSMT                SMALLINT NOT NULL,
CCC004*      PMPR_HAFTZ_VON                 DATE NOT NULL,
CCC004*      PMPR_HAFTZ_BIS                 DATE NOT NULL,
CCC004*      PMPR_PG_TARIF                  CHAR(15) NOT NULL,
CCC004*      PMPR_RWERT_BEITRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PROV_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_PROV_VERDIENT             SMALLINT NOT NULL,
CCC004*      PMPR_VERS_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_AE_KZ                     CHAR(1) NOT NULL,
CCC004*      PMPR_TLSN_TARIF                CHAR(50) NOT NULL,
CCC004*      PMPR_TLSN_LAUFZEIT             SMALLINT NOT NULL,
CCC004*      PMPR_TLSN_VRBRAUCH             INITEGER NOT NULL,
CCC004*      KNR_TLSN_STORNO                SMALLINT NOT NULL,
CCC004*      PMPR_TLSN_Z_NR                 CHAR(20) NOT NULL,
CCC004*      PMPR_GEL_GEVO                  CHAR(7) NOT NULL,
CCC004*      PMPR_GEL_RABST                 CHAR(5) NOT NULL,
CCC004*      PMPR_GEL_EALTER                SMALLINT NOT NULL,
CCC004*      PMPR_NBB_SUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_AUW_SUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPR_VERTRAGSTEIL              SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0315                           *
      ******************************************************************
       01  DCL-TBPA0315.
           10 PMPR-ID              PIC S9(15)V USAGE COMP-3.
           10 PMPR-VERSION         PIC S9(4) USAGE COMP.
           10 PMPR-V-GUELTIG-AB    PIC X(26).
           10 PMPR-V-GUELTIG-BIS   PIC X(10).
           10 PMPR-V-KENNUNG       PIC X(8).
           10 PMBE-ID              PIC S9(15)V USAGE COMP-3.
           10 PMBE-VERSION         PIC S9(4) USAGE COMP.
           10 PRINTE               PIC S9(15)V USAGE COMP-3.
           10 PGEB-GES             PIC S9(4) USAGE COMP.
           10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
           10 PMPR-PGV             PIC S9(4) USAGE COMP.
           10 PMPR-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PG-BEITRAG      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PG-SUMME        PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PG-PROV-STZ     PIC S9(4)V9(4) USAGE COMP-3.
           10 PMPR-PG-GSMT-PROV    PIC S9(9)V9(2) USAGE COMP-3.
           10 PRKO-KOND-NR-GSP     PIC S9(4) USAGE COMP.
           10 PRKO-KOND-NR-AP-FP   PIC S9(4) USAGE COMP.
           10 PMPR-PG-DATEN        PIC X(30).
           10 PMPR-VSTAT           PIC X(4).
           10 PMPR-VSTAT-UNR       PIC S9(4) USAGE COMP.
           10 PMPR-LAST-UPDATE     PIC X(26).
           10 PMPR-KENNUNG         PIC X(8).
           10 PMPR-PG-PRODART      PIC S9(4) USAGE COMP.
           10 KNR-DYN-KZ-1         PIC S9(4) USAGE COMP.
           10 KNR-DYN-KZ-2         PIC S9(4) USAGE COMP.
           10 KNR-BEW-HRKFT        PIC S9(4) USAGE COMP.
           10 PMPR-BEW-SCHL        PIC S9(9) USAGE COMP.
           10 PMPR-PRO-RATA-Z      PIC S9(4) USAGE COMP.
           10 PMPR-PRO-RATA-N      PIC S9(4) USAGE COMP.
           10 PMPR-BWERT-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-RWERT-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-BZ-DAUER        PIC S9(4) USAGE COMP.
           10 PMPR-BEITRAG-JAHR    PIC S9(7)V9(2) USAGE COMP-3.
           10 PMPR-NEUST-KZ        PIC X(1).
           10 PMPR-TARIF-SCHLGRP   PIC X(15).
           10 KNR-TARIF-KZ         PIC S9(4) USAGE COMP.
           10 PMPR-TARIF-GRP       PIC S9(4) USAGE COMP.
           10 PMPR-DAUER-GSMT      PIC S9(4) USAGE COMP.
           10 PMPR-HAFTZ-VON       PIC X(10).
           10 PMPR-HAFTZ-BIS       PIC X(10).
           10 PMPR-PG-TARIF        PIC X(15).
           10 PMPR-RWERT-BEITRAG   PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PROV-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-PROV-VERDIENT   PIC S9(4) USAGE COMP.
           10 PMPR-VERS-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-AE-KZ           PIC X(1).
           10 PMPR-TLSN-TARIF      PIC X(50).
           10 PMPR-TLSN-LAUFZEIT   PIC S9(4)    USAGE COMP.
           10 PMPR-TLSN-VRBRAUCH   PIC S9(09)   USAGE COMP.
           10 KNR-TLSN-STORNO      PIC S9(4)    USAGE COMP.
           10 PMPR-TLSN-Z-NR       PIC X(20).
           10 PMPR-GEL-GEVO        PIC X(7).
           10 PMPR-GEL-RABST       PIC X(5).
           10 PMPR-GEL-EALTER      PIC S9(4)    USAGE COMP.
           10 PMPR-NBB-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-AUW-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
           10 PMPR-VERTRAGSTEIL    PIC S9(4)    USAGE COMP.
      ******************************************************************

