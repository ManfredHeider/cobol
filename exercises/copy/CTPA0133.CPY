      ******************************************************************
      *AENDERUNGEN:                                                    *
      *AUFTRAG ! DATUM  !VER ! AENDERUNG                 !NAME         *
      *-----------------------------------------------------------------
      *APA01539!20.03.13!2.3 ! KALYPSO ERGAENZUNGEN      !HARALANOVA   *
      *APA01539!18.02.14!2.4 ! KALYPSO ERGAENZUNGEN      !RAHNER       *
      *APA01932!31.03.15!2.5 ! LUCY ERGAENZUNGEN         !KÖPRÜCÜ      *
      *APA02275!30.01.17!2.6 ! ERWEITERT UM FlexN-Felder !KÖPRÜCÜ      *
      *APA02353!03.03.17!2.13! ERWEITERT UM Teleosn      !Buch         *
      *APA02579!24.04.18!2.14! GENERALI LEBEN            !SCHEIDEL     *
      *DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    !BUCH         *
      *DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        !SCHEIDEL     *
      *        !        !    !                           !             *
      *        !        !    !                           !             *
      *        !        !    !                           !             *
      *ENDE-LOGBUCH                                                    *
      *----------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBPA0133)                                         *
      *        LIBRARY(T130005.PA010000.SRCDCL(CTPA0133))              *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBPA0133 TABLE
CCC004*    ( PMPP_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPG_ID                        DECIMAL(15, 0) NOT NULL,
CCC004*      PMPG_VERSION                   SMALLINT NOT NULL,
CCC004*      PGEB_GES                       SMALLINT NOT NULL,
CCC004*      PROD_PROD_SCHL                 SMALLINT NOT NULL,
CCC004*      PMPP_PGV                       SMALLINT NOT NULL,
CCC004*      PMPP_PPFL_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PPFL_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PG_BEITRAG                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PG_SUMME                  DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PG_PROV_STZ               DECIMAL(8, 4) NOT NULL,
CCC004*      PMPP_PG_GSMT_PROV              DECIMAL(11, 2) NOT NULL,
CCC004*      PRKO_KOND_NR_GSP               SMALLINT NOT NULL,
CCC004*      PMPP_PG_PROD_DATEN             CHAR(30) NOT NULL,
CCC004*      PMPP_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      PMPP_KENNUNG                   CHAR(8) NOT NULL,
CCC004*      PMPPH_LAST_UPDATE              TIMESTAMP NOT NULL,
CCC004*      PMPPH_KENNUNG                  CHAR(8) NOT NULL,
CCC004*      PMPP_PG_PRODART                SMALLINT NOT NULL,
CCC004*      KNR_DYN_KZ_1                   SMALLINT NOT NULL,
CCC004*      KNR_DYN_KZ_2                   SMALLINT NOT NULL,
CCC004*      KNR_BEW_HRKFT                  SMALLINT NOT NULL,
CCC004*      PMPP_BEW_SCHL                  INTEGER NOT NULL,
CCC004*      PMPP_PRO_RATA_Z                SMALLINT NOT NULL,
CCC004*      PMPP_PRO_RATA_N                SMALLINT NOT NULL,
CCC004*      PMPP_BWERT_SUMME               DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_RWERT_SUMME               DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_BZ_DAUER                  SMALLINT NOT NULL,
CCC004*      PMPP_BEITRAG_JAHR              DECIMAL(9, 2) NOT NULL,
CCC004*      PMPP_NEUST_KZ                  CHAR(1) NOT NULL,
CCC004*      PMPP_TARIF_SCHLGRP             CHAR(15) NOT NULL,
CCC004*      KNR_TARIF_KZ                   SMALLINT NOT NULL,
CCC004*      PMPP_TARIF_GRP                 SMALLINT NOT NULL,
CCC004*      PMPP_DAUER_GSMT                SMALLINT NOT NULL,
CCC004*      PMPP_HAFTZ_VON                 DATE NOT NULL,
CCC004*      PMPP_HAFTZ_BIS                 DATE NOT NULL,
CCC004*      PMPP_PG_TARIF                  CHAR(15) NOT NULL,
CCC004*      PMPP_RWERT_BEITRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PROV_BEITRAG              DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PROV_VERDIENT             SMALLINT NOT NULL,
CCC004*      PMPP_VERS_SUMME                DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_BUZTAR                    SMALLINT NOT NULL,
CCC004*      PMPP_AE_KZ                     CHAR(1) NOT NULL,
CCC004*      PMPP_PG_UST_BETRAG             DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_PG_UST_SATZ               DECIMAL(6, 3) NOT NULL,
CCC004*      PMPP_VSP_KOMBI_NR              INTEGER NOT NULL,
CCC004*      PMPP_VSP_KOMBI_BZ              CHAR(3) NOT NULL,
CCC004*      PMPP_VSP_FN_BER                CHAR(1) NOT NULL,
CCC004*      PMPP_VSP_FN_REL                CHAR(1) NOT NULL,
CCC004*      PMPP_TLSN_TARIF                CHAR(50) NOT NULL,
CCC004*      PMPP_TLSN_LAUFZEIT             SMALLINT NOT NULL,
CCC004*      PMPP_TLSN_VRBRAUCH             INTEGER NOT NULL,
CCC004*      KNR_TLSN_STORNO                SMALLINT NOT NULL,
CCC004*      PMPP_TLSN_Z_NR                 CHAR(20) NOT NULL,
CCC004*      PMPP_GEL_GEVO                  CHAR(7) NOT NULL,
CCC004*      PMPP_GEL_TARBEZ                CHAR(20) NOT NULL,
CCC004*      PMPP_GEL_RABST                 CHAR(5) NOT NULL,
CCC004*      PMPP_GE3L_EALTER               SMALLINT NOT NULL,
CCC004*      PMPP_NBB_SUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_AUW_SUMME                 DECIMAL(11, 2) NOT NULL,
CCC004*      PMPP_VERTRAGSTEIL              SMALLINT NOT NULL
CCC003*  ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBPA0133                           *
      ******************************************************************
       01  TBPA0133-GESAMT.
         05 DCL-TBPA0133.
            10 PMPP-ID              PIC S9(15)V USAGE COMP-3.
            10 PMPG-ID              PIC S9(15)V USAGE COMP-3.
            10 PMPG-VERSION         PIC S9(4) USAGE COMP.
            10 PGEB-GES             PIC S9(4) USAGE COMP.
            10 PROD-PROD-SCHL       PIC S9(4) USAGE COMP.
            10 PMPP-PGV             PIC S9(4) USAGE COMP.
            10 PMPP-PPFL-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PPFL-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PG-BEITRAG      PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PG-SUMME        PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PG-PROV-STZ     PIC S9(4)V9(4) USAGE COMP-3.
            10 PMPP-PG-GSMT-PROV    PIC S9(9)V9(2) USAGE COMP-3.
            10 PRKO-KOND-NR-GSP     PIC S9(4) USAGE COMP.
            10 PMPP-PG-PROD-DATEN   PIC X(30).
            10 PMPP-LAST-UPDATE     PIC X(26).
            10 PMPP-KENNUNG         PIC X(8).
            10 PMPPH-LAST-UPDATE    PIC X(26).
            10 PMPPH-KENNUNG        PIC X(8).
            10 PMPP-PG-PRODART      PIC S9(4) USAGE COMP.
            10 KNR-DYN-KZ-1         PIC S9(4) USAGE COMP.
            10 KNR-DYN-KZ-2         PIC S9(4) USAGE COMP.
            10 KNR-BEW-HRKFT        PIC S9(4) USAGE COMP.
            10 PMPP-BEW-SCHL        PIC S9(9) USAGE COMP.
            10 PMPP-PRO-RATA-Z      PIC S9(4) USAGE COMP.
            10 PMPP-PRO-RATA-N      PIC S9(4) USAGE COMP.
            10 PMPP-BWERT-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-RWERT-SUMME     PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-BZ-DAUER        PIC S9(4) USAGE COMP.
            10 PMPP-BEITRAG-JAHR    PIC S9(7)V9(2) USAGE COMP-3.
            10 PMPP-NEUST-KZ        PIC X(1).
            10 PMPP-TARIF-SCHLGRP   PIC X(15).
            10 KNR-TARIF-KZ         PIC S9(4) USAGE COMP.
            10 PMPP-TARIF-GRP       PIC S9(4) USAGE COMP.
            10 PMPP-DAUER-GSMT      PIC S9(4) USAGE COMP.
            10 PMPP-HAFTZ-VON       PIC X(10).
            10 PMPP-HAFTZ-BIS       PIC X(10).
            10 PMPP-PG-TARIF        PIC X(15).
            10 PMPP-RWERT-BEITRAG   PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PROV-BEITRAG    PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PROV-VERDIENT   PIC S9(4) USAGE COMP.
            10 PMPP-VERS-SUMME      PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-BUZTAR          PIC S9(4) USAGE COMP.
            10 PMPP-AE-KZ           PIC X(1).
            10 PMPP-PG-UST-BETRAG   PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-PG-UST-SATZ     PIC S9(3)V9(3) USAGE COMP-3.
            10 PMPP-VSP-KOMBI-NR    PIC S9(05) USAGE COMP.
            10 PMPP-VSP-KOMBI-BZ    PIC X(3).
            10 PMPP-VSP-FN-BER      PIC X(1).
            10 PMPP-VSP-FN-REL      PIC X(1).
            10 PMPP-TLSN-TARIF      PIC X(050).
            10 PMPP-TLSN-LAUFZEIT   PIC S9(004) COMP.
            10 PMPP-TLSN-VRBRAUCH   PIC S9(009) COMP.
            10 KNR-TLSN-STORNO      PIC S9(004) COMP.
            10 PMPP-TLSN-Z-NR       PIC X(020).
            10 PMPP-GEL-GEVO        PIC X(7).
            10 PMPP-GEL-TARBEZ      PIC X(20).
            10 PMPP-GEL-RABST       PIC X(5).
            10 PMPP-GEL-EALTER      PIC S9(4) USAGE COMP.
            10 PMPP-NBB-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-AUW-SUMME       PIC S9(9)V9(2) USAGE COMP-3.
            10 PMPP-VERTRAGSTEIL    PIC S9(4) USAGE COMP.
      ******************************************************************

