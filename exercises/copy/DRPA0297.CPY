      *****************************************************************
      *    COPY-MEMBER: DRPA0297                                      *
      *    --------------------------------------------------------   *
      *    DATENZEILE PMPP                                            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.05.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.05.97! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!29.07.97!    ! EINBAU PG-BEITRAG,        ! WOLFF    *
      *         !        !    !        PG-SUMME           !          *
      *         !        !    ! UMBENENNEN KOND-NR-GSP IN !          *
      *         !        !    !            KOND-NR        !          *
      * AFPA0012!29.08.97!    ! ÄNDERUNG VON FELDNAMEN,   ! WOLFF    *
      *         !        !    ! KOND-NR ENTFÄLLT          !          *
      * AFPA0012!08.09.97!    ! KOND-NR-GSP WIEDER REIN   ! WOLFF    *
      * AFPA0012!26.09.97!    ! ÄNDERUNG DER STELLEN BEI  ! WOLFF    *
      *         !        !    ! BETRÄGEN                  !          *
      *         !        !    ! PREFIX PKON IN PRKO BEI   !          *
      *         !        !    ! KOND-NR-GSP GEÄNDERT      !          *
      * AFPA0012!13.10.97!    ! PG-DATEN UMBENANNT        ! WOLFF    *
      * APA01526!19.02.13!2.1 ! NEUE ATTRIBUTE HINZU      ! STORCH   *
      * APA01932!30.03.15!2.2 ! NEUE ATTRIBUTE FÜR LUCY   ! KÖPRÜCÜ  *
      * APA02220!10.03.16!2.3 ! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PMPP-PG-PROV-STZ         !          *
      * APA02275!30.01.17!2.4 ! ERWEITERT UM FlexN-Felder ! KÖPRÜCÜ  *
      * APA02353!20.02.17!2.6 ! NEUE ATTRIBUTE HINZU      ! WANG     *
      * APA02579!24.04.18!2.7 ! GENERALI LEBEN            ! SCHEIDEL *
      * DSY04038!14.09.21!    ! NBB- UND AUW-SUMME NEU    ! BUCH     *
      * DSY04569!12.05.22!    ! VERTRAGSTEIL HINZU        ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT NUR DIE DATENZEILE FÜR PMPP.          *
      *                                                              *
      ****************************************************************
      *
             30 PMPP-DATEN.
                35 PMPP-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PMPG-ID             PIC S9(015)       PACKED-DECIMAL.
                35 PGEB-GES            PIC S9(004)       BINARY.
                35 PROD-PROD-SCHL      PIC S9(004)       BINARY.
                35 PMPP-PGV            PIC S9(004)       BINARY.
                35 PMPP-PPFL-BEITRAG   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-PPFL-SUMME     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-PG-BEITRAG     PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-PG-SUMME       PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-PG-PROV-STZ    PIC S9(004)V9(04) PACKED-DECIMAL.
                35 PMPP-PG-GSMT-PROV   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PRKO-KOND-NR-GSP    PIC S9(004)       BINARY.
                35 PMPP-PG-PROD-DATEN  PIC  X(030).
                35 PMPP-PG-PRODART     PIC S9(004)       BINARY.
                35 KNR-DYN-KZ-1        PIC S9(004)       BINARY.
                35 KNR-DYN-KZ-2        PIC S9(004)       BINARY.
                35 KNR-BEW-HRKFT       PIC S9(004)       BINARY.
                35 PMPP-BEW-SCHL       PIC S9(009)       BINARY.
                35 PMPP-PRO-RATA-Z     PIC S9(004)       BINARY.
                35 PMPP-PRO-RATA-N     PIC S9(004)       BINARY.
                35 PMPP-BWERT-SUMME    PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-RWERT-SUMME    PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-BZ-DAUER       PIC S9(004)       BINARY.
                35 PMPP-BEITRAG-JAHR   PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-NEUST-KZ       PIC  X(001).
                35 PMPP-TARIF-SCHLGRP  PIC  X(015).
                35 KNR-TARIF-KZ        PIC S9(004)       BINARY.
                35 PMPP-TARIF-GRP      PIC S9(004)       BINARY.
                35 PMPP-DAUER-GSMT     PIC S9(004)       BINARY.
                35 PMPP-HAFTZ-VON      PIC  X(010).
                35 PMPP-HAFTZ-BIS      PIC  X(010).
                35 PMPP-PG-TARIF       PIC  X(015).
                35 PMPP-RWERT-BEITRAG  PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-PROV-BEITRAG   PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-PROV-VERDIENT  PIC S9(004)       BINARY.
                35 PMPP-VERS-SUMME     PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-BUZTAR         PIC S9(004)       BINARY.
                35 PMPP-AE-KZ          PIC  X(001).
                35 PMPP-PG-UST-BETRAG  PIC S9(011)V9(02) PACKED-DECIMAL.
                35 PMPP-PG-UST-SATZ    PIC S9(003)V9(03) PACKED-DECIMAL.
                35 PMPP-VSP-KOMBI-NR   PIC S9(005)       BINARY.
                35 PMPP-VSP-KOMBI-BZ   PIC  X(003).
                35 PMPP-VSP-FN-BER     PIC  X(001).
                35 PMPP-VSP-FN-REL     PIC  X(001).
                35 PMPP-TLSN-TARIF     PIC  X(050).
                35 PMPP-TLSN-LAUFZEIT  PIC S9(002)       BINARY.
                35 PMPP-TLSN-VRBRAUCH  PIC S9(010)       BINARY.
                35 KNR-TLSN-STORNO     PIC S9(004)       BINARY.
                35 PMPP-TLSN-Z-NR      PIC  X(020).
                35 PMPP-GEL-GEVO       PIC  X(007).
                35 PMPP-GEL-TARBEZ     PIC  X(020).
                35 PMPP-GEL-RABST      PIC  X(005).
                35 PMPP-GEL-EALTER     PIC S9(004)       BINARY.
                35 PMPP-NBB-SUMME      PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-AUW-SUMME      PIC S9(009)V9(02) PACKED-DECIMAL.
                35 PMPP-VERTRAGSTEIL   PIC S9(004)       BINARY.
      ********************** ENDE COPY-MEMBER ***********************

