      *****************************************************************
      *    COPY-MEMBER: DRPA1245                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLENSATZ MA-ABRECHNUNG                           *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    25.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!25.05.99!2.1 ! NEU                       ! WOLFF    *
      * APA00388!04.10.05!2.2 ! PRODSCHL DREISTELLIG      ! SCHEIDEL *
      *         !        !    ! PDPT-FIRST-INSERT UND     !          *
      *         !        !    ! PMBE-RFORLGE-NR HINZU     !          *
      *         !        !    ! SATZLAENGE 300 BYTES      !          *
      * FPA00846!16.05.06!2.3 ! HAFTZEIT-KZ HINZU         ! SCHEIDEL *
      * APA00452!10.11.06!2.4 ! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * APA01526!23.04.13!3.0 ! UMSTELLUNG AML KALYPSO    ! BUCH     *
      * APA01932!09.04.15!2.11! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA01932!16.07.15!    ! ZIELSTATUS ERGÄNZT        ! BUCH     *
      * APA02208!29.01.16!2.12! PMPT-ID ERGÄNZT           ! BUCH     *
      * APA02208!22.03.16!2.13! STEUERJAHR ERGÄNZT        ! BUCH     *
      * APA02304!25.01.17!2.14! EINFÜHRUNG FLEXN          ! BUCH     *
      * APA02492!01.09.17!      EINFÜHRUNG FOLGE-NEU      ! BUCH     *
      * APA02927!21.08.20!2.20! PEST-AVERM-KARR           ! BUCH     *
      * DSY04261!08.12.21!    ! NBB-AUW-SUMME UND KZ      ! BUCH     *
      * DSY04569!12.05.22!    ! WIEDERHERSTELLUNG 08.12.21! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
             06 DRPA1245.
               07 PGEB-GES                         PIC  9(02).
               07 PROD-PROD-SCHL-SORT              PIC  9(03).
               07 VNR-SORT                         PIC  X(15).
               07 VNR-SORT-AML REDEFINES VNR-SORT.
                  10 VNR-SORT-FHRND-NULL           PIC  X(01).
                  10 VNR-SORT-SPARTE               PIC  X(01).
                  10 VNR-SORT-ORD-NR               PIC  X(07).
                  10 VNR-SORT-UNTER-NR             PIC  X(03).
                  10 VNR-SORT-SCHICHT-NR           PIC  X(03).
               07 TARIF-KZ                         PIC  9(04).
               07 TARIF-BEZEICHNUNG                PIC  X(15).
               07 PROV-BETRAG-AUS                  PIC S9(09)V99.
               07 PROV-BETRAG-RUECK                PIC S9(09)V99.
               07 KUNDENNAME                       PIC  X(20).
               07 VP-NAME                          PIC  X(20).
               07 DGV                              PIC  9(05).
               07 AVERM                            PIC  9(07).
               07 PRODART                          PIC  9(02).
               07 HAFTZ-VON                        PIC  X(10).
               07 HAFTZ-BIS                        PIC  X(10).
               07 PRO-RATA-SUMME                   PIC S9(09)V99.
               07 EINH-EMP                         PIC S9(10)V9.
               07 ST-NR                            PIC  9(07).
               07 DIFF-PROV-STF                    PIC S9(03)V99.
               07 VM-GESCHFT-KZ                    PIC  X(01).
               07 EIGEN-KZ                         PIC  X(01).
               07 PPFL-SUMME                       PIC S9(09)V99.
               07 FAELLIG-VON                      PIC  X(10).
               07 PDPT-ID                          PIC  9(15).
               07 VNR                              PIC  X(15).
               07 VNR-AML REDEFINES VNR.
                  10 VNR-FHRND-NULL                PIC  X(01).
                  10 VNR-SPARTE                    PIC  X(01).
                  10 VNR-ORD-NR                    PIC  X(07).
                  10 VNR-UNTER-NR                  PIC  X(03).
                  10 VNR-SCHICHT-NR                PIC  X(03).
               07 RFOLGE-NR                        PIC  9(03).
               07 HAFTZEIT-KZ                      PIC  X(02).
               07 FIRST-INSERT                     PIC  X(26).
               07 MA-ABR-DAT                       PIC  X(10).
               07 DYN-KZ-1                         PIC  9(04).
               07 DYN-KZ-2                         PIC  9(04).
               07 POL-DAT                          PIC  X(10).
               07 PRO-RATA-FAK-Z                   PIC  9(03).
               07 PRO-RATA-FAK-N                   PIC  9(03).
               07 PROD-PROD-SCHL                   PIC  9(03).
               07 PROV-SCHL                        PIC  X(05).
               07 EMP-TYP                          PIC  9(04).
               07 RAB-STF                          PIC  9(04).
               07 RWERT-SUMME                      PIC S9(09)V99.
               07 SAMMEL-NR                        PIC  9(09).
               07 BESVV                            PIC  9(01).
               07 PGV                              PIC  9(04).
               07 BUZ-TARIF                        PIC  9(04).
               07 BEW-SCHL.
                  10 BEW-HRKFT                     PIC  X(01).
                  10 BEW-ART                       PIC  X(06).
               07 UST-BETRAG-AUS                   PIC S9(09)V99.
               07 UST-BETRAG-RUECK                 PIC S9(09)V99.
               07 UST-KORREKTUR-KZ                 PIC  X(01).
               07 KORREKTUR-ZIEL-STATUS            PIC  X(01).
               07 UST-BERECH-KZ                    PIC  X(01).
               07 UST-ABR-NR                       PIC  X(15).
               07 UST-SATZ                         PIC  9(02)V99.
               07 PG-RECH-NR                       PIC  X(12).
               07 FAELLIG-BIS                      PIC  X(10).
               07 PMPT-ID                          PIC  9(15).
               07 STEUERJAHR                       PIC  X(04).
               07 VSP-FLEXN-RELEVANZ               PIC  X(01).
               07 VSP-FLEXN-FOLGE-KZ               PIC  X(01).
               07 VSP-PRODUKT-KOMBI-NR             PIC  9(05).
               07 VSP-PRODUKT-KOMBI-BEZ            PIC  X(03).
               07 VSP-FLEXN-BERECHTIGUNG           PIC  X(01).
               07 PEST-AVERM-KARR                  PIC  9(07).
               07 NBB-SUMME                        PIC S9(09)V99.
               07 AUW-SUMME                        PIC S9(09)V99.
               07 GENERALI-BONUS-KZ                PIC  X(01).
      
