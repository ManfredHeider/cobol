      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0332                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    PARTNERGESELLSCHAF - TCKI0332                             *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME     *
      * --------------------------------------------------------------*
      * AKI00001!28.09.98! NEUERSTELLUNG                   !V.D.GOLTZ *
      * --------!--------!---------------------------------!----------*
      *         !17.10.98! Ergänzt um die Kundennr.        !v.d.Goltz *
      *         !        ! Gruppen.                        !          *
      * --------!--------!---------------------------------!----------*
      *AKI00274 !24.04.02! Neue Ges. DB24, BfG in          !v.d.Goltz *
      *         !        ! SEB geändert.                   !          *
      * --------!--------!---------------------------------!----------*
      *AKI00302 !17.09.02! Ges. DB24 in DB geändert.       !v.d.Goltz *
      * --------!--------!---------------------------------!----------*
      *AKI00657 !21.05.07! Ges. AGI eingefuegt             ! HSE      *
      *         !        ! (DIT DEAKTIVIERT)               !          *
      * --------!--------!---------------------------------!----------*
      *AKI00803 !01.05.09! GES. SEB-BANK (66)              !SCHLEICHER*
      *         !        !  EINGEFÜGT                      !          *
      * --------!--------!---------------------------------!----------*
      *AKI00966 !13.08.10! DVAG-Wien eingefügt             !I. Frenzel*
      * --------!--------!---------------------------------!----------*
      *AKI01342 !03.03.15! Commerzbank (COBA)              !D. Dylong *
      *         !        ! Deutsche Verechnungsstel-       !          *
      *         !        ! le Mittelstand (DVM)            !          *
      * --------!--------!---------------------------------!----------  *
      *AKI01499 !23.02.17! BKK-Linde                       ! MKE      *
      * AKI01548!25.09.17 Teilbetriebsabspaltung           ! IFR      *
      *         !         Generali AT Bank                 !          *
      *         !         (Ges 39 + 69)                    !          *
      * --------!--------!---------------------------------!----------*
      * AKI01570!08.03.18! Geiger Edelmetall (79) u.       !BST       *
      *         !        ! Generali Deutschland            !          *
      *         !        ! (11, 21, 42)                    !          *
      *         !        !                                 !          *
      * --------!--------!---------------------------------!----------*
      * AKI01665!21.02.19! GC&C (12)                       ! SDM      *
      *         !        ! Generali Basisdaten             !          *
      *---------------------------------------------------------------*
      * AKI01711!21.08.19! G67: Integration DSL-Bank /     !Schumacher*
      *         !        !      Refactoring                !          *
      *---------------------------------------------------------------*
      *DBO-10172!09.02.23!G80: Neue GesellschaftsID für    !u131774   *
      *         !        !Geiger/Umsatzsteuerpflichtige Pro!          *
      *         !        !dukte                            !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      ****************************************************************
       01  FILLER.
           05 K-AMV                     PIC S9(004) BINARY VALUE 10.
           05 K-GEN-DE-SACH             PIC S9(004) BINARY VALUE 11.
           05 K-GEN-DE-GCANDC           PIC S9(004) BINARY VALUE 12.
           05 K-SACH-ALLIANZ            PIC S9(004) BINARY VALUE 15.
           05 K-SACH-GENERALI           PIC S9(004) BINARY VALUE 16.
           05 K-AML                     PIC S9(004) BINARY VALUE 20.
           05 K-GEN-DE-LEBEN            PIC S9(004) BINARY VALUE 21.
           05 K-LEBEN-ALLIANZ           PIC S9(004) BINARY VALUE 25.
           05 K-LEBEN-GENERALI          PIC S9(004) BINARY VALUE 26.
      *    05 K-VOFUE                   PIC S9(004) BINARY VALUE 21.
           05 K-GENERALI-CAPITAL        PIC S9(004) BINARY VALUE 33.
           05 K-INV-DWS                 PIC S9(004) BINARY VALUE 38.
           05 K-INV-GENERALI            PIC S9(004) BINARY VALUE 39.
      *    05 K-DIT                     PIC S9(004) BINARY VALUE 40.
           05 K-AGI                     PIC S9(004) BINARY VALUE 40.
           05 K-III                     PIC S9(004) BINARY VALUE 41.
           05 K-GEN-DE-INV              PIC S9(004) BINARY VALUE 42.
           05 K-INV-AGI                 PIC S9(004) BINARY VALUE 44.
      *    05 K-BFG-INVEST              PIC S9(004) BINARY VALUE 46.
      *    05 K-BFG-IMMOINVEST          PIC S9(004) BINARY VALUE 47.
           05 K-SEB-INVEST              PIC S9(004) BINARY VALUE 46.
           05 K-SEB-IMMOINVEST          PIC S9(004) BINARY VALUE 47.
      *    05 K-DB24                    PIC S9(004) BINARY VALUE 48.
           05 K-DB                      PIC S9(004) BINARY VALUE 48.
           05 K-DGI                     PIC S9(004) BINARY VALUE 49.
           05 K-CENTRAL                 PIC S9(004) BINARY VALUE 50.
           05 K-BKK-LINDE               PIC S9(004) BINARY VALUE 51.
           05 K-DREBA                   PIC S9(004) BINARY VALUE 62.
           05 K-SEB-BANK                PIC S9(004) BINARY VALUE 66.
           05 K-DSL-BANK                PIC S9(004) BINARY VALUE 67.
           05 K-COBA                    PIC S9(004) BINARY VALUE 68.
           05 K-GENERALI-BANK           PIC S9(004) BINARY VALUE 69.
           05 K-BADENIA                 PIC S9(004) BINARY VALUE 70.
           05 K-S-BAU                   PIC S9(004) BINARY VALUE 75.
           05 K-ROLAND                  PIC S9(004) BINARY VALUE 80.
           05 K-ADVOCARD                PIC S9(004) BINARY VALUE 81.
           05 K-DMB                     PIC S9(004) BINARY VALUE 82.
           05 K-DVM                     PIC S9(004) BINARY VALUE 90.
           05 K-REPLIKATION             PIC S9(004) BINARY VALUE 99.
           05 K-FREMDVERTRAG            PIC S9(004) BINARY VALUE 999.
           05 K-AMV-GRUPPE              PIC S9(004) BINARY VALUE 10.
           05 K-AML-GRUPPE              PIC S9(004) BINARY VALUE 20.
           05 K-BANK-GRUPPE             PIC S9(004) BINARY VALUE 40.
           05 K-CENTRAL-GRUPPE          PIC S9(004) BINARY VALUE 50.
           05 K-BADENIA-GRUPPE          PIC S9(004) BINARY VALUE 70.
           05 K-GEIGER                  PIC S9(004) BINARY VALUE 79.
           05 K-GEIGER-UST              PIC S9(004) BINARY VALUE 80.
           05 K-RECHTSCHUTZ-GRUPPE      PIC S9(004) BINARY VALUE 80.
           05 K-REPLIKATION-GRUPPE      PIC S9(004) BINARY VALUE 99.
      *
      ********  ENDE COPY-MEMBER DSKI0332  ***********








