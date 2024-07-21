      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0901 DATENFELDER FÜR DIALOG PO24         *
      *                          'PROVISIONSDETAIL PROV-ERGEBNIS'    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    20.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!20.04.98!NEU                             !BOHLINGER *
      *         !25.05.98!VM-KENNZEICHEN,                 !BOHLINGER *
      *         !        !KNR-BERCH-WAEHRUNG-BK           !          *
      *         !28.05.98!PMPT-FAELLIG-..                 !BOHLINGER *
      *         !01.09.98!PDPT-ID                         !BOHLINGER *
      *         !15.02.99!PVER-VNR-ALT                    !BOHLINGER *
      * APA00132!07.05.01!NEUES FELD PRO-RATA-SUMME       ! STORCH   *
      * APA00172!25.02.02!NEUES FELD PG-SUMME             ! STORCH   *
      * FPA00540!03.03.03!NEUES FELD PGV-KZ               ! STORCH   *
      * APA00341!22.02.05!ERWEITERUNG HAFTUNG-DAUER       ! SCHEIDEL *
      *         !        !UND HAFTUNG-GESAMT              !          *
      * APA00414!12.01.06!NOCH EINE ERWEITERUNG VON       ! SCHEIDEL *
      *         !        !HAFTUNGSDAUER UND -GESAMT       !          *
      * APA00622!09.08.07!NEUE FELDER STRUKTURTERMIN      ! WANG     *
      *         !        !UND AUSSCHUET-AMBUS             !          *
      * FPA00963!17.03.08!PMPR-PG-BEITRAG HINZU           ! WANG     *
      * APA01526!02.05.13!FELDER DES NEUEN BILDES 3       ! STORCH   *
      * APA01932!14.04.15!NEUE FELDER AUF BILD 1          ! STORCH   *
      * APA02069!08.01.16!VERSICHERTE-PERSON NEU          ! WANG     *
      * APA02208!08.02.16!NEUES FELD STEUERJAHR           ! STORCH   *
      * APA02220!10.03.16!4 VOR-UND NACHKOMMASTELLEN      ! WANG     *
      *         !        !/GRUND-PROV-STZ                 !          *
      *         !11.04.16!PMPR-PG-PROV-STZ ANPASSEN       ! WANG     *
      * APA02309!31.10.16!T-NUMMER NEU (GES 48)           ! KÖPRÜCÜ  *
      * APA02275!31.01.17!NEUE FELDER FÜR FLEX-N          ! STORCH   *
      * APA02353!01.03.17!NEUE FELDER FÜR TELESON         ! STORCH   *
      * APA02579!07.05.18!NEUE FELDER FÜR FRISCHER        ! STORCH   *
      *         !        !WIND                            !          *
      * APA02278!06.08.18!NEUES FELD HIST. UNR            ! STORCH   *
      * APA02884!23.09.19!NEUES FELD BEZUGSJAHR           ! STORCH   *
      * DSY03267!19.01.21!NEUE FELDER SEITE 2             ! STORCH   *
      * DSY04417!17.01.22!NEUE FELDER SEITE 2             ! STORCH   *
      * DSY04566!25.05.22!NEUES FELD VERTRAGSTEIL         ! STORCH   *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATENFELDER FÜR DIE EIN- UND      *
      *    AUSGABEDATEN DES DIALOGS PO24                             *
      *                                                              *
      ****************************************************************

           25 PO-DRPA0901-DATEN.
             30 DATEN-PO24.
      ***       KEYFELDER
                35 PEST-ID            PIC S9(015)       PACKED-DECIMAL.
                35 PMPT-FAELLIG-VON   PIC  X(010).
                35 PMPT-FAELLIG-BIS   PIC  X(010).
                35 PDPT-ID            PIC S9(015)       PACKED-DECIMAL.
                35 PDPT-BUCH-RFOLGE   PIC S9(004) BINARY.

      ***       DATENFELDER MASKE 1
                35 PVER-VNR-ALT       PIC  X(015).
                35 PDPT-PRODART       PIC S9(004) BINARY.
                35 PDPT-DGV           PIC S9(004) BINARY.
                35 PMPR-PG-SUMME      PIC S9(009)V99    PACKED-DECIMAL.
                35 PMPR-PG-BEITRAG    PIC S9(009)V99    PACKED-DECIMAL.
                35 PPFL-BEITRAG-PT    PIC S9(009)V99    PACKED-DECIMAL.
                35 PVPT-PPFL-BEGINN   PIC  X(010).
                35 PMPT-GSMT-PROV     PIC S9(009)V99    PACKED-DECIMAL.
                35 PMBE-GSMT-PROV-DAT PIC  X(010).
                35 PEST-STRUK-DAT     PIC  X(010).
                35 PMPT-PPFL-BEITRAG  PIC S9(009)V99    PACKED-DECIMAL.
                35 PMPT-PPFL-SUMME    PIC S9(009)V99    PACKED-DECIMAL.
                35 PMBE-PG-RECH-NR    PIC  X(012).
                35 PMPT-GRUND-PROV    PIC S9(009)V99    PACKED-DECIMAL.
                35 PMPT-EINH          PIC S9(007)V99    PACKED-DECIMAL.

                35 PMPT-VSTAT         PIC  X(004).
                35 AVERM-KD-BETR      PIC S9(009) BINARY.
                35 PMPR-PG-PROV-STZ   PIC S9(004)V9(04) PACKED-DECIMAL.
                35 PMPT-UMST-BETRAG   PIC S9(009)V99    PACKED-DECIMAL.
                35 PVPT-LAUFZEIT      PIC S9(004) BINARY.
                35 KNR-ZAHLWEISE      PIC S9(004) BINARY.
                35 KNR-ZAHLWEISE-BM   PIC  X(010).
                35 PMPT-UMST-SATZ     PIC S9(003)V9(03) PACKED-DECIMAL.
                35 GRUND-PROV-STZ     PIC S9(004)V9(04) PACKED-DECIMAL.
                35 PDPT-MA-ABR-DAT    PIC  X(010).
250522          35 PMPR-VERTRAGSTEIL  PIC S9(004) BINARY.

      ***       DATENFELDER MASKE 2
                35 S-HAFTUNG              PIC  X(001).
                35 HAFTUNG-DAUER-MM       PIC  X(004).
                35 HAFTUNG-GESAMT-MM      PIC  X(004).
                35 HAFTUNG-VON            PIC  X(010).
                35 HAFTUNG-BIS            PIC  X(010).
                35 PRO-RATA-SUMME         PIC S9(009)V99 PACKED-DECIMAL.
                35 PRKO-KOND-NR-GSP       PIC S9(004) BINARY.
                35 KOND-REGEL-GSMT-PROV-BL PIC  X(040).
                35 PMBE-PG-ABR-DAT        PIC  X(010).
                35 PMBE-PG-EINL-DAT       PIC  X(010).
                35 PMPT-LAST-UPD-DAT      PIC  X(010).
                35 PMPT-KENNUNG           PIC  X(008).
                35 PMPR-PGV-KZ            PIC  X(001).
                35 PMBE-STRUKTURTERMIN    PIC  X(010).
                35 PMPR-AUSSCHUET-AMBUS   PIC S9(011)V99 PACKED-DECIMAL.
                35 KNR-PMBE-ART           PIC S9(004) BINARY.
                35 VERSICHERTE-PERSON     PIC S9(002) BINARY.
                35 PG-T-NUMMER            PIC  X(020).
310117          35 PMPT-VSP-KOMBI-NR      PIC S9(005) BINARY.
310117          35 PMPT-VSP-KOMBI-BZ      PIC  X(003).
310117          35 PMPT-VSP-FN-BER        PIC  X(001).
310117          35 PMPT-VSP-FN-REL        PIC  X(001).
310117          35 PMPT-VSP-FN-FP-KZ      PIC  X(001).
010317          35 PMPR-TLSN-TARIF        PIC  X(050).
010317          35 PMPR-TLSN-LAUFZEIT     PIC S9(004) BINARY.
010317          35 PMPR-TLSN-VRBRAUCH     PIC S9(009) BINARY.
010317          35 KNR-TLSN-STORNO        PIC S9(004) BINARY.
010317          35 TLSN-STORNO-TEXT       PIC  X(020).
010317          35 PMPR-TLSN-Z-NR         PIC  X(020).
010317          35 KNR-TLSN-PARTNER       PIC S9(004) BINARY.
010317          35 TLSN-PARTNER-TEXT      PIC  X(020).
190121          35 KZ-VORSCHUESS-PROV     PIC  X(001).
190121          35 KZ-E-ANTRAG            PIC  X(001).
190121          35 KZ-ERSATZ              PIC  X(001).


      ***       DATENFELDER MASKE 3
                35 PMBE-RATENBEITRAG      PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-BWERT-SUMME       PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-RWERT-SUMME       PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-BZ-DAUER          PIC S9(004) BINARY.
                35 PMPR-DAUER-GSMT        PIC S9(004) BINARY.
                35 PMPR-BEITRAG-JAHR      PIC S9(007)V99 PACKED-DECIMAL.
                35 PMPR-PROV-BEITRAG      PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-RWERT-BEITRAG     PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-VERS-SUMME        PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-PRORATA-BEITRAG   PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-PPFL-BEITRAG      PIC S9(009)V99 PACKED-DECIMAL.
                35 PMPR-PRO-RATA-Z        PIC S9(004) BINARY.
                35 PMPR-PRO-RATA-N        PIC S9(004) BINARY.
                35 PMPR-PPFL-SUMME        PIC S9(009)V99 PACKED-DECIMAL.
                35 PVER-URSPR-POL-DAT     PIC  X(010).
                35 PMBE-POL-DAT           PIC  X(010).
                35 PMBE-PROV-BEGINN       PIC  X(010).
                35 PMBE-PROV-ABLAUF       PIC  X(010).
                35 PMBE-INKST-DAT         PIC  X(010).
                35 KNR-INK-ART            PIC S9(004) BINARY.
                35 KNR-INK-ART-BM         PIC  X(010).
                35 PMBE-VVG-KZ            PIC  X(001).
                35 PMPR-NEUST-KZ          PIC  X(001).
                35 KNR-DEPOT-KZ-BK        PIC  X(004).
                35 KNR-DEPOT-KZ-BM        PIC  X(010).
                35 PMBE-KZ-EIGENV         PIC  X(001).
                35 PMPR-PG-TARIF          PIC  X(015).
                35 KNR-RAB-ST             PIC S9(004) BINARY.
                35 KNR-RAB-ST-BK          PIC  X(003).
                35 KNR-DYN-KZ2-BK         PIC  X(004).
                35 KNR-DYN-KZ2-BM         PIC  X(010).
                35 PMPR-PG-PRODART        PIC S9(004) BINARY.
                35 PMBE-SAM-NR            PIC S9(009) BINARY.
070518          35 PMBE-GEL-KENNUNG       PIC  X(006).
070518          35 PMPR-GEL-GEVO          PIC  X(007).
070518          35 PMPR-GEL-RABST         PIC  X(005).
070518          35 PMPR-GEL-EALTER        PIC S9(004) BINARY.
070518          35 KNR-BEW-HRKFT          PIC S9(004) BINARY.
070518          35 KNR-BEW-HRKFT-BM       PIC  X(010).
060818          35 PMBE-HIST-UNR          PIC S9(004) BINARY.
170122          35 PMPR-NBB-SUMME         PIC S9(009)V99 PACKED-DECIMAL.
170122          35 PMPR-AUW-SUMME         PIC S9(009)V99 PACKED-DECIMAL.

      ***       DATENFELDER MASKE PO25
                35 VM-KENNZEICHEN         PIC  X(001).
                35 KNR-BERCH-WAEHRUNG-BK  PIC  X(004).
                35 PMPR-PROV-VERDIENT     PIC S9(004) BINARY.
                35 PDPT-UST-JAHR          PIC S9(004) BINARY.
230919          35 PMBE-BEZUGSJAHR        PIC S9(004) BINARY.
      *
