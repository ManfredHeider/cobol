      *****************************************************************
      *    COPY-MEMBER: DRPA0240                                      *
      *    --------------------------------------------------------   *
      *    STANDARD-KONSTANTEN SPEZIELL FüR PROJEKT 'DAISY'           *
      *                                                               *
      *    IM DRAT0009 WERDEN ALLGEMEINE KONSTANTEN DEFINIERT, DIE    *
      *    UNABHäNGIG VOM PROJEKT GüLTIG SIND. HIER DAGEGEN WERDEN    *
      *    NUR SOLCHE KONSTANTEN DEFINIERT, DIE INNERHALB 'DAISY'     *
      *    GüLTIG SIND.                                               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: AUERBACH                                         *
      *    DATUM:    31.01.1997                                       *
      *****************************************************************
      *    VERSION   1    08.06.95 16:19:49
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME     *
      * --------------------------------------------------------------*
      * AFPA0002!31.01.97! NEU                             ! AUERBACH *
      * AFPA0002!11.03.97! KORREKTUR K-VSTAT 'O---'        ! AUERBACH *
      * AFPA0002!08.04.97! K-VTF HINZUGEFÜGT               ! SCHÄFER  *
      *         !        !                                 !          *
      * AFPA0012!20.06.97! NEUE ZVP-ZURODUNGEN             ! WOLFF    *
      *         !        !                                 !          *
      * AFPA0010!24.06.97! K-VSTAT-ABGERECHNET HINZU-      ! STORCH   *
      *         !        ! GEFüGT                          !          *
      *         !        !                                 !          *
      * AFPA0020!07.07.97! K-SELEKTIONSKRIT-....           ! BOHLINGER*
      *         !        !                                 !          *
      * AFPA0012!13.08.97! NEU: K-PVPT-ZEILEN-MAX          ! WOLFF    *
      *         !        !                                 !          *
      * AFPA0012!14.08.97! NEU: K-PEBD-ZEILEN-MAX          ! WOLFF    *
      *         !        !                                 !          *
      * AFPA0012!19.08.97! NEU: K-ABWEICHUNG               ! WOLFF    *
      *         !        !                                 !          *
      * AFPA0012!26.09.97! K-ABWEICHUNG MIT 2 NACH-        ! WOLFF    *
      *         !        ! KOMMASTELLEN                    !          *
      *         !        !                                 !          *
      * AFPA0012!01.10.97! NEUE ZVP-FUNKTIONEN             ! WOLFF    *
      *         !        !                                 !          *
      * AFPA0012!16.10.97! NEUE ZVP-FUNKTIONEN             ! JäGER    *
      * AFPA0012!20.10.97! K-ABWEICHUNG NACH DRAT0163      ! AUERBACH *
      *         !28.10.97! K-GESAMTPROV-PRUEFEN --->       ! MUND     *
      *         !        ! K-GSMT-PROV-PRUEFEN             !          *
      *         !        !                                 !          *
      * AFPA0012!03.11.97! K-PVPT-ZEILEN-MAX GEÄND.;       ! WOLFF    *
      *         !        ! K-PEBD-ZEILEN-MAX RAUS-         !          *
      *         !        ! GENOMMEN (SIEHE K-PE-S-         !          *
      *         !        ! ZEILEN-MAX);                    !          *
      *         !        ! NEU: K-PEST-ZEILEN-MAX          !          *
      *         !        ! NEU: K-PMPR-ZEILEN-MAX          !          *
      *         !        ! NEU: K-PMPT-ZEILEN-MAX          !          *
      *         !        ! NEU: K-AMV-VVNR-MAX             !          *
      * AFPA0012!07.11.97! NEUE ZVP-FUNKTIONEN             ! JäGER    *
      *         !        !                                 !          *
      *         !        !                                 !          *
      *         !        ! NEU: K-PMBE-ZEILEN-MAX          ! PARENT   *
      *         !        ! NEU: K-PMDS-ZEILEN-MAX          ! PARENT   *
      *         !        ! NEU: K-PDPT-ZEILEN-MAX          ! PARENT   *
      *         !        ! NEU: K-PDPE-ZEILEN-MAX          ! PARENT   *
      * APA00012!07.01.98! NEU: K-PMPP-ZEILEN-MAX          ! JäGER    *
      *         !        !                                 !          *
      * APA00012!12.01.98! NEU: K-PVOR-ZEILEN-MAX          ! JäGER    *
      *         !        !                                 !          *
      * APA00012!13.01.98! NEU: K-PROD-EINF-DAISY          ! WOLFF    *
      *         !        !                                 !          *
      * APA00012!18.03.98! NEU: K-DAT-MIN-DAISY            ! WOLFF    *
      *         !        ! NEU: K-DAT-MAX-DAISY            !          *
      *         !        !                                 !          *
      * APA00012!30.03.98! NEU: K-LAUFZEIT-PRUEFEN         ! WOLFF    *
      *         !        ! K-GSMT-PROV-EVS-PRUEFEN         !          *
      *         !        ! K-GSMT-PROV-P85-PRUEFEN         !          *
      *         !        ! K-GSMT-PROV-PRUEF-KOND          !          *
      *         !        !                                 !          *
      * APA00012!06.05.98! NEU: K-VSTAT-EH-GEKUERZT        ! BOHLINGER*
      *         !        !      K-VSTAT-EH-NEUTRAL         !          *
      * APA00012!09.06.98!NEU: K-EINHEITEN-BERECHNEN2      ! BORK
      *         !        !     K-GRUNDPROV-BERECH-2        !          *
      * APA00012!03.08.98!NEU: K-VSTAT-BUSTAND-UNZUL       ! FOLTZ
      *         !        !                                 !          *
      * APA00012!31.08.98!NEU: K-PEBD-LAENGE               ! PARENT   *
      * APA00012!31.08.98!NEU: K-PDPE-LAENGE               ! PARENT   *
      *         !        !                                 !          *
      * APA00012!        ! CRPA0018                        !          *
      *         !22.09.98!   - GEÄNDERT: K-PROD-EINF-      ! WOLFF    *
      *         !        !               DAISY             !          *
      *         !23.09.98!   - NEU: K-NORMIEREN-JNP        ! WOLFF    *
      *         !        !                                 !          *
      * APA00012!24.09.98! NEU:                            ! STORCH   *
      *         !        ! K-PROV-STRUKT-DSY-PRUEF         !          *
      *         !        ! K-INNENDIENST-PRUEFEN           !          *
      *         !        !                                 !          *
      * APA00012!27.10.98! NEU:                            ! BOHLINGER*
      *         !        ! K-VM-EINFUEGEN-AUSTAUSCHEN      !          *
      *         !        ! K-VM-LOESCHEN                   !          *
      * APA00012!13.04.99! VSTAT-GRUPPE EINGEFÜGT          ! MUND     *
      * APA00012!30.04.99! REIHENFOLGE BEZOGENE            ! MUND     *
      *         !        ! VSTATI ERGÄNZT                  !          *
      *         !        !                                 !          *
      * APA00039!25.06.99! K-PROD-EINF-DAISY RAUSGE-       ! WOLFF    *
      *         !        ! NOMMEN                          !          *
      * APA00012!05.07.99! REIHENFOLGEBEZOGENEN            ! MUND     *
      *         !        ! VSTATUS I263 ERGÄNZT            !          *
      * APA00012!07.07.99! I258, I259 ALS REIHENFOLGE      ! MUND     *
      *         !        ! FEHLER                          !          *
      * APA00012!07.07.99! NEUE VSTAT-GRUPPE FÜR           ! MUND     *
      *         !        ! FOLGEFEHLER                     !          *
      * APA00091!19.04.00! K-MAX-DIFF-PROV-SATZ=149        ! HEIDER   *
      * APA00090!27.04.00! PVER- UND PEW-ZUSTAND NEU       ! MUND     *
      * APA00067!28.04.00! NEUE ZVP-VERFAHREN              ! STORCH   *
      *         !08.05.00! NEUE REIHENFOLGEFEHLER          ! STORCH   *
      *         !        ! I191                            !          *
      * APA00090!17.05.00! NEUE VSTATGRP                   ! GRAY     *
      * APA00090!29.05.00! PVER- U.PEW-ZUSTAND             ! MUND     *
      *         !        ! ENTFERNT                        !          *
      * APA00105!25.09.00! NEUE KONSTANTEN FÜR SUCH-       ! JÄGER    *
      *         !        ! FUNKTION VORBEWEGUNG            !          *
      * APA00113!20.11.00! I178 ERGÄNZT                    ! MUND     *
      * APA00131!18.05.01! K-CKV-HAFTZEIT HINZU            ! KLEEMEIER*
      * AAN00315!23.08.01! K-STANDARTLAUFZEIT-RB           ! RAHNER   *
      * APA00150!14.09.01! K-STANDARD-LAUFZEIT-X           ! STORCH   *
      * APA00162!30.10.01! K-SUCHE-FAELLIG-GT-O-PGV        ! KLEEMEIER*
      * APA00173!11.01.02! K-ABWEICHUNG-DVAG-SUM           ! MUND     *
      *         !        ! K-ABWEICHUNG-DVAG-EINZEL        !          *
      *         !        ! K-ABWEICHUNG-DVAG-WIEN-SUM      !          *
      *         !        ! K-ABWEICHUNG-DVAG-WIEN-EIN      ZEL        *
      * APA00165!04.02.02! SUCHFUNKTIONEN 5, 6, 7          ! KLEEMEIER*
      *         !        ! + WIEDERVORLAGEFRIST-WIK        !          *
      * APA00165!12.03.02! I284+I286 = REIHENFOLGEF.       ! KLEEMEIER*
      * APA00191!15.05.02! NEU: K-LAUFZEITSATZ UND         ! STORCH   *
      *         !        ! K-MINDESTLAUFZEIT               !          *
      * APA00207!15.07.02! K-SONSTIGE-HAFTZEIT             ! STORCH   *
      * APA00206!14.08.02! K-GENERALI-LEBEN-HAFTZEIT       ! MUND     *
      * APA00206!30.08.02! K-WIENER-LEBEN-HAFTZEIT         ! KLEEMEIER*
      * FPA00426!20.09.02! K-DAT-NULL-DAISY-SST            ! STORCH   *
      * FPA00464!18.11.02! K-PEST-ZEILEN-MAX ERHOEHT       ! KLEEMEIER*
      * FPA00505!06.01.03! K-WIEDERINKRAFTSETZUNG-LV       ! Jäger    *
      *         !        ! K-LV-GESELLSCHAFTEN  NEU        !          *
      * FPA00453!25.02.03! K-SUCHFUNKTION ERWEITERT        ! JÄGER    *
      *         !27.02.03! FRIST FÜR WIK 30 TAGE           ! JÄGER    *
      *         !        ! NEUE PGV-KENNZEICHEN            !          *
      *         !07.03.03! I276 ALS REIHENFOLGEFEHLER      ! JÄGER    *
      * APA00226!26.06.03! I289 MIT PRINTE 0               ! JÄGER    *
      * FPA00632!15.09.03! K-PEST-ZEILEN-MAX ERHOEHT       ! STORCH   *
      * APA00310!17.06.03! K-LAUFZEITSATZ AENDERN          ! WANG     *
      * APA00323!08.11.04! K-KONVERTIERUNG                 ! WANG     *
      * APA00295!01.12.04! K-AVBG-VTF HINZU                ! WANG     *
      * APA00330!27.12.04! Suche-2013 neu                  ! Jäger    *
      *         !        ! Anpasung                        ! Jäger    *
      * APA00341!22.02.05! HAFTZEIT DER CKV IN TAGEN       ! SCHEIDEL *
      *         !        ! WIEDERVORLAGEFRIST BEI          !          *
      *         !        ! KV 0 TAGE                       !          *
      * APA00349!10.05.05! K-RISIKO HINZU                  ! WANG     *
      * APA00351!12.07.05! HAFTZEIT DIT                    ! WANG     *
      * FPA00790!29.07.05! WIEDERVORLAGEFRIST CKV=30       ! SCHEIDEL *
      * APA00414!12.01.06! HAFTZEIT-MAX CKV 1096 D.        ! SCHEIDEL *
      *         !        ! DIV. HAFTZEITEN 4STELLIG        !          *
      * AAN00614!25.04.06! ZVP: EH-BERECHNUNG PEW          ! HEIDER   *
      * APA00511!27.07.06! Anpassung WV-Frist bei          ! Jäger    *
      *         !        ! WIK KV auf 10 Tage              !          *
      * APA00517!11.08.06! Zurück auf 30 Tage              ! Jäger    *
      * APA00495!09.10.06! NEUE KONSTANTEN ZU 1068         ! JÄGER    *
      * AAN00655!12.10.06! ZVP'ANTRAG TRANSAKTIONEN'       ! HEIDER   *
      * APA00495!23.10.06! NEUE KONSTANTEN ZU 3026         ! JÄGER    *
      *         !        ! NEUE KONSTANTE WG. HAFTZE.      ! JÄGER    *
      * FKI01068!05.01.07! K-DAT-MAX-DAISY AUF             ! HÜBNER   *
      *         !        !   31.12.2599 GESETZT            !          *
      * APA00631!22.05.07! K-PVPT-ZEILEN-MAX AUF 100       ! SCHEIDEL *
      * APA00638!10.07.07! NEUE KONSTANTE ZU 3013,         ! SCHEIDEL *
      *         !        !  WIK-FRIST-INVEST               !          *
      * APA00622!30.11.07! NEU: K-FOLGE                    ! STORCH   *
      * APA00724!26.02.08! NEU: K-FONDS-HAFTZEIT-VSP       ! WANG     *
      * APA00866!29.01.09! NEUE FREIGABEFÄHIGER            ! STORCH   *
      *         !        ! VSTATUS K109                    !          *
      * APA00940!21.09.09! I300 ALS REIHENFOLGEFEHLER      ! BUCH     *
      * APA01349!14.03.12! HAFTZEIT-MAX CKV 1827 TAGE      ! SCHEIDEL *
      * APA01310!07.08.12! NEUSTORNO VORBEWEG.             ! BUCH     *
      * APA01465!08.10.12! NEUE SUCHFUNKTION FÜR PGV       ! BUCH     *
      *         !        ! 4014                            !          *
      * APA01478!11.12.12! NEU:K-SONSTIGE-HAFTZEIT-12      ! WANG     *
      *         !        !    ,K-SONSTIGE-HAFTZEIT-60      !          *
      * APA01525!07.03.13! NEU:                            ! WANG     *
      *         !        !K-GENERALI-LEBEN-HAFTZEIT-0      !          *
      * APA01786!28.05.14! K-PRODUKT-KEINE-RUNDUNG         ! STORCH   *
      * APA01882!08.09.14!Wiedervorlagefrist CKV = 0       ! Buch     *
      * APA01980!22.01.15!AML SERVICE FEE PGV 4018         ! BUCH     *
      * FPA01463!30.09.15!WV-FRIST CKV AUF 5 TAGE          ! SCHEIDEL *
      * APA01932!31.03.15!K-UMSATZ-STEUER-SATZ NEU         ! WANG     *
      *         !17.04.15!K110 NEU                         !          *
      *         !18.05.15!K-PDPE-LAENGE ÄNDERN             !          *
      *         !19.05.15!NEUE KONSTANTEN FÜR LUCY         ! KÖPRÜCÜ  *
      *         !21.10.15!K-ABWEICHUNG-UMST-BETRAG         ! WANG     *
      * APA02169!12.01.16!CKV: NEUE SUCHFUNKTIONEN         ! KÖPRÜCÜ  *
      * FPA01515!14.06.16!SCHALTER UMSATZSTEUERPFL.PG      ! BUCH     *
      * APA02347!14.12.16!EINFÜHRUNG BKK-LINDE             ! BUCH     *
      * APA02353!17.02.17!TELESON HINZU                    ! WANG     *
      * APA02360!20.04.17!FINGERHAUS HINZU                 ! WANG     *
      * APA02438!20.04.17!DEVOLO HINZU                     ! WANG     *
      * APA02396!20.04.17!LOHNWERK HINZU                   ! WANG     *
      * APA02396!16.06.17!SCHALTER PPFL-BEGINN             ! Kranz    *
      * APA02473!28.08.17!Cadooz hinzu                     ! Kranz    *
      * APA02698!18.09.18!K-DAT-MAX-DAISY FÜR CENTRAL      ! BUCH     *
      *         !        !ERHÖHT                           !          *
      * APA02733!11.10.18!WIEDERVORLAGEFRIST GES.11        ! WANG     *
      *         !        !=0                               !          *
      * APA02573!06.12.18!Schalter Vertragslöschung        ! Kranz    *
      * FPA01716!12.03.19!K-PEST-ZEILEN-MAX auf 100        ! KÖPRÜCÜ  *
      * APA02846!18.09.19!NEUE UMSATZSTEUERGESELL.         ! STORCH   *
      * APA02915!26.03.20!NEUE UMSATZSTEUERGESELL. 86      ! BUCH     *
      * APA02930!30.06.20!TEMPORÄRER UMSATZSTEUERSATZ      ! STORCH   *
      * APA02958!19.11.20!Neuer I-STATUS I337              ! KÖPRÜCÜ  *
      * DSY03552!08.06.21! Einf. Sonst.Ges.AT Haftpf.      ! Kranz    *
      * DSY04892!23.08.22! NEUE GES. 80 MIT UST            ! SCHEIDEL *
      * DSY04892!20.09.22! ABLÖSUNG S-UMSATZSTEUER-PG      ! BUCH     *
      *         !        !                                 !          *
      *         !        !                                 !          *
      *         !        !                                 !          *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      ***  NUMERISCHE KONSTANTEN
        01 FILLER.
           05  K-PROD-ZEILEN-MAX         PIC S9(004) BINARY VALUE 15.
 
031197***  MAX. ANZAHL DER PEBD-EINTRÄGE             (DRPA0433)
031197***  MAX. ANZAHL DER PDPE-EINTRÄGE             (DRPA0451)
           05  K-PE-S-ZEILEN-MAX         PIC S9(004) BINARY VALUE 20.
 
031197***  MAX. ANZAHL DER PVPT-EINTRÄGE (DRPA0432)
031197***  MAX. ANZAHL DER PVPT-EINTRÄGE (DRPA0420)
220507     05  K-PVPT-ZEILEN-MAX         PIC S9(004) BINARY VALUE 100.
 
031197***  MAX. ANZAHL DER PEST-EINTRÄGE (DRPA0587)
120319     05  K-PEST-ZEILEN-MAX         PIC S9(004) BINARY VALUE 100.
 
120198***  MAX. ANZAHL DER PVOR-EINTRÄGE (DRPA0614)
120198     05  K-PVOR-ZEILEN-MAX         PIC S9(004) BINARY VALUE 10.
 
031197***  MAX. ANZAHL DER PMPR-EINTRÄGE (DRPA0432)
031197     05  K-PMPR-ZEILEN-MAX         PIC S9(004) BINARY VALUE 15.
 
031197***  MAX. ANZAHL DER PMPT-EINTRÄGE (DRPA0421)
031197     05  K-PMPT-ZEILEN-MAX         PIC S9(004) BINARY VALUE 80.
 
070198***  MAX. ANZAHL DER PMPP-EINTRÄGE (DRPA0290)
070198     05  K-PMPP-ZEILEN-MAX         PIC S9(004) BINARY VALUE 50.
 
031197***  MAX. ANZAHL DER VVNR          (DRPA0288)
031197     05  K-AMV-VVNR-MAX            PIC S9(004) BINARY VALUE 10.
 
151297***  MAX. ANZAHL DER PMBE-EINTRÄGE (DRPA0411)
151297     05  K-PMBE-ZEILEN-MAX         PIC S9(004) BINARY VALUE 10.
 
151297***  MAX. ANZAHL DER PMDS-EINTRÄGE (DRPA0570)
151297     05  K-PMDS-ZEILEN-MAX         PIC S9(004) BINARY VALUE 10.
 
291297***  MAX. ANZAHL DER PDPT-EINTRÄGE (DRPA0571)
291297     05  K-PDPT-ZEILEN-MAX         PIC S9(004) BINARY VALUE 80.
 
291297***  MAX. ANZAHL DER PDPE-EINTRÄGE (DRPA0415)
291297     05  K-PDPE-ZEILEN-MAX         PIC S9(004) BINARY VALUE 20.
 
      ***  MAX. SUMME DES DIFFERENZ-PROVISIONS-SATZ JE PE-STRUKTUR
190400     05  K-MAX-DIFF-PROV-SATZ      PIC S9(004) BINARY VALUE +149.
 
310898***  LOGISCHE LÄNGEN VON PEBD UND PDPE INNERHALB PEST BZW. PDPT
310898     05  K-PEBD-LAENGE             PIC S9(004) BINARY VALUE 12.
180515     05  K-PDPE-LAENGE             PIC S9(004) BINARY VALUE 61.
 
      ***  STELLEN-NR DES VERTRIEBSFONDS
           05  K-VTF                     PIC S9(009) BINARY
                                         VALUE 8000020.
011204     05  K-AVBG-VTF                PIC S9(009) BINARY
011204                                   VALUE 5000020.
 
180398***  DATUM DER UNTER-/OBERGRENZEN IN DAISY
180398     05  K-DAT-MIN-DAISY           PIC  X(010) VALUE '02.01.1900'.
180918     05  K-DAT-MAX-DAISY           PIC  X(010) VALUE '31.12.9999'.
 
200902***  "NULL-DATUM", FÜR DIE VB-ABRECHNUNG BENÖTIGT
200902     05  K-DAT-NULL-DAISY-SST      PIC X(010) VALUE '00.00.0000'.
 
220205***  HAFTZEITDAUER FUER CENTRAL IN TAGEN
140312     05  K-CKV-HAFTZEIT-MAX        PIC  9(004) VALUE 1827.
 
141216***  HAFTZEITDAUER FUER CENTRAL IN TAGEN
141216     05  K-BKK-HAFTZEIT-MAX        PIC  9(004) VALUE 1827.
 
150702***  HAFTZEITDAUER FUER GESELLSCHAFT 91 IN MONATEN
120106*    05  K-SONSTIGE-HAFTZEIT       PIC  9(003) VALUE  030.
120106     05  K-SONSTIGE-HAFTZEIT       PIC  9(004) VALUE   30.
111212     05  K-SONSTIGE-HAFTZEIT-12    PIC  9(004) VALUE  365.
111212     05  K-SONSTIGE-HAFTZEIT-60    PIC  9(004) VALUE 1826.
 
140802***  HAFTZEITDAUER FUER GESELLSCHAFT 26 IN MONATEN
120106*    05  K-GENERALI-LEBEN-HAFTZEIT     PIC  9(003) VALUE 036.
120106     05  K-GENERALI-LEBEN-HAFTZEIT     PIC  9(004) VALUE  36.
311006     05  K-GENERALI-LEBEN-HAFTZEIT-12  PIC  9(004) VALUE  12.
311006     05  K-GENERALI-LEBEN-HAFTZEIT-36  PIC  9(004) VALUE  36.
311006     05  K-GENERALI-LEBEN-HAFTZEIT-48  PIC  9(004) VALUE  48.
311006     05  K-GENERALI-LEBEN-HAFTZEIT-60  PIC  9(004) VALUE  60.
070313     05  K-GENERALI-LEBEN-HAFTZEIT-0   PIC  9(004) VALUE   0.

080621***  HAFTZEITDAUER FUER GESELLSCHAFT 55 IN MONATEN
080621     05  K-SOGATHP-HAFTZEIT-60 PIC  9(003) VALUE 060.             

 
300802***  HAFTZEITDAUER FUER GESELLSCHAFT 16 IN MONATEN
120106     05  K-WIENER-LEBEN-HAFTZEIT   PIC  9(004) VALUE  36.
 
271204***  HAFTZEITDAUER FUER GESELLSCHAFT 48 IN MONATEN
260208     05  K-FONDS-HAFTZEIT-VSP      PIC  9(004) VALUE  12.
120106     05  K-FONDS-HAFTZEIT          PIC  9(004) VALUE  24.
100707     05  K-FONDS-HAFTZEIT-RRP      PIC  9(004) VALUE  60.
 
120705***  HAFTZEITDAUER FUER GESELLSCHAFT 40 IN MONATEN
120106     05  K-HAFTZEIT-DIT          PIC  9(004) VALUE  12.
120106     05  K-HAFTZEIT-DIT-SPAR-VL  PIC  9(004) VALUE 100.
 
060103***  LV-GESELLSCHAFTEN + DWS
060103     05 K-STORNO-GRUPPE            PIC S9(004) BINARY.
060103        88 K-LV-GESELLSCHAFT       VALUE 0025 0026.
271204        88 K-FONDS-GESELLSCHAFT    VALUE 0048.
141216        88 K-KV-GESELLSCHAFT       VALUE 0050 0051.
111018        88 K-SACH-GESELLSCHAFT     VALUE 0011.
 
280514***  LAUFZEITABHÄNGIGE PRODUKTE AM-LEBEN
280514     05 K-LZ-PRODUKT               PIC S9(004) BINARY.
280514        88 K-LZ-PRODUKTE-AML       VALUE 0142.
 
040202***  WIEDERVORLAGEFRIST FUER WIK IN TAGEN
270203     05  K-WIEDERVORLAGEFRIST-WIK    PIC S9(004) BINARY VALUE 30.
060103     05  K-WIEDERVORLAGEFRIST-WIK-LV PIC S9(004) BINARY VALUE 0.
300915     05  K-WIEDERVORLAGEFRIST-WIK-KV PIC S9(004) BINARY VALUE 5.
100707     05  K-WIEDERVORLAGEFRIST-WIK-IN PIC S9(004) BINARY VALUE 0.
111018     05  K-WIEDERVORLAGEFRIST-WIK-SH PIC S9(004) BINARY VALUE 0.
 
230801**** FUER DIE LAUFZEIT BEI RECHTSBERATUNGSGESELLSCHAFTEN
230801**** (GES. 81 UND 82) WIRD IM ANTRAGSBEREICH KEINE LAUFZEIT
230801**** ERMITTELT. DIE LAUFZEIT SPIELT ABER EINE ROLLE IM DAISY
230801**** ABGRECHNETEN GESCHAEFT (BEI LAUFZEIT > 60 MONATE HOEHERE
230801**** GESAMTPROVISION). IM EINGEREICHTEN GESCHAEFT, ZUR EINHEITEN
230801**** BERECHNUNG WIRD DIE GRUNDPROVISION VERWENDET. DIESE
230801**** IST UNABHAENGIG VON DER LAUFZEIT. DIE VERSCHIEDENEN LAUF-
230801**** ZEITEN WERDEN IN DER DAISY/PEW-KONFIGURATION ABGELEGT. DAMIT
230801**** DIE PEW-AUFBEREITUNGSPROGRAMME (GREIFEN AUF KONFIGURATION ZU)
230801**** NICHT ABBRECHEN, WIRD EIN STANDARTWERT IN DIE LAUFZEIT
230801**** GESTELLT. DIESER WERT BETRAEGT 12 MONATE
230801*
230801     05  K-STANDART-LAUFZEIT-RB   PIC 9(002)  VALUE 12.
140901     05  K-STANDARD-LAUFZEIT-X    PIC X(003)  VALUE '012'.
 
150502     05  K-LAUFZEITSATZ           PIC S9(3)V9(3) COMP-3
170604*                                     VALUE 2,000.
170604                                      VALUE 5,000.
150502     05  K-MINDESTLAUFZEIT        PIC  9(002) VALUE 4.
 
 
      *** ALPHANUMERISCHE KONSTANTEN
       01  FILLER.
 
      ***  DIESER TEXT WIRD AUF DER PRäSENTATIONSSCHICHT (DIALOG, LISTE)
      ***  AUSGEGEBEN, WENN EIN VSTATUS NICHT IN DER TABELLE DEFINIERT
      ***  IST UND DEMZUFOLGE KEIN TEXT ERMITTELT WERDEN KANN.
           05  K-VSTAT-TEXT-NOT-FOUND    PIC  X(050) VALUE
               '--> VSTATUS IST NICHT IN DER DATENBANK DEFINIERT !'.
 
      ***  KONSTANTEN DER FUNKTIONEN FüR DIE ZVP-ZUORDNUNG
           05  K-AVERM-VM-ERMITTELN        PIC  X(020)
                                       VALUE 'AVERM/VM ERMITTELN  '.
           05  K-EMPF-TYPEN-SETZEN         PIC  X(020)
                                       VALUE 'EMPF-TYPEN SETZEN   '.
           05  K-PROV-STRUK-PRUEFEN        PIC  X(020)
                                       VALUE 'PROV-STRUK PRÜFEN   '.
           05  K-PEW-PRUEF-UND-BERECH      PIC  X(020)
                                       VALUE 'PEW PRÜF UND BERECH '.
           05  K-PEW-INHALTL-PRUEFEN       PIC  X(020)
                                       VALUE 'PEW INHALTL. PRÜFEN '.
           05  K-GES-PROV-BERECHNEN        PIC  X(020)
                                       VALUE 'GESAMTPROV. BERECHN.'.
           05  K-GRUND-PROV-BERECHNEN      PIC  X(020)
                                       VALUE 'GRUNDPROV. BERECHNEN'.
           05  K-MA-PROV-BERECHNEN         PIC  X(020)
                                       VALUE 'MA-PROV. BERECHNEN  '.
           05  K-EINHEITEN-BERECHNEN       PIC  X(020)
                                       VALUE 'EINHEITEN BERECHNEN '.
200697     05  K-FAELLIGKEIT-PRUEFEN       PIC  X(020)
200697                                 VALUE 'FÄLLIGKEIT PRÜFEN   '.
200697     05  K-GSMT-PROV-PRUEFEN         PIC  X(020)
200697                                 VALUE 'GESAMTPROV. PRÜFEN  '.
200697     05  K-PM-PRUEFEN-NORMIEREN      PIC  X(020)
200697                                 VALUE 'PM PRÜFEN/NORMIEREN '.
200697     05  K-NPM-INHALT-PRUEFEN        PIC  X(020)
200697                                 VALUE 'NPM INHALT. PRÜFEN  '.
200697     05  K-NPM-VERD-PRUEFEN          PIC  X(020)
200697                                 VALUE 'NPM VERD./PRÜFEN    '.
200697     05  K-EXT-VNR-AUS-PM-ERM        PIC  X(020)
200697                                 VALUE 'EXT. VNR AUS PM ERM.'.
011097     05  K-GSMT-PROV-BERECH-2        PIC  X(020)
011097                                 VALUE 'GSMT-PROV BERECH. 2 '.
011097     05  K-BEITRAG-NORMIEREN         PIC  X(020)
011097                                 VALUE 'BEITRAG NORMIEREN   '.
161097     05  K-KOND-GSMT-PROV            PIC  X(020)
161097                                 VALUE 'KONDART GSMT-PROV   '.
161097     05  K-KOND-GRUND-PROV           PIC  X(020)
161097                                 VALUE 'KONDART GRUND-PROV  '.
161097     05  K-KOND-EINH                 PIC  X(020)
161097                                 VALUE 'KONDART EINH        '.
161097     05  K-KOND-KD-BETR              PIC  X(020)
161097                                 VALUE 'KONDART KD-BETR     '.
161097     05  K-KOND-EE-PRUEFEN           PIC  X(020)
161097                                 VALUE 'KONDART EE-PRUEFEN  '.
161097     05  K-KOND-AP-FP-REGELN         PIC  X(020)
161097                                 VALUE 'KONDART AP-FP-REGELN'.
071197     05  K-PROD-N-D-PROV-PRUEFEN     PIC  X(020)
071197                                 VALUE 'PROD-NDP-PRUEFEN    '.
071197     05  K-WV-ZEITRAUM-ERMITTELN     PIC  X(020)
071197                                 VALUE 'WVZEITRAUM-ERMITTELN'.
071197     05  K-AEHNL-PROD-PRUEFEN-T      PIC  X(020)
071197                                 VALUE 'AEHNL-PROD-PRUEFEN-T'.
071197     05  K-AEHNL-PROD-PRUEFEN-E      PIC  X(020)
071197                                 VALUE 'AEHNL-PROD-PRUEFEN-E'.
300398     05  K-LAUFZEIT-PRUEFEN          PIC  X(020)
300398                                 VALUE 'LAUFZEIT PRÜFEN     '.
300398     05  K-GSMT-PROV-EVS-PRUEFEN     PIC  X(020)
300398                                 VALUE 'GSMT-PROV EVS PRÜFEN'.
300398     05  K-GSMT-PROV-P85-PRUEFEN     PIC  X(020)
300398                                 VALUE 'GSMT-PROV P85 PRÜFEN'.
300398     05  K-GSMT-PROV-PRUEF-KOND      PIC  X(020)
300398                                 VALUE 'GSMT-PROV PRÜF. KOND'.
090698     05  K-GRUND-PROV-BERECH-2       PIC  X(020)
090698                                 VALUE 'GRUNDPROV. BERECH.2 '.
090698     05  K-EINHEITEN-BERECHNEN2      PIC  X(020)
090698                                 VALUE 'EINHEITEN BERECHNEN2'.
230998     05  K-NORMIEREN-JNP             PIC  X(020)
230998                                 VALUE 'NORMIEREN JNP       '.
240998     05  K-PROV-STRUKT-DSY-PRUEF     PIC  X(020)
240998                                 VALUE 'PROV-STRUKT-DSY PRÜF'.
240998     05  K-INNENDIENST-PRUEFEN       PIC  X(020)
240998                                 VALUE 'INNENDIENST PRüFEN  '.
280400     05  K-PIN-PRUEFEN               PIC  X(020)
280400                                 VALUE 'PIN-PRUEFEN         '.
280400     05  K-VVNR-PRUEFEN              PIC  X(020)
280400                                 VALUE 'VVNR-PRUEFEN        '.
280400     05  K-PRODART-ERMITTELN         PIC  X(020)
280400                                 VALUE 'PRODART-ERMITTELN   '.
280400     05  K-FAEL-MIG-DAT-PRUEF        PIC  X(020)
280400                                 VALUE 'FAEL-MIG-DAT PRUEFEN'.
250406     05  K-EINHEITEN-PEW-BERECHNEN   PIC  X(020)
250406                                 VALUE 'EINHEITEN PEW BERECH'.
121006     05  K-ANTRAG-TRANSAKTIONEN      PIC  X(020)
121006                                 VALUE 'ANTRAG TRANSAKTIONEN'.
      *
110102*    ABWEICHUNGEN BEI EINZELBEWEGUNGEN BZW. KUMULIERTEN BEWEGUNGEN
110102*    UNTERSCHIEDEN NACH DVAG UND DVAG-WIEN.
110102     05  K-ABWEICHUNG-DVAG-SUM          PIC S9(002)V99
110102                                        PACKED-DECIMAL VALUE 0,05.
110102     05  K-ABWEICHUNG-DVAG-EINZEL       PIC S9(002)V99
110102                                        PACKED-DECIMAL VALUE 0,05.
110102     05  K-ABWEICHUNG-DVAG-WIEN-SUM     PIC S9(002)V99
110102                                        PACKED-DECIMAL VALUE 0,35.
110102     05  K-ABWEICHUNG-DVAG-WIEN-EINZEL  PIC S9(002)V99
110102                                        PACKED-DECIMAL VALUE 0,07.
 
      ***  GROBKLASSIFIZIERUNG VSTATUS (1. STELLE)
           05  K-VSTAT-KLASSE-SCHWER       PIC X(001) VALUE 'S'.
           05  K-VSTAT-KLASSE-FORMAL       PIC X(001) VALUE 'F'.
           05  K-VSTAT-KLASSE-INHALT       PIC X(001) VALUE 'I'.
           05  K-VSTAT-KLASSE-KOMPETENZ    PIC X(001) VALUE 'K'.
           05  K-VSTAT-KLASSE-HINWEIS      PIC X(001) VALUE 'H'.
           05  K-VSTAT-KLASSE-ERLEDIGT     PIC X(001) VALUE 'E'.
           05  K-VSTAT-KLASSE-OFFEN        PIC X(001) VALUE 'O'.
 
130499***  VSTATUSGRUPPEN
130499     05 S-VSTAT-GRUPPE              PIC X(004).
130499        88 S-VSTATGRP-FREIGABFAEHIGESTATI
290109           VALUE 'K100' 'K101' 'K102' 'K103' 'K104' 'K107' 'K109'
170415                 'K110'.
130499        88 S-VSTATGRP-REIHENFOLGE-FEHLER
300499           VALUE 'I183' 'I185' 'I187' 'I189' 'I198' 'I207' 'I214'
070303                 'I284' 'I286' 'I276'
070799           'I255' 'I263' 'I258' 'I259' 'I191'
210909*          'I300'.
191120           'I300' 'I337'.
130499        88 S-VSTATGRP-PE-STRUKTUR-FEHLER
130499         VALUE 'I104' 'I105'
130499         'I106' 'I108' 'I109' 'I111' 'I117' 'I122' 'I124'.
070799        88 S-VSTATGRP-FOLGEFEHLER
070799         VALUE 'I200' 'I172'.
170500        88 S-VSTATGRP-PRINTE-UNDEF
260603         VALUE 'I271' 'I272' 'I273' 'I178' 'I289'.
 
110102***  GESELLSCHAFT UNTERSCHIEDEN NACH DEUTSCHEN UND ÖSTERREICHERN
110102     05 S-GES-GRUPPE              PIC 9(004).
110102        88 DVAG-GESELLSCHAFT
110102           VALUES 0010 0040 0041 0043 0046 0047 0049 0050 0062
110102                 0063 0064 0066 0068 0070 0081 0082.
110102        88 DVAG-WIEN-GESELLSCHAFT
110102           VALUES 0044 0045 0065 0085.
 
      ***  SPEZIELLE VSTATI
           05  K-VSTAT-OK                  PIC X(004) VALUE 'E000'.
           05  K-VSTAT-OK-MANUELL          PIC X(004) VALUE 'E001'.
           05  K-VSTAT-STORNO              PIC X(004) VALUE 'E900'.
240697     05  K-VSTAT-ABGERECHNET         PIC X(004) VALUE 'E901'.
110397     05  K-VSTAT-GEMELDET-AN-PG      PIC X(004) VALUE 'O000'.
110397     05  K-VSTAT-NICHT-GEMELDET      PIC X(004) VALUE 'O100'.
060598     05  K-VSTAT-EH-GEKUERZT         PIC X(004) VALUE 'H120'.
060598     05  K-VSTAT-EH-NEUTRAL          PIC X(004) VALUE 'N   '.
030898     05  K-VSTAT-BUSTAND-UNZUL       PIC X(004) VALUE 'I246'.
      *
      ***  KONSTANTENNAMEN FüR SELEKTIONSKRITERIEN
070797     05  K-SELEKT-KRITERIUM-PRODART  PIC X(002) VALUE 'PA'.
070797     05  K-SELEKT-KRITERIUM-LAUFZEIT PIC X(002) VALUE 'LZ'.
      *
      ***  KONSTANTENNAMEN FÜR SUCHFUNKTION VORBEWEGUNG
250900     05  K-SUCHE-FAELLIG-LE-O-PGV    PIC 9(001) VALUE 1.
250900     05  K-SUCHE-FAELLIG-LE-F-1031   PIC 9(001) VALUE 2.
250900     05  K-SUCHE-FAELLIG-EQ-F-4012   PIC 9(001) VALUE 3.
250900     05  K-SUCHE-FAELLIG-EQ-F-4013   PIC 9(001) VALUE 4.
250900     05  K-SUCHE-FAELLIG-LE-O-PVPT   PIC 9(001) VALUE 5.
040202     05  K-SUCHE-FAELLIG-EQ-F-3010   PIC 9(001) VALUE 6.
040202     05  K-SUCHE-FAELLIG-LE-F-3010   PIC 9(001) VALUE 7.
040202     05  K-SUCHE-FAELLIG-EQ-P-3010   PIC 9(001) VALUE 8.
301001     05  K-SUCHE-FAELLIG-GT-O-PGV    PIC 9(001) VALUE 9.
250203     05  K-SUCHE-NEUZUGANG           PIC 9(002) VALUE 10.
271204     05  K-SUCHE-FAELLIG-LE-F-2013   PIC 9(002) VALUE 11.
091006     05  K-SUCHE-FAELLIG-O-PS-3026   PIC 9(002) VALUE 12.
091006     05  K-SUCHE-FAELLIG-O-PS-1068   PIC 9(002) VALUE 13.
100707     05  K-SUCHE-FAELLIG-EQ-3013     PIC 9(002) VALUE 14.
070812     05  K-SUCHE-NEUSTORNO-1080      PIC 9(002) VALUE 15.
070812     05  K-SUCHE-NEUSTORNO-2080      PIC 9(002) VALUE 16.
070812     05  K-SUCHE-NEUSTORNO-3080      PIC 9(002) VALUE 17.
070812     05  K-SUCHE-NEUSTORNO-4080      PIC 9(002) VALUE 18.
070812     05  K-SUCHE-NEUSTORNO-1051      PIC 9(002) VALUE 19.
081012     05  K-SUCHE-FAELLIG-EQ-F-4016   PIC 9(002) VALUE 20.
220115     05  K-SUCHE-AML-SF-4018         PIC 9(002) VALUE 21.
190515     05  K-SUCHE-DV-O-RECHNUNGSNR    PIC 9(002) VALUE 22.
190515     05  K-SUCHE-DV-RECHNUNGSNR      PIC 9(002) VALUE 23.
120116     05  K-SUCHE-NEUZUGANG-MIT-PS    PIC 9(002) VALUE 24.
120116     05  K-SUCHE-FAELLIG-EQ-4030     PIC 9(002) VALUE 25.
      *
      ***  KONSTANTENNAMEN FÜR PGV-KENNZEICHEN BEI LV
270203     05  K-DYNAMIK                   PIC X(001) VALUE 'D'.
270203     05  K-HERABSETZUNG              PIC X(001) VALUE 'H'.
081104     05  K-KONVERTIERUNG             PIC X(001) VALUE 'K'.
100505     05  K-RISIKO                    PIC X(001) VALUE 'R'.
091006     05  K-NEUHAFTUNG                PIC X(001) VALUE 'N'.
301107     05  K-FOLGE                     PIC X(001) VALUE 'F'.
      *
      ***  KONSTANTENNAMEN FÜR VM IN DER PE-STRUKTUR ANPASSEN
281098     05  K-VM-EINFUEGEN-AUSTAUSCHEN  PIC 9(002) VALUE 01.
281098     05  K-VM-LOESCHEN               PIC 9(002) VALUE 02.
      *
      ***  KONSTANTEN DES UMSATZSTEUERSATZES
310315     05  K-UMSATZ-STEUER-SATZ        PIC 9(003)V999 VALUE 19.
300620     05  K-UMSATZ-STEUER-SATZ-TEMP   PIC 9(003)V999 VALUE 16.
 
      ***  SCHALTER FÜR UMSATZSTEUERPFLICHTIGE GESELLSCHAFTEN
200922*    05  S-UMSATZSTEUER-PG           PIC S9(004) BINARY.
200922*      88 S-UMSATZSTEUER-PG-JA       VALUE 80 86 87
200922*                                          90 92 93 94 96 98.
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRPA0240) ********
 
160617***  SCHALTER FÜR PROVISIONSPFLICHTER BEGINN
160617     05  S-PPFL-BEGINN               PIC S9(004) BINARY.
160617       88 S-PPFL-BEGINN-DAT-VERTRAG          VALUE 87 90 93 94 96.
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRPA0240) ********
 
061218***  SCHALTER FÜR Vertragslöschung (DSGVO)
061218     05  S-VERTRAG-LOESCHEN          PIC S9(004) BINARY.
061218       88 S-VERTRAG-LOESCHEN-JA VALUE 10 20 50 51 70 81 90
                                      15 16 25 26 33 38 39 44 69 75.
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRPA0240) ********



