      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0107.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRAT0107.CPv  $
      *
      *   Rev 2.14   Sep 19 2016 13:12:16   T13EX44
      *APA02151: BasisRente verteilte Abschlussprovisionen
      *
      *   Rev 2.13   Aug 12 2015 12:23:50   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: MSC
      *
      *   Rev 2.12   Jul 24 2007 12:19:30   t13ae34
      *AAN00764: Funktion Setzen Antragstatus
      *
      *   Rev 2.11   16 May 2007 13:12:56   t13ae46
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.10   01 Dec 2006 14:06:56   t13ae34
      *AAT00006: DRAT0044 für AAN00655
      *
      *   Rev 2.9   08 Mar 2001 13:47:22   T13AE34
      *ACV00015-CTV aus Neudialogen
      *
      *   Rev 2.8   Apr 14 2000 11:18:40   T13EX23
      *APA00067 - Neue RC - K-RC-OK-VERF-UNDEF
      *
      *   Rev 2.7   Jan 18 2000 14:42:54   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.6   Nov 25 1999 09:05:22   T13EX21
      *APA00063 - Neue Konstante K-RC-DTA-B-N-BEENDET
      *
      *
      *   Rev 2.5   Aug 31 1999 08:21:00   T13EX25
      *FPA00063 Neue Konstante definiert
      *
      *   Rev 2.4   Mar 25 1999 12:46:28   T13AE18
      *APA00012 Neue Konstante definiert
      *
      *   Rev 2.3   Mar 12 1999 08:25:48   T13EX49
      *AKI00018
      *
      *   Rev 2.2   Mar 10 1999 14:49:32   T13AE41
      *Neuer RC NPM-auf-PMBE
      *
      *   Rev 2.1   Mar 04 1999 14:59:26   T13EX21
      *neue K-RC's
      *
      *   Rev 1.2   11 Nov 1998 08:59:26   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.1   19 Jun 1998 15:34:26   T13AE34
      *AAT00006 - Konstantencopy
      *
      *   Rev 1.0   Mar 07 1997 09:17:20   P13PVCS
      *Neuerstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KONSTANTEN UPRO RÜCKGABE WERTE(DRAT0107)      *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GIESE                                            *
      *    DATUM:    30.05.1996                                       *
      *****************************************************************
      *    VERSION   0
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !30.05.96! 01 ! NEU                       ! GIESE    *
      *         !        !    !                           !          *
200697* AFPA0012!20.06.97! 02 ! K-RC-VSTAT-GEFUELLT EIN-  ! WOLFF    *
      *         !        !    ! GEFÜGT                    !          *
      *         !        !    !                           !          *
140897* AFPA0012!14.08.97!    ! DIVERSE K-RC-.. EINGEFÜGT ! PRIGGEN  *
140897*         !        !    !                           !          *
011097* AFPA0012!01.10.97!    ! NEUE K-RC'S EINGEFÜGT ALS ! WOLFF    *
011097*         !        !    ! UNTERTEILUNG VON          !          *
011097*         !        !    ! K-RC-VSTAT-GEFUELLT       !          *
011097*         !        !    !                           !          *
281097* AFPA0012!28.10.97!    ! DIVERSE K-RC-.. EINGEFÜGT ! WOLFF    *
281097*         !        !    !                           !          *
061197* AFPA0012!28.10.97!    ! NEU: K-RC-MELDUNGSFEHLER  ! WOLFF    *
111197*         !11.11.97!    ! NEU: K-RC-AEND-MIT-HIST   ! MUND     *
111197*         !        !    !      K-RC-AEND-OHNE-HIST  !          *
121197*         !12.11.97!    ! NEU: K-RC-PEW-NFD-OK      ! MUND     *
121197*         !        !    !      K-RC-PEW-NFD-NOK     !          *
121197*         !        !    !      K-RC-PEW-FD-OK       !          *
121197*         !        !    !      K-RC-PEW-FD-NOK      !          *
121197*         !        !    !      K-RC-NEU-PROD-JA     !          *
121197*         !        !    !      K-RC-NEU-PROD-NEIN   !          *
181197*         !18.11.97!    ! NEU: K-RC-KEY-NOT-UNIQUE  ! Jäger    *
101297*         !10.12.97!    ! NEU: K-RC-ABGERECHNET     ! MUND     *
181297*         !18.12.97!    ! NEU: K-RC-KEINE-EVS-BEARB ! WOLFF    *
280498*         !28.04.98!    ! NEU: K-RC-NICHT-BUCHEN    ! WOLFF    *
      *         !19.06.98!    ! K-RC-EURO-WKZ-DATUM       ! HEIDER   *
      *         !        !    ! K-RC-EURO-WKZ-PRODUKT     ! HEIDER   *
      *         !        !    ! K-RC-EURO-PHASE-HEIMAT    ! HEIDER   *
      *         !        !    ! K-RC-EURO-PHASE-UMSTELLUNG! HEIDER   *
      *         !        !    ! K-RC-EURO-PHASE-EURO      ! HEIDER   *
      *         !        !    !                           !          *
070798*         !07.07.98!    ! K-RC-NEUTRAL-ZULAESSIG    ! PARENT   *
070798*         !07.07.98!    ! K-RC-NEUTRAL-UNZULAESSIG  ! PARENT   *
070798*         !07.07.98!    ! K-RC-UMBUCH-ZULAESSIG     ! PARENT   *
070798*         !07.07.98!    ! K-RC-UMBUCH-UNZULAESSIG   ! PARENT   *
200798*         !20.07.98!    ! K-RC-KEIN-VERTRAG         ! GRÜNER   *
200798*         !20.07.98!    ! K-RC-GBUCH-ERSTELLT       !   FOLTZ  *
200798*         !20.07.98!    ! K-RC-GBUCH-NICHT-ERSTELLBAR   FOLTZ  *
200798*         !20.07.98!    ! K-RC-GBUCH-AUF-NICHT-RELEVANT FOLTZ  *
270798*         !27.07.98!    ! K-RC-NEUT-ZULAESSIG       !   FOLTZ  *
270798*         !27.07.98!    ! K-RC-NEUT-UNZULAESS       !   FOLTZ  *
270798*         !27.07.98!    ! K-RC-NEUT-OK-NOT-FOUND    !   FOLTZ  *
270798*         !27.07.98!    ! K-RC-NEUT-OK              !   FOLTZ  *
270798*         !27.07.98!    ! K-RC-NEUT-OK-ALLE         !   FOLTZ  *
270798*         !30.07.98!    !                           !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-OK               !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-OK-O-NEUT        !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-OK-NEUT-GBA      !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-OK-NUR-NEU       !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-NOK-O-AEND       !   FOLTZ  *
300798*         !30.07.98!    ! K-RC-UMB-NOK              !   FOLTZ  *
091098*         !09.10.98!    ! K-RC-PE-S-NICHT-EIND      !BOHLINGER *
091098*         !09.10.98!    ! K-RC-PE-S-NICHT-ERST      !BOHLINGER *
161098*         !16.10.98!    ! K-RC-UNZULAESSIG          ! STORCH   *
      *         !27.10.98!    ! K-RC-KEIN-AVERM-VORHANDEN !BOHLINGER *
      *         !        !    ! K-RC-KEIN-VM-VORHANDEN    !          *
      *         !        !    ! K-RC-VM-UNZULAESSIG       !          *
      *         !        !    ! K-RC-VM-BEREITS-IN-PE-S   !          *
      *         !10.02.99!    ! K-RC-OK-PEWA-O-PROD       !BOHLINGER *
      *         !24.02.99!    ! K-RC-VST-IN-VGR-NFD       !LÜTTGES   *
      *         !04.03.99!    ! K-RC-OK-KEINE-AEND        !BOHLINGER *
      *         !        !    ! K-RC-OK-PE-S-BEREINIGT    !          *
      *         !04.03.99!    ! K-RC-PEST-IDENTISCH       ! STORCH   *
      *         !        !    ! K-RC-PEST-GLEICH          !          *
      *         !        !    ! K-RC-PEST-UNGLEICH        !          *
090399*         !04.03.99!    ! K-RC-NPM-AUF-PMBE         !LÜTTGES   *
      *         !05.03.99!    ! K-RC-PE-S-VORHANDEN       !BOHLINGER *
      * FPA00063!31.08.99!    ! K-RC-PROD-NOT-FOUND       !JÄGER     *
      * APA00063!06.10.99!    ! K-RC-DTA-B-N-BEENDET      ! STORCH   *
      * APA00???!08.12.99!    ! K-RC-ZULAESSIG            ! STORCH   *
      * APA00067!13.04.00! 2.7! K-RC-OK-VERF-UNDEF        ! GRAY     *
      * ACV00015!08.03.01! 2.9! K-RC-OK-MULT-ROWS         !HEIDER    *
      * AAN00655!22.11.06!2.10! K-RC-OK-MULT-ROWS         !HEIDER    *
      * APA00604!18.04.07!2.11! ABSCHALTUNG PEW           !WANG      *
      * AAN00764!24.07.07!2.12! K-RC-NO-UPDATE            !HEIDER    *
      * APA01932!11.08.15!2.13! K-RC-NOT-OK-SEL &         !SCHEIDEL  *
      *         !        !    ! K-RC-NOT-OK-INS           !          *
      * APA02151!29.08.16!2.14! K-RC-NF-NOTWEN            !SCHEIDEL  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *
       01  FILLER.
           05  K-RC-OK                       PIC  X(03) VALUE 'OK '.
130400     05  K-RC-OK-VERF-UNDEF            PIC  X(03) VALUE 'OKU'.
           05  K-RC-NOK                      PIC  X(03) VALUE 'NOK'.
           05  K-RC-INSERT                   PIC  X(03) VALUE 'INS'.
           05  K-RC-UPDATE                   PIC  X(03) VALUE 'UPD'.
240707     05  K-RC-NO-UPDATE                PIC  X(03) VALUE 'NUP'.
           05  K-RC-EOF                      PIC  X(03) VALUE 'EOF'.
           05  K-RC-NOT-FOUND                PIC  X(03) VALUE 'NFD'.
290816     05  K-RC-NF-NOTWEN                PIC  X(03) VALUE 'NFN'.
           05  K-RC-DUPLICATE-VALUE          PIC  X(03) VALUE 'DVA'.
           05  K-RC-CHANGED                  PIC  X(03) VALUE 'CHA'.
           05  K-RC-NOT-CHANGED              PIC  X(03) VALUE 'NCH'.
           05  K-RC-SPERKZ-NOK               PIC  X(03) VALUE 'SPN'.
           05  K-RC-FEHLZA-NOK               PIC  X(03) VALUE 'FZN'.
           05  K-RC-PROZNT-NOK               PIC  X(03) VALUE 'PZN'.
           05  K-RC-LEER                     PIC  X(03) VALUE 'LER'.
200697     05  K-RC-VSTAT-GEFUELLT           PIC  X(03) VALUE 'VSG'.
140897     05  K-RC-KEINE-AENDERUNG          PIC  X(03) VALUE 'KAE'.
140897     05  K-RC-AEND-AP-AUF-FP           PIC  X(03) VALUE 'AAF'.
140897     05  K-RC-AEND-UND-SPLIT           PIC  X(03) VALUE 'AUS'.
011097     05  K-RC-VSTAT-MIT-FEHL           PIC  X(03) VALUE 'VMF'.
011097     05  K-RC-VSTAT-OHNE-FEHL          PIC  X(03) VALUE 'VOF'.
281097*            (KEE = KEINE ERSATZ-/ERHOEHUNG)
281097     05  K-RC-KEE-KEIN-AVERM           PIC  X(03) VALUE 'KKA'.
281097     05  K-RC-KEE-KEIN-KDB             PIC  X(03) VALUE 'KKK'.
281097     05  K-RC-KEE-KDB-DIFF             PIC  X(03) VALUE 'KKD'.
061197     05  K-RC-MELDUNGSFEHLER           PIC  X(03) VALUE 'MFE'.
111197     05  K-RC-AEND-MIT-HIST            PIC  X(03) VALUE 'AMH'.
111197     05  K-RC-AEND-OHNE-HIST           PIC  X(03) VALUE 'AOH'.
180407*    05  K-RC-PEW-NFD-OK               PIC  X(03) VALUE 'PNO'.
180407*    05  K-RC-PEW-NFD-NOK              PIC  X(03) VALUE 'PNN'.
180407*    05  K-RC-PEW-FD-OK                PIC  X(03) VALUE 'PFO'.
180407*    05  K-RC-PEW-FD-NOK               PIC  X(03) VALUE 'PFN'.
180407     05  K-RC-ANTRAG-NFD-OK            PIC  X(03) VALUE 'ANO'.
180407     05  K-RC-ANTRAG-NFD-NOK           PIC  X(03) VALUE 'ANN'.
180407     05  K-RC-ANTRAG-FD-OK             PIC  X(03) VALUE 'AFO'.
180407     05  K-RC-ANTRAG-FD-NOK            PIC  X(03) VALUE 'AFN'.
121197     05  K-RC-NEU-PROD-JA              PIC  X(03) VALUE 'NPJ'.
121197     05  K-RC-NEU-PROD-NEIN            PIC  X(03) VALUE 'NPN'.
181197     05  K-RC-KEY-NOT-UNIQUE           PIC  X(03) VALUE 'KNU'.
101297     05  K-RC-ABGERECHNET              PIC  X(03) VALUE 'ABG'.
181297     05  K-RC-KEINE-EVS-BEARB          PIC  X(03) VALUE 'KEB'.
280498     05  K-RC-NICHT-BUCHEN             PIC  X(03) VALUE 'NBU'.
190698     05  K-RC-EURO-WKZ-DATUM           PIC  X(03) VALUE 'DAT'.
190698     05  K-RC-EURO-WKZ-PRODUKT         PIC  X(03) VALUE 'PRO'.
190698     05  K-RC-EURO-PHASE-HEIMAT        PIC  X(03) VALUE 'PHH'.
190698     05  K-RC-EURO-PHASE-UMSTELLUNG    PIC  X(03) VALUE 'PHU'.
190698     05  K-RC-EURO-PHASE-EURO          PIC  X(03) VALUE 'PHE'.
070798     05  K-RC-NEUT-ZULAESSIG           PIC  X(03) VALUE 'NZU'.
070798     05  K-RC-NEUT-UNZULAESS           PIC  X(03) VALUE 'NUZ'.
270798     05  K-RC-NEUT-OK-NOT-FND          PIC  X(03) VALUE 'NNF'.
270798     05  K-RC-NEUT-OK                  PIC  X(03) VALUE 'NEO'.
270798     05  K-RC-NEUT-OK-ALLE             PIC  X(03) VALUE 'NEA'.
300798     05  K-RC-UMB-OK                   PIC  X(03) VALUE 'UOK'.
300798     05  K-RC-UMB-OK-O-NEUT            PIC  X(03) VALUE 'UON'.
300798     05  K-RC-UMB-OK-NEUT-GBA          PIC  X(03) VALUE 'UNG'.
300798     05  K-RC-UMB-OK-NUR-NEU           PIC  X(03) VALUE 'UNN'.
070798     05  K-RC-UMB-ZULAESSIG            PIC  X(03) VALUE 'UZU'.
070798     05  K-RC-UMB-UNZULAESSIG          PIC  X(03) VALUE 'UUZ'.
300798     05  K-RC-UMB-NOK-O-AEND           PIC  X(03) VALUE 'UOA'.
300798     05  K-RC-UMB-NOK                  PIC  X(03) VALUE 'UNO'.
200798     05  K-RC-KEIN-VERTRAG             PIC  X(03) VALUE 'KVT'.
200798     05  K-RC-GBU-ERSTELLT             PIC  X(03) VALUE 'GER'.
200798     05  K-RC-GBU-NOT-ERSTbar          PIC  X(03) VALUE 'GNE'.
200798     05  K-RC-GBU-NOT-RELEV            PIC  X(03) VALUE 'GNR'.
091098     05  K-RC-PE-S-NICHT-EIND          PIC  X(03) VALUE 'PND'.
091098     05  K-RC-PE-S-NICHT-ERST          PIC  X(03) VALUE 'PNE'.
161098     05  K-RC-UNZULAESSIG              PIC  X(03) VALUE 'UNZ'.
271098     05  K-RC-KEIN-AVERM-VORHANDEN     PIC  X(03) VALUE 'KAV'.
271098     05  K-RC-KEIN-VM-VORHANDEN        PIC  X(03) VALUE 'KVM'.
271098     05  K-RC-VM-UNZULAESSIG           PIC  X(03) VALUE 'VMU'.
271098     05  K-RC-VM-BEREITS-IN-PE-S       PIC  X(03) VALUE 'VBE'.
100299     05  K-RC-OK-PEWA-O-PROD           PIC  X(03) VALUE 'OOP'.
100299     05  K-RC-VST-IN-VGR-NFD           PIC  X(03) VALUE 'VNF'.
040399     05  K-RC-OK-KEINE-AEND            PIC  X(03) VALUE 'OKK'.
040399     05  K-RC-OK-PE-S-BEREINIGT        PIC  X(03) VALUE 'OKB'.
040399     05  K-RC-PEST-IDENTISCH           PIC  X(03) VALUE 'PID'.
040399     05  K-RC-PEST-GLEICH              PIC  X(03) VALUE 'PGL'.
040399     05  K-RC-PEST-UNGLEICH            PIC  X(03) VALUE 'PUG'.
090399     05  K-RC-NPM-AUF-PMBE             PIC  X(03) VALUE 'NAB'.
050399     05  K-RC-PE-S-VORHANDEN           PIC  X(03) VALUE 'PEH'.
310899     05  K-RC-PROD-NOT-FOUND           PIC  X(03) VALUE 'PNF'.
061099     05  K-RC-DTA-B-N-BEENDET          PIC  X(03) VALUE 'DNB'.
081299     05  K-RC-ZULAESSIG                PIC  X(03) VALUE 'ZUL'.
080301*        MEHRERE ERGEBNISZEILEN
080301     05  K-RC-MULT-ROWS                PIC  X(03) VALUE 'MUR'.
221106*        PE-STRUKTUR KONNTE ERMITTELT WERDEN
221106     05  K-RC-PE-S-ERMITTELT           PIC  X(03) VALUE 'PEO'.
221106*        PE-STRUKTUR KONNTE NICHT ERMITTELT WERDEN
221106     05  K-RC-PE-S-NICHT-ERMITTELT     PIC  X(03) VALUE 'PEN'.
221106*        ELEKTRON. VORSCHLAGSDATEN KONNTEN ERMITTELT WERDEN
221106     05  K-RC-VOR-ERMITTELT            PIC  X(03) VALUE 'VDO'.
221106*        ELEKTRON. VORSCHLGSDATEN KONNTEN NICHT ERMITTELT WERDEN
221106     05  K-RC-VOR-NICHT-ERMITTELT      PIC  X(03) VALUE 'VDN'.
110815*        SELECT NICHT OK
110815     05  K-RC-NOT-OK-SEL               PIC  X(03) VALUE 'NOS'.
110815*        INSERT NICHT OK
110815     05  K-RC-NOT-OK-INS               PIC  X(03) VALUE 'NOI'.
      *  Konstantennamen max. bis ^  (wg. Verwendung für PO-...-RC's)
      ******** ENDE COPY-MEMBER STANDARD-KONSTANTEN (DRAT0107) ********
 

