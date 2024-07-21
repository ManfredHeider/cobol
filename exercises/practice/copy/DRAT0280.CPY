      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0280                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DEN VARIABLEN KOMMUNIKATIONSBEREICH      *
      *    ZWISCHEN PO23 UND PO26                                     *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    08.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *
      * ---------------------------------------------     -----------*
      * APA00012!08.09.98! NEU                            ! STORCH   *
      *         !29.10.98! ERWEITERUNGEN                  ! STORCH   *
      *         !05.02.99! ERWEITERUNGEN                  !BOHLINGER *
      *         !25.03.99! ERWEITERUNGEN                  !BOHLINGER *
      *         !28.04.99! NEU: AUFRUFMODUS ANZEIGEN      !BOHLINGER *
      * APA00258!24.11.30! ERWEITERUNGEN                  ! WANG     *
      * FPA00686!29.03.04! KORREKTUR                      ! WANG     *
      * APA00295!26.11.04! PO-DRAT0280-CADD-S-UMB-KUE     ! WANG     *
      *         !        ! HINZU                          !          *
      * APA00639!25.06.07! ERW. DER PIN AUF 9 STELLEN     ! WANG     *
      * APA00622!13.06.07! PI-KNR-VGEB-GES HINZU          ! STORCH   *
      * APA01101!05.03.12! VGES DURCH MANDANT ERSETZT     ! STORCH   *
      *         !        ! *-ZEILEN ENTFERNT              !          *
      * APA01526!08.05.13! ERWEITERUNG UM UNTERNR.        ! STORCH   *
      *         !        ! UND SCHICHTNUMMER              !          *
      * APA02699!03.09.18! ERWEITERUNG PROD               ! WANG     *
      * DSY04437!06.07.22! ERWEITERUNG PO-BEREICH         ! STORCH   *
      *         !        !                                !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DEN VARIABLEN KOMMUNIKATIONSBEREICH
      *    ZWISCHEN DEN DIALOGEN P023 UND PO26
      *    - INPUT (VON AUFRUFENDEM DIALOG)
      *      ACHTUNG: AUF CADD-I-KOMM-VAR DARF NUR BEI
      *      !!!      DIALOG-INIT ZUGEGRIFFEN WERDEN; FALLS
      *      !!!      DIE INFOS SPAETER NOCH WICHTIG SIND,
      *      !!!      MUESSEN SIE IN CADI-SICHERUNG GESICHERT
      *      !!!      WERDEN.
      *      !!!      GRUND: DAS FELD WIRD EVTL. IN AUFGERUFENEN
      *      !!!      UNTERANWENDUNGEN UEBERSCHRIEBEN
      *
      ****************************************************************

       01  PI-DRAT0280.
           05  PI-DRAT0280-CADD-I-KOMM-VAR            PIC  X(128).
           05  FILLER REDEFINES   PI-DRAT0280-CADD-I-KOMM-VAR.

               10  PI-DRAT0280-COPY-ID                PIC  X(008).
                   88 PI-DRAT0280-COPY-ID-DAISY       VALUE 'DRAT0280'.
      *

      ***      AUFRUFMODI
               10  PI-DRAT0280-CADD-MODUS             PIC  X(002).
      ***             PE-STRUKTUR FüR EINZELNE BUCHUNG (AP/FP)
                   88 PI-DRAT0280-CADD-MODUS-PES      VALUE 'PB'.
      ***             URSPRUNGS PE-S FüR VERTRAG/ANTRAG
                   88 PI-DRAT0280-CADD-MODUS-UPES-V   VALUE 'PV'.
      ***             URSPRUNGS PE-S FüR PRODUKTTEIL
                   88 PI-DRAT0280-CADD-MODUS-UPES-PT  VALUE 'PP'.

      ***             ANZEIGEMODUS
                   88 PI-DRAT0280-CADD-MODUS-ANZ      VALUE 'AN'.

      ***      STEUERINFOS
               10  PI-DRAT0280-CADD-STEUER-INFO       PIC  X(004).

      ***          NUR FüR ANZEIGEMODUS
      ***          PE-STRUKTUR ANZEIGEN / STRUKTUR AUS DIALOGMODUL
                   88 PI-DRAT0280-CADD-PEST-ANZ-DM  VALUE 'ANZM'.
      ***          PE-STRUKTUR ANZEIGEN / STRUKTUR AUS DATENBANK
                   88 PI-DRAT0280-CADD-PEST-ANZ-DB  VALUE 'ANZD'.

      ***          FüR AENDERUNGSMODUS
      ***          PE-STRUKTUR ÄNDERN BZW. ERSTELLEN
                   88 PI-DRAT0280-CADD-PEST-AND     VALUE 'AEND'.
      ***          PE-STRUKTUR ERSTELLEN WIRD SPÄTER REALISIERT
      *            88 PI-DRAT0280-CADD-PEST-NEU       VALUE 'NEU'.


      ***      DRUCKAUFB. SCHLÜSSELFELDER FÜR DIE ANZEIGE IN PO26
               10  PI-DRAT0280-CADD-GES             PIC  9(002).
               10  PI-DRAT0280-CADD-GES-BM          PIC  X(010).
               10  PI-DRAT0280-CADD-VNR             PIC  X(015).
               10  PI-DRAT0280-CADD-PIN             PIC  X(009).
      *
      ***      HIER WERDEN DIE ID'S ÜBERGEBEN
               10  PI-DRAT0280-CADD-PEST-ID         PIC  S9(015) COMP-3.
               10  PI-DRAT0280-CADD-PDPT-ID         PIC  S9(015) COMP-3.
               10  PI-DRAT0280-CADD-S-PDPT-ID       PIC  X(001).
               10  PI-DRAT0280-CADD-PRINTE          PIC  S9(015) COMP-3.

      ***      SCHALTER SETZEN, WENN PO26 VON PO11 (VM-ÄNDERUNG)
      ***                       AUFGERUFEN WIRD.
               10  PI-DRAT0280-CADD-S-VM-KORREKT    PIC  X(001).

      ***      SCHALTER SETZEN, WENN PO26 VON PO15 (MODUS 'PP')
      ***      AUFGERUFEN WIRD.
               10  PI-DRAT0280-CADD-S-ABGEL-EVS     PIC  X(001).

      ***      HIER WIRD DAS PEST-ENTSTEH-KENNZEICHEN üBERGEBEN
               10  PI-DRAT0280-CADD-PEST-ENTSTEH    PIC  S9(004) BINARY.

      ***      HIER WIRD DER RC-CODE VON KI ÜBERGEBEN
               10  PI-DRAT0280-CADD-RC              PIC  X(003).

      ***      HIER WIRD DER MANDANT DER MELDUNG ÜBERGEBEN
               10  PI-DRAT0280-KNR-MANDANT          PIC  S9(004) BINARY.

      ***      ÜBERGABE DER UNTERNUMMER, WENN GEFÜLLT
               10  PI-DRAT0280-CADD-UNTER-NR        PIC  X(003).
               10  PI-DRAT0280-CADD-S-UNTER-NR      PIC  X(001).

      ***      ÜBERGABE DER SCHICHTNUMMER, WENN GEFÜLLT
               10  PI-DRAT0280-CADD-SCHICHT-NR      PIC  X(003).
               10  PI-DRAT0280-CADD-S-SCHICHT-NR    PIC  X(001).

030918***      HIER WIRD DAS PRODUKT ÜBERGEBEN
               10  PI-DRAT0280-CADD-PROD            PIC  S9(004) BINARY.

       01  PO-DRAT0280.
           05  PO-DRAT0280-CADD-O-KOMM-VAR          PIC  X(128).
           05  FILLER REDEFINES   PO-DRAT0280-CADD-O-KOMM-VAR.

      ***      HIER WIRD DER RETURNCODE ÜBERGEBEN
               10  PO-DRAT0280-CADD-ZST-PE-S        PIC  X(001).
                   88 PO-DRAT0280-CADD-UNVERAENDERT     VALUE '1'.
                   88 PO-DRAT0280-CADD-VERAEND-OK       VALUE '2'.
                   88 PO-DRAT0280-CADD-VERAEND-NOK      VALUE '3'.
                   88 PO-DRAT0280-CADD-ABBR-PF3         VALUE '4'.

               10  PO-DRAT0280-CADD-PEST-ID         PIC  S9(015)
                                                             COMP-3.
               10 PO-DRAT0280-CADD-AVERM            PIC 9(007).
               10 PO-DRAT0280-CADD-PRINTE           PIC S9(15)
                                                     PACKED-DECIMAL.
               10 PO-DRAT0280-CADD-S-UMB            PIC X.
                  88 PO-DRAT0280-CADD-S-UMB-KI          VALUE '1'.
                  88 PO-DRAT0280-CADD-S-UMB-KUE         VALUE '2'.
                  88 PO-DRAT0280-CADD-S-UMB-DAISY       VALUE SPACE.
                  
060722         10 PO-DRAT0280-CADD-S-RFT            PIC X.
      *        
      ******* ENDE COPY-MEMBER DRAT0280  ****************************

