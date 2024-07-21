      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *COPY-MEMBER: DRPA1731  PEG-DATENSATZ GES. 20 - AM LEBEN        *
      *                                                               *
      *---------------------------------------------------------------*
      *    ERSTELLT: STORCH-SCHÄFER                                   *
      *    DATUM:    12.02.13                                         *
      *****************************************************************
      *    
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!12.02.13!2.1 ! NEU                       ! STORCH   *
      * APA01844!13.08.14!2.2 ! FÜR VEP-STUFE2: HIST-UNR  ! STRAUSS  *
      * DSY03965!14.09.21!    ! NEU NBBS UND AUW          ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AM LEBEN DTAS.                                            *
      ****************************************************************
      *
             30 AM-LEBEN-DATENSATZ.
                35 VNR                      PIC  9(15).
                35 UNTERNUMMER              PIC  9(03).
                35 SCHICHTNUMMER            PIC  9(03).
                35 UMW-NR                   PIC  9(13).
                35 VERTRAG-BEGINN           PIC  X(10).
                35 VERTRAGSABLAUF           PIC  X(10).
                35 UR-POLICIER-DATUM        PIC  X(10).
                35 POLICIER-DATUM           PIC  X(10).
                35 FAELLIG-VON              PIC  X(10).
                35 INKASSO-STAND            PIC  X(10).
                35 BEWEGUNGSDATUM           PIC  X(10).
                35 HAFTZEIT-VON             PIC  X(10).
                35 HAFTZEIT-BIS             PIC  X(10).
                35 AVERM                    PIC  9(07).
                35 PGEB-GES                 PIC  9(02).
                35 VN-NNAME                 PIC  X(25).
                35 VN-VNAME                 PIC  X(20).
                35 VP-NNAME                 PIC  X(25).
                35 VP-VNAME                 PIC  X(20).
                35 BZ-NNAME                 PIC  X(25).
                35 BZ-VNAME                 PIC  X(20).
                35 ABR-DAT-PG               PIC  X(10).
                35 ZAHLWEISE                PIC  X(02).
                35 VERTRAG-WAEHRUNG         PIC  X(03).
                35 PG-PRODART               PIC  9(01).
                35 DYNAMIK-KZ2              PIC  9(01).
                35 DYNAMIK-KZ1              PIC  9(01).
                35 PG-TARIF                 PIC  X(15).
                35 BEW-HERKUNFT             PIC  X(01).
                35 BEW-SCHLUESSEL           PIC  9(06).
                35 PROV-SATZ                PIC  9(03)V9(03).
                35 PROV-BETRAG              PIC S9(07)V9(02).
                35 REDUZ-BEW-SUMME          PIC S9(07)V9(02).
                35 BEW-SUMME                PIC S9(07)V9(02).
                35 PPFL-SUMME               PIC S9(07)V9(02).
                35 PRO-RATA-ZAEHLER         PIC S9(03).
                35 PRO-RATA-NENNER          PIC S9(03).
                35 BZ-DAUER                 PIC  9(04).
                35 JAHRESBEITRAG            PIC S9(07)V9(02).
                35 RATENBEITRAG             PIC S9(07)V9(02).
                35 VERFUEGART               PIC  9(03).
                35 SAMMELNUMMER             PIC S9(09).
                35 RABATTSTUFE              PIC  X(03).
                35 VVG-KZ                   PIC  X(01).
                35 NEUSTORNO-KZ             PIC  X(01).
                35 TARIFSCHL                PIC S9(05).
                35 TARIFSCHL-GRP            PIC S9(03).
                35 TARIF-GRP                PIC  9(03).
                35 TARIF-KZ                 PIC  9(02).
                35 INKASSOART               PIC  9(02).
                35 GESAMTDAUER              PIC S9(04).
                35 DEPOT-KZ                 PIC  9(01).
                35 PROV-SCHL                PIC  X(05).
                35 FEIV-MIG-KZ              PIC  X(01).
                35 VERM-GES                 PIC  9(02).
                35 BES-VERSORG-VERSICH      PIC  9(01).
                35 BUZ-TARIF                PIC  9(04).
                35 STRASSE-VN               PIC  X(30).
                35 ORT-VN                   PIC  X(30).
                35 PLZ-VN                   PIC  X(05).
                35 RED-BWERTBTG             PIC  S9(07)V9(02).
                35 PPFL-BEITRAG             PIC  S9(07)V9(02).
                35 PROV-VERDIENT            PIC  9(04).
                35 VS-SUMME                 PIC  S9(09).
                35 AE-KZ                    PIC  X(01).
      ***          BEARBEITUNGSTERMIN.
                35 BEARB-DATUM              PIC  9(08).
      ***          KOMPLEMENTÄR-BEARBEITUNGSDATUM 99999999 - TAGESDATUM
                35 BEARB-ZEIT-NR            PIC  9(07).
      ***          KOMPLEMENTÄR-ZEITSTEMPEL ALS REIHENFOLGENUMMER
130814***          NEUES FELD HISTOR.UNTER-NR FÜR VEP STUFE2
130814          35 HIST-UNR                 PIC  9(03).
140921***          NETTO-BEITRAGS-BEWERTUNGSSUMME
140921          35 NBB-SUMME                PIC S9(09)V9(02).
140921***          PROVISIONSPFLICHTIGE BEITRAGSSUMME
140921          35 AUW-SUMME                PIC S9(09)V9(02).
      *
      ******* ENDE COPY-MEMBER **************************************
