      * T13EX33   Stand: 18/08/98    15:38:03
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DSKI0333.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DSKI0333.CPv  $
      *
      *   Rev 2.7   Nov 03 2010 10:50:24   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Konstan
      *ten-Copy
      *
      *   Rev 2.6   Oct 01 2010 11:13:10   t13ex27
      *AKI00966: Datenbestandsverarbeitung  DVAG Österreich-Konstan
      *ten-Copy
      *
      *   Rev 2.5   19 Oct 2006 09:06:28   t13ae31
      *AKI00615: Einführung Pflegerentenversicherung
      *
      *   Rev 2.4   Jul 16 2004 11:02:40   t13ex27
      *AKI00418: Integration Kinderprodukt in der Vermögensplanung
      *
      *   Rev 2.3   Mar 09 2000 10:01:50   T13AE31
      *AKI85
      *
      *   Rev 2.2   Sep 30 1999 09:02:22   T13EX41
      *AKI00049
      *-VERARBEITUNG WÄHRUNGSKENN-
      * ZEICHEN ZWECKS EURO-FÄHIG-
      * KEIT
      *
      *
      *   Rev 2.1   Mar 11 1999 07:59:50   T13EX33
      *Übernahme Host
      *
      *   Rev 1.0   30 Jun 1998 10:25:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *    COPY-MEMBER: DSKI0333                                     *
      *                                                              *
      *    KONSTANTEN FÜR DIE AUSPRÄGUNGEN DER SUPPORTTABELLE        *
      *                                                              *
      *    PRODUKT - TCKI0333                                        *
      *                                                              *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!22.07.98!01.01! NEUERSTELLUNG            ! HUEBNER  *
      * --------!--------!-----!--------------------------!----------*
      * FKI00057!15.06.99!     ! PRODUKTID 94, 95, 96     !C.HEINECK *
      *         !        !     ! EINGEFUEGT               !          *
      * AKI85   !04.02.00!     ! PRODUKTID 97 UND 98      !P.HUEBNER *
      *         !        !     ! EINGEFUEGT               !          *
      * AKI00418!07.06.04!2.4  ! Neu - Kinderinvaliditäts-!I. Frenzel*
      *         !        !     ! versicherung             !          *
      *         !        !     !                          !          *
      * AKI00615!30.09.06!     ! neue Produkte 100-109    ! Hübner   *
      * AKI00966!13.08.10!2.6  ! neue Produkte 110-139    !I. Frenzel*
      * AKI00966!03.11.10!2.7  ! neue Produkte 140-149    !I. Frenzel*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  FILLER.
      * PRODUKTID  PRODUKTSPARTEID  HAUPTPRODUKTID  PRODUKTTEXT
      *--------------------------------------------------------
      *
      * 2                1  --------------  INVESTMENTFONDS
           05 K-INVFOND-SPARTE-1            PIC 99 VALUE 02.
      *11                5  --------------  HAFTPFLICHT
           05 K-HAFTPFL-SPARTE-5            PIC 99 VALUE 11.
      *37                8  --------------  AKTIE
           05 K-AKTIE-SPARTE-8              PIC 99 VALUE 37.
      *40                8  --------------  INVESTMENTFONDS
           05 K-INVFOND-SPARTE-8            PIC 99 VALUE 40.
      *43                8  --------------  SONSTIGE
           05 K-SONSTIG-SPARTE-8            PIC 99 VALUE 43.
      *53               10  --------------  AKTIE
           05 K-AKTIE-SPARTE-10             PIC 99 VALUE 53.
      *56               10  --------------  INVESTMENTFONDS
           05 K-INVFOND-SPARTE-10           PIC 99 VALUE 56.
      *59               10  --------------  SONSTIGE
           05 K-SONSTIG-SPARTE-10           PIC 99 VALUE 59.
      *60                3  --------------  KRANKENVERSICHERUNG
           05 K-KRANKEN-SPARTE-3            PIC 99 VALUE 60.
      *61                9  --------------  VERBINDLICHKEITEN
           05 K-VERBIND-SPARTE-9            PIC 99 VALUE 61.
      * 1                1  --------------  BAUSPAREN
           05 K-BAUSPAR-SPARTE-1            PIC 99 VALUE 01.
      * 3                1  --------------  KAPITALBILDENDE LEBENSVERSIC
           05 K-KAPBILV-SPARTE-1            PIC 99 VALUE 03.
      * 4                2  --------------  BAUSPAREN
           05 K-BAUSPAR-SPARTE-2            PIC 99 VALUE 04.
      * 5                3              60  GKV-PFLICHT
           05 K-GKVPFL-SPARTE-3             PIC 99 VALUE 05.
      * 6                3              60  GKV-FREIWILLIG
           05 K-GKVFREI-SPARTE-3            PIC 99 VALUE 06.
      * 7                3              60  PKV-VOLL
           05 K-PKVVOLL-SPARTE-3            PIC 99 VALUE 07.
      * 8                3              60  PKV-ZUSATZ
           05 K-PKVZUS-SPARTE-3             PIC 99 VALUE 08.
      * 9                4  --------------  UNFALLVERSICHERUNG
           05 K-UNFALL-SPARTE-4             PIC 99 VALUE 09.
      *10                5  --------------  GLAS
           05 K-GLAS-SPARTE-5               PIC 99 VALUE 10.
      *12                5  --------------  HAUSRAT
           05 K-HAUSRAT-SPARTE-5            PIC 99 VALUE 12.
      *13                5  --------------  RECHTSSCHUTZ
           05 K-RECHT-SPARTE-5              PIC 99 VALUE 13.
      *14                5  --------------  SCHUTZBRIEF
           05 K-SCHBRIEF-SPARTE-5           PIC 99 VALUE 14.
      *15                5  --------------  WOHNGEBäUDE
           05 K-WOHNGEB-SPARTE-5            PIC 99 VALUE 15.
      *16                5  --------------  SONSTIGE
           05 K-SONSTIG-SPARTE-5            PIC 99 VALUE 16.
      *17                5              11  PRIVATHAFTPFLICHT
           05 K-PRIVHPF-SPARTE-5            PIC 99 VALUE 17.
      *18                5              11  PRIVATHAFTPFLICHT ALS EINZEL
           05 K-PHPFEIN-SPARTE-5            PIC 99 VALUE 18.
      *19                5              11  TIERHALTERHAFTPFLICHT - PRIV
           05 K-TIERHPF-SPARTE-5            PIC 99 VALUE 19.
      *20                5              11  WASSERSPORTHAFTPFLICHT - PRI
           05 K-WASPHPF-SPARTE-5            PIC 99 VALUE 20.
      *21                5              11  HAUS - UND GRUNDBESITZERHAFT
           05 K-HAGRHPF-SPARTE-5            PIC 99 VALUE 21.
      *22                5              11  GEWäSSERSCHADENHAFTPFLICHT -
           05 K-GEWAHPF-SPARTE-5            PIC 99 VALUE 22.
      *23                5              11  BAUHERRENHAFTPFLICHT
           05 K-BAUHHPF-SPARTE-5            PIC 99 VALUE 23.
      *24                5              11  BETRIEBSHAFTPFLICHT OHNE PH
           05 K-BHPFOPH-SPARTE-5            PIC 99 VALUE 24.
      *25                5              11  BERUFSHAFTPFLICHT OHNE PH
           05 K-BERUFHPFOPH-SPARTE-5        PIC 99 VALUE 25.
      *26                5              11  BETRIEBSHAFTPFLICHT EINSCHL.
           05 K-BHPFEPH-SPARTE-5            PIC 99 VALUE 26.
      *27                5              11  BERUFSHAFTPFLICHT EINSCHL. P
           05 K-BERUFHPFEPH-SPARTE-5        PIC 99 VALUE 27.
      *28                5              11  VERMöGENSB.HAFTPFLICHT EINSC
           05 K-VMBHPF-SPARTE-5             PIC 99 VALUE 28.
      *29                6  --------------  KFZ
           05 K-KFZ-SPARTE-6                PIC 99 VALUE 29.
      *30                7  --------------  KAPITALBILDENDE LEBENSVERSIC
           05 K-KABILV-SPARTE-7             PIC 99 VALUE 30.
      *31                7  --------------  KAPITALBILD. LV MIT TEILAUSZ
           05 K-KABILVMTZ-SPARTE-7          PIC 99 VALUE 31.
      *32                7  --------------  FONDSGEBUNDENE LEBENSVERSICH
           05 K-FONDGLV-SPARTE-7            PIC 99 VALUE 32.
      *33                7  --------------  RISIKO LEBENSVERSICHERUNG
           05 K-RISIKOLV-SPARTE-7           PIC 99 VALUE 33.
      *34                7  --------------  RENTENVERSICHERUNG
           05 K-RENTENV-SPARTE-7            PIC 99 VALUE 34.
      *35                7  --------------  FONDSGEBUNDENE RENTENVERSICH
           05 K-FONDGRV-SPARTE-7            PIC 99 VALUE 35.
      *36                7  --------------  SELBSTSTäND BERUFSUNFäHIGKEI
           05 K-SSBUZVS-SPARTE-7            PIC 99 VALUE 36.
      *38                8  --------------  FESTGELD
           05 K-FESTGELD-SPARTE-8           PIC 99 VALUE 38.
      *39                8  --------------  FESTVERZINSLICHE WERTPAPIERE
           05 K-FESTWERTP-SPARTE-8          PIC 99 VALUE 39.
      *41                8  --------------  SPARBRIEFE
           05 K-SPARBRIEF-SPARTE-8          PIC 99 VALUE 41.
      *42                8  --------------  SPAREINLAGEN
           05 K-SPAREIN-SPARTE-8            PIC 99 VALUE 42.
      *45                9              61  BAUSPARDARLEHEN
           05 K-BAUDARL-SPARTE-9            PIC 99 VALUE 45.
      *46                9              61  KONTOüBERZIEHUNG
           05 K-KTODISPO-SPARTE-9           PIC 99 VALUE 46.
      *47                9              61  LEASING
           05 K-LEASING-SPARTE-9            PIC 99 VALUE 47.
      *48                9              61  PRIVATDARLEHEN
           05 K-PRIVDARL-SPARTE-9           PIC 99 VALUE 48.
      *49                9              61  RATENKREDIT
           05 K-RATENKR-SPARTE-9            PIC 99 VALUE 49.
      *50                9              61  GEWERBEDARLEHEN
           05 K-GEWERBE-SPARTE-9            PIC 99 VALUE 50.
      *51                9              61  SONSTIGE VERPFLICHTUNGEN
           05 K-SONVERP-SPARTE-9            PIC 99 VALUE 51.
      *52                9              61  SONSTIGE DARLEHEN
           05 K-SONDARL-SPARTE-9            PIC 99 VALUE 52.
      *54               10  --------------  FESTGELD
           05 K-FESTGELD-SPARTE-10          PIC 99 VALUE 54.
      *55               10  --------------  FESTVERZINSLICHE WERTPAPIERE
           05 K-FESTWERTP-SPARTE-10         PIC 99 VALUE 55.
      *57               10  --------------  SPARBRIEFE
           05 K-SPARBRIEF-SPARTE-10         PIC 99 VALUE 57.
      *58               10  --------------  SPAREINLAGEN
           05 K-SPAREIN-SPARTE-10           PIC 99 VALUE 58.
      *62                7  --------------  AUSBILDUNGS-VERSICHERUNG
           05 K-AUSBILD-SPARTE-7            PIC 99 VALUE 62.
      *63                1  --------------  PRäMIENSPAREN
           05 K-PRSPAR-SPARTE-1             PIC 99 VALUE 63.
      *64                1               2  AKTIENFONDS
           05 K-AKTFOND-SPARTE-1            PIC 99 VALUE 64.
      *65                1               2  RENTENFONDS
           05 K-RENTFOND-SPARTE-1           PIC 99 VALUE 65.
      *66                1               2  GESCHLOSSENER IMMOBILIENFOND
           05 K-GIMMFOND-SPARTE-1           PIC 99 VALUE 66.
      *67                1               2  OFFENER IMMOBILIENFONDS
           05 K-OIMMFOND-SPARTE-1           PIC 99 VALUE 67.
      *68                1               2  SONSTIGER FONDS
           05 K-SONFOND-SPARTE-1            PIC 99 VALUE 68.
      *69                8              40  AKTIENFONDS
           05 K-AKTFOND-SPARTE-8            PIC 99 VALUE 69.
      *70                8              40  RENTENFONDS
           05 K-RENTFOND-SPARTE-8           PIC 99 VALUE 70.
      *71                8              40  GESCHLOSSENER IMMOBILIENFOND
           05 K-GIMMFOND-SPARTE-8           PIC 99 VALUE 71.
      *72                8              40  OFFENER IMMOBILIENFONDS
           05 K-OIMMFOND-SPARTE-8           PIC 99 VALUE 72.
      *73                8              40  SONSTIGER FONDS
           05 K-SONFOND-SPARTE-8            PIC 99 VALUE 73.
      *74               10              56  AKTIENFONDS
           05 K-AKTFOND-SPARTE-10           PIC 99 VALUE 74.
      *75               10              56  RENTENFONDS
           05 K-RENTFOND-SPARTE-10          PIC 99 VALUE 75.
      *76               10              56  GESCHLOSSENER IMMOBILIENFOND
           05 K-GIMMFOND-SPARTE-10          PIC 99 VALUE 76.
      *77               10              56  OFFENER IMMOBILIENFONDS
           05 K-OIMMFOND-SPARTE-10          PIC 99 VALUE 77.
      *78               10              56  SONSTIGER FONDS
           05 K-SONFOND-SPARTE-10           PIC 99 VALUE 78.
      *79                8              43  UNTERNEHMENSBETEILIGUNGEN
           05 K-UNTBET-SPARTE-8             PIC 99 VALUE 79.
      *80                8              43  TERMINGESCHäFTE
           05 K-TERMING-SPARTE-8            PIC 99 VALUE 80.
      *81                8              43  SONSTIGE SPEKULATIONSGESCHäF
           05 K-SONSPEK-SPARTE-8            PIC 99 VALUE 81.
      *82                8              43  SONSTIGE
           05 K-SONSTIG-SPARTE-8            PIC 99 VALUE 82.
      *83               10              59  UNTERNEHMENSBETEILIGUNGEN
           05 K-UNTBET-SPARTE-10            PIC 99 VALUE 83.
      *84               10              59  TERMINGESCHäFTE
           05 K-TERMING-SPARTE-10           PIC 99 VALUE 84.
      *85               10              59  SONSTIGE SPEKULATIONSGESCHäF
           05 K-SONSPEK-SPARTE-10           PIC 99 VALUE 85.
      *86               10              59  SONSTIGE
           05 K-SONSTIG-SPARTE-10           PIC 99 VALUE 86.
      *87                8              37  AKTIE
           05 K-AKTIE-SPARTE-8              PIC 99 VALUE 87.
      *88                8              37  BELEGSCHAFTSAKTIE
           05 K-BELAKTIE-SPARTE-8           PIC 99 VALUE 88.
      *89               10              53  AKTIE
           05 K-AKTIE-SPARTE-10             PIC 99 VALUE 89.
      *90               10              53  BELEGSCHAFTSAKTIE
           05 K-BELAKTIE-SPARTE-10          PIC 99 VALUE 90.
      *91                7  --------------  FONDSGEB. LV MIT TEILAUSZAHL
           05 K-FONDLVMT-SPARTE-7           PIC 99 VALUE 91.
      *92                5              11  VERMöGENSSCHADENHAFTPFLICHT
           05 K-VMSHPF-SPARTE-5             PIC 99 VALUE 92.
      *93                5              11  PRODUKTHAFTPFLICHT
           05 K-PRODHPF-SPARTE-5            PIC 99 VALUE 93.
      *94                5              11  WASSERSPORTHAFTPFLICHT - GEW
           05 K-WASPHPFGEW-SPARTE-5         PIC 99 VALUE 94.
      *95                5              11  TIERHALTERHAFTPFLICHT - GEWE
           05 K-TIERHPFGEW-SPARTE-5         PIC 99 VALUE 95.
      *96                5              11  GEWäSSERSCHADENHAFTPFLICHT -
           05 K-GEWAHPFPRV-SPARTE-5         PIC 99 VALUE 96.
      *
      *97                7                  fondsgeb. Rentenvers. mit te
           05 K-FOND-RV-SPARTE-7            PIC 99 VALUE 97.
      *
      *98                7                  rentenvers. mit teilauszahlu
           05 K-RV-SPARTE-7                 PIC 99 VALUE 98.
      *99                7                  Kinderinvaliditätsvers.
           05 K-KIV-SPARTE-7                PIC 99 VALUE 99.
      *
300906*100
300906     05 K-KONTOKORR-SPARTE-11         PIC 999 VALUE 100.
      *
300906*101
300906     05 K-SPAREINLAGE-SPARTE-11         PIC 999 VALUE 101.
      *
300906*102
300906     05 K-KREDITKARTE-SPARTE-11         PIC 999 VALUE 102.
      *
300906*103
300906     05 K-RATENKREDIT-SPARTE-11         PIC 999 VALUE 103.
      *
300906*104
300906     05 K-DARLEHEN-SPARTE-11         PIC 999 VALUE 104.
      *
300906*105
300906     05 K-PRIV-PFLRENTE-SPARTE-7         PIC 999 VALUE 105.

300906*106
300906     05 K-AN-IMMO-DRL-O-AFIN-SPARTE-2  PIC 999 VALUE 106.

300906*107
300906     05 K-AN-IMMO-DRL-M-AFIN-SPARTE-2  PIC 999 VALUE 107.

300906*108
300906     05 K-EN-IMMO-DRL-O-AFIN-SPARTE-2 PIC 999 VALUE 108.

300906*109
300906     05 K-EN-IMMO-DRL-O-AFIN-SPARTE-2 PIC 999 VALUE 109.

130810*110
130810     05 K-BERUFSHAFTPFLICHT-SPARTE-5      PIC 999 VALUE 110.

130810*111
130810     05 K-IND-HAFTPFLICHT-SPARTE-5        PIC 999 VALUE 111.

130810*112
130810     05 K-PRIVATE-KV-SPARTE-3             PIC 999 VALUE 112.

130810*113
130810     05 K-GESETZLICHE-KV-SPARTE-3         PIC 999 VALUE 113.

130810*114
130810     05 K-ZWISCHENKREDIT-SPARTE-9         PIC 999 VALUE 114.

130810*115
130810     05 K-BANKVORAUSDARLEHEN-SPARTE-9     PIC 999 VALUE 115.

130810*116
130810     05 K-REISEGEPAECK-SPARTE-5           PIC 999 VALUE 116.

130810*117
130810     05 K-DWS-ALTERSVORSORGE-SPARTE-8     PIC 999 VALUE 117.

130810*118
130810     05 K-AGI-ALTERSVORSORGE-SPARTE-8     PIC 999 VALUE 118.

130810*119
130810     05 K-PARKDEPOT-SPARTE-7              PIC 999 VALUE 119.

130810*120
130810     05 K-FONDSGEBUNDENE-LV-SPARTE-7      PIC 999 VALUE 120.

130810*121
130810     05 K-KLASSISCHE-LV-SPARTE-7          PIC 999 VALUE 121.

130810*122
130810     05 K-START-ZIEL-VORSORGE-SPARTE-7    PIC 999 VALUE 122.

130810*123
130810     05 K-START-ZIEL-POLIZZE-SPARTE-7     PIC 999 VALUE 123.

130810*124
130810     05 K-RISIKO-LV-SPARTE-7              PIC 999 VALUE 124.

130810*125
130810     05 K-WUNSCHPOLIZZE-SPARTE-7          PIC 999 VALUE 125.

130810*126
130810     05 K-SONSTIGE-LV-SPARTE-7            PIC 999 VALUE 126.

130810*127
130810     05 K-INVESTMENTPLAN-SPARTE-8         PIC 999 VALUE 127.

130810*128
130810     05 K-VERMOEGENSSPARPLAN-SPARTE-8     PIC 999 VALUE 128.

130810*129
130810     05 K-WERTPAPIERDEPOT-SPARTE-8        PIC 999 VALUE 129.

130810*130
130810     05 K-WOHNKREDIT-SPARTE-11            PIC 999 VALUE 130.

130810*131
130810     05 K-PRIVATKREDIT-SPARTE-11          PIC 999 VALUE 131.

130810*132
130810     05 K-SPAREINLAGE2-SPARTE-11          PIC 999 VALUE 132.

130810*133
130810     05 K-GIROKONTO-SPARTE-11             PIC 999 VALUE 133.

130810*134
130810     05 K-BAUSPARDARLEHEN-SPARTE-11       PIC 999 VALUE 134.

130810*135
130810     05 K-BAUSPARVERTRAG-SPARTE-2         PIC 999 VALUE 135.

130810*136
130810     05 K-DVAG-WIEN-KFZ-SPARTE-6          PIC 999 VALUE 136.

130810*137
130810     05 K-DVAG-WIEN-SACH-SPARTE-5         PIC 999 VALUE 137.

130810*138
130810     05 K-DVAG-WIEN-UNFALL-SPARTE-4       PIC 999 VALUE 138.

130810*139
130810     05 K-DVAG-WIEN-KRANKEN-SPARTE-3      PIC 999 VALUE 139.

031110*140
031110     05 K-DVAG-WIEN-SACH-EIGENHEIM        PIC 999 VALUE 140.

031110*141
031110     05 K-DVAG-WIEN-SACH-FEUER            PIC 999 VALUE 141.

031110*142
031110     05 K-DVAG-WIEN-SACH-TIPTAT           PIC 999 VALUE 142.

031110*143
031110     05 K-DVAG-WIEN-SACH-HAUSHALT         PIC 999 VALUE 143.

031110*144
031110     05 K-DVAG-WIEN-SACH-BAUWESEN         PIC 999 VALUE 144.

031110*145
031110     05 K-DVAG-WIEN-SACH-RECHTSSCHUTZ     PIC 999 VALUE 145.

031110*146
031110     05 K-DVAG-WIEN-SACH-ELEKTRONIK       PIC 999 VALUE 146.

031110*147
031110     05 K-DVAG-WIEN-SACH-MASCHBRUCH       PIC 999 VALUE 147.

031110*148
031110     05 K-DVAG-WIEN-SACH-HAFT             PIC 999 VALUE 148.

031110*149
031110     05 K-DVAG-WIEN-SACH-BETRIEBSUNT      PIC 999 VALUE 149.

      ********  ENDE COPY-MEMBER DSKI0333  ***********
