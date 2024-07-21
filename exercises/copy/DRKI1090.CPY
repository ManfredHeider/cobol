
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki1090.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRKI1090.CPv  $
      *
      *   Rev 2.2   Apr 15 2003 11:11:00   t13ex39
      *AKI00294 ISIN neues Satzformat
      *
      *   Rev 2.1   06 Jul 2000 13:27:24   T13EX39
      *AKI00100 SEB-INVEST
      *
      *   Rev 2.1   Dec 30 1999 17:01:16   T13EX47
      *
      *
      *   Rev 2.0   Feb 27 1999 12:58:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:27:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 0 9 0    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER KSDS VON BFG INVEST IN NEUER STRUKTUR        *
      *  (SEB-BEZEICHNUNG : ADAS001D)                                  *
      *  LAENGE   :  864 BYTES                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *24.03.00!NEUERSTELLUNG                          !WHO      ! 01  *
      *AKI00100!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
301002*30.11.02!Anpassung ISIN                         !WHO      !     *
301002*AKI00294! BFG  in SEB  geändert                 !         !     *
      *--------!---------------------------------------!---------!-----*
      *--------!---------------------------------------!---------!-----*

       01  DRKI1090.
      *    ** PRIMÄRKEY
           05 KOMPL-SEB-KEY                      PIC 9(10).
           05 FILLER REDEFINES KOMPL-SEB-KEY.
              15 KOMPL-SEB-KEY-1-8               PIC 9(08).
              15 KOMPL-SEB-KEY-9-10              PIC 9(02).
      *    ** VERTRAGSZUSTAND
           05 KOMPL-SEB-VERTZUSTAND              PIC 9(02).
         02 KOMPL-SEB-INV-STRUK.
           05 KOMPL-SEB-DEPOTNR                  PIC 9(08).
           05 KOMPL-SEB-FILLER REDEFINES KOMPL-SEB-DEPOTNR.
              10 A-SATZ-ART                      PIC X(01).
              10 FILLER                          PIC X(07).
           05 KOMPL-SEB-F01                      PIC X.
      *    ** UNTERDEPOTNUMMER
           05 KOMPL-SEB-UNTERDEPNR               PIC 9(02).
           05 KOMPL-SEB-F02                      PIC X.
      *    ** DEPOTINHABER ANREDEKENNZEICHEN
           05 KOMPL-SEB-ANREDE                   PIC 9(03).
           05 KOMPL-SEB-F03                      PIC X.
      *    ** DEPOTINHABER TITELKENNZEICHEN
           05 KOMPL-SEB-TITEL                    PIC 9(03).
           05 KOMPL-SEB-F04                      PIC X.
      *    ** DEPOTINHABER NAME
           05 KOMPL-SEB-NAME                     PIC X(40).
           05 KOMPL-SEB-F04A                     PIC X.
      *    ** DEPOTINHABER VORNAME
           05 KOMPL-SEB-VORNAME                  PIC X(40).
           05 KOMPL-SEB-F05                      PIC X.
      *    ** DEPOTINHABER NAMENSERGAENZUNG 1
           05 KOMPL-SEB-ERGAENZ1                 PIC X(40).
           05 KOMPL-SEB-F06                      PIC X.
      *    ** DEPOTINHABER NAMENSERGAENZUNG 2
           05 KOMPL-SEB-ERGAENZ2                 PIC X(40).
           05 KOMPL-SEB-F07                      PIC X.
      *    ** DEPOTINHABER STRASSE
           05 KOMPL-SEB-STRASSE                  PIC X(42).
           05 KOMPL-SEB-F08                      PIC X.
      *    ** DEPOTINHABER PLZ DER STRASSENANSCHRIFT
           05 KOMPL-SEB-PLZ                      PIC X(08).
           05 KOMPL-SEB-F09                      PIC X.
      *    ** DEPOTINHABER ORT
           05 KOMPL-SEB-ORT                      PIC X(40).
           05 KOMPL-SEB-F10                      PIC X.
      *    ** DEPOTINHABER ISO-CODE LAND
           05 KOMPL-SEB-LAND                     PIC X(03).
           05 KOMPL-SEB-F11                      PIC X.
      *    ** VERSICHERUNGSNUMMER
           05 KOMPL-SEB-VERSNR                   PIC X(15).
           05 KOMPL-SEB-F12                      PIC X.
      *    ** MITARBEITERNUMMER
           05 KOMPL-SEB-VERS-VERMITTLER          PIC X(30).
           05 KOMPL-SEB-F13                      PIC X.
      *    ** DEPOTWERT ERRECHNET AUS STUECKEBESTAND X ANTEILWERT
           05 KOMPL-SEB-BESTAND                  PIC 9(16),9(02).
           05 KOMPL-SEB-BESTAND-R REDEFINES KOMPL-SEB-BESTAND.
             10 BESTAND-VK             PIC X(16).
             10 BESTAND-KOMMA          PIC X(01).
             10 BESTAND-NK             PIC X(02).
           05 KOMPL-SEB-F14                      PIC X.
      *    ** DATUM DES FONDSKURSES (TTMMJHJJ)
           05 KOMPL-SEB-KURSDATUM      PIC 9(08) .
           05 KOMPL-SEB-F15                      PIC X.
      *    ** KENNZEICHEN SPESENFREI (UMSATZPROVISION)
           05 KOMPL-SEB-SPESFR                   PIC X.
           05 KOMPL-SEB-F16                      PIC X.
      *    ** NUMMER DES VERMOEGENSGEGENSTANDS (FONDS)
           05 KOMPL-SEB-VERMGEGENSTAND           PIC 9(09).
           05 KOMPL-SEB-F17                      PIC X.
      *    ** VERMITTLERNUMMER
           05 KOMPL-SEB-VERMITTLER               PIC 9(18).
           05 KOMPL-SEB-F18                      PIC X.
      *    ** DEPOTINHABER GEBURTSDATUM (TTMMJHJJ)
           05 KOMPL-SEB-DEPOTINH1-GEBDAT         PIC 9(08).
           05 KOMPL-SEB-F19                      PIC X.
      *    ** DEPOTINHABER BERUF
           05 KOMPL-SEB-DEPOTINH1-BERUF          PIC 9(03).
           05 KOMPL-SEB-F20                      PIC X.
      *    ** DEPOTMITINHABER ANREDEKENNZEICHEN
           05 KOMPL-SEB-DEPOTINH2-ANREDE         PIC 9(03).
           05 KOMPL-SEB-F21                      PIC X.
      *    ** DEPOTMITINHABER TITEL
           05 KOMPL-SEB-DEPOTINH2-TITEL          PIC 9(03).
           05 KOMPL-SEB-F22                      PIC X.
      *    ** DEPOTMITINHABER NAME
           05 KOMPL-SEB-DEPOTINH2-NAME           PIC X(40).
           05 KOMPL-SEB-F22A                     PIC X.
      *    ** DEPOTMITINHABER VORNAME
           05 KOMPL-SEB-DEPOTINH2-VORNAME        PIC X(40).
           05 KOMPL-SEB-F23                      PIC X.
      *    ** DEPOTMITINHABER GEBURTSDATUM (TTMMJHJJ)
           05 KOMPL-SEB-DEPOTINH2-GEBDAT         PIC 9(08).
           05 KOMPL-SEB-F24                      PIC X.
      *    ** VERSANDADRESSE ANREDEKENNZEICHEN
           05 KOMPL-SEB-VERSAND-ANREDE           PIC 9(03).
           05 KOMPL-SEB-F25                      PIC X.
      *    ** VERSANDADRESSE TITEL
           05 KOMPL-SEB-VERSAND-TITEL            PIC 9(03).
           05 KOMPL-SEB-F26                      PIC X.
      *    ** VERSANDADRESSE NAME
           05 KOMPL-SEB-VERSAND-NAME             PIC X(40).
           05 KOMPL-SEB-F26A                     PIC X.
      *    ** VERSANDADRESSE VORNAME
           05 KOMPL-SEB-VERSAND-VORNAME          PIC X(40).
           05 KOMPL-SEB-F27                      PIC X.
      *    ** VERSANDADRESSE NAMENSERGAENZUNG 1
           05 KOMPL-SEB-VERSAND-ERGAENZ1         PIC X(40).
           05 KOMPL-SEB-F28                      PIC X.
      *    ** VERSANDADRESSE NAMENSERGAENZUNG 2
           05 KOMPL-SEB-VERSAND-ERGAENZ2         PIC X(40).
           05 KOMPL-SEB-F29                      PIC X.
      *    ** VERSANDADRESSE STRASSE
           05 KOMPL-SEB-VERSAND-STRASSE          PIC X(42).
           05 KOMPL-SEB-F30                      PIC X.
      *    ** VERSANDADRESSE PLZ DER STRASSENANSCHRIFT
           05 KOMPL-SEB-VERSAND-PLZ              PIC X(08).
           05 KOMPL-SEB-F31                      PIC X.
      *    ** VERSANDADRESSE ORT
           05 KOMPL-SEB-VERSAND-ORT              PIC X(40).
           05 KOMPL-SEB-F32                      PIC X.
      *    ** VERSANDADRESSE ISO-CODE LAND
           05 KOMPL-SEB-VERSAND-LAND             PIC X(03).
           05 KOMPL-SEB-F33                      PIC X.
      *    ** STUECKEBESTAND
           05 KOMPL-SEB-STUECKE                  PIC 9(15),9(03).
           05 KOMPL-SEB-STUECKE-R REDEFINES KOMPL-SEB-STUECKE.
             10 STUECKE-VK             PIC X(15).
             10 STUECKE-KOMMA          PIC X(01).
             10 STUECKE-NK             PIC X(03).
           05 KOMPL-SEB-F34                      PIC X.
      *    ** DATUM DER EROEFFNUNG (TTMMJHJJ)
           05 KOMPL-SEB-EROEFF-DAT               PIC 9(08).
           05 KOMPL-SEB-F35                      PIC X.
      *    ** EINZAHLSPERRE
           05 KOMPL-SEB-SPERRE-EIN               PIC 9(03).
           05 KOMPL-SEB-F36                      PIC X.
      *    ** AUSZAHLSPERRE
           05 KOMPL-SEB-SPERRE-AUS               PIC 9(03).
           05 KOMPL-SEB-F37                      PIC X.
      *    ** ISO-CODE-WAEHRUNG VERMOEGEGENSGEGENSTAND
           05 KOMPL-SEB-WHR-VERMG                PIC X(03).
           05 KOMPL-SEB-F38                      PIC X.
      *    ** ISO-CODE-WAEHRUNG VERMOEGEGENSGEGENSTAND
           05 KOMPL-SEB-PRODUKT                  PIC 9(10).
           05 KOMPL-SEB-F39                      PIC X.
      *    ** KATEGORIE
           05 KOMPL-SEB-KATEGORIE                PIC 9(03).
           05 KOMPL-SEB-F40                      PIC X.
      *    ** FESTLEGUNGSDATUM BEI VL (TTMMJHJJ)
           05 KOMPL-SEB-FESTLEG-DAT              PIC 9(08).
           05 KOMPL-SEB-F41                      PIC X.
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
           05 KOMPL-SEB-RATE                     PIC 9(14),9(02).
           05 KOMPL-SEB-RATE-R REDEFINES KOMPL-SEB-RATE.
             10 RATE-VK                PIC X(14).
             10 RATE-KOMMA             PIC X(01).
             10 RATE-NK                PIC X(02).
           05 KOMPL-SEB-F42                      PIC X.
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
           05 KOMPL-SEB-ZYKLUS                   PIC 9(02).
           05 KOMPL-SEB-F43                      PIC X.
      *    ** VERTRAGSART
           05 KOMPL-SEB-VERTRAG                  PIC X(04).
           05 KOMPL-SEB-F44                      PIC X.
      *    ** WAEHRUNGSKENNZEICHEN DER RATE
           05 KOMPL-SEB-RATE-ISO                 PIC X(03).
           05 KOMPL-SEB-F45                      PIC X.
      *    ** NUMMER DER RISIKOKLASSE
           05 KOMPL-SEB-RISIKO-KL                PIC 9(09).
      *    ** ';'
           05 KOMPL-SEB-F46                      PIC X    .
      *    ** AAUDP-ERG1
           05 KOMPL-SEB-ERG1                     PIC X(40).
      *    ** ';'
           05 KOMPL-SEB-F47                      PIC X    .
      *    ** AAUDP-REFERENZ
           05 KOMPL-SEB-REFERENZ                 PIC X(30).
      *    ** ';'
           05 KOMPL-SEB-F48                      PIC X    .
      *    ** ISOCODENW
           05 KOMPL-SEB-ISOCODENW                PIC X(03).
      *    ** ';'
           05 KOMPL-SEB-F49                      PIC X    .
      *    ** DSK
           05 KOMPL-SEB-DSK                      PIC 9(01).
      *    ** ';'
           05 KOMPL-SEB-F50                      PIC X    .
      *    ** ANLEGERGRUPPEGRUPPE
           05 KOMPL-SEB-AAANL-NR                 PIC 9(08).
      *    ** ';'** ';'
           05 KOMPL-SEB-F51                      PIC X    .
      *    ** KENNZEICHEN BEI MEHREREN ANSPARPLAENENLAENEN
           05 KOMPL-SEB-ADPER-KZ                 PIC X(01).
      *    ** ';'** ';'
           05 KOMPL-SEB-F52                      PIC X    .
      *    ** EXTERNE DEPOTNUMMERNUMMER
           05 KOMPL-SEB-EXTDEP-NR                PIC X(15).
      *    ** ';'** ';'
           05 KOMPL-SEB-F53                      PIC X    .
      *
      *    ** ERGAENZUNGSZEILE 3EILE 3
           05 KOMPL-SEB-ERG3                     PIC X(40).
      *    ** ';'** ';'
           05 KOMPL-SEB-F54                      PIC X    .

      *    ** VERSANDADRESSE NAMENSERGAENZUNG 3
           05 KOMPL-SEB-VERSAND-ERGAENZ3         PIC X(40).
      *    ** ';'
           05 KOMPL-SEB-F55                      PIC X    .

      *    ** WERTPAPIERKENNNUMMER
           05 KOMPL-SEB-WPKN                     PIC X(11) .
      *    ** ';'
           05 KOMPL-SEB-F56                      PIC X     .

      *    ** ISIN
           05 KOMPL-SEB-ISIN                     PIC X(12) .
      *    ** ';'
           05 KOMPL-SEB-F57                      PIC X
                                                      .
      *    ** DEPOTSTATUS
           05 KOMPL-SEB-DEPOTSTATUS              PIC 9(01) .
      *    ** ';'
           05 KOMPL-SEB-F58                      PIC X     .

      *    ** PERSONENNUMMER
           05 KOMPL-SEB-PERSONENNUMMER           PIC 9(10) .
      *    ** ';'
           05 KOMPL-SEB-F59                      PIC X     .

      *    ** VERMITTLERSTATUS
           05 KOMPL-SEB-VERMITTLERSTATUS         PIC 9(01) .
      *    ** ';'
           05 KOMPL-SEB-F60                      PIC X     .

      *    ** DEPOTWERT IN ABRECHNUNGSWAEHRUNG
           05 KOMPL-SEB-DEPOTWERT-ABR-WHG        PIC 9(14),9(02).
      *    ** ';'
           05 KOMPL-SEB-F61                      PIC X          .

      *    ** UMSATZDEVISENKURS
           05 KOMPL-SEB-UMSATZDEVISENKURS        PIC 9(12),9(06).
      *    ** ';'
           05 KOMPL-SEB-F62                      PIC X     .

      *    ** '0A' (HEX)
           05 KOMPL-SEB-ENDE-KZ                  PIC X.
