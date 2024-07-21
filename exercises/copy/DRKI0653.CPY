      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0653.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0653.CPv  $
      *
      *   Rev 2.4   Dec 06 2004 13:12:38   t13ex58
      *FKI00892: SEB INVEST
      *
      *   Rev 2.3   Apr 15 2003 11:11:16   t13ex39
      *AKI00294 ISIN neues Satzformat
      *
      *   Rev 2.2   06 Jul 2000 14:26:56   T13EX39
      *AKI00100 SEB-INVEST
      *
      *   Rev 2.1   Dec 30 1999 17:01:16   T13EX47
      *
      *
      *   Rev 2.0   Feb 27 1999 12:58:10 SEB-  P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:27:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 0 6 5 3    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER DTA VON BFG INVEST IN NEUER STRUKTUR         *
      *  (SEB-BEZEICHNUNG : ADAS001D)
      *  LAENGE   :  857 BYTES                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *24.03.00!NEUERSTELLUNG                          !WHO      ! 01  *
      *AKI00100!                                       !         !     *
      *--------!---------------------------------------!---------!-----*
301002*30.11.02!Anpassung ISIN                         !P. Pohl  !     *
301002*AKI00294! BFG  in SEB  geändert                 !WHO      !     *
      *--------!---------------------------------------!---------!-----*

       01  DRKI0653.
           02 SEB-SATZ.
              05 SEB-DEPOTNR                  PIC X(08).
              05 SEB-FILLER REDEFINES SEB-DEPOTNR.
                 10 SEB-A-SATZ-ART            PIC X(01).
                 10 SEB-FILLER                PIC X(07).
              05 SEB-F01                      PIC X.
      *    ** UNTERDEPOTNUMMER
              05 SEB-UNTERDEPNR               PIC 9(02).
              05 SEB-F02                      PIC X.
      *    ** DEPOTINHABER ANREDEKENNZEICHEN
              05 SEB-ANREDE                   PIC 9(03).
              05 SEB-F03                      PIC X.
      *    ** DEPOTINHABER TITELKENNZEICHEN
              05 SEB-TITEL                    PIC 9(03).
              05 SEB-F04                      PIC X.
      *    ** DEPOTINHABER NAME
              05 SEB-NAME                     PIC X(40).
              05 SEB-F04A                     PIC X.
      *    ** DEPOTINHABER VORNAME
              05 SEB-VORNAME                  PIC X(40).
              05 SEB-F05                      PIC X.
      *    ** DEPOTINHABER NAMENSERGAENZUNG 1
              05 SEB-ERGAENZ1                 PIC X(40).
              05 SEB-F06                      PIC X.
      *    ** DEPOTINHABER NAMENSERGAENZUNG 2
              05 SEB-ERGAENZ2                 PIC X(40).
              05 SEB-F07                      PIC X.
      *    ** DEPOTINHABER STRASSE
              05 SEB-STRASSE                  PIC X(42).
              05 SEB-F08                      PIC X.
      *    ** DEPOTINHABER PLZ DER STRASSENANSCHRIFT
              05 SEB-PLZ                      PIC X(08).
              05 SEB-F09                      PIC X.
      *    ** DEPOTINHABER ORT
              05 SEB-ORT                      PIC X(40).
              05 SEB-F10                      PIC X.
      *    ** DEPOTINHABER ISO-CODE LAND
              05 SEB-LAND                     PIC X(03).
              05 SEB-F11                      PIC X.
      *    ** VERSICHERUNGSNUMMER
              05 SEB-VERSNR                   PIC X(15).
              05 SEB-F12                      PIC X.
      *    ** MITARBEITERNUMMER
              05 SEB-VERS-VERMITTLER          PIC X(30).
              05 FILLER REDEFINES SEB-VERS-VERMITTLER.
                 15 FILLER                    PIC X(03).
                 15 SEB-DVAG-VBNR             PIC 9(07).
                 15 FILLER                    PIC X(20).
              05 SEB-F13                      PIC X.
      *    ** DEPOTWERT ERRECHNET AUS STUECKEBESTAND X ANTEILWERT
              05 SEB-BESTAND                  PIC 9(16),9(02).
              05 SEB-BESTAND-R REDEFINES SEB-BESTAND.
                10 SEB-BESTAND-VK             PIC X(16).
                10 SEB-BESTAND-KOMMA          PIC X(01).
                10 SEB-BESTAND-NK             PIC X(02).
              05 SEB-F14                      PIC X.
      *    ** DATUM DES FONDSKURSES (TTMMJHJJ)
              05 SEB-KURSDATUM.
                 10 SEB-KURSDATUM-TT          PIC 9(02).
                 10 SEB-KURSDATUM-MM          PIC 9(02).
                 10 SEB-KURSDATUM-JH          PIC 9(02).
                 10 SEB-KURSDATUM-JJ          PIC 9(02).
              05 SEB-F15                      PIC X.
      *    ** KENNZEICHEN SPESENFREI (UMSATZPROVISION)
              05 SEB-SPESFR                   PIC X.
              05 SEB-F16                      PIC X.
      *    ** NUMMER DES VERMOEGENSGEGENSTANDS (FONDS)
              05 SEB-VERMGEGENSTAND           PIC 9(09).
              05 SEB-F17                      PIC X.
      *    ** VERMITTLERNUMMER
              05 SEB-VERMITTLER               PIC 9(18).
              05 SEB-F18                      PIC X.
      *    ** DEPOTINHABER GEBURTSDATUM (TTMMJHJJ)
              05 SEB-DEPOTINH1-GEBDAT         PIC 9(08).
              05 SEB-F19                      PIC X.
      *    ** DEPOTINHABER BERUF
              05 SEB-DEPOTINH1-BERUF          PIC 9(03).
              05 SEB-F20                      PIC X.
      *    ** DEPOTMITINHABER ANREDEKENNZEICHEN
              05 SEB-DEPOTINH2-ANREDE         PIC 9(03).
              05 SEB-F21                      PIC X.
      *    ** DEPOTMITINHABER TITEL
              05 SEB-DEPOTINH2-TITEL          PIC 9(03).
              05 SEB-F22                      PIC X.
      *    ** DEPOTMITINHABER NAME
              05 SEB-DEPOTINH2-NAME           PIC X(40).
              05 SEB-F22A                     PIC X.
      *    ** DEPOTMITINHABER VORNAME
              05 SEB-DEPOTINH2-VORNAME        PIC X(40).
              05 SEB-F23                      PIC X.
      *    ** DEPOTMITINHABER GEBURTSDATUM (TTMMJHJJ)
              05 SEB-DEPOTINH2-GEBDAT         PIC 9(08).
              05 SEB-F24                      PIC X.
      *    ** VERSANDADRESSE ANREDEKENNZEICHEN
              05 SEB-VERSAND-ANREDE           PIC 9(03).
              05 SEB-F25                      PIC X.
      *    ** VERSANDADRESSE TITEL
              05 SEB-VERSAND-TITEL            PIC 9(03).
              05 SEB-F26                      PIC X.
      *    ** VERSANDADRESSE NAME
              05 SEB-VERSAND-NAME             PIC X(40).
              05 SEB-F26A                     PIC X.
      *    ** VERSANDADRESSE VORNAME
              05 SEB-VERSAND-VORNAME          PIC X(40).
              05 SEB-F27                      PIC X.
      *    ** VERSANDADRESSE NAMENSERGAENZUNG 1
              05 SEB-VERSAND-ERGAENZ1         PIC X(40).
              05 SEB-F28                      PIC X.
      *    ** VERSANDADRESSE NAMENSERGAENZUNG 2
              05 SEB-VERSAND-ERGAENZ2         PIC X(40).
              05 SEB-F29                      PIC X.
      *    ** VERSANDADRESSE STRASSE
              05 SEB-VERSAND-STRASSE          PIC X(42).
              05 SEB-F30                      PIC X.
      *    ** VERSANDADRESSE PLZ DER STRASSENANSCHRIFT
              05 SEB-VERSAND-PLZ              PIC X(08).
              05 SEB-F31                      PIC X.
      *    ** VERSANDADRESSE ORT
              05 SEB-VERSAND-ORT              PIC X(40).
              05 SEB-F32                      PIC X.
      *    ** VERSANDADRESSE ISO-CODE LAND
              05 SEB-VERSAND-LAND             PIC X(03).
              05 SEB-F33                      PIC X.
      *************************************************************
      *                                                           *
      *            V E R T R A G S T E I L                        *
      *                                                           *
      * ***********************************************************
      *    ** STUECKEBESTAND
              05 SEB-STUECKE                  PIC 9(15),9(03).
              05 SEB-STUECKE-R REDEFINES SEB-STUECKE.
                10 SEB-STUECKE-VK             PIC X(15).
                10 SEB-STUECKE-KOMMA          PIC X(01).
                10 SEB-STUECKE-NK             PIC X(03).
              05 SEB-F34                      PIC X.
      *    ** DATUM DER EROEFFNUNG (TTMMJHJJ)
              05 SEB-EROEFF-DAT.
                 10 SEB-EROEFF-DAT-TT         PIC 9(02).
                 10 SEB-EROEFF-DAT-MM         PIC 9(02).
                 10 SEB-EROEFF-DAT-JH         PIC 9(02).
                 10 SEB-EROEFF-DAT-JJ         PIC 9(02).
              05 SEB-F35                      PIC X.
      *    ** EINZAHLSPERRE
              05 SEB-SPERRE-EIN               PIC 9(03).
              05 SEB-F36                      PIC X.
      *    ** AUSZAHLSPERRE
              05 SEB-SPERRE-AUS               PIC 9(03).
              05 SEB-F37                      PIC X.
      *    ** ISO-CODE-WAEHRUNG VERMOEGEGENSGEGENSTAND
              05 SEB-WHR-VERMG                PIC X(03).
              05 SEB-F38                      PIC X.
      *    ** ISO-CODE-WAEHRUNG VERMOEGEGENSGEGENSTAND
              05 SEB-PRODUKT                  PIC 9(10).
              05 SEB-F39                      PIC X.
      *    ** KATEGORIE
              05 SEB-KATEGORIE                PIC 9(03).
              05 SEB-F40                      PIC X.
      *    ** FESTLEGUNGSDATUM BEI VL (TTMMJHJJ)
              05 SEB-FESTLEG-DAT              PIC X(08).
              05 SEB-F41                      PIC X.
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
              05 SEB-RATE                     PIC 9(14),9(02).
              05 SEB-RATE-R REDEFINES SEB-RATE.
                10 SEB-RATE-VK                PIC X(14).
                10 SEB-RATE-KOMMA             PIC X(01).
                10 SEB-RATE-NK                PIC X(02).
              05 SEB-F42                      PIC X.
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
              05 SEB-ZYKLUS                   PIC 9(02).
              05 SEB-F43                      PIC X.
      *    ** VERTRAGSART
              05 SEB-VERTRAG                  PIC X(04).
              05 SEB-F44                      PIC X.
      *    ** WAEHRUNGSKENNZEICHEN DER RATE
              05 SEB-RATE-ISO                 PIC X(03).
              05 SEB-F45                      PIC X.
      *    ** NUMMER DER RISIKOKLASSE
           05 SEB-RISIKO-KL                PIC 9(09).
      *    ** ';'
           05 SEB-F46                      PIC X    .
      *    ** AAUDP-ERG1
           05 SEB-ERG1                     PIC X(40).
      *    ** ';'
           05 SEB-F47                      PIC X    .
      *    ** AAUDP-REFERENZ
           05 SEB-REFERENZ                 PIC X(30).
      *    ** ';'
           05 SEB-F48                      PIC X    .
      *    ** ISOCODENW
           05 SEB-ISOCODENW                PIC X(03).
      *    ** ';'
           05 SEB-F49                      PIC X    .
      *    ** DSK
           05 SEB-DSK                      PIC 9(01).
      *    ** ';'
           05 SEB-F50                      PIC X    .
      *    ** ANLEGERGRUPPEGRUPPE
           05 SEB-AAANL-NR                 PIC 9(08).
      *    ** ';'** ';'
           05 SEB-F51                      PIC X    .
      *    ** KENNZEICHEN BEI MEHREREN ANSPARPLAENENLAENEN
           05 SEB-ADPER-KZ                 PIC X(01).
      *    ** ';'** ';'
           05 SEB-F52                      PIC X    .
      *    ** EXTERNE DEPOTNUMMERNUMMER
           05 SEB-EXTDEP-NR                PIC X(15).
      *    ** ';'** ';'
           05 SEB-F53                      PIC X    .
      *
      *    ** ERGAENZUNGSZEILE 3EILE 3
           05 SEB-ERG3                     PIC X(40).
      *    ** ';'** ';'
           05 SEB-F54                      PIC X    .

      *    ** VERSANDADRESSE NAMENSERGAENZUNG 3
           05 SEB-VERSAND-ERGAENZ3         PIC X(40).
      *    ** ';'
           05 SEB-F55                      PIC X    .

      *    ** WERTPAPIERKENNNUMMER
           05 SEB-WPKN                     PIC X(11) .
      *    ** ';'
           05 SEB-F56                      PIC X     .

      *    ** ISIN
           05 SEB-ISIN                     PIC X(12) .
      *    ** ';'
           05 SEB-F57                      PIC X
                                                      .
      *    ** DEPOTSTATUS
           05 SEB-DEPOTSTATUS              PIC 9(01) .
      *    ** ';'
           05 SEB-F58                      PIC X     .

      *    ** PERSONENNUMMER
           05 SEB-PERSONENNUMMER           PIC 9(10) .
      *    ** ';'
           05 SEB-F59                      PIC X     .

      *    ** VERMITTLERSTATUS
           05 SEB-VERMITTLERSTATUS         PIC 9(01) .
      *    ** ';'
           05 SEB-F60                      PIC X     .

      *    ** DEPOTWERT IN ABRECHNUNGSWAEHRUNG
           05 SEB-DEPOTWERT-ABR-WHG        PIC 9(14),9(02).
      *    ** ';'
           05 SEB-F61                      PIC X          .

      *    ** UMSATZDEVISENKURS
           05 SEB-UMSATZDEVISENKURS        PIC 9(12),9(06).
      *    ** ';'
           05 SEB-F62                      PIC X     .

031204     05 FILLER                       PIC X(240).

      *    ** '0A' (HEX)
           05 SEB-ENDE-KZ                  PIC X.

           02 FILLER REDEFINES SEB-SATZ.
      *    ** 'A'
              05 SEB-V-SA                   PIC X(01).
      *    ** ERSTELLDATUM (JHJJMMTT)
              05 SEB-ERST-DAT               PIC X(08).
      *    ** VON DATUM (JHJJMMTT)
              05 SEB-VON-DAT                PIC X(08).
      *    ** BIS DATUM (JHJJMMTT)
              05 SEB-BIS-DAT                PIC X(08).
      *    ** VERMITTLER-NR - VORLAUFKARTE
              05 A-VERMITTLER               PIC 9(18).
031204*       05 FILLER                     PIC X(1096).
031204        05 FILLER                     PIC X(1336).
      *    ** 'X'
              05 A-SATZENDE                 PIC X(01).

           02 FILLER REDEFINES SEB-SATZ.
      *    ** 'E'
              05 E-SATZ-ART               PIC X(01).
      *    ** SATZ ANZAHL C-SAETZE
              05 SEB-N-ANZ                PIC 9(07).
      *    ** 'X'
031204*       05 FILLER                   PIC X(1131).
031204        05 FILLER                   PIC X(1371).
              05 E-SATZENDE               PIC X(01).
