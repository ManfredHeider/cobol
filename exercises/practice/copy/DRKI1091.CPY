
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1091.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1091.CPv  $
      *
      *   Rev 2.2   Feb 02 2005 15:44:34   t13ex39
      *FKI00906: SEB-INVEST  Vertragszustand
      *
      *   Rev 2.1   06 Jul 2000 13:27:42   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 2.1   Dec 30 1999 17:01:16   T13EX47
      *
      *
      *   Rev 2.0   Feb 27 1999 12:58:10 BFG-  P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   25 Aug 1998 09:27:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 0 9 1    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER DTA VON BFG INVEST IN NEUER STRUKTUR         *
      *  (BFG-BEZEICHNUNG : ADAS001D)
      *  LAENGE   :  854 BYTES                                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00100!24.03.00!01.1! NEU                       ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00906!26.01.05!02.0!Vertragszustand wieder auf !Hornung   *
      *         !        !    !aktiv setzen, bei erneut   !          *
      *         !        !    !gelieferten Verträgen.     !          *
      *         !        !    !Dafür Feld BNK-PRODUKT     !          *
      *         !        !    !ersetzt durch              !          *
      *         !        !    !BNK-VERTRAGSZUSTAND-DATUM  !          *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *

       01  DRKI1091.

      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
           05 BNK-VERARB-KZ                PIC X(01).

      *       PERSONEN-VERARBEITUNGSKENNZEICHEN
      *                D = UPDATE
      *                I = INSERT
           05 BNK-PERSON1-VERARB-KZ        PIC X(01).
           05 BNK-PERSON2-VERARB-KZ        PIC X(01).

           05 BNK-VNR                      PIC X(15).

           05 BNK-DEPOTNR                  PIC 9(08).
           05 BNK-FILLER REDEFINES BNK-DEPOTNR.
              10 BNK-A-SATZ-ART            PIC X(01).
              10 BNK-FILLER                PIC X(07).
      *    ** UNTERDEPOTNUMMER
           05 BNK-UNTERDEPNR               PIC 9(02).
      *    ** DEPOTINHABER ANREDEKENNZEICHEN
           05 BNK-ANREDE                   PIC 9(03).
           05 BNK-AEND-ANREDE-KZ           PIC X(01).
      *    ** DEPOTINHABER TITELKENNZEICHEN
           05 BNK-TITEL                    PIC 9(03).
           05 BNK-AEND-TITEL-KZ            PIC X(01).
      *    ** DEPOTINHABER NAME
           05 BNK-NAME                     PIC X(40).
           05 BNK-NAME-ALT                 PIC X(40).
           05 BNK-AEND-NAME-KZ             PIC X(01).
      *    ** DEPOTINHABER VORNAME
           05 BNK-VORNAME                  PIC X(40).
           05 BNK-VORNAME-ALT              PIC X(40).
           05 BNK-AEND-VORNAME-KZ          PIC X(01).
      *    ** DEPOTINHABER NAMENSERGAENDZUNG 1
           05 BNK-ERGAENZ1                  PIC X(40).
           05 BNK-AEND-ERGAENDZ1-KZ         PIC X(01).
      *    ** DEPOTINHABER NAMENSERGAENDZUNG 2
           05 BNK-ERGAENZ2                  PIC X(40).
           05 BNK-AEND-ERGAENDZ2-KZ         PIC X(01).
      *    ** DEPOTINHABER STRASSE
           05 BNK-STRASSE                  PIC X(42).
           05 BNK-AEND-STRASSE-KZ          PIC X(01).
      *    ** DEPOTINHABER PLZ DER STRASSENANSCHRIFT
           05 BNK-PLZ                      PIC X(08).
           05 BNK-AEND-PLZ-KZ              PIC X(01).
      *    ** DEPOTINHABER ORT
           05 BNK-ORT                      PIC X(40).
           05 BNK-AEND-ORT-KZ              PIC X(01).
      *    ** DEPOTINHABER ISO-CODE LAND
           05 BNK-LAND                     PIC X(03).
           05 BNK-AEND-LAND-KZ              PIC X(01).
      *    ** VERSICHERUNGSNUMMER
           05 BNK-VERSNR                   PIC X(15).
           05 BNK-AEND-VERSNR-KZ           PIC X(01).
      *    ** MITARBEITERNUMMER
           05 BNK-VERS-VERMITTLER          PIC X(30).
              05 FILLER REDEFINES BNK-VERS-VERMITTLER.
                 15 FILLER                    PIC X(03).
                 15 BNK-DVAG-VBNR             PIC 9(07).
                 15 FILLER                    PIC X(20).
           05 BNK-AEND-VERS-VERMITTLER-KZ  PIC X(01).
      *    ** DEPOTWERT ERRECHNET AUS STUECKEBESTAND X ANTEILWERT
           05 BNK-BESTAND                  PIC 9(16),9(02).
           05 BNK-BESTAND-R REDEFINES BNK-BESTAND.
             10 BNK-BESTAND-VK             PIC X(16).
             10 BNK-BESTAND-KOMMA          PIC X(01).
             10 BNK-BESTAND-NK             PIC X(02).
           05 BNK-AEND-BESTAND-KZ          PIC X(01).
      *    ** DATUM DES FONDSKURSES (TTMMJHJJ)
           05 BNK-KURSDATUM.
              10 BNK-KURSDATUM-TT          PIC 9(02).
              10 BNK-KURSDATUM-MM          PIC 9(02).
              10 BNK-KURSDATUM-JH          PIC 9(02).
              10 BNK-KURSDATUM-JJ          PIC 9(02).
           05 BNK-AEND-KURSDATUM-KZ         PIC X(01).
      *    ** KENNZEICHEN SPESENFREI (UMSATZPROVISION)
           05 BNK-SPESFR                   PIC X.
           05 BNK-AEND-SPESFR-KZ            PIC X(01).
      *    ** NUMMER DES VERMOEGENSGEGENSTANDS (FONDS)
           05 BNK-VERMGEGENSTAND           PIC 9(09).
           05 BNK-AEND-VERMGEGENSTAND-KZ    PIC X(01).
      *    ** VERMITTLERNUMMER
           05 BNK-VERMITTLER               PIC 9(10).
           05 BNK-filler redefines BNK-VERMITTLER.
              10 BNK-filler                     pic 9(05).
              10 BNK-c1-ges                     pic 9(02).
              10 BNK-c1-orggs                   pic 9(03).
           05 BNK-AEND-VERMITTLER-KZ        PIC X(01).
      *    ** DEPOTINHABER GEBURTSDATUM (TTMMJHJJ)
           05 BNK-DEPOTINH1-GEBDAT.
              10 BNK-DEPOTINH1-TT          PIC 9(02).
              10 BNK-DEPOTINH1-MM          PIC 9(02).
              10 BNK-DEPOTINH1-JH          PIC 9(02).
              10 BNK-DEPOTINH1-JJ          PIC 9(02).
           05 BNK-AEND-DEPOTINH1-GEBDAT-KZ  PIC X(01).
      *    ** DEPOTINHABER BERUF
           05 BNK-DEPOTINH1-BERUF          PIC 9(03).
           05 BNK-AEND-DEPOTINH1-BERUF-KZ   PIC X(01).
      *    ** DEPOTMITINHABER ANREDEKENNZEICHEN
           05 BNK-DEPOTINH2-ANREDE         PIC 9(03).
           05 BNK-AEND-DEPOTINH2-ANREDE-KZ  PIC X(01).
      *    ** DEPOTMITINHABER TITEL
           05 BNK-DEPOTINH2-TITEL          PIC 9(03).
           05 BNK-AEND-DEPOTINH2-TITEL-KZ   PIC X(01).
      *    ** DEPOTMITINHABER NAME
           05 BNK-DEPOTINH2-NAME           PIC X(40).
           05 BNK-DEPOTINH2-NAME-ALT       PIC X(40).
           05 BNK-AEND-DEPOTINH2-NAME-KZ    PIC X(01).
      *    ** DEPOTMITINHABER VORNAME
           05 BNK-DEPOTINH2-VORNAME        PIC X(40).
           05 BNK-DEPOTINH2-VORNAME-ALT    PIC X(40).
           05 BNK-AEND-DEPOTINH2-VORNAME-KZ PIC X(01).
      *    ** DEPOTMITINHABER GEBURTSDATUM (TTMMJHJJ)
           05 BNK-DEPOTINH2-GEBDAT.
              10 BNK-DEPOTINH2-TT          PIC 9(02).
              10 BNK-DEPOTINH2-MM          PIC 9(02).
              10 BNK-DEPOTINH2-JH          PIC 9(02).
              10 BNK-DEPOTINH2-JJ          PIC 9(02).
           05 BNK-AEND-DEPOTINH2-GEBDAT-KZ         PIC X(01).
      *************************************************************
      *                                                           *
      *            V E R T R A G S T E I L                        *
      *                                                           *
      * ***********************************************************
      *    ** STUECKEBESTAND
           05 BNK-STUECKE                  PIC 9(15),9(03).
           05 BNK-STUECKE-R REDEFINES BNK-STUECKE.
             10 BNK-STUECKE-VK             PIC X(15).
             10 BNK-STUECKE-KOMMA          PIC X(01).
             10 BNK-STUECKE-NK             PIC X(03).
           05 BNK-AEND-STUECKE-KZ          PIC X(01).
      *    ** DATUM DER EROEFFNUNG (TTMMJHJJ)
           05 BNK-EROEFF-DAT.
              10 BNK-EROEFF-DAT-TT         PIC 9(02).
              10 BNK-EROEFF-DAT-MM         PIC 9(02).
              10 BNK-EROEFF-DAT-JH         PIC 9(02).
              10 BNK-EROEFF-DAT-JJ         PIC 9(02).
           05 BNK-AEND-EROEFF-DAT-KZ       PIC X(01).
      *    ** EINZAHLSPERRE
           05 BNK-SPERRE-EIN               PIC 9(03).
           05 BNK-AEND-SPERRE-EIN-KZ       PIC X(01).
      *    ** AUSZAHLSPERRE
           05 BNK-SPERRE-AUS               PIC 9(03).
           05 BNK-AEND-SPERRE-AUS-KZ       PIC X(01).
      *    ** ISO-CODE-WAEHRUNG VERMOEGEGENSGEGENSTAND
           05 BNK-WHR-VERMG                PIC X(03).
           05 BNK-AEND-WHR-VERMG-KZ        PIC X(01).
260105*    05 BNK-PRODUKT                  PIC 9(10).
260105     05 BNK-VERTRAGSZUSTAND-DATUM    PIC X(10).
           05 BNK-AEND-PRODUKT-KZ          PIC X(01).
      *    ** KATEGORIE
           05 BNK-KATEGORIE                PIC 9(03).
           05 BNK-AEND-KATEGORIE-KZ        PIC X(01).
      *    ** FESTLEGUNGSDATUM BEI VL (TTMMJHJJ)
           05 BNK-FESTLEG-DAT              PIC 9(08).
           05 BNK-AEND-FESTLEG-DAT-KZ      PIC X(01).
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
           05 BNK-RATE                     PIC 9(14),9(02).
           05 BNK-RATE-R REDEFINES BNK-RATE.
             10 BNK-RATE-VK                PIC X(14).
             10 BNK-RATE-KOMMA             PIC X(01).
             10 BNK-RATE-NK                PIC X(02).
           05 BNK-AEND-RATE-KZ             PIC X(01).
      *    ** BETRAG AUS GUELTIGEM ANSPARPLAN (SUMME BEI MEHREREN)
           05 BNK-ZYKLUS                   PIC 9(02).
           05 BNK-AEND-ZYKLUS-KZ           PIC X(01).
      *    ** VERTRAGSART
           05 BNK-VERTRAG                  PIC X(04).
           05 BNK-AEND-VERTRAG-KZ          PIC X(01).
      *    ** WAEHRUNGSKENNZEICHEN DER RATE
           05 BNK-RATE-ISO                 PIC X(03).
           05 BNK-AEND-RATE-ISO-KZ         PIC X(01).
      *    ** VERTRAGSZUSTAND
           05 BNK-VERTZUSTAND              PIC 9(02).
           05 BNK-AEND-VERTZUSTAND-KZ      PIC X(01).
