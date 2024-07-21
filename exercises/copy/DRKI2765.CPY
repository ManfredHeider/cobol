      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2765.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2765.CPv  $
      *
      *   Rev 2.1   Sep 02 2013 10:07:44   T13ex19
      *AKI01204: SEPA - DB-Online - Aktu. BTGZAHLER-Banken
      *
      *   Rev 2.0   Jun 19 2013 13:40:48   t13ex19
      *Archiverstellung
      *
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 6 5    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER für Aktualisierung der Beitragszahlertabellen     *
      *  BTGZAHLERDEPOT/BTGZAHLERBANKINGP/BTGZAHLERKREDITK/            *
      *  BTGZAHLERSPARPLAN                                             *
      *    enthält Daten zum Beitragszahler und dessen Bankverbindung  *
      *    im SEPA-Format, d.h. IBAN und BIC                           *
      *                                                                *
      *    SATZLÄNGE 310                                               *
      *                                                                *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01204!19.06.13!    ! NEU                       ! B. Stoldt*
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2765.
           05  BZ-GES                     PIC  9(002).
           05  BZ-VNR                     PIC  9(015).
           05  BZ-ROLLE                   PIC  X(001).

           05  BZ-NNAME                   PIC  X(030).
           05  BZ-AEND-NNAME-KZ           PIC  X(001).
      *        J WENN NACHNAME GEAENDERT

           05  BZ-VNAME                   PIC  X(030).
           05  BZ-AEND-VNAME-KZ           PIC  X(001).
      *        J WENN VORNAME GEAENDERT

           05  BZ-GEBDATUM                PIC  X(010).
           05  BZ-AEND-GEBDATUM-KZ        PIC  X(001).
      *        J WENN GEBDATUM GEAENDERT

           05  BZ-ANREDE                  PIC S9(002).
           05  BZ-AEND-ANREDE-KZ          PIC  X(001).
      *        J WENN ANREDE GEAENDERT

           05  BZ-STR                     PIC  X(040).
           05  BZ-AEND-STR-KZ             PIC  X(001).
      *        J WENN STRASSE GEAENDERT

           05  BZ-PLZ                     PIC  X(009).
           05  BZ-AEND-PLZ-KZ             PIC  X(001).
      *        J WENN PLZ GEAENDERT

           05  BZ-ORT                     PIC  X(030).
           05  BZ-AEND-ORT-KZ             PIC  X(001).
      *        J WENN ORT GEAENDERT

           05  BZ-AUSL-KZ                 PIC  X(003).
           05  BZ-AEND-AUSL-KZ            PIC  X(001).
      *        J WENN AUSL-KZ GEAENDERT

           05  BZ-IBAN                    PIC  X(034).
           05  BZ-AEND-IBAN-KZ            PIC  X(001).
      *        J WENN IBAN GEAENDERT

           05  BZ-BIC                     PIC  X(011).
           05  BZ-AEND-BIC-KZ             PIC  X(001).
      *        J WENN BIC GEAENDERT

           05  BZ-ABW-BZ                  PIC  X(001).
           05  BZ-AEND-ABW-BZ-KZ          PIC  X(001).
      *        J WENN KENNZ. ABW. BZ GEAENDERT

           05  BZ-MAND-REFNR              PIC  X(035).
           05  BZ-AEND-MAND-REFNR-KZ      PIC  X(001).
      *        J WENN MANDATSREFERENZ GEAENDERT

           05  BZ-PRODUKT-ID              PIC  9(004).
      *        immer gefüllt
           05  BZ-UNTERNR                 PIC  9(002).
      *        immer gefüllt
           05  BZ-DBPRODUKT-LFDNR         PIC  9(004).
      *        nur bei Satzart KK und BP gefüllt

           05  BZ-ISIN                    PIC  X(012).
      *        nur bei Satzart SP gefüllt

           05  BZ-VL-KZ                   PIC  X(001).
      *        nur bei Satzart SP gefüllt

           05  BZ-GHZLFDNR                PIC  9(004).
      *        nur bei Satzart SP gefüllt

           05  BZ-LFDNR                   PIC  9(004).
           05  BZ-SATZART                 PIC  X(002).
      *        DP=BTGZAHLERDEPOT
      *        BP=BTGZAHLERBANKINP
      *        KK=BTGZAHLERKREDITK
      *        SP=BTGZAHLERSPARPLAN

           05  FILLER                     PIC  X(011).

           05  BZ-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

      *
      *---------------------------------------------------------------*
