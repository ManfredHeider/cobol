      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2719.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2719.CPv  $
      *
      *   Rev 2.1   Jun 06 2013 09:08:02   t13ex19
      *AKI01204: SEPA - DB-Online-Aktu. BTGZAHLERVTG
      *
      *   Rev 2.0   Jan 14 2013 13:46:58   t13ex19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 7 1 9    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER für Aktualisierung der Tabelle BTGZAHLERVTG       *
      *    enthält Daten zum Beitragszahler und dessen Bankverbindung  *
      *    im SEPA-Format, d.h. IBAN und BIC                           *
      *                                                                *
      *    SATZLÄNGE   310                                             *
      *                                                                *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01204!14.01.13!    ! NEU                       ! B. Stoldt*
      * --------!--------!----!---------------------------!----------*
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  DRKI2719.
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

           05  FILLER                     PIC  X(044).

           05  BZ-AKTUAL-KZ               PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

