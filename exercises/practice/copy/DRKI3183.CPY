      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3183.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3183.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 08:12:30   t13ac20
      *AKI01534: G20 - Projekt LENA: Kunde
      *
      *   Rev 2.0   Dec 11 2017 11:27:30   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    DTA KUNDE
      *    AMLEBEN - G20 (PSLIFE)
      *
      *
      *    SATZLÄNGE VARIABEL
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!11.12.17!2.1 ! NEUERSTELLUNG             ! SWO
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
      *    HAUPTSATZ SA 'C'
           05 HAUPTSATZ.
              10 SA                              PIC  X(001).
              10 VNR                             PIC  X(015).
              10 ROLLE                           PIC  X(004).
              10 NACHNAME                        PIC  X(040).
              10 VORNAME                         PIC  X(040).
              10 GEBDAT                          PIC  X(010).
              10 ANREDE                          PIC S9(002).
              10 GESCHLECHT                      PIC  X(001).
              10 STRASSE                         PIC  X(040).
              10 POSTFACH                        PIC  X(006).
              10 AUSL-KZN                        PIC  X(003).
              10 PLZ                             PIC  X(005).
              10 ORT                             PIC  X(040).
              10 IBAN                            PIC  X(034).
              10 BIC                             PIC  X(011).
      *
      *    VORSATZ   SA 'A'
           05 VORSATZ  REDEFINES HAUPTSATZ.
              10 V-SA                            PIC  X(001).
              10 V-ABSENDER                      PIC  X(003).
              10 V-ERSTELLDAT                    PIC  X(010).
              10 V-EMPFAENGER                    PIC  X(004).
              10 V-INHALT                        PIC  X(020).
      *
      *    NACHSATZ  SA 'E'
           05 NACHSATZ REDEFINES HAUPTSATZ.
              10 N-SA                            PIC  X(001).
              10 N-ANZSATZ                       PIC  9(009).

