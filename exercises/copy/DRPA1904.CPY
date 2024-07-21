      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  16.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1904.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1904.CPv  $
      *
      *   Rev 2.1   Sep 26 2018 12:42:20   t13ex67
      *APA02676: Daisy Auskunftsdialog - Ges. 20 - zulässige Produk
      *tschlüssel zu PGV´s
      *
      *   Rev 2.0   Aug 16 2018 08:36:04   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1904                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1904                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. STORCH-SCHAEFER                               *
      *    DATUM:    16.08.2018                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02676!16.08.18!2.1 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1904
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION DRPA1904 DES BUFFER-OZP ZYPA0032 ERMITTELT
      *    DIE ERGEBNISMENGE FÜR DIALOG SF42 'PRODUKT-TARIFZUORDNUNG'
      *
      ****************************************************************
      *
       01  PI-DRPA1904.
           05  PI-DRPA1904-UFKT                    PIC  9(002).
           05  PI-DRPA1904-NAME-ZW-SPEICHER        PIC  X(008).
           05  PI-DRPA1904-LOG-SATZ-NR             PIC  9(005).
           05  PI-DRPA1904-AUSWAHL-KRITERIEN.
               10 PI-DRPA1904-PGEB-GES             PIC 9(002).
               10 PI-DRPA1904-PROD-PROD-SCHL       PIC S9(004) BINARY.
               10 PI-DRPA1904-S-PROD-SCHL          PIC X(001).
               10 PI-DRPA1904-KNR-PGV              PIC S9(004) BINARY.
               10 PI-DRPA1904-S-PGV                PIC X(001).
      *
       01  PO-DRPA1904.
           05  PO-DRPA1904-RC                      PIC X(03).
               88 PO-DRPA1904-RC-OK                    VALUE 'OK '.
               88 PO-DRPA1904-RC-EOF                   VALUE 'EOF'.
               88 PO-DRPA1904-RC-LEER                  VALUE 'LER'.

           05  PO-DRPA1904-ANZAHL-ERG-ZEILEN       PIC 9(05).

           05  PO-DRPA1904-ERGEBNISZEILE.
               35 KNR-PGV                          PIC S9(004) BINARY.
               35 PPGZ-GUELTIG-AB                  PIC  X(010).
               35 PROD-PROD-SCHL                   PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


