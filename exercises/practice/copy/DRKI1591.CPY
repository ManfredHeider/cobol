      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1591.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1591.CPv  $
      *
      *   Rev 2.2   Mar 11 2004 10:03:44   t13ex49
      *Einführung AdresseOrtsteil
      *
      *   Rev 2.1   Feb 02 2004 07:54:58   t13ex49
      *AKI00268 - Übergabe post. Prüfung - Komplettlauf
      *
      *   Rev 2.0   Sep 24 2003 07:44:36   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00268!23.09.03!    ! Neu                       ! P. Pohl   *
      *         !16.12.03!    ! Anpassung Parameterbereich! P. Pohl   *
      *         !        !    ! und Ortsteil (Länge 341)  !           *
      *         !26.02.04!    ! 2 Feldnamen angepasst     ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Kontrolldatei der postalisch geprüften Adressen nach der
      * Übernahme in Tabelle ADRESSE
      *
      * Satzlänge 341 Byte
      *
      * Hinweis: Der erste Teil (bis einschliesslich DRKI1591-TX1)
      *          muss einen identischen Aufbau zu DRKI1590 haben
      *
       01 DRKI1591.
           05  DRKI1591-AdresseID                 PIC X(26).
           05  DRKI1591-ADRESSELANDID             PIC X(03).
           05  DRKI1591-ADRESSEBDSLDID            PIC X(03).
           05  FILLER                             PIC X(01).
           05  DRKI1591-ADRESSEPLZ                PIC X(15).
           05  FILLER                             PIC X(01).
           05  DRKI1591-ADRESSEORT                PIC X(30).
           05  DRKI1591-ORTSHERKUNFT              PIC X(01).
           05  DRKI1591-ADRESSESTRHAUSNUM         PIC X(45).
           05  FILLER                             PIC X(01).
           05  DRKI1591-ADRESSESUCHSTRASSE        PIC X(45).
           05  FILLER                             PIC X(01).
           05  FILLER                             PIC X(30).
           05  DRKI1591-ORTSTEIL                  PIC X(30).
      *    05  DRKI1591-TX1                       PIC X(54).
           05  DRKI1591-TX1                       PIC X(65).
           05  FILLER REDEFINES DRKI1591-TX1.
               10  DRKI1591-EKZ                   PIC X(01).
               10  DRKI1591-Ortssuche             PIC X(01).
               10  DRKI1591-Strassensuche         PIC X(01).
               10  DRKI1591-PLZ-Aenderung         PIC X(01).
               10  DRKI1591-Orts-Aenderung        PIC X(01).
               10  DRKI1591-Strassen-Aenderung    PIC X(01).
               10  DRKI1591-Strassen-Kuerzung     PIC X(01).
               10  DRKI1591-Strassen-Umschl       PIC X(01).
               10  DRKI1591-Alias                 PIC X(01).
               10  DRKI1591-RC                    PIC X(02).
               10  DRKI1591-Postfachpruefung      PIC X(01).
               10  DRKI1591-Postfach-PLZ          PIC X(05).
               10  DRKI1591-Gemeindekennziffer    PIC X(08).
               10  FILLER REDEFINES DRKI1591-Gemeindekennziffer.
                   15  DRKI1591-Bundesland        PIC X(02).
      *---------------
      *                01 = Schleswig-Holstein       SLH
      *                02 = Hamburg                  HAM
      *                03 = Niedersachsen            NSA
      *                04 = Bremen                   BRE
      *                05 = Nordrhein-Westfalen      NRW
      *                06 = Hessen                   HES
      *                07 = Rheinland-Pfalz          RPF
      *                08 = Baden-Württemberg        BAW
      *                09 = Bayern                   BAY
      *                10 = Saarland                 SAE
      *                11 = Berlin                   BLN + BLO
      *                12 = Brandenburg              BRA
      *                13 = Mecklenburg-Vorpommern   MEV
      *                14 = Sachsen                  SAC
      *                15 = Sachsen-Anhalt           SAA
      *                16 = Thüringen                THU
                   15  DRKI1591-Regierungsbezirk  PIC X(02).
                   15  DRKI1591-Kreis             PIC X(02).
                   15  DRKI1591-Gemeinde          PIC X(02).
               10  DRKI1591-Strassenverzeichnis   PIC X(01).
      *            P = Post-Strassenverzeichnis
      *            E = erweitertes Strassenverzeichnis
      *            N = kein Strassenverzeichnis
               10  DRKI1591-Orts-NR               PIC S9(09) COMP-3.
               10  DRKI1591-STR-V-NR              PIC S9(09) COMP-3.
               10  DRKI1591-STR-NR                PIC S9(07) COMP-3.
               10  DRKI1591-Orts-Suchschritt      PIC S9(03) COMP-3.
               10  DRKI1591-STR-Suchschritt       PIC S9(03) COMP-3.
               10  DRKI1591-PLZ-TYP               PIC X(04).
               10  DRKI1591-Suchergebnis          PIC X(01).
               10  FILLER                         PIC X(02).
               10  FILLER                         PIC X(02).
               10  DRKI1591-O-W-KZ                PIC X(01).
               10  DRKI1591-STR-KL                PIC X(01).
               10  DRKI1591-RC-STA                PIC X(02).
               10  DRKI1591-STA-BER               PIC X(01).
               10  DRKI1591-STA-ANZ               PIC X(02).
               10  DRKI1591-STA-AEHN              PIC X(01).
               10  DRKI1591-TEILORTPRUEFUNG       PIC X(01).
      *             0  = Teilort nicht angeben
      *             1  = Teilort kann angegeben werden, ist jedoch
      *                 nicht erforderlich
      *             2  = Teilort erforderlich
               10  Filler                         PIC X(01).
               10  DRKI1591-STR-ART               PIC X(01).
               10  DRKI1591-Orts-Umschl           PIC X(01).
      *            ' ' = keine Umschlüsselung
      *             1  = Umschlüsselung Gebietsreform
      *             2  = Ortsteil in gültigen Ortsnamen umgeschlüsselt
      *
           05  DRKI1591-ADRESSEID-2               PIC X(13).
           05  DRKI1591-BEREITS-VORHANDEN         PIC X(01).
           05  DRKI1591-RESERVE-2                 PIC X(30).


      *----------- Ende DRKI1591 ---------------------------

