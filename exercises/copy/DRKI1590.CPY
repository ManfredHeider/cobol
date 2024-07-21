      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.09.2003
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1590.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1590.CPv  $
      *
      *   Rev 2.1   Feb 02 2004 07:54:42   t13ex49
      *AKI00268 - Übergabe post. Prüfung - Komplettlauf
      *
      *   Rev 2.0   Sep 23 2003 11:50:00   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00268!23.09.03!    ! Neu                       ! P. Pohl   *
      *         !16.12.03!    ! Anpassung Parameter und   ! P. Pohl   *
      *         !        !    ! Teilort                   !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der postalisch geprüften Adressen für die
      * Übernahme in Tabelle ADRESSE
      *
      * Satzlänge 297
      *
      *
       01 DRKI1590.
           05  DRKI1590-AdresseID                 PIC X(26).
           05  DRKI1590-ADRESSELANDID             PIC X(03).
           05  DRKI1590-ADRESSEBDSLDID            PIC X(03).
           05  FILLER                             PIC X(01).
           05  DRKI1590-ADRESSEPLZ                PIC X(15).
           05  FILLER                             PIC X(01).
           05  DRKI1590-ADRESSEORT                PIC X(30).
           05  DRKI1590-ORTSHERKUNFT              PIC X(01).
           05  DRKI1590-ADRESSESTRHAUSNUM         PIC X(45).
           05  FILLER                             PIC X(01).
           05  DRKI1590-ADRESSESUCHSTRASSE        PIC X(45).
           05  FILLER                             PIC X(01).
           05  FILLER                             PIC X(30).
           05  DRKI1590-ORTSTEIL                  PIC X(30).
      *    05  DRKI1590-TX1                       PIC X(54).
           05  DRKI1590-TX1                       PIC X(65).
           05  FILLER REDEFINES DRKI1590-TX1.
               10  DRKI1590-EKZ                   PIC X(01).
               10  DRKI1590-Ortssuche             PIC X(01).
               10  DRKI1590-Strassensuche         PIC X(01).
               10  DRKI1590-PLZ-Aenderung         PIC X(01).
               10  DRKI1590-Orts-Aenderung        PIC X(01).
               10  DRKI1590-Strassen-Aenderung    PIC X(01).
               10  DRKI1590-Strassen-Kuerzung     PIC X(01).
               10  DRKI1590-Strassen-Umschl       PIC X(01).
               10  DRKI1590-Alias                 PIC X(01).
               10  DRKI1590-RC                    PIC X(02).
               10  DRKI1590-Postfachpruefung      PIC X(01).
               10  DRKI1590-Postfach-PLZ          PIC X(05).
               10  DRKI1590-Gemeindekennziffer    PIC X(08).
               10  FILLER REDEFINES DRKI1590-Gemeindekennziffer.
                   15  DRKI1590-Bundesland        PIC X(02).
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
                   15  DRKI1590-Regierungsbezirk  PIC X(02).
                   15  DRKI1590-Kreis             PIC X(02).
                   15  DRKI1590-Gemeinde          PIC X(02).
               10  DRKI1590-Strassenverzeichnis   PIC X(01).
      *            P = Post-Strassenverzeichnis
      *            E = erweitertes Strassenverzeichnis
      *            N = kein Strassenverzeichnis
               10  DRKI1590-Orts-NR               PIC S9(09) COMP-3.
               10  DRKI1590-STR-V-NR              PIC S9(09) COMP-3.
               10  DRKI1590-STR-NR                PIC S9(07) COMP-3.
               10  DRKI1590-Orts-Suchschritt      PIC S9(03) COMP-3.
               10  DRKI1590-STR-Suchschritt       PIC S9(03) COMP-3.
               10  DRKI1590-PLZ-TYP               PIC X(04).
               10  DRKI1590-Suchergebnis          PIC X(01).
               10  FILLER                         PIC X(02).
               10  FILLER                         PIC X(02).
               10  DRKI1590-O-W-KZ                PIC X(01).
               10  DRKI1590-STR-KL                PIC X(01).
               10  DRKI1590-RC-STA                PIC X(02).
               10  DRKI1590-STA-BER               PIC X(01).
               10  DRKI1590-STA-ANZ               PIC X(02).
               10  DRKI1590-STA-AEHN              PIC X(01).
               10  DRKI1590-TEILORTPRUEFUNG       PIC X(01).
      *             0  = Teilort nicht angeben
      *             1  = Teilort kann angegeben werden, ist jedoch
      *                 nicht erforderlich
      *             2  = Teilort erforderlich
               10  Filler                         PIC X(01).
               10  DRKI1590-STR-ART               PIC X(01).
               10  DRKI1590-Orts-Umschl           PIC X(01).
      *            ' ' = keine Umschlüsselung
      *             1  = Umschlüsselung Gebietsreform
      *             2  = Ortsteil in gültigen Ortsnamen umgeschlüsselt
      *----------- Ende DRKI1590 ---------------------------
      *
