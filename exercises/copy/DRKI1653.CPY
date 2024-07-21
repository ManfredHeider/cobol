      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1653.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1653.CPv  $
      *
      *   Rev 2.1   Mar 09 2005 06:20:50   t13ex49
      *AKI00268: Post.Prüfung vor Kundenaktualisierung - DB
      *
      *   Rev 2.0   Mar 12 2004 11:31:12   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00268!12.03.04!    ! Neu                       ! P. Pohl   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der postalisch geprüften Adressen für die
      * Übernahme in Tabelle ADRESSE
      *
      * Satzlänge              297
      * Felder aus DRKI0170     89
      * Gesamtlänge            386
      *
       01 DRKI1653.
           05  DRKI1653-AdresseID                 PIC X(26).
           05  DRKI1653-ADRESSELANDID             PIC X(03).
           05  DRKI1653-ADRESSEBDSLDID            PIC X(03).
           05  FILLER                             PIC X(01).
           05  DRKI1653-ADRESSEPLZ                PIC X(15).
           05  FILLER                             PIC X(01).
           05  DRKI1653-ADRESSEORT                PIC X(30).
           05  DRKI1653-ORTSHERKUNFT              PIC X(01).
           05  DRKI1653-ADRESSESTRHAUSNUM         PIC X(45).
           05  FILLER                             PIC X(01).
           05  DRKI1653-ADRESSESUCHSTRASSE        PIC X(45).
           05  FILLER                             PIC X(01).
           05  FILLER                             PIC X(30).
           05  DRKI1653-ORTSTEIL                  PIC X(30).
      *    05  DRKI1653-TX1                       PIC X(54).
           05  DRKI1653-TX1                       PIC X(65).
           05  FILLER REDEFINES DRKI1653-TX1.
               10  DRKI1653-EKZ                   PIC X(01).
               10  DRKI1653-Ortssuche             PIC X(01).
               10  DRKI1653-Strassensuche         PIC X(01).
               10  DRKI1653-PLZ-Aenderung         PIC X(01).
               10  DRKI1653-Orts-Aenderung        PIC X(01).
               10  DRKI1653-Strassen-Aenderung    PIC X(01).
               10  DRKI1653-Strassen-Kuerzung     PIC X(01).
               10  DRKI1653-Strassen-Umschl       PIC X(01).
               10  DRKI1653-Alias                 PIC X(01).
               10  DRKI1653-RC                    PIC X(02).
               10  DRKI1653-Postfachpruefung      PIC X(01).
               10  DRKI1653-Postfach-PLZ          PIC X(05).
               10  DRKI1653-Gemeindekennziffer    PIC X(08).
               10  FILLER REDEFINES DRKI1653-Gemeindekennziffer.
                   15  DRKI1653-Bundesland        PIC X(02).
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
                   15  DRKI1653-Regierungsbezirk  PIC X(02).
                   15  DRKI1653-Kreis             PIC X(02).
                   15  DRKI1653-Gemeinde          PIC X(02).
               10  DRKI1653-Strassenverzeichnis   PIC X(01).
      *            P = Post-Strassenverzeichnis
      *            E = erweitertes Strassenverzeichnis
      *            N = kein Strassenverzeichnis
               10  DRKI1653-Orts-NR               PIC S9(09) COMP-3.
               10  DRKI1653-STR-V-NR              PIC S9(09) COMP-3.
               10  DRKI1653-STR-NR                PIC S9(07) COMP-3.
               10  DRKI1653-Orts-Suchschritt      PIC S9(03) COMP-3.
               10  DRKI1653-STR-Suchschritt       PIC S9(03) COMP-3.
               10  DRKI1653-PLZ-TYP               PIC X(04).
               10  DRKI1653-Suchergebnis          PIC X(01).
               10  FILLER                         PIC X(02).
               10  FILLER                         PIC X(02).
               10  DRKI1653-O-W-KZ                PIC X(01).
               10  DRKI1653-STR-KL                PIC X(01).
               10  DRKI1653-RC-STA                PIC X(02).
               10  DRKI1653-STA-BER               PIC X(01).
               10  DRKI1653-STA-ANZ               PIC X(02).
               10  DRKI1653-STA-AEHN              PIC X(01).
               10  DRKI1653-TEILORTPRUEFUNG       PIC X(01).
      *             0  = Teilort nicht angeben
      *             1  = Teilort kann angegeben werden, ist jedoch
      *                 nicht erforderlich
      *             2  = Teilort erforderlich
               10  Filler                         PIC X(01).
               10  DRKI1653-STR-ART               PIC X(01).
               10  DRKI1653-Orts-Umschl           PIC X(01).
      *            ' ' = keine Umschlüsselung
      *             1  = Umschlüsselung Gebietsreform
      *             2  = Ortsteil in gültigen Ortsnamen umgeschlüsselt
      *-----------------------------------------------------------
      * Daten aus DRKI0170
           05  DRKI1653-ADRESSEPLZ-ALT            PIC X(09).
           05  DRKI1653-ADRESSEORT-ALT            PIC X(40).
           05  DRKI1653-ADRESSESTR-ALT            PIC X(40).




      *----------- Ende DRKI1653 ---------------------------
      *

