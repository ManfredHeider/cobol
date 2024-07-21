      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0015.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0015.CPv  $
      *
      *   Rev 2.2   Dec 10 2004 14:36:20   t13ex57
      *AAS00500: Akt. Handynummer und Adresse im VB-Portal
      *
      *   Rev 2.1   Dec 08 2004 15:53:20   t13ex57
      *AAS00500: Akt. Handynummer und Adresse im VB-Portal
      *
      *   Rev 2.0   Nov 16 2004 16:27:56   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AAS00500!16.11.04!    ! Neu                       ! U. Boll   *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Eingabedatei der postalisch geprüften Adressen für die
      * Übernahme in Tabelle MAAN
      *
      * Satzlänge 297
      *
      *
       01 DRAS0015.
           05  DRAS0015-AdresseID                 PIC X(26).
           05  DRAS0015-ADRESSELANDID             PIC X(03).
           05  FILLER                             PIC X(01).
           05  DRAS0015-ADRESSEPLZ                PIC X(09).
           05  FILLER                             PIC X(01).
           05  DRAS0015-ADRESSEORT                PIC X(40).
           05  FILLER                             PIC X(01).
           05  DRAS0015-ADRESSESTRHAUSNUM         PIC X(40).
           05  DRAS0015-ADRESSREST.
               10  DRAS0015-ZUSTELL               PIC X(01).
               10  DRAS0015-TELEFON               PIC X(14).
               10  DRAS0015-TELEFAX               PIC X(14).
               10  DRAS0015-ADRESSEORT-VB         PIC X(40).
               10  DRAS0015-ADRESSESTRHAUSNUM-VB  PIC X(40).
               10  FILLER                         PIC X(02).
      *    05  FILLER                             PIC X(82).
           05  DRAS0015-TX1                       PIC X(65).
           05  FILLER REDEFINES DRAS0015-TX1.
               10  DRAS0015-EKZ                   PIC X(01).
               10  DRAS0015-Ortssuche             PIC X(01).
               10  DRAS0015-Strassensuche         PIC X(01).
               10  DRAS0015-PLZ-Aenderung         PIC X(01).
               10  DRAS0015-Orts-Aenderung        PIC X(01).
               10  DRAS0015-Strassen-Aenderung    PIC X(01).
               10  DRAS0015-Strassen-Kuerzung     PIC X(01).
               10  DRAS0015-Strassen-Umschl       PIC X(01).
               10  DRAS0015-Alias                 PIC X(01).
               10  DRAS0015-RC                    PIC X(02).
               10  DRAS0015-Postfachpruefung      PIC X(01).
               10  DRAS0015-Postfach-PLZ          PIC X(05).
               10  DRAS0015-Gemeindekennziffer    PIC X(08).
               10  FILLER REDEFINES DRAS0015-Gemeindekennziffer.
                   15  DRAS0015-Bundesland        PIC X(02).
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
                   15  DRAS0015-Regierungsbezirk  PIC X(02).
                   15  DRAS0015-Kreis             PIC X(02).
                   15  DRAS0015-Gemeinde          PIC X(02).
               10  DRAS0015-Strassenverzeichnis   PIC X(01).
      *            P = Post-Strassenverzeichnis
      *            E = erweitertes Strassenverzeichnis
      *            N = kein Strassenverzeichnis
               10  DRAS0015-Orts-NR               PIC S9(09) COMP-3.
               10  DRAS0015-STR-V-NR              PIC S9(09) COMP-3.
               10  DRAS0015-STR-NR                PIC S9(07) COMP-3.
               10  DRAS0015-Orts-Suchschritt      PIC S9(03) COMP-3.
               10  DRAS0015-STR-Suchschritt       PIC S9(03) COMP-3.
               10  DRAS0015-PLZ-TYP               PIC X(04).
               10  DRAS0015-Suchergebnis          PIC X(01).
               10  FILLER                         PIC X(02).
               10  FILLER                         PIC X(02).
               10  DRAS0015-O-W-KZ                PIC X(01).
               10  DRAS0015-STR-KL                PIC X(01).
               10  DRAS0015-RC-STA                PIC X(02).
               10  DRAS0015-STA-BER               PIC X(01).
               10  DRAS0015-STA-ANZ               PIC X(02).
               10  DRAS0015-STA-AEHN              PIC X(01).
               10  DRAS0015-TEILORTPRUEFUNG       PIC X(01).
      *             0  = Teilort nicht angeben
      *             1  = Teilort kann angegeben werden, ist jedoch
      *                 nicht erforderlich
      *             2  = Teilort erforderlich
               10  Filler                         PIC X(01).
               10  DRAS0015-STR-ART               PIC X(01).
               10  DRAS0015-Orts-Umschl           PIC X(01).
      *            ' ' = keine Umschlüsselung
      *             1  = Umschlüsselung Gebietsreform
      *             2  = Ortsteil in gültigen Ortsnamen umgeschlüsselt
      *----------- Ende DRAS0015 ---------------------------
      *
