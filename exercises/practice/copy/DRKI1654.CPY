      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.03.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1654.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1654.CPv  $
      *
      *   Rev 2.1   Mar 07 2005 09:22:20   t13ex49
      *AKI00268: Postalische Prüfung - Ortsteil - Teil 2
      *
      *   Rev 2.0   Mar 15 2004 16:34:40   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1654 POSTPRUEF-Felder definieren         *
      *    -------------------------------------------------------   *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00268!15.03.04! 01 ! NEUERSTELLUNG             ! P. Pohl  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1654                                 *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1654.
           05  PI-DRKI1654-DATEN.

             07  PI-DRKI1654-TX1                       PIC X(65).
             07  FILLER REDEFINES PI-DRKI1654-TX1.
                 10  PI-DRKI1654-EKZ                   PIC X(01).
                 10  PI-DRKI1654-Ortssuche             PIC X(01).
                 10  PI-DRKI1654-Strassensuche         PIC X(01).
                 10  PI-DRKI1654-PLZ-Aenderung         PIC X(01).
                 10  PI-DRKI1654-Orts-Aenderung        PIC X(01).
                 10  PI-DRKI1654-Strassen-Aenderung    PIC X(01).
                 10  PI-DRKI1654-Strassen-Kuerzung     PIC X(01).
                 10  PI-DRKI1654-Strassen-Umschl       PIC X(01).
                 10  PI-DRKI1654-Alias                 PIC X(01).
                 10  PI-DRKI1654-RC                    PIC X(02).
                 10  PI-DRKI1654-Postfachpruefung      PIC X(01).
                 10  PI-DRKI1654-Postfach-PLZ          PIC X(05).
                 10  PI-DRKI1654-Gemeindekennziffer    PIC X(08).
                 10  FILLER REDEFINES PI-DRKI1654-Gemeindekennziffer.
                     15  PI-DRKI1654-Bundesland        PIC X(02).
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
                     15  PI-DRKI1654-Regierungsbezirk  PIC X(02).
                     15  PI-DRKI1654-Kreis             PIC X(02).
                     15  PI-DRKI1654-Gemeinde          PIC X(02).
                 10  PI-DRKI1654-Strassenverz          PIC X(01).
      *              P = Post-Strassenverzeichnis
      *              E = erweitertes Strassenverzeichnis
      *              N = kein Strassenverzeichnis
                 10  PI-DRKI1654-Orts-NR             PIC S9(09) COMP-3.
                 10  PI-DRKI1654-STR-V-NR            PIC S9(09) COMP-3.
                 10  PI-DRKI1654-STR-NR              PIC S9(07) COMP-3.
                 10  PI-DRKI1654-Orts-Suchschritt    PIC S9(03) COMP-3.
                 10  PI-DRKI1654-STR-Suchschritt     PIC S9(03) COMP-3.
                 10  PI-DRKI1654-PLZ-TYP               PIC X(04).
                 10  PI-DRKI1654-Suchergebnis          PIC X(01).
                 10  FILLER                            PIC X(02).
                 10  FILLER                            PIC X(02).
                 10  PI-DRKI1654-O-W-KZ                PIC X(01).
                 10  PI-DRKI1654-STR-KL                PIC X(01).
                 10  PI-DRKI1654-RC-STA                PIC X(02).
                 10  PI-DRKI1654-STA-BER               PIC X(01).
                 10  PI-DRKI1654-STA-ANZ               PIC X(02).
                 10  PI-DRKI1654-STA-AEHN              PIC X(01).
                 10  PI-DRKI1654-TEILORTPRUEFUNG       PIC X(01).
      *               0  = Teilort nicht angeben
      *               1  = Teilort kann angegeben werden, ist jedoch
      *                   nicht erforderlich
      *               2  = Teilort erforderlich
                 10  Filler                            PIC X(01).
                 10  PI-DRKI1654-STR-ART               PIC X(01).
                 10  PI-DRKI1654-Orts-Umschl           PIC X(01).
      *              ' ' = keine Umschlüsselung
      *               1  = Umschlüsselung Gebietsreform
      *               2  = Ortsteil in gültigen Ortsnamen umgeschlüsselt
             07  PI-DRKI1654-DATUM                     PIC X(06).
      *
       01 PO-DRKI1654.
          05 PO-DRKI1654-DATEN.
             10 PO-DRKI1654-ADRESSEPOSTPRUEF     PIC X(25).
             10 PO-DRKI1654-ADRESSEPOSTPRUEFID   PIC S9(004) COMP.

          05 PO-DRKI1654-RC                      PIC  X(003).
             88 PO-DRKI1654-RC-OK                         VALUE 'OK '.
             88 PO-DRKI1654-RC-NOK                        VALUE 'NOK'.
      *
      ****** Ende Copy-Member ****************************************

