      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.07.2007
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2051.CPY  $
      *    $Revision:   2.28  $
      *
      *    $Log:   L:/mf/CPY/DRKI2051.CPv  $
      *
      *   Rev 2.28   Feb 05 2021 10:03:48   t13ae31
      *AKI01827: G10 SUH VSPKZ Programme
      *
      *   Rev 2.27   Jun 17 2020 09:58:14   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.26   Jun 19 2019 16:31:06   u130176
      *AKI01681:  VSP 2019 - Anpassung DTA
      *
      *   Rev 2.25   Jun 08 2018 12:30:40   t13ae31
      *AKI01567: G10 Umstellung auf KSDS in DB2 Pgm
      *
      *   Rev 2.24   Aug 10 2017 09:41:40   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. Copies
      *
      *   Rev 2.23   Mar 27 2017 09:28:14   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.22   Mar 01 2017 10:59:06   t13ae31
      *AKI01493: SUH neue Felder FLEXN Pgme
      *
      *   Rev 2.21   Sep 19 2016 07:30:20   t13ae31
      *AKI01458: SUH Namensvorwort bei VP Copies
      *
      *   Rev 2.20   Jun 23 2016 10:32:10   t13ae31
      *AKI01438: G10 SUH Angleichung DTA Pgme
      *
      *   Rev 2.19   May 05 2015 09:00:56   t13ae31
      *AKI01358: SUH Ermittlungsprogramm überarbeiten
      *
      *   Rev 2.18   Mar 19 2015 08:41:48   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.17   Mar 19 2015 08:03:18   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.16   Feb 23 2012 11:12:06   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.15   Feb 03 2012 15:01:04   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.14   Aug 05 2011 11:13:22   t13ae31
      *AKI01037: SUH - FK-And. 613 Erm+Aufb+Akt
      *
      *   Rev 2.13   Mar 07 2011 13:28:22   t13ae31
      *AKI01012: VSP Vorteilsargumentation
      *
      *   Rev 2.12   Jun 15 2010 09:49:50   t13ae31
      *AKI00974: VSP 2010
      *
      *   Rev 2.11   Feb 18 2009 14:10:02   t13ae31
      *AKI00795: Diff.deck in FK Best.verträgen Ermittl.
      *
      *   Rev 2.10   Dec 12 2008 14:42:36   t13ex52
      *AKI00772: Reisegepäckversicherung
      *
      *   Rev 2.8   Dec 05 2008 09:32:58   t13ex52
      *FKL00012: Pruefen SA401 auf vorkommende SA5nn
      *
      *   Rev 2.7   Jun 21 2008 12:50:26   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.6   Apr 09 2008 16:21:34   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.5   Apr 02 2008 10:29:30   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.4   Mar 13 2008 14:28:28   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.3   Feb 12 2008 09:42:50   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.2   Jan 22 2008 12:16:54   t13ex14
      *AKL00665: Allfinanz; REOB-PK/FK Copystrecken
      *
      *   Rev 2.1   Dec 04 2007 16:45:36   t13ex52
      *AKL00664: Allfinanz: FK Bestandsdaten AM-Vers
      *
      *   Rev 2.0   Jul 19 2007 09:14:08   t13ex52
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - EINGABEDATEI REOB DTA AENDERUNGS- / KOMPLETTBE-          *
      *       STAND KORREKTUREN MIT VOR- UND NACHSATZ.                 *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *19.07.07! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
210108*21.01.08! Satzarten  f. Reob-PK eingefügt      ! WMI     !      *
      *        !                                      !         !      *
090408*09.04.08! Alle Satzarten auf 1.600 Byte        ! HSL     !      *
      *        ! gekürzt. Achtung !!! Dokumentation   !         !      *
      *        ! wurde nicht aktalisiert.             !         !      *
      *        !                                      !         !      *
210508*21.05.08! Erweiterung um je 1 Feld in der Enti-! HSL     !      *
      *        ! taet 701 und 702.                    !         !      *
      *        ! Entfernen nicht benoetigter Zaehler  !         !      *
      *        ! aus dem Nachsatz.                    !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
170908*17.09.08! Verarbeitung zur Deaktitivierung der ! HSL     !      *
      *        ! Entitaet 524[PK-Vertrag]             !         !      *
      *        !                                      !         !      *
091208*09.12.08! Integration neues AMV-Produkt Reise- ! HSL     !      *
      *        ! paeck-Versicherung Entitaet 504.     !         !      *
      *        ! Es wird hierfuer die nicht mehr be-  !         !      *
      *        ! noetigte Entitaet 524 in die Entitaet!         !      *
      *        ! 504 geaendert.                       !         !      *
      *        !                                      !         !      *
170209*17.02.09! Differenzdeckung FK eingebaut        !P. Hübner!      *
      *        !                                      !         !      *
100610*10.06.10! VSP2010 eingebaut                    !P. Hübner!      *
      *        !                                      !         !      *
220211*AKI01012|  Vorteilsargumentation eingefügt     |P. Hübner!      *
      *        !                                      !         !      *
030811*AKI01037! Änd. SA 613                          !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01102! neue SA 617                          !B. Stoldt!      *
      *        !                                      !         !      *
310115*AKI01295! neue SA 522 und 705 und Änd. 521     !P. Hübner!      *
      *        !                                      !         !      *
200415*AKI01358! Verkuerzung Feldnamen                !P. Hübner!      *
      *        ! AF-REOB-DTA in SUH-DTA               !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
120416*AKI01424! Erweiterung SA400 Bestandsmanagement !P. Hübner!      *
      *        !                                      !         !      *
180416*AKI01426! neue SA505 Reiseversicherung         !P. Hübner!      *
      *        !                                      !         !      *
120516*AKI01438! neue Felder SA400 SA401 Produktbez   !P. Hübner!      *
      *        !                                      !         !      *
260716*AKI01458! neue Felder SA614 Namensvorwort      !P. Hübner!      *
      *        !                                      !         !      *
080816*AKI01460! neue Felder SA400 SA510 SMART-HOME   !P. Hübner!      *
      *        !                                      !         !      *
010217*AKI01493! neue Felder SA400 FLEXN              !P. Hübner!      *
      *        !                                      !         !      *
      *FKI01544! FVD entfernt                         !P. Hübner!      *
      *        !                                      !         !      *
170717*AKI01530! SA614 Beruf auf 48 Stellen           !P. Hübner!      *
      *        !                                      !         !      *
200218*AKI01567! SL NACHSATZ auf 1600       m         !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01681! *-KUENDBEST wir nun alpahnumerisch   !B. Lorson!      *
      *        ! geliefert, Doku angepasst            !         !      *
      *        !                                      !         !      *
      *AKI01785! 2 neue Felder FLEXN SA401            !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01827! neues Feld SA400 PKBS-KBEZ für VSPKZ !P. Hübner!      *
      *--------------------------------------------------------------- *
      *                                                                *
      ******************************************************************
      *                                                                *
      *             V O R S A T Z - SATZART 000                        *
      *                                                                *
      *     LAENGE:     1.600 BYTES               STAND: 17.02.2009    *
      *     davon:                                                     *
      *     Key...:        63 BYTES                                    *
      *     Header:       132 BYTES                                    *
      *     Filler...:  1.365 BYTES                                    *
      *                                                                *
      ******************************************************************
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
       01 SUH-DTA-VORSATZ.
         05 SUH-DTA-V-KEY.
      ***  Vorsatzinformation: Vertragsnummer immer 0
           10 SUH-DTA-VKEY-VSNR                    PIC 9(15).
      ***  Vorsatzinformation: VEZ-Host-ID
           10 SUH-DTA-VKEY-CVEZHOSTNR              PIC X(15).
      ***  Vorsatzinformation: Nummer der Satzart
      ***  Beim Vorsatz immer 0
           10 SUH-DTA-VKEY-SATZART                 PIC 9(03).
      ***  Vorsatz ET-Key für die einzelnen Satzarten immer SPACE
           10 SUH-DTA-VHD-ETKEY                    PIC X(30).
 
         05 SUH-DTA-V-HEADER-FILLER                PIC x(132).
      ****
         05 SUH-DTA-VALLG-KEY.
      **** ABSENDER = 'AMV'
           10 SUH-DTA-VALLG-ABSENDER                 PIC X(03).
      ****
      *** ERFASSUNGS-/AENDERUNGSDATUM = 'JJJJMMTT'
      *** Erstelldatum des DTA, dies benötigen wir unsere maschinelle
      *** DTA Kontrolle.
           10 SUH-DTA-VALLG-DATUM                    PIC 9(08).
      ****
      **** EMPFAENGER = 'ALLFINANZ' / 'DVAG'
           10 SUH-DTA-VALLG-EMPFAENGER               PIC X(09).
      ****
      **** INHALTSANGABE = 'REOB-BESTAND FK-'
           10 SUH-DTA-VALLG-TEXT                     PIC X(16).
      ****
      **** INHALTSANGABE = 'INIT' FÜR KOMPLETTBESTAND
      ****                 'AEND' FÜR ÄNDERUNGSBESTAND
           10 SUH-DTA-VALLG-VERARBKNZ                PIC X(04).
      ****
           10 SUH-DTA-VALLG-LEER                     PIC X(1365).
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                   A L L F I N A N Z - R E O B                  *
      *                    D T A - D A T E N S A T Z                   *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 1.600 BYTES                 STAND: 17.02.2009      *
      *                                                                *
      **************************************************************** *
 
      **************************************************************** *
      *                                                                *
      *                   400 - V E R T R A G - 400                    *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      **************************************************************** *
 
      ************************ H E A D E R *****************************
       01 SUH-DTA-400-VERTRAG.
           05 SUH-DTA-400-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 4002
             10  SUH-DTA-400-CVNR                   PIC 9(15).
      ***  Schlüssel: VEZ-Host-ID FeldNr. 9999
             10 SUH-DTA-400-CVEZHOSTNR              PIC X(15).
      ***  Headerinformation: Nummer der Satzart
             10 SUH-DTA-400-SATZART                 PIC 999.
      ***  Headerinformation: Entitätenkey für die einzelnen Satzarten
             10 SUH-DTA-ETKEY-400.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4009
               15 SUH-DTA-400-NGESELLST             PIC 9(9).
               15 SUH-DTA-400-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
      * ****
      ********** Allgemeine Headerdaten
           05 SUH-DTA-ALLG-HEADER-400.
      ***  Headerinformation: Vermögensberater
             10 SUH-DTA-HD-VB-NUMMER                 PIC 9(7).
 
      ***  Headerinformation: Herkunftssystem
             10 SUH-DTA-HD-HERKUNFT                  PIC XXX.
 
      ***  Headerinformation: Aenderungskennzeichen
      ***  Inhalt: 00 = neuer Vertrag(immer der gesamte Vertrag)
      ***          01 = Vertragsänderung
      ***          99 = Vertragsstorno(immer der gesamte Vertrag)
             10 SUH-DTA-HD-AEND-KNZ                  PIC 99.
 
      ***  Headerinformation: DVAG-interne Vertragsnummer PIF oder PIN
             10 SUH-DTA-HD-PIN                       PIC 9(9).
 
      ***  Headerinformation: Prüfziffer zur PIN
             10 SUH-DTA-HD-PIN-PRUEF                 PIC 99.
 
      ***  Headerinformation: Quittungsnummer
             10 SUH-DTA-HD-QUITTUNGSNR               PIC 9(15).
 
      ***  Headerinformation: Erstellungsdatum (im Format JJJJMMTT)
             10 SUH-DTA-HD-ERSTELL-DATUM             PIC 9(8).
 
      ***  Headerinformation: Erstellungszeit (im Format HHMMSS)
             10 SUH-DTA-HD-ERSTELL-ZEIT              PIC 9(6).
 
      ***  Headerinformation: Leerstellen
             10  SUH-DTA-HD-LEERSTELLEN               PIC X(80).
 
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
           05 SUH-DTA-ALLG-DATEN-400.
 
      ***  Datenfeld: Antragsnummer FeldNr. 4003
             10  SUH-DTA-400-CANNR             PIC X(18).
 
      ***  Datenfeld: Merkmal Vermittler 1 zum Vertrag FeldNr. 4004
             10  SUH-DTA-400-CV1MERKM          PIC X(20).
 
      ***  Datenfeld: Vermittlernummer 1 zum Vertrag FeldNr. 4005
             10  SUH-DTA-400-CV1VTVMNR         PIC X(10).
 
      ***  Datenfeld: Merkmal Vermittler 2 zum Vertrag FeldNr. 4006
             10  SUH-DTA-400-CV2MERKM          PIC X(20).
 
      ***  Datenfeld: Vermittlernummer 2 zum Vertrag FeldNr. 4007
             10  SUH-DTA-400-CV2VTVMNR         PIC X(10).
 
      ***  Datenfeld: Tarif/Produkt FeldNr. 4008
             10  SUH-DTA-400-CPBRDNR           PIC X(50).
 
      ***  Datenfeld: Vertragssparte FeldNr. 4010
             10  SUH-DTA-400-NSPARTE           PIC 9(9).
 
      ***  Datenfeld: Vertragsart FeldNr. 4011
             10  SUH-DTA-400-NVTART            PIC 9(9).
 
      ***  Datenfeld: Vertragsbeginn Format: JJJJMMTT FeldNr. 4012
             10  SUH-DTA-400-CVBEGINN          PIC 9(8).
 
      ***  Datenfeld: Vertragsablauf Format: JJJJMMTT FeldNr. 4013
             10  SUH-DTA-400-CVABLAUF          PIC 9(8).
 
      ***  Datenfeld: Vertragsstatus FeldNr. 4014
             10  SUH-DTA-400-NVTSTATUS         PIC 9(9).
 
      ***  Datenfeld: Versichertes Risiko FeldNr. 4015
             10  SUH-DTA-400-CVERSRISK         PIC X(23).
 
      ***  Datenfeld: Änderungsdatum Format: JJJJMMTT FeldNr. 4016
             10  SUH-DTA-400-CVAEND1D          PIC 9(8).
 
      ***  Datenfeld: Änderungsgrund FeldNr. 4017
             10  SUH-DTA-400-CVAEND1G          PIC X(50).
 
      ***  Datenfeld: AFB-Anteil in Prozent FeldNr. 4018
             10  SUH-DTA-400-NAFBPROZ          PIC 99V999.
 
      ***  Datenfeld: Bündelungskennzeichen FeldNr. 4020
             10  SUH-DTA-400-CBUEKZ            PIC X.
 
      ***  Datenfeld: Vertragsinfo FeldNr. 4022
             10  SUH-DTA-400-CVTZINFO          PIC X(80).
 
      ***  Datenfeld: Vertragssumme FeldNr. 4027
             10  SUH-DTA-400-NVSUMME           PIC 9(10).
 
      ***  Datenfeld: Ursprungswährung FeldNr. 4030
             10  SUH-DTA-400-NVTURWAEHR        PIC 9(9).
 
      ***  Datenfeld: Inkassoart FeldNr. 4031
             10  SUH-DTA-400-CINKFORM          PIC X(30).
 
      ***  Datenfeld: Versicherungszahlungsweise FeldNr. 4032
             10  SUH-DTA-400-NINKZHLWSE        PIC 9(9).
 
      ***  Datenfeld: Beitrag gemäss Zahlungsweise FeldNr. 4033
             10  SUH-DTA-400-NINKVSGBTR        PIC 9(10)V99.
 
      ***  Datenfeld: Jahresgesamtbeitrag brutto gemäss
      ***             Zahlungsweise. FeldNr. 4034
             10  SUH-DTA-400-NINKJGSBTR        PIC 9(10)V99.
 
      ***  Datenfeld: Fälligkeitsdatum Format: JJJJMMTT FeldNr. 4035
             10  SUH-DTA-400-CINKFLGDAT        PIC 9(8).
 
      ***  Datenfeld: Jahresgesamtbeitrag netto FeldNr. 4036
             10  SUH-DTA-400-NINKJGNBTR        PIC 9(8)V99.
 
      ***  Datenfeld: Mahnstufe FeldNr. 4037
             10  SUH-DTA-400-CINKMHNSTF        PIC X(25).
 
      ***  Datenfeld: Mahndatum Format: JJJJMMTT FeldNr. 4038
             10  SUH-DTA-400-CINKMHNDAT        PIC 9(8).
 
      ***  Datenfeld: Rückstandsbetrag FeldNr. 4039
             10  SUH-DTA-400-NINKRSBTR         PIC 9(7)V99.
 
      ***  Datenfeld: Laufzeitrabatt in % FeldNr. 4040
             10  SUH-DTA-400-NINKLFZRB         PIC 99.
 
      ***  Datenfeld: Ratenzuschlag FeldNr. 4041
             10  SUH-DTA-400-NINKRTZS          PIC 99.
 
      ***  Datenfeld: Zeichnung FeldNr. 4042
             10  SUH-DTA-400-CINKZCHNNG        PIC X(25).
 
      ***  Datenfeld: F/B-Beitrag brutto FeldNr. 4043
             10  SUH-DTA-400-NINKFBBTR         PIC 9(8)V99.
 
      ***  Datenfeld: F/B-Summe FeldNr. 4044
             10  SUH-DTA-400-NINKFBSUM         PIC 9(12).
 
      ***  Datenfeld: Einlösungsbeitrag brutto FeldNr. 4045
             10  SUH-DTA-400-NINKELBTBR        PIC 9(8)V99.
 
      ***  Datenfeld: Einlösungsbeitrag netto FeldNr. 4046
             10  SUH-DTA-400-NINKELBTNT        PIC 9(8)V99.
 
      ***  Datenfeld: Letzte Synchronisation Format: JJJJMMTT
      ***             FeldNr. 4047
             10  SUH-DTA-400-CINKLSTSYN        PIC 9(8).
 
      ***  Datenfeld: Letzte Agenturbearbeitung Format: JJJJMMTT
      ***             FeldNr. 4048
             10  SUH-DTA-400-CINKLSTAGT        PIC 9(8).
 
      ***  Datenfeld: Letzte Bewegung FeldNr. 4049
             10  SUH-DTA-400-CINKLSTBWG        PIC X(61).
 
      ***  Datenfeld: Kundenbonus FeldNr. 4050
             10  SUH-DTA-400-CINKKDBON         PIC X(26).
 
      ***  Datenfeld: Ostrabatt in % FeldNr. 4051
             10  SUH-DTA-400-NINKOSTRBT        PIC 99.
 
      ***  Datenfeld: Kurztext Bestandssicherungsaktion FeldNr. 4094
             10  SUH-DTA-400-CINKBSA           PIC X(6).
 
      ***  Datenfeld: Bankleitzahl FeldNr. 4100
             10  SUH-DTA-400-NINKBLZ           PIC 9(8).
 
      ***  Datenfeld: Kontonummer FeldNr. 4101
             10  SUH-DTA-400-CINKKTONR         PIC X(15).
 
      ***  Datenfeld: Kontoinhaber FeldNr. 4102
             10  SUH-DTA-400-CINKKTOINH        PIC X(40).
 
      ***  Datenfeld: Langtext Bestandssicherungsaktion FeldNr. 4110
             10  SUH-DTA-400-CVGBSALTXT        PIC X(50).
 
      ***  Datenfeld: Versicherungsgesellschaft Text FeldNr. 4111
             10  SUH-DTA-400-CGESELLST         PIC X(50).
 
      ***  Datenfeld: VD-Nr. FeldNr. 4112
             10  SUH-DTA-400-NVDNR             PIC 9(5).
 
      ***  Datenfeld: VD-Name FeldNr. 4113
             10  SUH-DTA-400-CVDNAME           PIC X(20).
 
      ***  Datenfeld: Vor-Vertragsnummer FeldNr. 4114
             10  SUH-DTA-400-CVORVNR           PIC X(17).
 
120416***  Datenfeld: alte Vertragsnummer
             10 SUH-DTA-400-ALTVNR             PIC X(17).
 
      ***  Datenfeld: Aktionsergebnis
      *** BMP = Internet / Protal    BMK = Post / Antwortkarte
      *** leer = Keine Antwort bzw. Teilnahme
120416       10 SUH-DTA-400-AKTIONERG          PIC X(3).
      ***  Datenfeld: Produktkombibezeichnung
120516       10 SUH-DTA-400-4150-PRDKOMBIBEZ   PIC X(50).
      ***  Datenfeld: Smart-Home KundenID Feld Nr. 4151
080816       10 SUH-DTA-400-4151-KUNDENIDSMH   PIC X(10).
 
      ***  Datenfeld: FLEX-VSP-Nachlass
010217       10 SUH-DTA-400-4156-FLEXNVSP      PIC 9(9)V9(2).
 
      ***  Datenfeld: FLEX-VSP-Nachlass gewährt
010217       10 SUH-DTA-400-4157-FLEXNVSPGW    PIC X.
 
      ***  Datenfeld: FLEX-VGV-Nachlass
010217       10 SUH-DTA-400-4158-FLEXNVGV      PIC 9(9)V9(2).
 
      ***  Datenfeld: FLEX-VGV-Nachlass gewährt
010217       10 SUH-DTA-400-4159-FLEXNVGVGW    PIC X.
 
      *** Datenfeld: PKBS-KBEZ (VSP = J / N)
             10 SUH-DTA-400-4152-PKBS-KBEZ      PIC X(3).
 
      ***  Datenfeld: Leerstellen
090408***    10  SUH-DTA-400-LEERSTELLEN       PIC X(538).
120416***    10  SUH-DTA-400-LEERSTELLEN       PIC X(518).
120516***    10  SUH-DTA-400-LEERSTELLEN       PIC X(468).
080816***    10  SUH-DTA-400-LEERSTELLEN       PIC X(458).
010217***    10  SUH-DTA-400-LEERSTELLEN       PIC X(434).
             10  SUH-DTA-400-LEERSTELLEN       PIC X(431).
 
      ******************************************************************
      *                                                                *
      *             401 - E I N Z E L V E R T R A G - 401              *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01  SUH-DTA-401-EINZELVERTRAG.
           05 SUH-DTA-401-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 4067
             10 SUH-DTA-401-CVNR                 PIC 9(15).
      ***  Schlüssel: VEZ-Host-ID FeldNr. 4064
             10 SUH-DTA-401-CVEZHOSTNR           PIC X(15).
      ***  Headerinformation: Nummer der Satzart
             10 SUH-DTA-401-SATZART              PIC 999.
      ***  Headerinformation: Entitätenkey für die einzelnen Satzarten
             10 SUH-DTA-HD-401-ETKEY.
      * **** Beginn der Redefinition fuer die Entitaeten-Schlüssel
      * **** je Satzart wird ein Entitäten-Schlüssel gebraucht.
 
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4066
               15 SUH-DTA-401-NGESELLST         PIC 9(9).
      ***  Schlüssel: VEZ-Sparte FeldNr. 4057
               15 SUH-DTA-401-NVEZSPARTE        PIC 9(9).
      ***  Schlüssel: VEZ-Tarif/Produkt FeldNr. 4058
               15 SUH-DTA-401-NVEZPBRDNR        PIC 9(9).
               15 SUH-DTA-401-LEER              PIC X(3).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      * ****
      ********** Allgemeine Headerdaten
           05 SUH-DTA-HD-ALLG-401-LEER          PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
           05 SUH-DTA-ALLG-DATEN-401.
      ***  Datenfeld: VEZ-Beginn FeldNr. 4059 Format: JJJJMMTT
             10  SUH-DTA-401-CVEZBEGINN        PIC 9(8).
 
      ***  Datenfeld: VEZ-Ablauf FeldNr. 4060 Format: JJJJMMTT
             10  SUH-DTA-401-CVEZABLAUF        PIC 9(8).
 
      ***  Datenfeld: VEZ-Status FeldNr. 4061
      ***             Dieses Feld hat vom Inhalt her die selben Werte
      ***             wie das Datenfeld: Vertragsstatus FeldNr. 4014
      ***             "SUH-DTA-400-NVTSTATUS" in der Satzart 400.
      ***             Jedoch können keine Querverbindungen zum Inhalt
      ***             des Feldes in der SA400 geprüft werden.
             10  SUH-DTA-401-NVEZSTATUS        PIC 9(9).
 
      ***  Datenfeld: VEZ-Bruttobeitrag gem. ZW FeldNr. 4062
             10  SUH-DTA-401-NVEZBTRG          PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Summe FeldNr. 4063
             10  SUH-DTA-401-NVEZSUMME         PIC 9(10).
 
      ***  Datenfeld: Vertragsart FeldNr. 4065
             10  SUH-DTA-401-NVTART            PIC 9(9).
 
      ***  Datenfeld: VEZ-Aenderungsgrund FeldNr. 4095
             10  SUH-DTA-401-CVEZAENDG         PIC X(50).
 
      ***  Datenfeld: VEZ-Aenderungsdatum Format: JJJJMMTT FeldNr. 4096
             10  SUH-DTA-401-CVEZAENDDT        PIC 9(8).
 
      ***  Datenfeld: VEZ-Nettojahresbeitrag FeldNr. 4106
             10  SUH-DTA-401-NVEZNJB           PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Bruttojahresbeitrag FeldNr. 4107
             10  SUH-DTA-401-NVEZBJB           PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Nettobeitrag gem. ZW FeldNr. 4108
             10  SUH-DTA-401-NVEZFBNZW         PIC 9(8)V99.
 
      ***  Datenfeld: VEZ-Datum der letzten Bearbeitung Format: JJJJMMTT
      ***             FeldNr. 4109
             10  SUH-DTA-401-CVEZLBEARB        PIC 9(8).
 
100610***  Datenfeld: Geschäftszweig FeldNr. 4115 ( FK FH PK )
             10  SUH-DTA-401-CGZWEIG           PIC XX.
 
      ***  Datenfeld: Leerstellen
090408****** 10  SUH-DTA-401-LEERSTELLEN         PIC X(1253).
100610*** Datenfeld: Rabattierter Jahresnettobeitr. Feldnr. 9301
      ***    inkl. TZ-Zuschlag exkl. Steuer
             10  SUH-DTA-401-RABNETTO          PIC 9(8)V99.
      ***  Datenfeld: Summe aller Komponentenbeiträge Feldnr. 9302
             10  SUH-DTA-401-STAKTBTR          PIC 9(8)V99.
220211***  Datenfeld: REOBPRODUKTNUMMER  Feldnr. 4164
220211       10  SUH-DTA-401-NREOBPRDNR        PIC 9(5).
 
      ***  Datenfeld: SUH-Produktartencode dient nur zur Info AMV
             10  SUH-DTA-401-100-PRDACD         PIC 9(05).
 
      ***  Datenfeld: SUH-Stornodatum EVT dient nur zur Info AMV
             10  SUH-DTA-401-134-STAVSDT        PIC X(10).
 
      ***  Datenfeld: SUH-Ruhedatum ab EVT dient nur zur Info AMV
             10  SUH-DTA-401-135-STAVRADT       PIC X(10).
 
      ***  Datenfeld: SUH-Ruhedatum bis EVT dient nur zur Info AMV
             10  SUH-DTA-401-136-STAVRBDT       PIC X(10).
 
120516***  Datenfeld: Produktbezeichnung
             10  SUH-DTA-401-PRODUKTBEZ         PIC X(50).
 
      ***  Datenfeld: FLEXN Betrag Feldnr. 4160
             10  SUH-DTA-401-4160-FLEXNSP            PIC 9(9)V99.
             10  SUH-DTA-401-4161-FLEXNSPGW          PIC X.
      *** V = FlexN gewährt G = FlexN geplatzt
      *** E = FlexN erhöht  S = FlexN storniert mit Guthaben
 
      ***  Datenfeld: Leerstellen
100610***    10  SUH-DTA-401-LEERSTELLEN         PIC X(1233).
220211***    10  SUH-DTA-401-LEERSTELLEN         PIC X(1228).
120516***    10  SUH-DTA-401-LEERSTELLEN         PIC X(1143).
             10  SUH-DTA-401-LEESTELLEN          PIC X(1131).
 
 
      ******************************************************************
      *                                                                *
      *     403 - V E R T R A G S R I S I K O A D R E S S E - 403      *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01  SUH-DTA-403-RISIKOADRESSE.
         05 SUH-DTA-403-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6365
           10 SUH-DTA-403-CVNR                PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6359
      ***             immer mit Blank gefüllt. (Nur Satzart 403)
           10 SUH-DTA-403-CVEZHOSTNR          PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-403-SATZART                PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-HD-ETKEY-403.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6359
             15 SUH-DTA-HD-403-NGESELLST         PIC 9(9).
      ***  Schlüssel: Risikoadresse-HostID FeldNr. 6366
             15 SUH-DTA-HD-403-NRISADRHID        PIC 9(9).
             15 SUH-DTA-HD-403-LEER              PIC X(12).
 
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
      *************** Allgemeine Headerdaten
         05 SUH-DTA-HD-ALLG-403-LEER          PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-403.
      ***  Datenfeld: Strasse FeldNr. 6367
           10  SUH-DTA-403-CRISADRSTR        PIC X(40).
 
      ***  Datenfeld: PLZ FeldNr. 6368
           10  SUH-DTA-403-CRISADRPLZ        PIC X(6).
 
      ***  Datenfeld: Ort FeldNr. 6369
           10  SUH-DTA-403-CRISADRORT        PIC X(40).
 
      ***  Datenfeld: Länderkennzeichen FeldNr. 6370
           10  SUH-DTA-403-CRISADRLKZ        PIC 9(9).
 
      ***  Datenfeld: Leerstellen
090408     10  SUH-DTA-403-LEERSTELLEN       PIC X(1310).
      ***
091208******************************************************************
    >>*                                                                *
      *                504 - P K - R E I S E G E P A E C K -           *
      *                    V E R S I C H E R U N G                     *
      *     LAENGE:  505 BYTES genutzt           STAND: 09.12.2008     *
      *     LAENGE: 1600 BYTES gesamt                                  *
      ******************************************************************
         01 SUH-DTA-504-REISEGEPAECK.
           05 SUH-DTA-504-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 9189
             10 SUH-DTA-504-CVNR                     PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9191
             10 SUH-DTA-504-CVEZHOSTNR               PIC X(15).
      ***  Headerinformation: Nummer der Satzart
             10 SUH-DTA-504-SATZART                  PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
             10 SUH-DTA-ETKEY-504.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 9188
               15 SUH-DTA-504-NGESELLST              PIC 9(9).
               15 SUH-DTA-504-LEER                   PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
      ***
      *************** Allgemeine Headerdaten
           05 SUH-DTA-ALLG-504-LEER                  PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
           05 SUH-DTA-ALLG-DATEN-504.
      ***  Datenfeld: Bedingung FeldNr. 9177
             10 SUH-DTA-504-BEDINGUNG                PIC X(30).
 
      ***  Datenfeld: Versicherungssumme. FeldNr. 9178
             10 SUH-DTA-504-VERSSUMME                PIC 9(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 9179
             10 SUH-DTA-504-MAILING                  PIC X(70).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 9180
             10 SUH-DTA-504-ZUSATZINFO               PIC X(100).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9181
             10 SUH-DTA-504-DIFFDECK                 PIC X(2).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9182
             10 SUH-DTA-504-GESELLSCHAFT             PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9183
             10 SUH-DTA-504-DDVERSNR                 PIC X(40).
 
      ***  Datenfeld: Umstellungstermin   FeldNr. 9184
             10 SUH-DTA-504-UMSTELLTERMIN            PIC 9(8).
 
      ***  Datenfeld: Kündigungsbestätigung Fremdvertrag   FeldNr. 9185
      ***       J = Bestätigung liegt vor
      ***       N = Bestätigung liegt nicht vor
      ***       G = liegt vor (Kündigungsservice)
      ***       V = versandt  (Kündigungsservice)
      ***   (leer) = unbekannt
      ***  Die AMLeben verwendet etwas abweichende Bezeichnungen für
      ***       G = Versendung Kündigungsschreiben durch AM "Genehmigt"
      ***       V = Kündigungsschreiben durch AM "Verschickt"
             10 SUH-DTA-504-DDKUENDBEST              PIC X(2).
      ***
    <<***  Datenfeld: Leerstellen
091208       10 SUH-DTA-504-LEERSTELLEN              PIC X(1095).
 
      *****************************************************************
      *
      *              R E I S E V E R S I C H E R U N G
      *
      *     LÄNGE:   522 BYTES                  STAND: 18.01.2016
      *
      *****************************************************************
       01  SUH-DTA-505-REISEVERSICHERUNG.
         05 SUH-DTA-505-KEY.
    ***  Schlüssel: Vertragsnummer FeldNr. 9250
           10 SUH-DTA-505-CVNR                     PIC 9(15).
    ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9251
           10 SUH-DTA-505-CVEZHOSTNR               PIC X(15).
    ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-505-SATZART                  PIC 999.
    ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-505.
    ***  Schlüssel: Versicherungsgesellschaft FeldNr. 9252
             15 SUH-DTA-505-NGESELLST              PIC 9(9).
             15 SUH-DTA-505-LEER                   PIC X(21).
    ****** Ende der Redefinition fuer die Entitaeten-Schlüssel
    *******
    ***
    *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-505-LEER                  PIC X(132).
    *************** Ende *** H E A D E R *** Ende ********************
 
    ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-505.
    ***  Datenfeld: Bedingung FeldNr. 9260
           10 SUH-DTA-505-BEDINGUNG                PIC X(30).
 
    ***  Datenfeld: Versicherungssumme. Reisegepäck FeldNr. 9261
           10 SUH-DTA-505-VSUMRGEPCK               PIC 9(8).
 
    ***  Datenfeld: Versicherungssumme. Reiserücktritt FeldNr. 9262
           10 SUH-DTA-505-VSUMRRUECK               PIC 9(8).
 
    ***  Datenfeld: Mailingaktion FeldNr. 9663
           10 SUH-DTA-505-MAILING                  PIC X(70).
 
    ***  Datenfeld: Zusatzinfo FeldNr. 9664
           10 SUH-DTA-505-ZUSATZINFO               PIC X(100).
 
    ***  Datenfeld: Informationsquelle FeldNr. 9665
           10 SUH-DTA-505-LVTQUELLP                PIC 9(9).
           10 SUH-DTA-505-LVTQUELLE REDEFINES
              SUH-DTA-505-LVTQUELLP                PIC X(9).
 
    ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9670
           10 SUH-DTA-505-DIFFDECK                 PIC X(2).
 
    ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9671
           10 SUH-DTA-505-GESELLSCHAFT             PIC X(50).
 
    ***  Datenfeld: Versicherungsnummer FeldNr. 9672
           10 SUH-DTA-505-DDVERSNR                 PIC X(40).
 
    ***  Datenfeld: Umstellungstermin   FeldNr. 9673
           10 SUH-DTA-505-UMSTELLTERMIN            PIC 9(8).
 
    ***  Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 9674
    ***       ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-505-DDKUENDBEST              PIC X(2).
    ***
    ***  Datenfeld: Leerstellen
           10 SUH-DTA-505-LEERSTELLEN              PIC X(1078).
 
 
210108******************************************************************
>>    *                                                                *
      *              508 - G L A S - V E R T R A G - 508               *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01  SUH-DTA-508-GLAS.
         05 SUH-DTA-508-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5294
           10 SUH-DTA-508-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5292
           10 SUH-DTA-508-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-508-SATZART               PIC 999.
           10 SUH-DTA-ETKEY-508.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5293
             15 SUH-DTA-508-NGESELLST           PIC 9(9).
             15 SUH-DTA-508-LEER                PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-508-LEER          PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-508.
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5276
           10 SUH-DTA-508-CGLRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5277
           10 SUH-DTA-508-CGLRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5278
           10 SUH-DTA-508-CGLRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5279
           10 SUH-DTA-508-CGLRSHSNR         PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5280
           10 SUH-DTA-508-NGLRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5281
           10 SUH-DTA-508-CGLKLAUS          PIC X(80).
 
      ***  Datenfeld: Bedingung FeldNr. 5282
           10 SUH-DTA-508-CGLLBVBEDG        PIC X(16).
 
      ***  Datenfeld: Geb{udetyp FeldNr. 5283
           10 SUH-DTA-508-NGLLBGEBTY        PIC 9(9).
 
      ***  Datenfeld: Risiko FeldNr. 5285
           10 SUH-DTA-508-CGLLBRIS          PIC X(50).
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5286
           10 SUH-DTA-508-NGLJNMBTR         PIC XX.
 
      ***  Datenfeld: SB-Prz FeldNr. 5287
           10 SUH-DTA-508-NGLSBPRZ          PIC 99V99.
 
      ***  Datenfeld: SB-Fest FeldNr. 5288
           10 SUH-DTA-508-NGLSBFES          PIC 9(5).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5289
           10 SUH-DTA-508-CGLZUSINFO        PIC X(100).
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5290
           10 SUH-DTA-508-CGLDTLSTFV        PIC 9(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum JJJJMMTT FeldNr. 5291
           10 SUH-DTA-508-CGLDTLSTFB        PIC 9(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5940
           10 SUH-DTA-508-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5533
           10 SUH-DTA-508-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 5534 Format: JJJJMMTT
           10 SUH-DTA-508-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5536
           10 SUH-DTA-508-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5537
           10 SUH-DTA-508-CDIFFVSNR         PIC X(40).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 5538
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-508-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Leerstellen
090408     10 SUH-DTA-508-LEERSTELLEN         PIC X(861).
 
      *****************************************************************
      *                                                               *
      *       509 - H A F T P F L I C H T - V E R T R A G - 509       *
      *                                                               *
      *     LAENGE:                             STAND: 14.08.2007     *
      *                                                               *
      *****************************************************************
       01  SUH-DTA-509-HAFTPFLICHT.
         05 SUH-DTA-509-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5322
           10 SUH-DTA-509-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5320
           10 SUH-DTA-509-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-509-SATZART               PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-509.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5321
             15 SUH-DTA-509-NGESELLST             PIC 9(9).
             15 SUH-DTA-509-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-509-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-509.
      ***  Datenfeld: Bedingung FeldNr. 5297
           10  SUH-DTA-509-CHFVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5298
           10  SUH-DTA-509-CHFLBRIS          PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5299
           10  SUH-DTA-509-CHFKLAUS          PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5301
           10  SUH-DTA-509-CHFZUSINFO        PIC X(100).
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5302
           10  SUH-DTA-509-CHFRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5303
           10  SUH-DTA-509-CHFRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5304
           10  SUH-DTA-509-CHFRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5305
           10  SUH-DTA-509-NHFRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Mitversicherte Personen FeldNr. 5306
           10  SUH-DTA-509-CHFMVPERS         PIC X(35).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5307
           10  SUH-DTA-509-CHFRSHSNR         PIC X(15).
 
      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 5308
           10  SUH-DTA-509-NHFJNREGUL        PIC XX.
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5309
           10  SUH-DTA-509-NHFJNMBTR         PIC XX.
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5310
           10  SUH-DTA-509-CHFDTLSTFV        PIC 9(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5311
           10  SUH-DTA-509-CHFDTLSTFB        PIC 9(8).
 
      ***  Datenfeld: SB Ja ÄBool=01Ü/Nein ÄBool=00ÜÄBool=-1Ü/Unbekannt
      ***             FeldNr. 5312
           10  SUH-DTA-509-NHFJNSB           PIC XX.
 
      ***  Datenfeld: SB-Prz FeldNr. 5313
           10  SUH-DTA-509-NHFSBPRZ          PIC 999.
 
      ***  Datenfeld: SB-Fest FeldNr. 5314
           10  SUH-DTA-509-NHFSBFES          PIC 9(5).
 
      ***  Datenfeld: SB-Text FeldNr. 5315
           10  SUH-DTA-509-CHFSBTXT          PIC X(50).
 
      ***  Datenfeld: Auslandsch{den Ja/Nein FeldNr. 5316
           10  SUH-DTA-509-NHFJNAUSL         PIC XX.
 
      ***  Datenfeld: Verwandschaftsschaeden Ja/Nein FeldNr. 5317
           10  SUH-DTA-509-NHFJNVERW         PIC XX.
 
      ***  Datenfeld: Produkt FeldNr. 5318
           10  SUH-DTA-509-CHFPRD            PIC X(30).
 
      ***  Datenfeld: Namen Elternteil FeldNr. 5319
           10  SUH-DTA-509-CHNAMENEK         PIC X(45).
 
      ***  Datenfeld: Namen vollj{hrige Kinder FeldNr. 5859
           10  SUH-DTA-509-CHNAMEKIND        PIC X(45).
 
      ***  Datenfeld: abweichende VP FeldNr. 5860
           10  SUH-DTA-509-CHNAMEABVP        PIC X(35).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5941
           10  SUH-DTA-509-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5527
           10  SUH-DTA-509-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5528
           10  SUH-DTA-509-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5530
           10  SUH-DTA-509-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5531
           10  SUH-DTA-509-CDIFFVSNR         PIC X(40).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 5532
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10  SUH-DTA-509-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5987
           10  SUH-DTA-509-CZIELGRP          PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408     10  SUH-DTA-509-LEERSTELLEN       PIC X(593).
 
      *****************************************************************
      *                                                               *
      *           510 - H A U S R A T - V E R T R A G - 510           *
      *                                                               *
      *     LAENGE:                             STAND: 14.08.2007     *
      *                                                               *
      *****************************************************************
       01  SUH-DTA-510-HAUSRAT.
         05 SUH-DTA-510-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5362
           10 SUH-DTA-510-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5360
           10 SUH-DTA-510-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-510-SATZART               PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-510.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5361
             15 SUH-DTA-510-NGESELLST            PIC 9(9).
             15 SUH-DTA-510-LEER                 PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-510-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-510.
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5325
           10 SUH-DTA-510-CHRRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5326
           10 SUH-DTA-510-CHRRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5327
           10 SUH-DTA-510-CHRRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5328
           10 SUH-DTA-510-NHRRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Bedingung FeldNr. 5329
           10 SUH-DTA-510-CHRLBVBEDG        PIC X(16).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5330
           10 SUH-DTA-510-CHRRSHSNR         PIC X(15).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5331
           10 SUH-DTA-510-CHRLBKLAUS        PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5333
           10 SUH-DTA-510-CHRZUSINFO        PIC X(100).
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5334
           10 SUH-DTA-510-CHRDTLSTFV        PIC 9(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5335
           10 SUH-DTA-510-CHRDTLSTFB        PIC 9(8).
 
      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5336
           10 SUH-DTA-510-NHRJNSB           PIC XX.
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5337
           10 SUH-DTA-510-NHRSBPRZ          PIC 999.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5338
           10 SUH-DTA-510-NHRSBFES          PIC 9(5).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5339
           10 SUH-DTA-510-CHRSBTXT          PIC X(50).
 
      ***  Datenfeld: Nutzung FeldNr. 5340
           10 SUH-DTA-510-NHRLBNUTZ         PIC 9(9).
 
      ***  Datenfeld: Versicherungssumme FeldNr. 5341
           10 SUH-DTA-510-NHRVSUM           PIC 9(8).
 
      ***  Datenfeld: Wohnfl{che in qm FeldNr. 5342
           10 SUH-DTA-510-NHRWOHNFL         PIC 9(5).
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5343
           10 SUH-DTA-510-NHRJNUVZ          PIC XX.
 
      ***  Datenfeld: Geb{udetyp FeldNr. 5344
           10 SUH-DTA-510-NHRLBGEBTY        PIC 9(9).
 
      ***  Datenfeld: Eigentumsverh{ltnisse FeldNr. 5345
           10 SUH-DTA-510-NHRLBEIGEN        PIC 9(9).
 
      ***  Datenfeld: Tarifzone FeldNr. 5346
           10 SUH-DTA-510-CHRZONE           PIC X(4).
 
      ***  Datenfeld: Bauartklasse FeldNr. 5347
           10 SUH-DTA-510-NHRLBBRTKL        PIC 9(9).
 
      ***  Datenfeld: Alarmanlage Ja/Nein FeldNr. 5348
           10 SUH-DTA-510-NHRJNALARM        PIC XX.
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5349
           10 SUH-DTA-510-NHRJNFE           PIC XX.
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5350
           10 SUH-DTA-510-NHRJNST           PIC XX.
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5351
           10 SUH-DTA-510-NHRJNHA           PIC XX.
 
      ***  Datenfeld: Einbruch/Diebstahl Ja/Nein FeldNr. 5352
           10 SUH-DTA-510-NHRJNED           PIC XX.
 
      ***  Datenfeld: Glas Ja/Nein FeldNr. 5353
           10 SUH-DTA-510-NHRJNGL           PIC XX.
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5354
           10 SUH-DTA-510-NHRJNLW           PIC XX.
 
      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5355
           10 SUH-DTA-510-NHRJNEL           PIC XX.
 
      ***  Datenfeld: Bezugspr{mie FeldNr. 5356
           10 SUH-DTA-510-NHRBZGPRM         PIC 999V999.
 
      ***  Datenfeld: Leistungstext FeldNr. 5357
           10 SUH-DTA-510-CHRTXLEIST        PIC X(40).
 
      ***  Datenfeld: Summenanpassungsklausel FeldNr. 5358
           10 SUH-DTA-510-CHRTXSUAK         PIC X(32).
 
      ***  Datenfeld: Produkt FeldNr. 5359
           10 SUH-DTA-510-CHRPRD            PIC X(30).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5942
           10 SUH-DTA-510-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5521
           10 SUH-DTA-510-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5522
           10 SUH-DTA-510-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5524
           10 SUH-DTA-510-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5525
           10 SUH-DTA-510-CDIFFVSNR         PIC X(40).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 5526
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-510-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5986
           10 SUH-DTA-510-CZIELGRP          PIC X(30).
 
      ***  Datenfeld: Smart-Home Paket FeldNr. 5730
080816     10 SUH-DTA-510-SMHPAKET          PIC X(02).
 
      ***  Datenfeld: Smart-Home Datum FeldNr. 5731
080816     10 SUH-DTA-510-SMHDATUM.
080816        15 SUH-DTA-510-SMHTT          PIC X(2).
080816        15 SUH-DTA-510-SMHP1          PIC X.
080816        15 SUH-DTA-510-SMHMM          PIC X(2).
080816        15 SUH-DTA-510-SMHP2          PIC X.
080816        15 SUH-DTA-510-SMHJJJJ        PIC X(4).
 
      ***  Datenfeld: Smart-Home TrackID FeldNr. 5732
080816     10 SUH-DTA-510-SMHTRACKID        PIC X(60).
 
      ***  Datenfeld: Smart-Home Controlz FeldNr. 5733
080816     10 SUH-DTA-510-SMHCONTROLZ       PIC X(60).
 
      ***  Datenfeld: Leerstellen
090408***  10 SUH-DTA-510-LEERSTELLEN       PIC X(662).
080816     10 SUH-DTA-510-LEERSTELLEN       PIC X(530).
 
      ******************************************************************
      *                                                                *
      *            520 - U N F A L L - V E R T R A G - 520             *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-520-UNFALL.
         05 SUH-DTA-520-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5655
           10 SUH-DTA-520-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5653
           10 SUH-DTA-520-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-520-SATZART                   PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-520.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5654
             15 SUH-DTA-520-NGESELLST             PIC 9(9).
             15 SUH-DTA-520-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-520-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-520.
      ***  Datenfeld: Bedingung FeldNr. 5639
           10 SUH-DTA-520-CUFVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5640
           10 SUH-DTA-520-CUFLBRIS          PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5641
           10 SUH-DTA-520-CUFKLAUS          PIC X(80).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5643
           10 SUH-DTA-520-CUFZUSINFO        PIC X(100).
 
      ***  Datenfeld: Anzahl versicherte Personen FeldNr. 5644
           10 SUH-DTA-520-NUFVPANZ          PIC 9(5).
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5645
           10 SUH-DTA-520-CUFDTLSTFV        PIC 9(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5646
           10 SUH-DTA-520-CUFDTLSTFB        PIC 9(8).
 
      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5647
           10 SUH-DTA-520-NUFJNSB           PIC XX.
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5648
           10 SUH-DTA-520-NUFSBPRZ          PIC 999.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5649
           10 SUH-DTA-520-NUFSBFES          PIC 9(5).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5650
           10 SUH-DTA-520-CUFSBTXT          PIC X(50).
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5651
           10 SUH-DTA-520-NUFJNMBTR         PIC XX.
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5652
           10 SUH-DTA-520-CUFDTUMST         PIC 9(8).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5919
           10 SUH-DTA-520-CMAILING          PIC X(70).
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5547
           10 SUH-DTA-520-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5549
           10 SUH-DTA-520-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5550
           10 SUH-DTA-520-CDIFFVSNR         PIC X(40).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 5551
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-520-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Leerstelle
090408     10 SUH-DTA-520-LEERSTELLEN       PIC X(898).
 
      ******************************************************************
      *                                                                *
      *               521 - V G V - V E R T R A G - 521                *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-521-VGVVTRG.
         05 SUH-DTA-521-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5705
           10 SUH-DTA-521-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5703
           10 SUH-DTA-521-CVEZHOSTNR            PIC X(15).
      ***  Schlüssel: Nummer der Satzart
           10 SUH-DTA-521-SATZART               PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10  SUH-DTA-ETKEY-521.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5704
             15 SUH-DTA-521-NGESELLST             PIC 9(9).
             15 SUH-DTA-521-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-521-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-521.
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5658
           10 SUH-DTA-521-CVGRSPLZ          PIC X(6).
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5659
           10 SUH-DTA-521-CVGRSSTR          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5660
           10 SUH-DTA-521-CVGRSHSNR         PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5661
           10 SUH-DTA-521-CVGRSORT          PIC X(30).
 
      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5662
           10 SUH-DTA-521-NVGRSNKZ          PIC 9(9).
 
      ***  Datenfeld: Bedingung FeldNr. 5663
           10 SUH-DTA-521-CVGVBEDG          PIC X(16).
 
      ***  Datenfeld: Risiko FeldNr. 5664
           10 SUH-DTA-521-CVGRISIKO         PIC X(50).
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5665
           10 SUH-DTA-521-CVGKLAUS          PIC X(80).
 
      ***  Datenfeld: Beilagen FeldNr. 5666
           10 SUH-DTA-521-CVGBEILGN         PIC X(19).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5668
           10 SUH-DTA-521-CVGZUSINFO        PIC X(100).
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5669
           10 SUH-DTA-521-NVGJNFEUER        PIC XX.
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5670
           10 SUH-DTA-521-NVGJNLW           PIC XX.
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5671
           10 SUH-DTA-521-NVGJNST           PIC XX.
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5672
           10 SUH-DTA-521-NVGJNHA           PIC XX.
 
      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5673
           10 SUH-DTA-521-NVGJNSE           PIC XX.
 
      ***  Datenfeld: Geb{udenutzung FeldNr. 5675
           10 SUH-DTA-521-CVGGBNTZ          PIC X(26).
 
      ***  Datenfeld: gewerbliche Fl{che in Prozent FeldNr. 5677
           10 SUH-DTA-521-NVGGEWFL          PIC 999.
 
      ***  Datenfeld: Baujahr Format: JJJJMMTT FeldNr. 5678
           10 SUH-DTA-521-CVGBAUJ           PIC 9(8).
 
      ***  Datenfeld: Bauartklasse LB FeldNr. 5679
           10 SUH-DTA-521-NVGLBBAUKL        PIC 9(9).
 
      ***  Datenfeld: Rohbau bis Datum Format: JJJJMMTT FeldNr. 5680
           10 SUH-DTA-521-CVGDTRBB          PIC 9(8).
 
      ***  Datenfeld: Leistungsart LB FeldNr. 5681
           10 SUH-DTA-521-NVGLBLA           PIC 9(9).
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5934
           10 SUH-DTA-521-NVGJNUVZ          PIC XX.
 
      ***  Datenfeld: Grund f}r UVZ FeldNr. 5683
           10 SUH-DTA-521-CVGUVZGRD         PIC X(40).
 
      ***  Datenfeld: Versicherungsumme 1914 FeldNr. 5684
           10 SUH-DTA-521-NVGSUM1914        PIC 9(9).
 
      ***  Datenfeld: Staffelpr{mie Ja/Nein FeldNr. 5685
           10 SUH-DTA-521-NVGJNSTFPR        PIC XX.
 
      ***  Datenfeld: Zone Leitungswasser FeldNr. 5688
           10 SUH-DTA-521-NVGLWZONE         PIC 9.
 
      ***  Datenfeld: Zone Sturm  FeldNr. 5689
           10 SUH-DTA-521-NVGSTZONE         PIC 9.
 
      ***  Datenfeld: Zone Erdbeben FeldNr. 5690
           10 SUH-DTA-521-NVGEBZONE         PIC 9.
 
      ***  Datenfeld: Zone sonstige Elementarsch{den FeldNr. 5691
           10 SUH-DTA-521-NVGSEZONE         PIC 9.
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5692
           10 SUH-DTA-521-NVGJNMBTR         PIC XX.
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5693
           10 SUH-DTA-521-CVGDTLSTFV        PIC 9(8).
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5694
           10 SUH-DTA-521-CVGDTLSTFB        PIC 9(8).
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5695
           10 SUH-DTA-521-CVGTXTSB          PIC X(14).
 
      ***  Datenfeld: PRM-Satz Hauptwagnis FeldNr. 5696
           10 SUH-DTA-521-NVGHWPRMS         PIC 99V9(7).
 
      ***  Datenfeld: PRM-Satz weitere Elementargefahren FeldNr. 5697
           10 SUH-DTA-521-NVGWEPRMS         PIC 99V9(4).
 
      ***  Datenfeld: Anzahl Vorsch{den weitere Elementargefahren
      ***             FeldNr. 5698
           10 SUH-DTA-521-NVGWEVSANZ        PIC 99.
 
      ***  Datenfeld: Gefahrenklsse feuergef{hrlicher Betriebe/Lager
      ***             FeldNr. 5699
           10 SUH-DTA-521-CVGGFKFBL         PIC X.
 
      ***  Datenfeld: Fl{che in Prozent Gefahrenklasse feuerge-
      ***             gef{hrlicher Betrag FeldNr. 5700
           10 SUH-DTA-521-CVGFLFBL          PIC X(20).
 
      ***  Datenfeld: Produkt FeldNr. 5701
           10 SUH-DTA-521-CVGPRD            PIC X(30).
 
      ***  Datenfeld: Geb{udeart FeldNr. 5707
           10 SUH-DTA-521-CVGLBGEBAR        PIC X(50).
 
      ***  Datenfeld: Betriebsart LB FeldNr. 5708
           10 SUH-DTA-521-CVGLBBTART        PIC X(50).
 
      ***  Datenfeld: Modell FeldNr. 5920
           10 SUH-DTA-521-CMODELL           PIC X(18).
 
      ***  Datenfeld: Monopol Ja/Nein FeldNr. 5921
           10 SUH-DTA-521-NMONOPOL          PIC XX.
 
      ***  Datenfeld: Auslandsrisiko (Ja/Nein) FeldNr. 5922
           10 SUH-DTA-521-NAUSLRIS          PIC XX.
 
      ***  Datenfeld: Geb{udebezeichnung FeldNr. 5923
           10 SUH-DTA-521-CGEBBEZ           PIC X(80).
 
      ***  Datenfeld: Rohbauversicherung FeldNr. 5924
           10 SUH-DTA-521-CROHBAUV          PIC 9(9).
           10 FILLER                        PIC X(6).
 
      ***  Datenfeld: Wohnfl{che FeldNr. 5925
           10 SUH-DTA-521-NWOHNFL           PIC 9(5).
 
      ***  Datenfeld: Geschossanzahl FeldNr. 5926
           10 SUH-DTA-521-NGESCHOSSE        PIC 99.
 
      ***  Datenfeld: Keller vorhanden (Ja/Nein) FeldNr. 5927
           10 SUH-DTA-521-NKELLER           PIC XX.
 
      ***  Datenfeld: Schwimmbad im Geb{ude (Ja/Nein)FeldNr. 5928
           10 SUH-DTA-521-NSCHWIMM          PIC XX.
 
      ***  Datenfeld: Nebengeb{ude Entsch{digungsgrenze FeldNr. 5930
           10 SUH-DTA-521-NNEBENGEB         PIC 9(9).
 
      ***  Datenfeld: Nebengeb{ude FeldNr. 5931
           10 SUH-DTA-521-CNEBENGEB         PIC X(80).
 
      ***  Datenfeld: Sonstiges FeldNr. 5932
           10 SUH-DTA-521-CSONST            PIC X(80).
 
      ***  Datenfeld: Garagen/Carports FeldNr. 5933
           10 SUH-DTA-521-NGARAGEN          PIC 99.
 
      ***  Datenfeld: Zone !berschwemmung FeldNr. 5682
           10 SUH-DTA-521-NVGUEBZONE        PIC 99.
 
      ***  Datenfeld: Mailingaktion FeldNr. 5944
           10 SUH-DTA-521-CMAILING          PIC X(70).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5539
           10 SUH-DTA-521-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5540
           10 SUH-DTA-521-CUMSTELLTERM      PIC 9(8).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5542
           10 SUH-DTA-521-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5543
           10 SUH-DTA-521-CDIFFVSNR         PIC X(40).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 5544
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-521-NKUENDBEST        PIC X(2).
 
      ***  Datenfeld: Elementar bei Diff-Vers. Ja/Nein FeldNr. 5545
           10 SUH-DTA-521-NELEDIFFJN        PIC X(2).
 
      ***  Datenfeld: sonstige Nutzflaeche FeldNr. 5709
310115     10 SUH-DTA-521-SNUTZFL           PIC 9(5).
 
    <<***  Datenfeld: Leerstellen
           10 SUH-DTA-521-LEERSTELLEN       PIC X(240).
090408*** Entität auf 1.600 Byte gekürzt.
 
310115******************************************************************
      *                                                                *
      *                522 - P K - P H O T O V O L T A I K             *
      *                                                                *
      *     LAENGE:                             STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-522-PK-PV.
         05 SUH-DTA-522-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 8001
           10 SUH-DTA-522-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 8003
           10 SUH-DTA-522-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-522-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-522.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 8004
             15 SUH-DTA-522-NGESELLST             PIC 9(9).
             15 SUH-DTA-522-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-522-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-522.
 
      ***  Datenfeld: Bedingungen FeldNr. 8011
           10  SUH-DTA-522-CPVBEDG          PIC X(30).
 
      ***  Datenfeld: Mailingaktion FeldNr. 8012
           10  SUH-DTA-522-CMAILING          PIC X(70).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 8013
           10  SUH-DTA-522-CPVZUSINFO        PIC X(100).
 
      ***  Datenfeld: Informationsquelle FeldNr. 8014
           10 SUH-DTA-522-LVTQUELLP         PIC 9(9).
           10 SUH-DTA-522-LVTQUELLE REDEFINES
              SUH-DTA-522-LVTQUELLP         PIC X(9).
 
      ***  Datenfeld: Baujahr FeldNr. 8015 Form JJJJ
           10 SUH-DTA-522-CPVBAUJ           PIC 9(4).
 
      ***  Datenfeld: Anlagenleistung FeldNr. 8016
           10 SUH-DTA-522-CPVLEIST      PIC 9(4)V99.
 
      ***  Datenfeld: Abweichender Anlagenbetreiber FeldNr. 8017
               10 SUH-DTA-522-CABWANLBTR      PIC X(35).
 
      ***  Datenfeld: Mitversicherung von Batteriesystemen
      ***                    FeldNr. 8018
               10 SUH-DTA-522-MVBATTSYS      PIC XX.
 
      ***  Datenfeld: Gebaeudeart FeldNr. 8019
           10 SUH-DTA-522-CPVGEBART PIC X(50).
 
      ***  Datenfeld: Leerstellen
           10  SUH-DTA-522-LEERSTELLEN       PIC X(1099).
 
170908***
    <<***
      ******************************************************************
      *                                                                *
      *                523 - F K - V E R T R A G - 523                 *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-523-FK-VTRG.
         05 SUH-DTA-523-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 5890
           10 SUH-DTA-523-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5892
           10 SUH-DTA-523-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-523-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-523.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
             15 SUH-DTA-523-NGESELLST             PIC 9(9).
             15 SUH-DTA-523-LEER                  PIC X(21).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-523-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-523.
 
      ***  Datenfeld: Produkt/Tarif FeldNr. 5894
           10  SUH-DTA-523-CFKPROD           PIC X(50).
 
      ***  Datenfeld: Bedingungen FeldNr. 5895
           10  SUH-DTA-523-CFKVBEDG          PIC X(20).
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5896
           10  SUH-DTA-523-CFKZUSINFO        PIC X(100).
 
      ***  Datenfeld: Mailingaktion FeldNr. 5939
           10  SUH-DTA-523-CMAILING          PIC X(70).
 
170209***  Datenfeld: Gesellschaft-Klartext FeldNr. 9152
           10 SUH-DTA-523-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9153
           10 SUH-DTA-523-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 9154 i.d.F. YYYYMMDD
           10 SUH-DTA-523-CUMSTELLTERM      PIC 9(8).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 9155
      ***     ausführliche Beschreibung siehe bei Feld 9185
               10 SUH-DTA-523-NKUENDBESTAET      PIC XX.
 
      ***  Datenfeld: Leerstellen
090408*****10  SUH-DTA-523-LEERSTELLEN       PIC X(1165).
170209     10  SUH-DTA-523-LEERSTELLEN       PIC X(1065).
 
170908***
    <<***
      ******************************************************************
      *                                                                *
      *613 - S G W - B E S O N D E R E   V E R E I N B A R U N G - 613 *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-613-SGW.
         05 SUH-DTA-613-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6141
           10 SUH-DTA-613-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6139
           10 SUH-DTA-613-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-613-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-613.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6140
             15 SUH-DTA-613-NGESELLST             PIC 9(9).
      *** Schlüssel: Komponentengruppe FeldNr. 9317
030811       15  SUH-DTA-613-RGID              PIC 9(4).
      ***  Schlüssel: laufende Nummer FeldNr. 6136
             15 SUH-DTA-613-PSGBLFDNR             PIC 999.
030811       15 SUH-DTA-613-LEER                  PIC X(14).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-613-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-613.
 
      ***  Datenfeld: Bedingung/Klausel-Nummer FeldNr. 6137
           10  SUH-DTA-613-CSGBNRBVB         PIC X(20).
 
      ***  Datenfeld: Text besondere Vertragsbestandteile FeldNr. 6138
           10  SUH-DTA-613-CSGBTXTBVB        PIC X(80).
 
      ***  Datenfeld: Leerstellen
090408     10  SUH-DTA-613-LEERSTELLEN       PIC X(1305).
 
210108******************************************************************
>>    *                                                                *
      * 614 - U N F A L L - V E R S I C H E R T E   P E R S O N - 614  *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-614-UNFVP.
         05 SUH-DTA-614-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6166
           10 SUH-DTA-614-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6164
           10 SUH-DTA-614-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-614-SATZART               PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-614.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6165
             15 SUH-DTA-614-NGESELLST             PIC 9(9).
      ***  Schlüssel: laufende Nummer FeldNr. 6144
             15 SUH-DTA-614-PUFVPLFDNR            PIC 999.
             15 SUH-DTA-614-LEER                  PIC X(18).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-614-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-614.
      ***  Datenfeld: versicherte Person laufende Nummer (Maske)
      ***             FeldNr. 6145
           10 SUH-DTA-614-NUFVPVBNR         PIC 999.
 
      ***  Datenfeld: VP-Name FeldNr. 6146
           10 SUH-DTA-614-CUFVPNAME         PIC X(25).
 
      ***  Datenfeld: VP-Beruf FeldNr. 6147
170717***  10 SUH-DTA-614-CUFVPBERUF        PIC X(21).
170717     10 SUH-DTA-614-CUFVPBERUF        PIC X(48).
 
      ***  Datenfeld: VP-Geburtsdatum Format: JJJJMMTT FeldNr. 6148
           10 SUH-DTA-614-CUFVPGEBD         PIC 9(8).
 
      ***  Datenfeld: VP-Gefahrengruppe FeldNr. 6149
           10 SUH-DTA-614-CUFVPGEFGR        PIC X.
 
      ***  Datenfeld: VP-Beitrag FeldNr. 6150
           10 SUH-DTA-614-NUFVPBTR          PIC 9(7)V99.
 
      ***  Datenfeld: Tarifart FeldNr. 6151
           10 SUH-DTA-614-CUFTARIF          PIC X(30).
 
      ***  Datenfeld: Dynamik-Text FeldNr. 6152
           10 SUH-DTA-614-CUFDYNTEXT        PIC X(5).
 
      ***  Datenfeld: Dynamik-%-Satz FeldNr. 6153
           10 SUH-DTA-614-NUFDYNPRZ         PIC 99V99.
 
      ***  Datenfeld: Dynamik Ablehnungsjahr Format: JJJJMMTT
      ***             FeldNr. 6154
           10 SUH-DTA-614-CUFDYNABLJ        PIC 9(8).
 
      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 6155
           10 SUH-DTA-614-NUFJNREGUL        PIC XX.
 
      ***  Datenfeld: Rueckgewaehrsumme Ja/Nein FeldNr. 6156
           10 SUH-DTA-614-NUFSUMRUEK        PIC XX.
 
      ***  Datenfeld: Pr{mienr}ckgew{hr Ja/Nein FeldNr. 6157
           10 SUH-DTA-614-NUFJNUPR          PIC XX.
 
      ***  Datenfeld: Freizeitdeckung Ja/Nein FeldNr. 6158
           10 SUH-DTA-614-NUFJNFREIZ        PIC XX.
 
      ***  Datenfeld: Anzahl versicherte Personen je Personengruppe
      ***             FeldNr. 6159
           10 SUH-DTA-614-NUFVPGRANZ        PIC 9(5).
 
      ***  Datenfeld: Geschlecht Versicherte Person FeldNr. 6160
           10 SUH-DTA-614-NUFVPSEX          PIC 9(9).
 
      ***  Datenfeld: Pr{mienfreistellung KIZ Ja/Nein FeldNr. 6162
           10 SUH-DTA-614-NUFJNKIZFR        PIC XX.
 
      ***  Datenfeld: Pr{mie KZ FeldNr. 6163
           10 SUH-DTA-614-NUFKIZPRM         PIC 9(7)V99.
 
      ***  Datenfeld: PartnerHost-ID der versicherten Person
      ***             FeldNr. 6168
           10 SUH-DTA-614-CGPBHNR           PIC 9(15).
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5546
           10 SUH-DTA-614-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Zielgruppe FeldNr. 5988
           10 SUH-DTA-614-CZIELGRP          PIC X(30).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 3235
           10 SUH-DTA-614-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 3236
           10 SUH-DTA-614-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 3237 i.d.F. YYYYMMDD
           10 SUH-DTA-614-CUMSTELLTERM      PIC 9(8).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 3238
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-614-NKUENDBESTAET PIC XX.
 
      ***  Datenfeld: Nachname  FeldNr. 6161
260716     10 SUH-DTA-614-NACHNAME      PIC X(40).
 
      ***  Datenfeld: Vorname  FeldNr. 6169
260716     10 SUH-DTA-614-VORNAME       PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408*****10 SUH-DTA-614-LEERSTELLEN       PIC X(1111).
260716*****10 SUH-DTA-614-LEERSTELLEN       PIC X(1041).
170717     10 SUH-DTA-614-LEERSTELLEN       PIC X(1014).
 
      ******************************************************************
      *                                                                *
      *     615 - V G V - S E L B S T B E T E I L I G U N G - 615      *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-615-VGVSB.
         05 SUH-DTA-615-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6179
           10 SUH-DTA-615-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6177
           10 SUH-DTA-615-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-615-SATZART               PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-615.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6178
             15 SUH-DTA-615-NGESELLST             PIC 9(9).
      ***  Schlüssel: Laufende Nummer der Selbstbeteiligung
      ***             FeldNr. 6170
             15 SUH-DTA-615-PVGSBLFDNR            PIC 999.
             15 SUH-DTA-615-LEER                  PIC X(18).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-615-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-615.
      ***  Datenfeld: SelbstBeteiligung-Art FeldNr. 6171
           10 SUH-DTA-615-NVGLBSBART        PIC 9(9).
 
      ***  Datenfeld: SelbstBeteiligung-Bezeichnung FeldNr. 6172
           10 SUH-DTA-615-CVGSBBEZ          PIC X(30).
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 6173
           10 SUH-DTA-615-CVGSBPRZ          PIC X(14).
 
      ***  Datenfeld: SelbstBeteiligung-Minimum FeldNr. 6174
           10 SUH-DTA-615-NVGSBMIN          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Maximum FeldNr. 6175
           10 SUH-DTA-615-NVGSBMAX          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 6176
           10 SUH-DTA-615-NVGSBFES          PIC 9(5)V99.
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 6243
           10 SUH-DTA-615-CVGSBTXT          PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408     10 SUH-DTA-615-LEERSTELLEN       PIC X(1301).
 
      ******************************************************************
      *                                                                *
      *616-U N F A L L - M I T V E R S I C H E R T E   P E R S O N-616 *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-616-UNFMVP.
         05 SUH-DTA-616-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6460
           10 SUH-DTA-616-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6462
           10 SUH-DTA-616-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-616-SATZART              PIC 999.
      ***  Entit{tenkey f}r die einzelnen Satzarten
           10 SUH-DTA-ETKEY-616.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6459
             15 SUH-DTA-616-NGESELLST             PIC 9(9).
      ***  Schlüssel: VP laufende Nummer FeldNr. 6445
             15 SUH-DTA-616-PUFVPLFDNR            PIC 999.
      ***  Schlüssel: MVP laufende Nummer FeldNr. 6446
             15 SUH-DTA-616-PUFMVPLFDNR           PIC 999.
             15 SUH-DTA-616-LEER                  PIC X(15).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-616-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-616.
      ***  Datenfeld: Name FeldNr. 6447
           10 SUH-DTA-616-CNAME             PIC X(30).
 
      ***  Datenfeld: Vorname FeldNr. 6448
           10 SUH-DTA-616-CVNAME            PIC X(30).
 
      ***  Datenfeld: Geschlecht FeldNr. 6449
           10 SUH-DTA-616-NSEX              PIC 9(9).
 
      ***  Datenfeld: Geburtsdatum Format: JJJJMMTT FeldNr. 6450
           10 SUH-DTA-616-CGEBDAT           PIC 9(8).
 
      ***  Datenfeld: Beginn Format: JJJJMMTT FeldNr. 6451
           10 SUH-DTA-616-CBEGINN           PIC 9(8).
 
      ***  Datenfeld: Ende Format: JJJJMMTT FeldNr. 6452
           10 SUH-DTA-616-CENDE             PIC 9(8).
 
      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6457
           10 SUH-DTA-616-CGPBHNR           PIC 9(15).
 
    <<***  Datenfeld: Leerstellen
210108     10 SUH-DTA-616-LEERSTELLEN       PIC X(1297).
090408*** Entität auf 1.600 Byte gekürzt.
      *
      ******************************************************************
      *                                                                *
      *617 - UNFALL MIT BEITRAGSRÜCKGEWÄHR                       - 617 *
      *                                                                *
      *     LAENGE:                             STAND: 23.11.2011      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-617-UBR.
           05 SUH-DTA-617-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 139
             10 SUH-DTA-617-CVNR                    PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 140
             10 SUH-DTA-617-CVEZHOSTNR              PIC X(15).
      ***  Headerinformation: Nummer der Satzart
             10 SUH-DTA-617-SATZART                 PIC 9(03).
      ***  Entitätenkey für die einzelnen Satzarten
             10 SUH-DTA-ETKEY-617.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 138
               15 SUH-DTA-617-NGESELLST             PIC 9(09).
      ***  Schlüssel: Komponentengruppennummer FeldNr. 141
               15 SUH-DTA-617-PVRIGRP               PIC 9(04).
      ***  Schlüssel: laufendes Vertragsjahr FeldNr. 142
               15 SUH-DTA-617-LFD-JAHR          PIC 9(02).
               15 SUH-DTA-617-LEER                  PIC X(15).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
           05 SUH-DTA-ALLG-617-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
           05 SUH-DTA-ALLG-DATEN-617.
      ***  Tabelle  : Kapitalwerte je Monat im Vertragsjahr
             10 SUH-DTA-617-UBR-KAPW OCCURS 12.
 
      ***  Datenfeld: lfd. Nr.Datensatz FeldNr. 143
               15 SUH-DTA-617-UBR-NR            PIC 9(03).
 
      ***  Datenfeld: lfd. Vers.Monat im Vertragsjahr FeldNr. 144
               15 SUH-DTA-617-LFD-MONAT         PIC 9(02).
 
      ***  Datenfeld: lfd Kalenderjahr FeldNr. 145
               15 SUH-DTA-617-JAHR              PIC 9(04).
 
      ***  Datenfeld: lfd. Kalendermonat FeldNr. 146
               15 SUH-DTA-617-MONAT             PIC 9(02).
 
      ***  Datenfeld: Status des Datensatzes FeldNr. 147
               15 SUH-DTA-617-STATUSCD          PIC X.
 
      ***  Datenfeld: Erstelldatum des Datensatzes FeldNr. 148
               15 SUH-DTA-617-STAT-DAT          PIC 9(08).
 
      **** Datenfeld: garant. Rückkaufswert FeldNr. 149
               15 SUH-DTA-617-RUECKK-GAR        PIC 9(11)V9(04).
 
      **** Datenfeld: garant. Todesfallleistung FeldNr. 150
               15 SUH-DTA-617-LEITOD-GAR        PIC 9(11)V9(04).
 
      **** Datenfeld: Rückkaufswert incl. Übersch.-Betlg.
      ****        FeldNr. 151
               15 SUH-DTA-617-RUECK-UEBS        PIC 9(11)V9(04).
 
      **** Datenfeld: Todesfallleistung incl. Übersch.-Betlg.
      ****        FeldNr. 152
               15 SUH-DTA-617-TOD-UEBS          PIC 9(11)V9(04).
 
      **** Datenfeld: stille Reserve FeldNr. 153
               15 SUH-DTA-617-WEANW             PIC 9(11)V9(04).
 
      **** Datenfeld: Rechnungszins  FeldNr. 154
               15 SUH-DTA-617-RECHZINS          PIC 9(02)V9(02).
 
      ***  Datenfeld: Leerstellen
             10 SUH-DTA-617-LEERSTELLEN       PIC X(217).
      *** Entität auf 1.600 Byte gekürzt.
 
      ******************************************************************
      *                                                                *
      *      628 - V E R T R A G S R I S I K O G R U P P E - 628       *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-628-VTRGRISGRP.
         05 SUH-DTA-628-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6402
           10 SUH-DTA-628-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6404
           10 SUH-DTA-628-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-628-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-628.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6401
             15 SUH-DTA-628-NGESELLST             PIC 9(9).
      ***  Schlüssel: Subjekt-Host-ID FeldNr. 6399
             15 SUH-DTA-628-NVRGSUBHID            PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6352
             15 SUH-DTA-628-PVRGID                PIC 9(4).
             15 SUH-DTA-628-LEER                  PIC X(8).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-628-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-628.
 
      ***  Datenfeld: Risikogruppe-Typ FeldNr. 6353
           10 SUH-DTA-628-NVRGTYP           PIC 9(9).
 
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6354
           10 SUH-DTA-628-CVRGNAME          PIC X(50).
 
      ***  Datenfeld: Beginndatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6355
           10 SUH-DTA-628-CVRGBEG           PIC 9(8).
 
      ***  Datenfeld: Ablaufdatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6356
           10 SUH-DTA-628-CVRGABL           PIC 9(8).
 
      ***  Datenfeld: Nettobeitrag FeldNr. 6357
           10 SUH-DTA-628-NVRGNBTRG         PIC 9(8)V99.
 
      ***  Datenfeld: ID übergeordnete Risikogruppe FeldNr. 6358
           10 SUH-DTA-628-NVRGUEBID         PIC 9(4).
 
170209***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9147
           10 SUH-DTA-628-NDIFFDECKJN       PIC X(2).
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9148
           10 SUH-DTA-628-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9149
           10 SUH-DTA-628-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin FeldNr. 9150 i.d.F. YYYYMMDD
           10 SUH-DTA-628-CUMSTELLTERM      PIC 9(8).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 9151
      ***     ausführliche Beschreibung siehe bei Feld 9185
             10 SUH-DTA-628-NKUENDBESTAET              PIC X(2).
 
      ***  Datenfeld: Leerstellen
090408*****10 SUH-DTA-628-LEERSTELLEN       PIC X(1316).
170209     10 SUH-DTA-628-LEERSTELLEN       PIC X(1214).
 
      ******************************************************************
      *                                                                *
      *     629 - V E R T R A G S R I S I K O M E R K M A L - 629      *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-629-VTRGRISMKM.
         05 SUH-DTA-629-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6409
           10 SUH-DTA-629-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6411
           10 SUH-DTA-629-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-629-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-629.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6408
             15 SUH-DTA-629-NGESELLST             PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6361
             15  SUH-DTA-629-PVRGID               PIC 9(4).
      ***  Schlüssel: Risikomerkmalwert-ID FeldNr. 6362
             15 SUH-DTA-629-PVRISMID              PIC 999.
             15 SUH-DTA-629-LEER                  PIC X(14).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-629-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-629.
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6363
           10 SUH-DTA-629-CVRISMNAME        PIC X(50).
 
      ***  Datenfeld: Risikomerkmalwert FeldNr. 6364
           10 SUH-DTA-629-CVRISMWERT        PIC X(50).
 
      ***  Datenfeld: Leerstellen
090408     10 SUH-DTA-629-LEERSTELLEN       PIC X(1305).
 
      ******************************************************************
      *                                                                *
      * 630 - V E R T R A G S S E L B S T B E T E I L I G U N G - 630  *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-630-VTRGSB.
         05 SUH-DTA-630-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6416
           10 SUH-DTA-630-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6418
           10 SUH-DTA-630-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-630-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-630.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6415
             15 SUH-DTA-630-NGESELLST             PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6372
             15 SUH-DTA-630-PVRGID                PIC 9(4).
      ***  Schlüssel: Selbstbeteiligung-ID FeldNr. 6373
             15 SUH-DTA-630-PVRSBID               PIC 999.
             15 SUH-DTA-630-LEER                  PIC X(14).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-630-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-630.
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6374
           10 SUH-DTA-630-CVSBBEZ           PIC X(50).
 
      ***  Datenfeld: Wert in Prozent FeldNr. 6375
           10 SUH-DTA-630-NVSBPRZ           PIC 99.
 
      ***  Datenfeld: Wert in DM/Euro FeldNr. 6376
           10 SUH-DTA-630-NVSBBTRG          PIC 9(8)V99.
 
      ***  Datenfeld: Mindestbetrag in DM/Euro FeldNr. 6377
           10 SUH-DTA-630-NVSBMIN           PIC 9(8)V99.
 
      ***  Datenfeld: Höchstbetrag in DM/Euro FeldNr. 6378
           10 SUH-DTA-630-NVSBMAX           PIC 9(8)V99.
 
      ***  Datenfeld: Text zur Selbstbeteiligung FeldNr. 6379
           10 SUH-DTA-630-CVSBTXT           PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408     10 SUH-DTA-630-LEERSTELLEN       PIC X(1293).
 
      ******************************************************************
      *                                                                *
      *          631 - V E R T R A G S L E I S T U N G - 631           *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-631-VTRGLSTG.
         05 SUH-DTA-631-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 6392
           10 SUH-DTA-631-CVNR                   PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6394
           10 SUH-DTA-631-CVEZHOSTNR             PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-631-SATZART                PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-631.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6391
             15 SUH-DTA-631-NGESELLST                 PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 6381
             15 SUH-DTA-631-PVRGID                    PIC 9(4).
      ***  Schlüssel: Leistung-ID FeldNr. 6382
             15 SUH-DTA-631-PVLSTID                   PIC 999.
      ***  Schlüssel: Themengebietsschlüssel FeldNr. 6443
             15 SUH-DTA-631-PTGSCHL                   PIC 999.
             15 SUH-DTA-631-LEER                      PIC X(11).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-631-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-631.
      ***  Datenfeld: Name/Bezeichnung FeldNr. 6383
           10  SUH-DTA-631-CVLSTBEZ          PIC X(50).
 
      ***  Datenfeld: Wert DM/EUR FeldNr. 6384
           10  SUH-DTA-631-NVLSTSUM          PIC 9(9)V99.
 
      ***  Datenfeld: sonstiger Wert FeldNr. 6385
           10  SUH-DTA-631-NVLSTWRT          PIC 9(8)V99.
 
      ***  Datenfeld: sonstiger Wert Einheit FeldNr. 6386
           10  SUH-DTA-631-CVLSTWEH          PIC X(20).
 
      ***  Datenfeld: Text zur Leistung FeldNr. 6387
           10  SUH-DTA-631-CVLSTTXT          PIC X(30).
 
      ***  Datenfeld: Leerstellen
090408     10  SUH-DTA-631-LEERSTELLEN       PIC X(1284).
 
      ******************************************************************
      *                                                                *
      *         701 - Z U S C H L A G / A B S C H L A G - 701          *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-701-ZU-ABSCHLAG.
         05 SUH-DTA-701-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 7063
           10 SUH-DTA-701-CVNR                   PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7061
           10 SUH-DTA-701-CVEZHOSTNR             PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-701-SATZART                PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-701.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7062
             15 SUH-DTA-701-NGESELLST                 PIC 9(9).
      ***  Schlüssel: Universelle Laufende Nummer FeldNr. 7055
             15 SUH-DTA-701-PZUNILFDNR                PIC 999.
             15 SUH-DTA-701-LEER                      PIC X(18).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-701-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-701.
      ***  Datenfeld: Zu-/Abschlag in % FeldNr. 7057
           10 SUH-DTA-701-NZSLGPROZ         PIC 99V99.
 
      ***  Datenfeld: Zu-/Abschlag in WE FeldNr. 7058
           10 SUH-DTA-701-NZSLGWE           PIC 9(7)V99.
 
      ***  Datenfeld: Kürzel zum Zu-/Abschlag FeldNr. 7059
           10 SUH-DTA-701-CZSLGKRZL         PIC XXX.
 
      ***  Datenfeld: Erklärungstext zum Zu-/Abschlag FeldNr. 7060
           10 SUH-DTA-701-CZSLGTXT          PIC X(30).
 
210508***  Datenfeld: PZANR STAZ SL1 Zuschlags/Abschlagsnummer.
210508***             FeldNr. 7065.
210508     10 SUH-DTA-701-PZANR             PIC 9(09).
 
      ***  Datenfeld: Leerstellen
090408***  10 SUH-DTA-701-LEERSTELLEN       PIC X(1359).
210508     10 SUH-DTA-701-LEERSTELLEN       PIC X(1350).
      *
      ******************************************************************
      *                                                                *
      *            702 - V E R T R A G S R I S I K O - 702             *
      *                                                                *
      *     LAENGE:                             STAND: 14.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-702-VTRGRISIKO.
         05 SUH-DTA-702-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 7083
           10 SUH-DTA-702-CVNR                    PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7081
           10 SUH-DTA-702-CVEZHOSTNR              PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-702-SATZART                PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-702.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7082
             15 SUH-DTA-702-NGESELLST                 PIC 9(9).
      ***  Schlüssel: Risikogruppe-ID FeldNr. 7066
             15 SUH-DTA-702-PVRIGRP                   PIC 9(4).
      ***  Schlüssel: Themengebietsschlüssel FeldNr. 7067
             15 SUH-DTA-702-PTGSCHL                   PIC 999.
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7068
             15 SUH-DTA-702-PVRILFDNR                 PIC 999.
             15 SUH-DTA-702-LEER                      PIC X(11).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-702-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-702.
      ***  Datenfeld: Risikokennziffer FeldNr. 7069
           10 SUH-DTA-702-CVRIKENNZ         PIC X(10).
 
      ***  Datenfeld: Risikoname FeldNr. 7070
           10 SUH-DTA-702-CVRINAME          PIC X(50).
 
      ***  Datenfeld: Risikobeitrag FeldNr. 7071
           10 SUH-DTA-702-NVRIBTRG          PIC 9(7)V99.
 
      ***  Datenfeld: Risikosumme FeldNr. 7072
           10 SUH-DTA-702-NVRIVSUM          PIC 9(9)V99.
 
      ***  Datenfeld: Risiko-Entschädigungsgrenze FeldNr. 7073
           10 SUH-DTA-702-NVRIEGR           PIC 9(08)V99.
 
      ***  Datenfeld: Risiko-Entschädigungsgrenze-Einheit FeldNr. 7074
           10 SUH-DTA-702-NVRIEGREH         PIC 9(9).
 
      ***  Datenfeld: Risikomenge FeldNr. 7075
           10 SUH-DTA-702-NVRIMG            PIC 9(9).
 
      ***  Datenfeld: Risikomenge-Einheit FeldNr. 7076
           10 SUH-DTA-702-CVRIMGEH          PIC X(25).
 
      ***  Datenfeld: Risikosatz FeldNr. 7077
           10 SUH-DTA-702-NVRISATZ          PIC 999V9(7).
 
      ***  Datenfeld: Risikosatz-Einheit FeldNr. 7078
           10 SUH-DTA-702-NVRISATZEH        PIC 9(9).
 
      ***  Datenfeld: Risikotext FeldNr. 7079
           10 SUH-DTA-702-CVRITXT           PIC X(30).
 
      ***  Datenfeld: Risiko-Mitversichert (Ja/Nein)FeldNr. 7080
           10 SUH-DTA-702-NVRIMITVJN        PIC XX.
 
      ***  Datenfeld: Risikosumme-Einheit FeldNr. 7167
           10 SUH-DTA-702-CVRIVSUMEH        PIC X(20).
 
      ***  Datenfeld: Risikosumme unbegrenzt (Ja/Nein) FeldNr. 7168
           10 SUH-DTA-702-NVRIUNBJN         PIC XX.
 
210508***  Datenfeld: Risikomerkmal FeldNr. 7169
210508     10 SUH-DTA-702-RISMNR            PIC 9(09).
 
      ***  Datenfeld: Leerstellen
090408***  10 SUH-DTA-702-LEERSTELLEN       PIC X(1199).
210508     10 SUH-DTA-702-LEERSTELLEN       PIC X(1190).
      *
      *
310115******************************************************************
      *                                                                *
      *                705 - F R E M D V E R S I C H E R E R           *
      *                                                                *
      *     LAENGE:                             STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-705-FRV.
         05 SUH-DTA-705-KEY.
      ***  Schlüssel: Vertragsnummer FeldNr. 7501
           10 SUH-DTA-705-CVNR                  PIC 9(15).
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7502
           10 SUH-DTA-705-CVEZHOSTNR            PIC X(15).
      ***  Headerinformation: Nummer der Satzart
           10 SUH-DTA-705-SATZART               PIC 999.
      ***  Entitätenkey für die einzelnen Satzarten
           10 SUH-DTA-ETKEY-705.
      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7503
             15 SUH-DTA-705-NGESELLST             PIC 9(9).
      ***  Schlüssel: Risikogruppe FeldNr. 7504
             15 SUH-DTA-705-PVRIGRP               PIC 9(4).
      ***  Schlüssel: Themengebietsschluessel FeldNr. 7505
             15 SUH-DTA-705-PTGSCHL               PIC 999.
      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7506
             15 SUH-DTA-705-PVRILFDNR             PIC 999.
             15 SUH-DTA-705-LEER                  PIC X(11).
      * **** Ende der Redefinition fuer die Entitaeten-Schlüssel *******
 
      *************** Allgemeine Headerdaten
         05 SUH-DTA-ALLG-705-LEER                PIC X(132).
      *************** Ende *** H E A D E R *** Ende ********************
 
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-ALLG-DATEN-705.
 
      ***  Datenfeld: Differenzdeckungsgefahr FeldNr. 7511
           10  SUH-DTA-705-NDIFFGEF         PIC X(45).
 
      ***  Datenfeld: Differenzdeckungsgesellschaft FeldNr. 7512
           10  SUH-DTA-705-CGESELL           PIC X(50).
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 7513
           10  SUH-DTA-705-CDIFFVSNR         PIC X(40).
 
      ***  Datenfeld: Umstellungstermin  FeldNr. 7514
           10 SUH-DTA-705-CUMSTELLTERM      PIC 9(8).
 
      ***Datenfeld: Kuendigungsbest. Fremdvert. (J/N/V/G/ ) FeldNr. 7515
      ***     ausführliche Beschreibung siehe bei Feld 9185
           10 SUH-DTA-705-NKUENDBEST        PIC XX.
 
      ***  Datenfeld: Risikomerkmal FeldNr. 7529
           10 SUH-DTA-705-RISMNR      PIC 9(9).
 
      ***  Datenfeld: Leerstellen
           10  SUH-DTA-705-LEERSTELLEN       PIC X(1251).
      *                                                                *
      ******************************************************************
      *                                                                *
      *           N A C H S A T Z - SATZART 999                        *
      *                                                                *
      *     LAENGE:   1.600 BYTES               STAND: 14.08.2007      *
      *     davon:
      *     Key...:      63 Bytes
      *     Header:     132 BYTES                                      *
      *     Filler:   1.616 BYTES                                      *
      *                                                                *
      ******************************************************************
       01 SUH-DTA-NACHSATZ.
         05 SUH-DTA-N-KEY.
      ***  Nachsatzinformation: Vertragsnummer. Beim Nachsatz immer mit
      ***  999999999999999 gefuellt.
           10 SUH-DTA-N-KEY-VTRGNUMMER              PIC 9(15).
      ***  Nachsatzinformation: VEZ-Host-ID. Beim Nachsatz immer mit
      ***  SPACE gefuellt.
           10 SUH-DTA-N-KEY-CVEZHOSTNR              PIC X(15).
      ***  Nachsatzinformation: Nummer der Satzart. Beim Nach-
      ***  satz steht hier immer der Wert 999 drin.
           10 SUH-DTA-N-KEY-SATZART                 PIC 9(03).
      ***  Nachsatz ET-Key für die einzelnen Satzarten. Beim Nachsatz
      ***  immer mit SPACE gefuellt.
           10 SUH-DTA-NHD-ETKEY                     PIC X(30).
      ***  Beim Nachsatz immer mit SPACE gefuellt.
         05 SUH-DTA-N-HEADER-LEER                   PIC X(132).
      *
      ***  Allgemeiner Datenteil ************************************
         05 SUH-DTA-N-ALLG-DATEN.
      ***
      **** Datenfeld: SATZANZAHL OHNE VOR- UND NACHSATZ
           10 SUH-DTA-N-SATZANZAHL                     PIC 9(9).
 
210508***  Datenfeld: ANZAHL           SATZART 100
    >>**   10 SUH-DTA-N-ANZAHL-SA100                   PIC 9(9).
      ***
      ***  Datenfeld: ANZAHL           SATZART 101
      ***  10 SUH-DTA-N-ANZAHL-SA101                   PIC 9(9).
      ***
      ***  Datenfeld: ANZAHL           SATZART 102
      ***  10 SUH-DTA-N-ANZAHL-SA102                   PIC 9(9).
      ***
    <<***  Datenfeld: ANZAHL           SATZART 103
210508***  10 SUH-DTA-N-ANZAHL-SA103                   PIC 9(9).
 
      **** Datenfeld: ANZAHL VERTRAEGE SATZART 400
           10 SUH-DTA-N-ANZAHL-SA400                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 401 EINZELVERTRAG
           10 SUH-DTA-N-ANZAHL-SA401                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 403 VERTRAGSRISIKO-ADRESSE
           10 SUH-DTA-N-ANZAHL-SA403                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 508 GLAS
           10 SUH-DTA-N-ANZAHL-SA508                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 509 HAFTPFLICHT
           10 SUH-DTA-N-ANZAHL-SA509                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 510 HAUSRAT
           10 SUH-DTA-N-ANZAHL-SA510                   PIC 9(9).
 
210508***  Datenfeld: ANZAHL           SATZART 511
210508***  10 SUH-DTA-N-ANZAHL-SA511                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 520 UNFALL
           10 SUH-DTA-N-ANZAHL-SA520                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 521 WOHNGEBAEUDE
           10 SUH-DTA-N-ANZAHL-SA521                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 523 FK-VERTRAG
           10 SUH-DTA-N-ANZAHL-SA523                   PIC 9(9).
 
170908**** Datenfeld: ANZAHL SATZART 524 PK-VERTRAG
170908***  10 SUH-DTA-N-ANZAHL-SA524                   PIC 9(9).
 
210508***  Datenfeld: ANZAHL           SATZART 604
      ***  10 SUH-DTA-N-ANZAHL-SA604                   PIC 9(9).
      ***
      ***  Datenfeld: ANZAHL           SATZART 605
      ***  10 SUH-DTA-N-ANZAHL-SA605                   PIC 9(9).
      ***
      ***  Datenfeld: ANZAHL           SATZART 606
210508***  10 SUH-DTA-N-ANZAHL-SA606                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 613 SGW - BESONDERE VEREINBARUNG
           10 SUH-DTA-N-ANZAHL-SA613                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 614 UNFALL-VERSICHERTE PERSON
           10 SUH-DTA-N-ANZAHL-SA614                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 615 VGV-SELBSTBETEILIGUNG
           10 SUH-DTA-N-ANZAHL-SA615                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 616 UNFALL - MITVERSICHERTE PERSON
           10 SUH-DTA-N-ANZAHL-SA616                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 628 VERTRAGSRISIKOGRUPPE
           10 SUH-DTA-N-ANZAHL-SA628                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 629 VERTRAGSRISIKOMERKMAL
           10 SUH-DTA-N-ANZAHL-SA629                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 630 VERTRAGSSELBSTBETEILIGUNG
           10 SUH-DTA-N-ANZAHL-SA630                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 631 VERTRAGSLEISTUNG
           10 SUH-DTA-N-ANZAHL-SA631                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 701 ZUSCHLAG / ABSCHLAG
           10 SUH-DTA-N-ANZAHL-SA701                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 702 VERTRAGSRISIKO
           10 SUH-DTA-N-ANZAHL-SA702                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 522 PHOTOVOLTAIK
           10 SUH-DTA-N-ANZAHL-SA522                   PIC 9(9).
 
      **** Datenfeld: ANZAHL SATZART 705 FREMDVERSICHERER
           10 SUH-DTA-N-ANZAHL-SA705                   PIC 9(9).
 
      **** Leerzeichen
090408***  10 SUH-DTA-N-LEER                           PIC X(1144).
210508***  10 SUH-DTA-N-LEER                           PIC X(1216).
310115     10 SUH-DTA-N-LEER                           PIC X(1207).
200218*****10 SUH-DTA-N-LEER                           PIC X(1198).
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2051                       *
      ******************************************************************
 
 
 
 
 
 
 
 




