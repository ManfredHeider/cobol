      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - AUSGABEDATEI ALLFINANZ REOB fuer alle Datensaetze, die   *
      *       geaendert wurden. Aenderungskennzeichen auf Feldebene    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *24.07.07! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
210108*21.01.08! Satzarten  f. Reob-PK eingefügt      ! WMI     !      *
      *        !                                      !         !      *
090408*09.04.08! Entitäten auf 1.600 Byte gekürzt.    ! HSL     !      *
      *        ! Achtung !!! Dokumentation wurde nicht!         !      *
      *        ! aktualisiert.                        !         !      *
      *        !                                      !         !      *
270508*27.05.08! Erweiterung um je 1 Feld in der Enti-! HSL     !      *
      *        ! taet 701 und 702 und Verschiebung der!         !      *
      *        ! Felder INT-LFDNR und MAX-INTLFDNR an !         !      *
      *        ! das Ende der Satzarten.              !         !      *
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
170209*17.02.09! Differenzdeckung FK eingebaut        ! HHU     !      *
      *        !                                      !         !      *
100610*10.06.10! VSP2010 eingebaut                    ! HHU     !      *
      *        !                                      !         !      *
220211*AKI01012|  Vorteilsargumentation eingefügt     |P. Hübner       *
      *        !                                      !         !      *
030811*AKI01037! Änd. SA 613                          !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01102! neue SA 617                          !B. Stoldt!      *
      *        !                                      !         !      *
310115*AKI01295! neue SA 522 und 705 und Änd. 521     !P. Hübner!      *
      *        !                                      !         !      *
200415*AKI01358! Verkuerzung Feldnamen                !P. Hübner!      *
      *        ! AF-REOB-AUS in SUH-AUS               !         !      *
      *        !                                      !         !      *
120416*AKI01424! neue Felder Bestandsmanagement       !P. Hübner!      *
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
      *        ! unnötige alte Definitionen entfernt  !         !      *
      *        !                                      !         !      *
170717*AKI01530! SA614 Beruf auf 48 Stellen           !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01785! SA401 2 neue Felder zu FLEXN         !P. Hübner!      *
      *        !                                      !         !      *
      *FKI01676! SA616 Feldnamen angleichen           !P. Hübner!      *
      *        !                                      !         !      *
      *AKI01827! SA400 neues Feld VSPKZ               !P. Hübner!      *
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * ---------------------------------------------------------------
      * DBO-10885  !10.03.23! Das VSP-Kennzeichen wird im  ! t13ex78
      *            !        ! Original benötigt weil daraus!
      *            !        ! das Bündelkennzeichen abge-  !
      *            !        ! leitet wird.                 !
      *--------------------------------------------------------------- *
      *                                                                *
      *                   A L L F I N A N Z - R E O B                  *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 1.600 BYTES                 STAND: 19.07.2007      *
      *     Hypername: ALLFI-AEND-AUS-SATZ                             *
      **************************************************************** *

      **************************************************************** *
      *                                                                *
      *                         V E R T R A G                          *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      **************************************************************** *

       01 SUH-AUS-400-VERTRAG.

                   05  SUH-AUS-400-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-400-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4009
                   05  SUH-AUS-400-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 4002
                   05  SUH-AUS-400-CVNR                PIC 9(15).

      ***  Schlüssel: Vermögensberater
                   05  SUH-AUS-400-VBNR                PIC 9(7).
                   05  SUH-AUS-400-VBNR-UPD            PIC X.
      *            J = Änderung Vermögensberater

      ***  Schlüssel: Herkunftssystem
                   05  SUH-AUS-400-HERKUNFT            PIC XXX.
                   05  SUH-AUS-400-HERKUNFT-UPD        PIC X.
      *            J = Änderung Herkunftssystem

      ***  Schlüssel: DVAG-interne Vertragsnummer PIF oder PIN
                   05  SUH-AUS-400-PIN                 PIC 9(9).
                   05  SUH-AUS-400-PIN-UPD             PIC X.
      *            J = Änderung DVAG-interne Vertragsnummer PIF oder PIN

      ***  Schlüssel: Prüfziffer zur PIN
                   05  SUH-AUS-400-PIN-PRUEF           PIC 99.
                   05  SUH-AUS-400-PIN-PRUEF-UPD       PIC X.
      *            J = Änderung Prüfziffer zur PIN

      ***  Schlüssel: QuittungsNummer
                   05  SUH-AUS-400-QUITTNR             PIC 9(15).
                   05  SUH-AUS-400-QUITTNR-UPD         PIC X.
      *            J = Änderung QuittungsNr.

      ***  Schlüssel: Erstelldatum (im Format JJJJMMTT)
                   05  SUH-AUS-400-ERSTDAT             PIC 9(8).
                   05  SUH-AUS-400-ERSTDAT-UPD         PIC X.
      *            J = Änderung Erstell-Datum

      ***  Schlüssel: Erstellzeit (im Format HHMMSS)
                   05  SUH-AUS-400-ERSTZEIT            PIC 9(6).
                   05  SUH-AUS-400-ERSTZEIT-UPD        PIC X.
      *            J = Änderung Erstell-Datum

      ***  Datenfeld: Antragsnummer FeldNr. 4003
                   05  SUH-AUS-400-CANNR               PIC X(18).
                   05  SUH-AUS-400-CANNR-UPD           PIC X.
      *            J = Änderung Antragsnummer

      ***  Datenfeld: Merkmal Vermittler 1 zum Vertrag FeldNr. 4004
                   05  SUH-AUS-400-CV1MERKM            PIC X(20).
                   05  SUH-AUS-400-CV1MERKM-UPD        PIC X.
      *            J = Änderung Merkmal Vermittler 1

      ***  Datenfeld: Vermittlernummer 1 zum Vertrag FeldNr. 4005
                   05  SUH-AUS-400-CV1VTVMNR           PIC X(10).
                   05  SUH-AUS-400-CV1VTVMNR-UPD       PIC X.
      *            J = Änderung Merkmal Vermittlernummer 1

      ***  Datenfeld: Merkmal Vermittler 2 zum Vertrag FeldNr. 4006
                   05  SUH-AUS-400-CV2MERKM            PIC X(20).
                   05  SUH-AUS-400-CV2MERKM-UPD        PIC X.
      *            J = Änderung Merkmal Vermittler 2

      ***  Datenfeld: Vermittlernummer 2 zum Vertrag FeldNr. 4007
                   05  SUH-AUS-400-CV2VTVMNR           PIC X(10).
                   05  SUH-AUS-400-CV2VTVMNR-UPD       PIC X.
      *            J = Änderung Vermittlernummer 2

      ***  Datenfeld: Tarif/Produkt FeldNr. 4008
                   05  SUH-AUS-400-CPBRDNR             PIC X(50).
                   05  SUH-AUS-400-CPBRDNR-UPD         PIC X.
      *            J = Änderung Tarif/Produkt

      ***  Datenfeld: Vertragssparte FeldNr. 4010
                   05  SUH-AUS-400-NSPARTE             PIC 9(9).
                   05  SUH-AUS-400-NSPARTE-UPD         PIC X.
      *            J = Änderung Vertragssparte

      ***  Datenfeld: Vertragsart FeldNr. 4011
                   05  SUH-AUS-400-NVTART              PIC 9(9).
                   05  SUH-AUS-400-NVTART-UPD          PIC X.
      *            J = Änderung Vertragsart

      ***  Datenfeld: Vertragsbeginn Format: JJJJMMTT FeldNr. 4012
                   05  SUH-AUS-400-CVBEGINN            PIC 9(8).
                   05  SUH-AUS-400-CVBEGINN-UPD        PIC X.
      *            J = Änderung Vertragsbeginn

      ***  Datenfeld: Vertragsablauf Format: JJJJMMTT FeldNr. 4013
                   05  SUH-AUS-400-CVABLAUF            PIC 9(8).
                   05  SUH-AUS-400-CVABLAUF-UPD        PIC X.
      *            J = Änderung Vertragsablauf

      ***  Datenfeld: Vertragsstatus FeldNr. 4014
                   05  SUH-AUS-400-NVTSTATUS           PIC 9(9).
                   05  SUH-AUS-400-NVTSTATUS-UPD       PIC X.
      *            J = Änderung Vertragsstatus

      ***  Datenfeld: Versichertes Risiko FeldNr. 4015
                   05  SUH-AUS-400-CVERSRISK           PIC X(23).
                   05  SUH-AUS-400-CVERSRISK-UPD       PIC X.
      *            J = Änderung Versichertes Risiko

      ***  Datenfeld: Änderungsdatum Format: JJJJMMTT FeldNr. 4016
                   05  SUH-AUS-400-CVAEND1D            PIC 9(8).
                   05  SUH-AUS-400-CVAEND1D-UPD        PIC X.
      *            J = Änderung Änderungsdatum

      ***  Datenfeld: Änderungsgrund FeldNr. 4017
                   05  SUH-AUS-400-CVAEND1G            PIC X(50).
                   05  SUH-AUS-400-CVAEND1G-UPD        PIC X.
      *            J = Änderung Änderungsgrund

      ***  Datenfeld: AFB-Anteil in Prozent FeldNr. 4018
                   05  SUH-AUS-400-NAFBPROZ            PIC 99V999.
                   05  SUH-AUS-400-NAFBPROZ-UPD        PIC X.
      *            J = Änderung AFB-Anteil in Prozent

      ***  Datenfeld: Bündelungskennzeichen FeldNr. 4020
                   05  SUH-AUS-400-CBUEKZ              PIC X.
                   05  SUH-AUS-400-CBUEKZ-UPD          PIC X.
      *            J = Änderung Bündelungskennzeichen

      ***  Datenfeld: Vertragsinfo FeldNr. 4022
                   05  SUH-AUS-400-CVTZINFO            PIC X(80).
                   05  SUH-AUS-400-CVTZINFO-UPD        PIC X.
      *            J = Änderung Vertragsinfo

      ***  Datenfeld: Vertragssumme FeldNr. 4027
                   05  SUH-AUS-400-NVSUMME             PIC 9(10).
                   05  SUH-AUS-400-NVSUMME-UPD         PIC X.
      *            J = Änderung Vertragssumme

      ***  Datenfeld: Ursprungswährung FeldNr. 4030
                   05  SUH-AUS-400-NVTURWAEHR          PIC 9(9).
                   05  SUH-AUS-400-NVTURWAEHR-UPD      PIC X.
      *            J = Änderung Ursprungswährung

      ***  Datenfeld: Inkassoart FeldNr. 4031
                   05  SUH-AUS-400-CINKFORM            PIC X(30).
                   05  SUH-AUS-400-CINKFORM-UPD        PIC X.
      *            J = Änderung Inkassoart

      ***  Datenfeld: Versicherungszahlungsweise FeldNr. 4032
                   05  SUH-AUS-400-NINKZHLWSE          PIC 9(9).
                   05  SUH-AUS-400-NINKZHLWSE-UPD      PIC X.
      *            J = Änderung Versicherungszahlweise

      ***  Datenfeld: Beitrag gemäss Zahlungsweise FeldNr. 4033
                   05  SUH-AUS-400-NINKVSGBTR          PIC 9(10)V99.
                   05  SUH-AUS-400-NINKVSGBTR-UPD      PIC X.
      *            J = Änderung Beitrag gemäss Zahlungsweise

      ***  Datenfeld: Jahresgesamtbeitrag brutto gemäss
      ***             Zahlungsweise. FeldNr. 4034
                   05  SUH-AUS-400-NINKJGSBTR          PIC 9(10)V99.
                   05  SUH-AUS-400-NINKJGSBTR-UPD      PIC X.
      *            J = Änderung Jahresgesamtbeitrag brutto gemäss
      *                Zahlungsweise.

      ***  Datenfeld: Fälligkeitsdatum Format: JJJJMMTT FeldNr. 4035
                   05  SUH-AUS-400-CINKFLGDAT          PIC 9(8).
                   05  SUH-AUS-400-CINKFLGDAT-UPD      PIC X.
      *            J = Änderung Fälligkeitsdatum

      ***  Datenfeld: Jahresgesamtbeitrag netto FeldNr. 4036
                   05  SUH-AUS-400-NINKJGNBTR          PIC 9(8)V99.
                   05  SUH-AUS-400-NINKJGNBTR-UPD      PIC X.
      *            J = Änderung Jahresgesamtbeitrag netto

      ***  Datenfeld: Mahnstufe FeldNr. 4037
                   05  SUH-AUS-400-CINKMHNSTF          PIC X(25).
                   05  SUH-AUS-400-CINKMHNSTF-UPD      PIC X.
      *            J = Änderung Mahnstufe

      ***  Datenfeld: Mahndatum Format: JJJJMMTT FeldNr. 4038
                   05  SUH-AUS-400-CINKMHNDAT          PIC 9(8).
                   05  SUH-AUS-400-CINKMHNDAT-UPD      PIC X.
      *            J = Änderung Mahnstufe

      ***  Datenfeld: Rückstandsbetrag FeldNr. 4039
                   05  SUH-AUS-400-NINKRSBTR           PIC 9(7)V99.
                   05  SUH-AUS-400-NINKRSBTR-UPD       PIC X.
      *            J = Änderung Rückstandsbetrag

      ***  Datenfeld: Laufzeitrabatt in % FeldNr. 4040
                   05  SUH-AUS-400-NINKLFZRB           PIC 99.
                   05  SUH-AUS-400-NINKLFZRB-UPD       PIC X.
      *            J = Änderung Laufzeitrabatt in Proz.

      ***  Datenfeld: Ratenzuschlag FeldNr. 4041
                   05  SUH-AUS-400-NINKRTZS            PIC 99.
                   05  SUH-AUS-400-NINKRTZS-UPD        PIC X.
      *            J = Änderung Ratenzuschlag

      ***  Datenfeld: Zeichnung FeldNr. 4042
                   05  SUH-AUS-400-CINKZCHNNG          PIC X(25).
                   05  SUH-AUS-400-CINKZCHNNG-UPD      PIC X.
      *            J = Änderung Zeichnung

      ***  Datenfeld: F/B-Beitrag brutto FeldNr. 4043
                   05  SUH-AUS-400-NINKFBBTR           PIC 9(8)V99.
                   05  SUH-AUS-400-NINKFBBTR-UPD       PIC X.
      *            J = Änderung F/B-Beitrag brutto

      ***  Datenfeld: F/B-Summe FeldNr. 4044
                   05  SUH-AUS-400-NINKFBSUM           PIC 9(12).
                   05  SUH-AUS-400-NINKFBSUM-UPD       PIC X.
      *            J = Änderung F/B-Summe

      ***  Datenfeld: Einlösungsbeitrag brutto FeldNr. 4045
                   05  SUH-AUS-400-NINKELBTBR          PIC 9(8)V99.
                   05  SUH-AUS-400-NINKELBTBR-UPD      PIC X.
      *            J = Aenderung Einlösungsbeitrag brutto

      ***  Datenfeld: Einlösungsbeitrag netto FeldNr. 4046
                   05  SUH-AUS-400-NINKELBTNT          PIC 9(8)V99.
                   05  SUH-AUS-400-NINKELBTNT-UPD      PIC X.
      *            J = Aenderung Einlösungsbeitrag netto

      ***  Datenfeld: Letzte Synchronisation Format: JJJJMMTT
      ***             FeldNr. 4047
                   05  SUH-AUS-400-CINKLSTSYN          PIC 9(8).
                   05  SUH-AUS-400-CINKLSTSYN-UPD      PIC X.
      *            J = Aenderung Letzte Synchronisation

      ***  Datenfeld: Letzte Agenturbearbeitung Format: JJJJMMTT
      ***             FeldNr. 4048
                   05  SUH-AUS-400-CINKLSTAGT          PIC 9(8).
                   05  SUH-AUS-400-CINKLSTAGT-UPD      PIC X.
      *            J = Aenderung Letzte Agenturbearbeitung

      ***  Datenfeld: Letzte Bewegung FeldNr. 4049
                   05  SUH-AUS-400-CINKLSTBWG          PIC X(61).
                   05  SUH-AUS-400-CINKLSTBWG-UPD      PIC X.
      *            J = Aenderung Letzte Bewegung

      ***  Datenfeld: Kundenbonus FeldNr. 4050
                   05  SUH-AUS-400-CINKKDBON           PIC X(26).
                   05  SUH-AUS-400-CINKKDBON-UPD       PIC X.
      *            J = Aenderung Kundenbonus

      ***  Datenfeld: Ostrabatt FeldNr. 4051
                   05  SUH-AUS-400-NINKOSTRBT          PIC 99.
                   05  SUH-AUS-400-NINKOSTRBT-UPD      PIC X.
      *            J = Aenderung Ostrabatt

      ***  Datenfeld: Kurztext Bestandssicherungsaktion FeldNr. 4094
                   05  SUH-AUS-400-CINKBSA             PIC X(6).
                   05  SUH-AUS-400-CINKBSA-UPD         PIC X.
      *            J = Aenderung Kurztext Bestandssicherungsaktion

      ***  Datenfeld: Bankleitzahl FeldNr. 4100
                   05  SUH-AUS-400-NINKBLZ             PIC 9(8).
                   05  SUH-AUS-400-NINKBLZ-UPD         PIC X.
      *            J = Aenderung Bankleitzahl

      ***  Datenfeld: Kontonummer FeldNr. 4101
                   05  SUH-AUS-400-CINKKTONR           PIC X(15).
                   05  SUH-AUS-400-CINKKTONR-UPD       PIC X.
      *            J = Aenderung Kontonummer

      ***  Datenfeld: Kontoinhaber FeldNr. 4102
                   05  SUH-AUS-400-CINKKTOINH          PIC X(40).
                   05  SUH-AUS-400-CINKKTOINH-UPD      PIC X.
      *            J = Aenderung Kontoinhaber

      ***  Datenfeld: Langtext Bestandssicherungsaktion FeldNr. 4110
                   05  SUH-AUS-400-CVGBSALTXT          PIC X(50).
                   05  SUH-AUS-400-CVGBSALTXT-UPD      PIC X.
      *            J = Aenderung Langtext Bestandssicherungsaktion

      ***  Datenfeld: Versicherungsgesellschaft Text FeldNr. 4111
                   05  SUH-AUS-400-CGESELLST           PIC X(50).
                   05  SUH-AUS-400-CGESELLST-UPD       PIC X.
      *            J = Aenderung Versicherungsgesellschaft Text

      ***  Datenfeld: VD-Nr. FeldNr. 4112
                   05  SUH-AUS-400-NVDNR               PIC 9(5).
                   05  SUH-AUS-400-NVDNR-UPD           PIC X.
      *            J = Aenderung VD-Nr.

      ***  Datenfeld: VD-Name FeldNr. 4113
                   05  SUH-AUS-400-CVDNAME             PIC X(20).
                   05  SUH-AUS-400-CVDNAME-UPD         PIC X.
      *            J = Aenderung VD-Name

      ***  Datenfeld: Vor-Vertragsnummer FeldNr. 4114
                   05  SUH-AUS-400-CVORVNR             PIC X(17).
                   05  SUH-AUS-400-CVORVNR-UPD         PIC X.
      *            J = Aenderung Vor-Vertragsnummer

120416***  Datenfeld: Aktionsergebnis FeldNr. 4119
                   05  SUH-AUS-400-AKTIONERG        PIC X(3).
                   05  SUH-AUS-400-AKTIONERG-UPD    PIC X.
      *            J = Aenderung AKTIONERG BMG

120516***  Datenfeld: Produktkombibezeichnung 4150
                   05  SUH-AUS-400-PRDKOMBIBEZ      PIC X(50).
                   05  SUH-AUS-400-PRDKOMBIBEZ-UPD  PIC X.
      *            J = Aenderung AKTIONERG BMG

      ***  Datenfeld: Smart-Home KundenID Feld Nr. 4151
080816             05  SUH-AUS-400-KUNDENIDSMH        PIC X(10).
080816             05  SUH-AUS-400-KUNDENIDSMH-UPD    PIC X.

      ***  Datenfeld: VK-FLEX-VSP-Nachlass Feld 4156
010217             05 SUH-AUS-400-FLEXNVSP            PIC 9(9)V9(2).
010217             05 SUH-AUS-400-FLEXNVSP-UPD        PIC X.

      ***  Datenfeld: VK-FLEX-VSP-Nachlass gewährt Feld 4157
010217             05 SUH-AUS-400-FLEXNVSPGW          PIC X.
010217             05 SUH-AUS-400-FLEXNVSPGW-UPD      PIC X.

      ***  Datenfeld: VK-FLEX-VGV-Nachlass Feld 4158
010217             05 SUH-AUS-400-FLEXNVGV            PIC 9(9)V9(2).
010217             05 SUH-AUS-400-FLEXNVGV-UPD        PIC X.

      ***  Datenfeld: VK-FLEX-VGV-Nachlass gewährt Feld 4159
010217             05 SUH-AUS-400-FLEXNVGVGW          PIC X.
010217             05 SUH-AUS-400-FLEXNVGVGW-UPD      PIC X.

      ***Datenfeld: PKBS-KBEZ (VSPKZ) Feld 4152
010217             05 SUH-AUS-400-VSPKZ               PIC X.
010217             05 SUH-AUS-400-VSPKZ-UPD           PIC X.

      *** VSP-Kennzeichen "Original" - also das was GEDVers tatsächlich
      *** liefert und nicht das daraus abgeleitete J/N aus dem Feld
      *** 400 4152 obendrüber
                   05 SUH-AUS-400-VSP-KLARTEXT        PIC X(03).
                   05 SUH-AUS-400-VSP-KLARTEXT-UPD    PIC X(01).

      ***  Datenfeld: Leerstellen
120416****         05  SUH-AUS-400-LEERSTELLEN         PIC X(595).
120516****         05  SUH-AUS-400-LEERSTELLEN         PIC X(544).
080816****         05  SUH-AUS-400-LEERSTELLEN         PIC X(533).
010217****         05  SUH-AUS-400-LEERSTELLEN         PIC X(505).
                   05  SUH-AUS-400-LEERSTELLEN         PIC X(499).

      ******************************************************************
      *                                                                *
      *                   E I N Z E L V E R T R A G                    *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 15.08.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-401-EINZELVERTRAG.

                   05  SUH-AUS-401-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
      *
      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-401-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 4066
                   05  SUH-AUS-401-NGESELLST           PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 4067
                   05  SUH-AUS-401-CVNR                PIC 9(15).

      ***  Schlüssel: VEZ-Host-ID FeldNr. 4064
                   05  SUH-AUS-401-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: VEZ-Sparte FeldNr. 4057
                   05  SUH-AUS-401-NVEZSPARTE          PIC 9(9).
                   05  SUH-AUS-401-NVEZSPARTE-UPD      PIC X.
      *            J = Aenderung VEZ-Sparte

      ***  Datenfeld: VEZ-Tarif/Produkt FeldNr. 4058
                   05  SUH-AUS-401-NVEZPBRDNR          PIC 9(9).
                   05  SUH-AUS-401-NVEZPBRDNR-UPD      PIC X.
      *            J = Aenderung VEZ-Tarif/Produkt

      ***  Datenfeld: VEZ-Beginn Format: JJJJMMTT FeldNr. 4059
                   05  SUH-AUS-401-CVEZBEGINN          PIC 9(8).
                   05  SUH-AUS-401-CVEZBEGINN-UPD      PIC X.
      *            J = Aenderung VEZ-Beginn

      ***  Datenfeld: VEZ-Ablauf Format: JJJJMMTT FeldNr. 4060
                   05  SUH-AUS-401-CVEZABLAUF          PIC 9(8).
                   05  SUH-AUS-401-CVEZABLAUF-UPD      PIC X.
      *            J = Aenderung VEZ-Ablauf

      ***  Datenfeld: VEZ-Status FeldNr. 4061
      ***             Dieses Feld hat vom Inhalt her die selben Werte
      ***             wie das Datenfeld: Vertragsstatus FeldNr. 4014
      ***             "AF-REOB-DTA-400-NVTSTATUS" in der Satzart 400.
      ***             Jedoch können keine Querverbindungen zum Inhalt
      ***             des Feldes in der SA400 geprüft werden.
                   05  SUH-AUS-401-NVEZSTATUS          PIC 9(9).
                   05  SUH-AUS-401-NVEZSTATUS-UPD      PIC X.
      *            J = Aenderung VEZ-STATUS

      ***  Datenfeld: VEZ-Bruttobeitrag gem. ZW FeldNr. 4062
                   05  SUH-AUS-401-NVEZBTRG            PIC 9(8)V99.
                   05  SUH-AUS-401-NVEZBTRG-UPD        PIC X.
      *            J = Aenderung VEZ-Bruttobeitrag

      ***  Datenfeld: VEZ-Summe FeldNr. 4063
                   05  SUH-AUS-401-NVEZSUMME           PIC 9(10).
                   05  SUH-AUS-401-NVEZSUMME-UPD       PIC X.
      *            J = Aenderung VEZ-SUMME

      ***  Datenfeld: Vertragsart FeldNr. 4065
                   05  SUH-AUS-401-NVTART              PIC 9(9).
                   05  SUH-AUS-401-NVTART-UPD          PIC X.
      *            J = Aenderung Vertragsart

      ***  Datenfeld: VEZ-Aenderungsgrund FeldNr. 4095
                   05  SUH-AUS-401-CVEZAENDG           PIC X(50).
                   05  SUH-AUS-401-CVEZAENDG-UPD       PIC X.
      *            J = Aenderung VEZ-Aenderungsgrund

      ***  Datenfeld: VEZ-Aenderungsdatum Format: JJJJMMTT FeldNr. 4096
                   05  SUH-AUS-401-CVEZAENDDT          PIC 9(8).
                   05  SUH-AUS-401-CVEZAENDDT-UPD      PIC X.
      *            J = Aenderung VEZ-Aenderungsdatum

      ***  Datenfeld: VEZ-Nettojahresbeitrag FeldNr. 4106
                   05  SUH-AUS-401-NVEZNJB             PIC 9(8)V99.
                   05  SUH-AUS-401-NVEZNJB-UPD         PIC X.
      *            J = Aenderung VEZ-Nettojahresbeitrag

      ***  Datenfeld: VEZ-Bruttojahresbeitrag FeldNr. 4107
                   05  SUH-AUS-401-NVEZBJB             PIC 9(8)V99.
                   05  SUH-AUS-401-NVEZBJB-UPD         PIC X.
      *            J = Aenderung VEZ-Bruttojahresbeitrag

      ***  Datenfeld: VEZ-Nettobeitrag gem. ZW FeldNr. 4108
                   05  SUH-AUS-401-NVEZFBNZW           PIC 9(8)V99.
                   05  SUH-AUS-401-NVEZFBNZW-UPD       PIC X.
      *            J = Aenderung VEZ-Nettobeitrag gem. ZW

      ***  Datenfeld: VEZ-Datum der letzten Bearbeitung Format: JJJJMMTT
      ***             FeldNr. 4109
                   05  SUH-AUS-401-CVEZLBEARB          PIC 9(8).
                   05  SUH-AUS-401-CVEZLBEARB-UPD      PIC X.
      *            J = Aenderung VEZ-Datum der letzten Bearbeitung

      ***  Datenfeld: Geschäftszweig FeldNr. 4115 (?FK?, ?PK?)
                   05  SUH-AUS-401-CGZWEIG             PIC XX.

100610*** Datenfeld: Rabattierter Jahresnettobeitr. Feldnr. 9301
      ***    inkl. TZ-Zuschlag exkl. Steuer
                   05  SUH-AUS-401-RABNETTO          PIC 9(8)V99.
                   05  SUH-AUS-401-RABNETTO-UPD      PIC X.
      ***  Datenfeld: Summe aller Komponentenbeiträge Feldnr. 9302
                   05  SUH-AUS-401-STAKTBTR          PIC 9(8)V99.
                   05  SUH-AUS-401-STAKTBTR-UPD      PIC X.
220211***  Datenfeld: REOBPRODUKTNUMMER  Feldnr. 4164
220211             05  SUH-AUS-401-NREOBPRDNR        PIC 9(5).
220211             05  SUH-AUS-401-NREOBPRDNR-UPD    PIC X.
120516***  Datenfeld: Produktbezeichnung Feldnr. 4170
120516             05  SUH-AUS-401-PRODUKTBEZ        PIC X(50).
120516             05  SUH-AUS-401-PRODUKTBEZ-UPD    PIC X.
      ***  Datenfeld: VEZ-FlexN Betrag FeldNr. 4160
                   05  SUH-AUS-401-FLEXNSP           PIC 9(9)V99.
                   05  SUH-AUS-401-FLEXNSP-UPD       PIC X.
      *            J = Aenderung VEZ-FLEXNSP
      ***  Datenfeld: VEZ-Bruttobeitrag gem. ZW FeldNr. 4062
                   05  SUH-AUS-401-FLEXNSPGW         PIC X.
                   05  SUH-AUS-401-FLEXNSPGW-UPD     PIC X.
      *            J = Aenderung VEZ-FLEXNSPGW
      ***  Datenfeld: Leerstellen
100610******       05  SUH-AUS-401-LEERSTELLEN         PIC X(1347).
220211******       05  SUH-AUS-401-LEERSTELLEN         PIC X(1341).
120516******       05  SUH-AUS-401-LEERSTELLEN         PIC X(1290).
                   05  SUH-AUS-401-LEERSTELLEN         PIC X(1276).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-401-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *           V E R T R A G S R I S I K O A D R E S S E            *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************

       01 SUH-AUS-403-RISIKOADRESSE.

                   05  SUH-AUS-403-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
      *
      ***  Schlüssel: Nummer der ALLFINANZ-Satzart FeldNr.
                   05  SUH-AUS-403-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6359
                   05  SUH-AUS-403-NGESELLST           PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 6365
                   05  SUH-AUS-403-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6359
                   05  SUH-AUS-403-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikoadresse-Host-ID FeldNr. 6366
                   05  SUH-AUS-403-NRISADRHID          PIC 9(9).

      ***  Datenfeld: Strasse FeldNr. 6367
                   05  SUH-AUS-403-CRISADRSTR          PIC X(40).
                   05  SUH-AUS-403-CRISADRSTR-UPD      PIC X.
      *            J = Aenderung Strasse

      ***  Datenfeld: PLZ FeldNr. 6368
                   05  SUH-AUS-403-CRISADRPLZ          PIC X(6).
                   05  SUH-AUS-403-CRISADRPLZ-UPD      PIC X.
      *            J = Aenderung PLZ

      ***  Datenfeld: Ort FeldNr. 6369
                   05  SUH-AUS-403-CRISADRORT          PIC X(40).
                   05  SUH-AUS-403-CRISADRORT-UPD      PIC X.
      *            J = Aenderung Ort

      ***  Datenfeld: Länderkennzeichen FeldNr. 6370
                   05  SUH-AUS-403-CRISADRLKZ          PIC 9(9).
                   05  SUH-AUS-403-CRISADRLKZ-UPD      PIC X.
      *            J = Aenderung Länderkennzeichen

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-403-LEERSTELLEN           PIC X(1449).

091208******************************************************************
    >>*                                                                *
      *                504 - P K - R E I S E G E P A E C K -           *
      *                    V E R S I C H E R U N G                     *
      *     LAENGE:  505 BYTES genutzt           STAND: 09.12.2008     *
      *     LAENGE: 1600 BYTES gesamt                                  *
      ******************************************************************
       01 SUH-AUS-504-REISEGEPAECK.

          05  SUH-AUS-504-VERARB-KNZ              PIC X.
      *       Verarbeitungskennzeichen
      *       I = Insert U = Update D = Delete
      *
      ***  Headerinformation: Nummer der Satzart
          05 SUH-AUS-504-SATZART                  PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 9188
          05 SUH-AUS-504-NGESELLST                PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 9189
          05 SUH-AUS-504-CVNR                     PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9191
          05 SUH-AUS-504-CVEZHOSTNR               PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 9177
          05 SUH-AUS-504-BEDINGUNG                PIC X(30).
          05 SUH-AUS-504-BEDINGUNG-UPD            PIC X.
      *      J = Aenderung Bedingung

      ***  Datenfeld: Versicherungssumme. FeldNr. 9178
          05 SUH-AUS-504-VERSSUMME                PIC 9(8).
          05 SUH-AUS-504-VERSSUMME-UPD            PIC X.
      *      J = Aenderung Versicherungssumme

      ***  Datenfeld: Mailingaktion FeldNr. 9179
          05 SUH-AUS-504-MAILING                  PIC X(70).
          05 SUH-AUS-504-MAILING-UPD              PIC X.
      *      J = Aenderung Mailingaktion

      ***  Datenfeld: Zusatzinfo FeldNr. 9180
          05 SUH-AUS-504-ZUSATZINFO               PIC X(100).
          05 SUH-AUS-504-ZUSATZINFO-UPD           PIC X.
      *      J = Aenderung Zusatzinfo

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9181
          05 SUH-AUS-504-DIFFDECK                 PIC X(2).
          05 SUH-AUS-504-DIFFDECK-UPD             PIC X.
      *      J = Aenderung Differenzdeckung

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9182
          05 SUH-AUS-504-GESELLSCHAFT             PIC X(50).
          05 SUH-AUS-504-GESELLSCH-UPD            PIC X.
      *      J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 9183
          05 SUH-AUS-504-DDVERSNR                 PIC X(40).
          05 SUH-AUS-504-DDVERSNR-UPD             PIC X.
      *      J = Aenderung Versicherungssumme

      ***  Datenfeld: Umstellungstermin   FeldNr. 9184
          05 SUH-AUS-504-UMSTELLTERMIN            PIC 9(8).
          05 SUH-AUS-504-UMSTELLTRM-UPD           PIC X.
      *      J = Aenderung Umstellungstermin

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9185
          05 SUH-AUS-504-DDKUENDBEST              PIC X(2).
          05 SUH-AUS-504-DDKUENDBST-UPD           PIC X.
      *      J = Aenderung Kuendigungsbestaetigung Ja/Nein

      ***  Datenfeld: Leerstellen
          05 SUH-AUS-504-LEERSTELLEN              PIC X(1234).

    <<***  Datenfeld: Interne lfd. Nummer für die Datenbank
091208    05  SUH-AUS-504-INTLFDNR                PIC 9(4).


180416******************************************************************
      *                                                                *
      *                505 - R E I S E                                 *
      *                    V E R S I C H E R U N G                     *
      *     LAENGE:  385 BYTES genutzt           STAND: 18.04.2016     *
      *     LAENGE: 1600 BYTES gesamt                                  *
      ******************************************************************
       01 SUH-AUS-505-REISEVERSICHERUNG.

          05  SUH-AUS-505-VERARB-KNZ              PIC X.
      *       Verarbeitungskennzeichen
      *       I = Insert U = Update D = Delete
      *
      ***  Headerinformation: Nummer der Satzart
          05 SUH-AUS-505-SATZART                  PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 9252
          05 SUH-AUS-505-NGESELLST                PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 9250
          05 SUH-AUS-505-CVNR                     PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9251
          05 SUH-AUS-505-CVEZHOSTNR               PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 9260
          05 SUH-AUS-505-BEDINGUNG                PIC X(30).
          05 SUH-AUS-505-BEDINGUNG-UPD            PIC X.
      *      J = Aenderung Bedingung

      ***  Datenfeld: Versicherungssumme. Reisegepäck FeldNr. 9261
          05 SUH-AUS-505-VSUMRGEPCK               PIC 9(8).
          05 SUH-AUS-505-VSUMRGEPCK-UPD           PIC X.
      *      J = Aenderung Versicherungssumme Reisegepäck

      ***  Datenfeld: Versicherungssumme. Reiserücktritt FeldNr. 9262
          05 SUH-AUS-505-VSUMRRUECK               PIC 9(8).
          05 SUH-AUS-505-VSUMRRUECK-UPD           PIC X.
      *      J = Aenderung Versicherungssumme Reiserücktritt

      ***  Datenfeld: Mailingaktion FeldNr. 9663
          05 SUH-AUS-505-MAILING                  PIC X(70).
          05 SUH-AUS-505-MAILING-UPD              PIC X.
      *      J = Aenderung Mailingaktion

      ***  Datenfeld: Zusatzinfo FeldNr. 9664
          05 SUH-AUS-505-ZUSATZINFO               PIC X(100).
          05 SUH-AUS-505-ZUSATZINFO-UPD           PIC X.
      *      J = Aenderung Zusatzinfo

      ***  Datenfeld: Informationsquelle FeldNr. 9665
          05 SUH-AUS-505-LVTQUELLP                PIC 9(9).
          05 SUH-AUS-505-LVTQUELLP-UPD            PIC X.
      *      J = Aenderung Informationsquelle

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9670
          05 SUH-AUS-505-DIFFDECK                 PIC X(2).
          05 SUH-AUS-505-DIFFDECK-UPD             PIC X.
      *      J = Aenderung Differenzdeckung

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9671
          05 SUH-AUS-505-GESELLSCHAFT             PIC X(50).
          05 SUH-AUS-505-GESELLSCH-UPD            PIC X.
      *      J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 9672
          05 SUH-AUS-505-DDVERSNR                 PIC X(40).
          05 SUH-AUS-505-DDVERSNR-UPD             PIC X.
      *      J = Aenderung Versicherungssumme

      ***  Datenfeld: Umstellungstermin   FeldNr. 9673
          05 SUH-AUS-505-UMSTELLTERMIN            PIC 9(8).
          05 SUH-AUS-505-UMSTELLTRM-UPD           PIC X.
      *      J = Aenderung Umstellungstermin

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9674
          05 SUH-AUS-505-DDKUENDBEST              PIC X(2).
          05 SUH-AUS-505-DDKUENDBST-UPD           PIC X.
      *      J = Aenderung Kuendigungsbestaetigung Ja/Nein

      ***  Datenfeld: Leerstellen
          05 SUH-AUS-504-LEERSTELLEN              PIC X(1215).

    <<***  Datenfeld: Interne lfd. Nummer für die Datenbank
091208    05  SUH-AUS-505-INTLFDNR                PIC 9(4).


210108******************************************************************
>>    *                                                                *
      *                    G L A S - V E R T R A G                     *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-508-GLAS.

                   05  SUH-AUS-508-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
      *
      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-508-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5293
                   05  SUH-AUS-508-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5294
                   05  SUH-AUS-508-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5292
                   05  SUH-AUS-508-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5276
                   05  SUH-AUS-508-CGLRSPLZ            PIC X(6).
                   05  SUH-AUS-508-CGLRSPLZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5277
                   05  SUH-AUS-508-CGLRSSTR            PIC X(30).
                   05  SUH-AUS-508-CGLRSSTR-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: STRASSE

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5278
                   05  SUH-AUS-508-CGLRSORT            PIC X(30).
                   05  SUH-AUS-508-CGLRSORT-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: ORT

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5279
                   05  SUH-AUS-508-CGLRSHSNR           PIC X(15).
                   05  SUH-AUS-508-CGLRSHSNR-UPD       PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer

      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5280
                   05  SUH-AUS-508-NGLRSNKZ            PIC 9(9).
                   05  SUH-AUS-508-NGLRSNKZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: L{nderkennzeichen

      ***  Datenfeld: Klausel/Tarif FeldNr. 5281
                   05  SUH-AUS-508-CGLKLAUS            PIC X(80).
                   05  SUH-AUS-508-CGLKLAUS-UPD        PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Bedingung FeldNr. 5282
                   05  SUH-AUS-508-CGLLBVBEDG          PIC X(16).
                   05  SUH-AUS-508-CGLLBVBEDG-UPD      PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Geb{udetyp FeldNr. 5283
                   05  SUH-AUS-508-NGLLBGEBTY          PIC 9(9).
                   05  SUH-AUS-508-NGLLBGEBTY-UPD      PIC X.
      *            J = Aenderung Geb{udetyp

      ***  Datenfeld: Risiko FeldNr. 5285
                   05  SUH-AUS-508-CGLLBRIS            PIC X(50).
                   05  SUH-AUS-508-CGLLBRIS-UPD        PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5286
                   05  SUH-AUS-508-NGLJNMBTR           PIC XX.
                   05  SUH-AUS-508-NGLJNMBTR-UPD       PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: SB-Prz FeldNr. 5287
                   05  SUH-AUS-508-NGLSBPRZ            PIC 99V99.
                   05  SUH-AUS-508-NGLSBPRZ-UPD        PIC X.
      *            J = Aenderung SB-Prz.

      ***  Datenfeld: SB-Fest FeldNr. 5288
                   05  SUH-AUS-508-NGLSBFES            PIC 9(5).
                   05  SUH-AUS-508-NGLSBFES-UPD        PIC X.
      *            J = Aenderung SB-Fest

      ***  Datenfeld: Zusatzinfo FeldNr. 5289
                   05  SUH-AUS-508-CGLZUSINFO          PIC X(100).
                   05  SUH-AUS-508-CGLZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5290
                   05  SUH-AUS-508-CGLDTLSTFV          PIC 9(8).
                   05  SUH-AUS-508-CGLDTLSTFV-UPD      PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5291
                   05  SUH-AUS-508-CGLDTLSTFB          PIC 9(8).
                   05  SUH-AUS-508-CGLDTLSTFB-UPD      PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: Mailingaktion FeldNr. 5940
                   05  SUH-AUS-508-CMAILING            PIC X(70).
                   05  SUH-AUS-508-CMAILING-UPD        PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5533
                   05  SUH-AUS-508-NDIFFDECKJN         PIC X(2).
                   05  SUH-AUS-508-DIFFDECKJN-UPD      PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5534
                   05  SUH-AUS-508-CUMSTELLTERM        PIC 9(8).
                   05  SUH-AUS-508-CUMSTTERM-UPD       PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5536
                   05  SUH-AUS-508-CGESELL             PIC X(50).
                   05  SUH-AUS-508-CGESELL-UPD         PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5537
                   05  SUH-AUS-508-CDIFFVSNR           PIC X(40).
                   05  SUH-AUS-508-CDIFFVSNR-UPD       PIC X.
      *            J = Aenderung V dersicherungsNr.

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5538
                   05  SUH-AUS-508-NKUENDBEST          PIC X(2).
                   05  SUH-AUS-508-NKUENDBEST-UPD      PIC X.
      *            J = Aenderung V dersicherungsNr.

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-508-INTLFDNR            PIC 9(4).

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-508-LEERSTELLEN         PIC X(988).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-508-INTLFDNR            PIC 9(4).

      *****************************************************************
      *                                                               *
      *             H A F T P F L I C H T - V E R T R A G             *
      *                                                               *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                               *
      *****************************************************************
       01 SUH-AUS-509-HAFTPFLICHT.

                   05  SUH-AUS-509-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-509-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5321
                   05  SUH-AUS-509-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5322
                   05  SUH-AUS-509-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5320
                   05  SUH-AUS-509-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 5297
                   05  SUH-AUS-509-CHFVBEDG            PIC X(16).
                   05  SUH-AUS-509-CHFVBEDG-UPD        PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risiko FeldNr. 5298
                   05  SUH-AUS-509-CHFLBRIS            PIC X(50).
                   05  SUH-AUS-509-CHFLBRIS-UPD        PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Klausel/Tarif FeldNr. 5299
                   05  SUH-AUS-509-CHFKLAUS            PIC X(80).
                   05  SUH-AUS-509-CHFKLAUS-UPD        PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Zusatzinfo FeldNr. 5301
                   05  SUH-AUS-509-CHFZUSINFO          PIC X(100).
                   05  SUH-AUS-509-CHFZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5302
                   05  SUH-AUS-509-CHFRSPLZ            PIC X(6).
                   05  SUH-AUS-509-CHFRSPLZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5303
                   05  SUH-AUS-509-CHFRSSTR            PIC X(30).
                   05  SUH-AUS-509-CHFRSSTR-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Strasse

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5304
                   05  SUH-AUS-509-CHFRSORT            PIC X(30).
                   05  SUH-AUS-509-CHFRSORT-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Ort

      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5305
                   05  SUH-AUS-509-NHFRSNKZ            PIC 9(9).
                   05  SUH-AUS-509-NHFRSNKZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: L{nderkennzeichen

      ***  Datenfeld: Mitversicherte Personen FeldNr. 5306
                   05  SUH-AUS-509-CHFMVPERS           PIC X(35).
                   05  SUH-AUS-509-CHFMVPERS-UPD       PIC X.
      *            J = Aenderung Mitversicherte Person

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5307
                   05  SUH-AUS-509-CHFRSHSNR           PIC X(15).
                   05  SUH-AUS-509-CHFRSHSNR-UPD       PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer

      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 5308
                   05  SUH-AUS-509-NHFJNREGUL          PIC XX.
                   05  SUH-AUS-509-NHFJNREGUL-UPD      PIC X.
      *            J = Aenderung Regulierung Ja/Nein

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5309
                   05  SUH-AUS-509-NHFJNMBTR           PIC XX.
                   05  SUH-AUS-509-NHFJNMBTR-UPD       PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5310
                   05  SUH-AUS-509-CHFDTLSTFV          PIC 9(8).
                   05  SUH-AUS-509-CHFDTLSTFV-UPD      PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5311
                   05  SUH-AUS-509-CHFDTLSTFB          PIC 9(8).
                   05  SUH-AUS-509-CHFDTLSTFB-UPD      PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: SB Ja/Nein FeldNr. 5312
                   05  SUH-AUS-509-NHFJNSB             PIC XX.
                   05  SUH-AUS-509-NHFJNSB-UPD         PIC X.
      *            J = Aenderung SB ja/Nein

      ***  Datenfeld: SB-Prz FeldNr. 5313
                   05  SUH-AUS-509-NHFSBPRZ            PIC 999.
                   05  SUH-AUS-509-NHFSBPRZ-UPD        PIC X.
      *            J = Aenderung SB Prz

      ***  Datenfeld: SB-Fest FeldNr. 5314
                   05  SUH-AUS-509-NHFSBFES            PIC 9(5).
                   05  SUH-AUS-509-NHFSBFES-UPD        PIC X.
      *            J = Aenderung SB Fest

      ***  Datenfeld: SB-Text FeldNr. 5315
                   05  SUH-AUS-509-CHFSBTXT            PIC X(50).
                   05  SUH-AUS-509-CHFSBTXT-UPD        PIC X.
      *            J = Aenderung SB Test

      ***  Datenfeld: Auslandsch{den Ja/Nein FeldNr. 5316
                   05  SUH-AUS-509-NHFJNAUSL           PIC XX.
                   05  SUH-AUS-509-NHFJNAUSL-UPD       PIC X.
      *            J = Aenderung Auslandsch{den

      ***  Datenfeld: Verwandschaftssch{den Ja/Nein FeldNr. 5317
                   05  SUH-AUS-509-NHFJNVERW           PIC XX.
                   05  SUH-AUS-509-NHFJNVERW-UPD       PIC X.
      *            J = Aenderung Verwandschaftssch{den Ja/Nein

      ***  Datenfeld: Produkt FeldNr. 5318
                   05  SUH-AUS-509-CHFPRD              PIC X(30).
                   05  SUH-AUS-509-CHFPRD-UPD          PIC X.
      *            J = Aenderung Produkt

      ***  Datenfeld: Namen Elternteil/vollj{hrige Kinder FeldNr. 5319
                   05  SUH-AUS-509-CHNAMENEK           PIC X(45).
                   05  SUH-AUS-509-CHNAMENEK-UPD       PIC X.
      *            J = Aenderung Namen Elternteil/vollj{hrige Kinder

      ***  Datenfeld: Namen Elternteil / vollj{hrige Kinder FeldNr. 5859
                   05  SUH-AUS-509-CHNAMEKIND          PIC X(45).
                   05  SUH-AUS-509-CHNAMEKIND-UPD      PIC X.
      *            J = Aenderung Namen Elternteil/vollj{hrige Kinder

      ***  Datenfeld: abweichende VP FeldNr. 5860
                   05  SUH-AUS-509-CHNAMEABVP          PIC X(35).
                   05  SUH-AUS-509-CHNAMEABVP-UPD      PIC X.
      *            J = Aenderung abweichende VP

      ***  Datenfeld: Mailingaktion FeldNr. 5941
                   05  SUH-AUS-509-CMAILING            PIC X(70).
                   05  SUH-AUS-509-CMAILING-UPD        PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5527
                   05  SUH-AUS-509-NDIFFDECKJN         PIC X(2).
                   05  SUH-AUS-509-DIFFDECKJN-UPD      PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5528
                   05  SUH-AUS-509-CUMSTELLTERM        PIC 9(8).
                   05  SUH-AUS-509-UMSTELLTRM-UPD      PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5530
                   05  SUH-AUS-509-CGESELL             PIC X(50).
                   05  SUH-AUS-509-CGESELL-UPD         PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5531
                   05  SUH-AUS-509-CDIFFVSNR           PIC X(40).
                   05  SUH-AUS-509-CDIFFVSNR-UPD       PIC X.
      *            J = Aenderung  Versicherungsnummer

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5532
                   05  SUH-AUS-509-NKUENDBEST          PIC X(2).
                   05  SUH-AUS-509-NKUENDBEST-UPD      PIC X.
      *            J = Aenderung K}ndigungsbest{tigung Ja/Nein

      ***  Datenfeld: Zielgruppe FeldNr. 5987
                   05  SUH-AUS-509-CZIELGRP            PIC X(30).
                   05  SUH-AUS-509-CZIELGRP-UPD        PIC X.
      *            J = Aenderung Zielgruppe

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-509-INTLFDNR            PIC 9(4).

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-509-LEERSTELLEN         PIC X(710).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-509-INTLFDNR            PIC 9(4).

      *****************************************************************
      *                                                               *
      *                 H A U S R A T - V E R T R A G                 *
      *                                                               *
      *     LAENGE: 1.600 Bytes                STAND: 26.06.2007      *
      *                                                               *
      *****************************************************************
       01 SUH-AUS-510-HAUSRAT.

                   05  SUH-AUS-510-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-510-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5361
                   05  SUH-AUS-510-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5362
                   05  SUH-AUS-510-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5360
                   05  SUH-AUS-510-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5325
                   05  SUH-AUS-510-CHRRSPLZ            PIC X(6).
                   05  SUH-AUS-510-CHRRSPLZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5326
                   05  SUH-AUS-510-CHRRSSTR            PIC X(30).
                   05  SUH-AUS-510-CHRRSSTR-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Strasse

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5327
                   05  SUH-AUS-510-CHRRSORT            PIC X(30).
                   05  SUH-AUS-510-CHRRSORT-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Ort

      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5328
                   05  SUH-AUS-510-NHRRSNKZ            PIC 9(9).
                   05  SUH-AUS-510-NHRRSNKZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: L{nderkennzeichen

      ***  Datenfeld: Bedingung FeldNr. 5329
                   05  SUH-AUS-510-CHRLBVBEDG          PIC X(16).
                   05  SUH-AUS-510-CHRLBVBEDG-UPD      PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5330
                   05  SUH-AUS-510-CHRRSHSNR           PIC X(15).
                   05  SUH-AUS-510-CHRRSHSNR-UPD       PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer

      ***  Datenfeld: Klausel/Tarif FeldNr. 5331
                   05  SUH-AUS-510-CHRLBKLAUS          PIC X(80).
                   05  SUH-AUS-510-CHRLBKLAUS-UPD      PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Zusatzinfo FeldNr. 5333
                   05  SUH-AUS-510-CHRZUSINFO          PIC X(100).
                   05  SUH-AUS-510-CHRZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5334
                   05  SUH-AUS-510-CHRDTLSTFV          PIC 9(8).
                   05  SUH-AUS-510-CHRDTLSTFV-UPD      PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5335
                   05  SUH-AUS-510-CHRDTLSTFB          PIC 9(8).
                   05  SUH-AUS-510-CHRDTLSTFB-UPD      PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5336
                   05  SUH-AUS-510-NHRJNSB             PIC XX.
                   05  SUH-AUS-510-NHRJNSB-UPD         PIC X.
      *            J = Aenderung SelbstBeteiligung Ja/Nein

      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5337
                   05  SUH-AUS-510-NHRSBPRZ            PIC 999.
                   05  SUH-AUS-510-NHRSBPRZ-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung Prozent

      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5338
                   05  SUH-AUS-510-NHRSBFES            PIC 9(5).
                   05  SUH-AUS-510-NHRSBFES-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung Fest

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5339
                   05  SUH-AUS-510-CHRSBTXT            PIC X(50).
                   05  SUH-AUS-510-CHRSBTXT-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung Text

      ***  Datenfeld: Nutzung FeldNr. 5340
                   05  SUH-AUS-510-NHRLBNUTZ           PIC 9(9).
                   05  SUH-AUS-510-NHRLBNUTZ-UPD       PIC X.
      *            J = Aenderung Nutzung

      ***  Datenfeld: Versicherungssumme FeldNr. 5341
                   05  SUH-AUS-510-NHRVSUM             PIC 9(8).
                   05  SUH-AUS-510-NHRVSUM-UPD         PIC X.
      *            J = Aenderung Versicherungssumme

      ***  Datenfeld: Wohnfl{che in qm FeldNr. 5342
                   05  SUH-AUS-510-NHRWOHNFL           PIC 9(5).
                   05  SUH-AUS-510-NHRWOHNFL-UPD       PIC X.
      *            J = Aenderung Wohnfl{che im qm

      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5343
                   05  SUH-AUS-510-NHRJNUVZ            PIC XX.
                   05  SUH-AUS-510-NHRJNUVZ-UPD        PIC X.
      *            J = Aenderung Unterversicherungsverzicht Ja/Nein

      ***  Datenfeld: Geb{udetyp FeldNr. 5344
                   05  SUH-AUS-510-NHRLBGEBTY          PIC 9(9).
                   05  SUH-AUS-510-NHRLBGEBTY-UPD      PIC X.
      *            J = Aenderung Geb{udetyp

      ***  Datenfeld: Eigentumsverh{ltnisse FeldNr. 5345
                   05  SUH-AUS-510-NHRLBEIGEN          PIC 9(9).
                   05  SUH-AUS-510-NHRLBEIGEN-UPD      PIC X.
      *            J = Aenderung Eigentumsverh{ltnisse

      ***  Datenfeld: Tarifzone FeldNr. 5346
                   05  SUH-AUS-510-CHRZONE             PIC X(4).
                   05  SUH-AUS-510-CHRZONE-UPD         PIC X.
      *            J = Aenderung Tarifzone

      ***  Datenfeld: Bauartklasse FeldNr. 5347
                   05  SUH-AUS-510-NHRLBBRTKL          PIC 9(9).
                   05  SUH-AUS-510-NHRLBBRTKL-UPD      PIC X.
      *            J = Aenderung Bauartklasse

      ***  Datenfeld: Alarmanlage Ja/Nein FeldNr. 5348
                   05  SUH-AUS-510-NHRJNALARM          PIC XX.
                   05  SUH-AUS-510-NHRJNALARM-UPD      PIC X.
      *            J = Aenderung Alarmanlage Ja/Nein

      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5349
                   05  SUH-AUS-510-NHRJNFE             PIC XX.
                   05  SUH-AUS-510-NHRJNFE-UPD         PIC X.
      *            J = Aenderung Feuer Ja/Nein

      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5350
                   05  SUH-AUS-510-NHRJNST             PIC XX.
                   05  SUH-AUS-510-NHRJNST-UPD         PIC X.
      *            J = Aenderung Sturm Ja/Nein

      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5351
                   05  SUH-AUS-510-NHRJNHA             PIC XX.
                   05  SUH-AUS-510-NHRJNHA-UPD         PIC X.
      *            J = Aenderung Hagel Ja/Nein

      ***  Datenfeld: Einbruch/Diebstahl Ja/Nein FeldNr. 5352
                   05  SUH-AUS-510-NHRJNED             PIC XX.
                   05  SUH-AUS-510-NHRJNED-UPD         PIC X.
      *            J = Aenderung Einbruch/Diebstahl Ja/Nein

      ***  Datenfeld: Glas Ja/Nein FeldNr. 5353
                   05  SUH-AUS-510-NHRJNGL             PIC XX.
                   05  SUH-AUS-510-NHRJNGL-UPD         PIC X.
      *            J = Aenderung Glas Ja/Nein

      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5354
                   05  SUH-AUS-510-NHRJNLW             PIC XX.
                   05  SUH-AUS-510-NHRJNLW-UPD         PIC X.
      *            J = Aenderung Leitungswasser Ja/Nein

      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5355

                   05  SUH-AUS-510-NHRJNEL             PIC XX.
                   05  SUH-AUS-510-NHRJNEL-UPD         PIC X.
      *            J = Aenderung Elementarsch{den

      ***  Datenfeld: Bezugspr{mie FeldNr. 5356
                   05  SUH-AUS-510-NHRBZGPRM           PIC 999V999.
                   05  SUH-AUS-510-NHRBZGPRM-UPD       PIC X.
      *            J = Aenderung Bezugspr{mie

      ***  Datenfeld: Leistungstext FeldNr. 5357
                   05  SUH-AUS-510-CHRTXLEIST          PIC X(40).
                   05  SUH-AUS-510-CHRTXLEIST-UPD      PIC X.
      *            J = Aenderung Leistungstext

      ***  Datenfeld: Summenanpassungsklausel FeldNr. 5358
                   05  SUH-AUS-510-CHRTXSUAK           PIC X(32).
                   05  SUH-AUS-510-CHRTXSUAK-UPD       PIC X.
      *            J = Aenderung Summenanpassungsklausel

      ***  Datenfeld: Produkt FeldNr. 5359
                   05  SUH-AUS-510-CHRPRD              PIC X(30).
                   05  SUH-AUS-510-CHRPRD-UPD          PIC X.
      *            J = Aenderung Produkt

      ***  Datenfeld: Mailingaktion FeldNr. 5942
                   05  SUH-AUS-510-CMAILING            PIC X(70).
                   05  SUH-AUS-510-CMAILING-UPD        PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5521
                   05  SUH-AUS-510-NDIFFDECKJN         PIC X(2).
                   05  SUH-AUS-510-DIFFDECKJN-UPD      PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5522
                   05  SUH-AUS-510-CUMSTELLTERM        PIC 9(8).
                   05  SUH-AUS-510-UMSTELLTER-UPD      PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5524
                   05  SUH-AUS-510-CGESELL             PIC X(50).
                   05  SUH-AUS-510-CGESELL-UPD         PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5525
                   05  SUH-AUS-510-CDIFFVSNR           PIC X(40).
                   05  SUH-AUS-510-CDIFFVSNR-UPD       PIC X.
      *            J = Aenderung Versicherungsnummer

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5526
                   05  SUH-AUS-510-NKUENDBEST          PIC X(2).
                   05  SUH-AUS-510-NKUENDBEST-UPD      PIC X.
      *            J = Aenderung K}ndigungsbest{tigung Ja/Nein

      ***  Datenfeld: Zielgruppe FeldNr. 5986
                   05  SUH-AUS-510-CZIELGRP            PIC X(30).
                   05  SUH-AUS-510-CZIELGRP-UPD        PIC X.
      *            J = Aenderung Zielgruppe

      ***  Datenfeld: Smart-Home Paket FeldNr. 5730
080816             05 SUH-AUS-510-SMHPAKET          PIC X(02).
080816             05 SUH-AUS-510-SMHPAKET-UPD      PIC X.
080816*            J = Aenderung SMH Paket

      ***  Datenfeld: Smart-Home Datum FeldNr. 5731 (im Format JJJJMMTT)
080816             05 SUH-AUS-510-SMHDATUM          PIC 9(08).
080816             05 SUH-AUS-510-SMHDATUM-UPD      PIC X.
080816*            J = Aenderung SMH Datum

      ***  Datenfeld: Smart-Home TrackID FeldNr. 5732
080816             05 SUH-AUS-510-SMHTRACKID        PIC X(60).
080816             05 SUH-AUS-510-SMHTRACKID-UPD    PIC X.
080816*            J = Aenderung SMH Track-ID

      ***  Datenfeld: Smart-Home Controlz FeldNr. 5733
080816             05 SUH-AUS-510-SMHCONTROLZ       PIC X(60).
080816             05 SUH-AUS-510-SMHCONTROLZ-UPD   PIC X.
080816*            J = Aenderung SMH Control

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-510-INTLFDNR            PIC 9(4).

      ***  Datenfeld: Leerstellen
090408***          05  SUH-AUS-510-LEERSTELLEN         PIC X(769).
080816             05  SUH-AUS-510-LEERSTELLEN         PIC X(635).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-510-INTLFDNR            PIC 9(4).


      ******************************************************************
      *                                                                *
      *                  U N F A L L - V E R T R A G                   *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-520-UNFALL.

                   05  SUH-AUS-520-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-520-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5654
                   05  SUH-AUS-520-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5655
                   05  SUH-AUS-520-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5653
                   05  SUH-AUS-520-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 5639
                   05  SUH-AUS-520-CUFVBEDG            PIC X(16).
                   05  SUH-AUS-520-CUFVBEDG-UPD        PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risiko FeldNr. 5640
                   05  SUH-AUS-520-CUFLBRIS            PIC X(50).
                   05  SUH-AUS-520-CUFLBRIS-UPD        PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Klausel/Tarif FeldNr. 5641
                   05  SUH-AUS-520-CUFKLAUS            PIC X(80).
                   05  SUH-AUS-520-CUFKLAUS-UPD        PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Zusatzinfo FeldNr. 5643
                   05  SUH-AUS-520-CUFZUSINFO          PIC X(100).
                   05  SUH-AUS-520-CUFZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Anzahl versicherte Personen FeldNr. 5644
                   05  SUH-AUS-520-NUFVPANZ            PIC 9(5).
                   05  SUH-AUS-520-NUFVPANZ-UPD        PIC X.
      *            J = Aenderung Anzahl versicherte Personen

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5645
                   05  SUH-AUS-520-CUFDTLSTFV          PIC 9(8).
                   05  SUH-AUS-520-CUFDTLSTFV-UPD      PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5646
                   05  SUH-AUS-520-CUFDTLSTFB          PIC 9(8).
                   05  SUH-AUS-520-CUFDTLSTFB-UPD      PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5647
                   05  SUH-AUS-520-NUFJNSB             PIC XX.
                   05  SUH-AUS-520-NUFJNSB-UPD         PIC X.
      *            J = Aenderung SelbstBeteiligung Ja/Nein

      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5648
                   05  SUH-AUS-520-NUFSBPRZ            PIC 999.
                   05  SUH-AUS-520-NUFSBPRZ-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Prz

      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5649
                   05  SUH-AUS-520-NUFSBFES            PIC 9(5).
                   05  SUH-AUS-520-NUFSBFES-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Fest

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5650
                   05  SUH-AUS-520-CUFSBTXT            PIC X(50).
                   05  SUH-AUS-520-CUFSBTXT-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Text

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5651
                   05  SUH-AUS-520-NUFJNMBTR           PIC XX.
                   05  SUH-AUS-520-NUFJNMBTR-UPD       PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5652
                   05  SUH-AUS-520-CUFDTUMST           PIC 9(8).
                   05  SUH-AUS-520-CUFDTUMST-UPD       PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Mailingaktion FeldNr. 5919
                   05  SUH-AUS-520-CMAILING            PIC X(70).
                   05  SUH-AUS-520-CMAILING-UPD        PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5547
                   05  SUH-AUS-520-CUMSTELLTERM        PIC 9(8).
                   05  SUH-AUS-520-UMSTELLTER-UPD      PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5549
                   05  SUH-AUS-520-CGESELL             PIC X(50).
                   05  SUH-AUS-520-CGESELL-UPD         PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5550
                   05  SUH-AUS-520-CDIFFVSNR           PIC X(40).
                   05  SUH-AUS-520-CDIFFVSNR-UPD       PIC X.
      *            J = Aenderung Versicherungsnummer

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein
      ***             NKUENDBESTAET in Bestlief FeldNr. 5551
                   05  SUH-AUS-520-NKUENDBEST          PIC X(2).
                   05  SUH-AUS-520-NKUENDBEST-UPD      PIC X.
      *            J = Aenderung K}ndigungsbest{tigung Ja/Nein

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-520-INTLFDNR            PIC 9(4).

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-520-LEERSTELLEN         PIC X(1028).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-520-INTLFDNR            PIC 9(4).


      ******************************************************************
      *                 VerbundeneGeb{udeVersicherung                  *
      *                     V G V - V E R T R A G                      *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-521-VGVVTRG.

                   05  SUH-AUS-521-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-521-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5704
                   05  SUH-AUS-521-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5705
                   05  SUH-AUS-521-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5703
                   05  SUH-AUS-521-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5658
                   05  SUH-AUS-521-CVGRSPLZ            PIC X(6).
                   05  SUH-AUS-521-CVGRSPLZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5659
                   05  SUH-AUS-521-CVGRSSTR            PIC X(30).
                   05  SUH-AUS-521-CVGRSSTR-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Strasse

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5660
                   05  SUH-AUS-521-CVGRSHSNR           PIC X(15).
                   05  SUH-AUS-521-CVGRSHSNR-UPD       PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5661
                   05  SUH-AUS-521-CVGRSORT            PIC X(30).
                   05  SUH-AUS-521-CVGRSORT-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: Ort

      ***  Datenfeld: Risikoanschrift: L{nderkennzeichen FeldNr. 5662
                   05  SUH-AUS-521-NVGRSNKZ            PIC 9(9).
                   05  SUH-AUS-521-NVGRSNKZ-UPD        PIC X.
      *            J = Aenderung Risikoanschrift: L{nderkennzeichen

      ***  Datenfeld: Bedingung FeldNr. 5663
                   05  SUH-AUS-521-CVGVBEDG            PIC X(16).
                   05  SUH-AUS-521-CVGVBEDG-UPD        PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risiko FeldNr. 5664
                   05  SUH-AUS-521-CVGRISIKO           PIC X(50).
                   05  SUH-AUS-521-CVGRISIKO-UPD       PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Klausel/Tarif FeldNr. 5665
                   05  SUH-AUS-521-CVGKLAUS            PIC X(80).
                   05  SUH-AUS-521-CVGKLAUS-UPD        PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Beilagen FeldNr. 5666
                   05  SUH-AUS-521-CVGBEILGN           PIC X(19).
                   05  SUH-AUS-521-CVGBEILGN-UPD       PIC X.
      *            J = Aenderung Beilagen

      ***  Datenfeld: Zusatzinfo FeldNr. 5668
                   05  SUH-AUS-521-CVGZUSINFO          PIC X(100).
                   05  SUH-AUS-521-CVGZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5669
                   05  SUH-AUS-521-NVGJNFEUER          PIC XX.
                   05  SUH-AUS-521-NVGJNFEUER-UPD      PIC X.
      *            J = Aenderung Feuer Ja/Nein

      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5670
                   05  SUH-AUS-521-NVGJNLW             PIC XX.
                   05  SUH-AUS-521-NVGJNLW-UPD         PIC X.
      *            J = Aenderung Leitungswasser Ja/Nein

      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5671
                   05  SUH-AUS-521-NVGJNST             PIC XX.
                   05  SUH-AUS-521-NVGJNST-UPD         PIC X.
      *            J = Aenderung Sturm Ja/Nein

      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5672
                   05  SUH-AUS-521-NVGJNHA             PIC XX.
                   05  SUH-AUS-521-NVGJNHA-UPD         PIC X.
      *            J = Aenderung Hagel Ja/Nein

      ***  Datenfeld: Elementarsch{den Ja/Nein FeldNr. 5673
                   05  SUH-AUS-521-NVGJNSE             PIC XX.
                   05  SUH-AUS-521-NVGJNSE-UPD         PIC X.
      *            J = Aenderung Elementarsch{den Ja/Nein

      ***  Datenfeld: Geb{udenutzung FeldNr. 5675
                   05  SUH-AUS-521-CVGGBNTZ            PIC X(26).
                   05  SUH-AUS-521-CVGGBNTZ-UPD        PIC X.
      *            J = Aenderung Geb{udenutzung Ja/Nein

      ***  Datenfeld: gewerbliche Fl{che in Prozent FeldNr. 5677
                   05  SUH-AUS-521-NVGGEWFL            PIC 999.
                   05  SUH-AUS-521-NVGGEWFL-UPD        PIC X.
      *            J = Aenderung gewerbliche Fl{che in Prozent

      ***  Datenfeld: Baujahr Format: JJJJMMTT FeldNr. 5678
                   05  SUH-AUS-521-CVGBAUJ             PIC 9(8).
                   05  SUH-AUS-521-CVGBAUJ-UPD         PIC X.
      *            J = Aenderung Baujahr

      ***  Datenfeld: Bauartklasse LB FeldNr. 5679
                   05  SUH-AUS-521-NVGLBBAUKL          PIC 9(9).
                   05  SUH-AUS-521-NVGLBBAUKL-UPD      PIC X.
      *            J = Aenderung Bauartklasse LB

      ***  Datenfeld: Rohbau bis Datum Format: JJJJMMTT FeldNr. 5680
                   05  SUH-AUS-521-CVGDTRBB            PIC 9(8).
                   05  SUH-AUS-521-CVGDTRBB-UPD        PIC X.
      *            J = Aenderung Rohbau bis Datum

      ***  Datenfeld: Leistungsart LB FeldNr. 5681
                   05  SUH-AUS-521-NVGLBLA             PIC 9(9).
                   05  SUH-AUS-521-NVGLBLA-UPD         PIC X.
      *            J = Aenderung Leistungsart LB

      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5934
                   05  SUH-AUS-521-NVGJNUVZ            PIC XX.
                   05  SUH-AUS-521-NVGJNUVZ-UPD        PIC X.
      *            J = Aenderung Unterversicherungsverzicht Ja/Nein

      ***  Datenfeld: Grund f}r UVZ FeldNr. 5683
                   05  SUH-AUS-521-CVGUVZGRD           PIC X(40).
                   05  SUH-AUS-521-CVGUVZGRD-UPD       PIC X.
      *            J = Aenderung Grund f}r UnterVersicherungsverZicht

      ***  Datenfeld: Versicherungsumme 1914 FeldNr. 5684
                   05  SUH-AUS-521-NVGSUM1914          PIC 9(9).
                   05  SUH-AUS-521-NVGSUM1914-UPD      PIC X.
      *            J = Aenderung Versicherungsumme 1914

      ***  Datenfeld: Staffelpr{mie Ja/Nein FeldNr. 5685
                   05  SUH-AUS-521-NVGJNSTFPR          PIC XX.
                   05  SUH-AUS-521-NVGJNSTFPR-UPD      PIC X.
      *            J = Aenderung Staffelpr{mie Ja/Nein

      ***  Datenfeld: Zone Leitungswasser FeldNr. 5688
                   05  SUH-AUS-521-NVGLWZONE           PIC 9.
                   05  SUH-AUS-521-NVGLWZONE-UPD       PIC X.
      *            J = Aenderung Zone Leitungswasser

      ***  Datenfeld: Zone Sturm  FeldNr. 5689
                   05  SUH-AUS-521-NVGSTZONE           PIC 9.
                   05  SUH-AUS-521-NVGSTZONE-UPD       PIC X.
      *            J = Aenderung Zone Sturm

      ***  Datenfeld: Zone Erdbeben FeldNr. 5690
                   05  SUH-AUS-521-NVGEBZONE           PIC 9.
                   05  SUH-AUS-521-NVGEBZONE-UPD       PIC X.
      *            J = Aenderung Zone Erdbeben

      ***  Datenfeld: Zone sonstige Elementarsch{den FeldNr. 5691
                   05  SUH-AUS-521-NVGSEZONE           PIC 9.
                   05  SUH-AUS-521-NVGSEZONE-UPD       PIC X.
      *            J = Aenderung Zone sonstige Elementarsch{den

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5692
                   05  SUH-AUS-521-NVGJNMBTR           PIC XX.
                   05  SUH-AUS-521-NVGJNMBTR-UPD       PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5693
                   05  SUH-AUS-521-CVGDTLSTFV          PIC 9(8).
                   05  SUH-AUS-521-CVGDTLSTFV-UPD      PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5694
                   05  SUH-AUS-521-CVGDTLSTFB          PIC 9(8).
                   05  SUH-AUS-521-CVGDTLSTFB-UPD      PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5695
                   05  SUH-AUS-521-CVGTXTSB            PIC X(14).
                   05  SUH-AUS-521-CVGTXTSB-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Text

      ***  Datenfeld: PRM-Satz Hauptwagnis FeldNr. 5696
                   05  SUH-AUS-521-NVGHWPRMS           PIC 99V9(7).
                   05  SUH-AUS-521-NVGHWPRMS-UPD       PIC X.
      *            J = Aenderung PRM-Satz Hauptwagnis

      ***  Datenfeld: PRM-Satz weitere Elementargefahren FeldNr. 5697
                   05  SUH-AUS-521-NVGWEPRMS           PIC 99V9(4).
                   05  SUH-AUS-521-NVGWEPRMS-UPD       PIC X.
      *            J = Aenderung PRM-Satz weitere Elementargefahren

      ***  Datenfeld: Anzahl Vorsch{den weitere Elementargefahren
      ***             FeldNr. 5698
                   05  SUH-AUS-521-NVGWEVSANZ          PIC 99.
                   05  SUH-AUS-521-NVGWEVSANZ-UPD      PIC X.
      *            J = Aenderung Anzahl Vorsch{den weitere Elementar-
      *                gefahren

      ***  Datenfeld: Gefahrenklasse feuergef{hrlicher Betriebe/Lager
      ***             FeldNr. 5699
                   05  SUH-AUS-521-CVGGFKFBL           PIC X.
                   05  SUH-AUS-521-CVGGFKFBL-UPD       PIC X.
      *            J = Aenderung Gefahrenklasse feuergef{hrlicher
      *                Betriebe/Lager

      ***  Datenfeld: Fl{che in Prozent Gefahrenklasse feuerge-
      ***             gef{hrlicher Betrag FeldNr. 5700
                   05  SUH-AUS-521-CVGFLFBL            PIC X(20).
                   05  SUH-AUS-521-CVGFLFBL-UPD        PIC X.
      *            J = Aenderung Fl{che in Prozent Gefahrenklasse
      *                feuergef{hrlicher Betrag

      ***  Datenfeld: Produkt FeldNr. 5701
                   05  SUH-AUS-521-CVGPRD              PIC X(30).
                   05  SUH-AUS-521-CVGPRD-UPD          PIC X.
      *            J = Aenderung Produkt

      ***  Datenfeld: Geb{udeart FeldNr. 5707
                   05  SUH-AUS-521-CVGLBGEBAR          PIC X(50).
                   05  SUH-AUS-521-CVGLBGEBAR-UPD      PIC X.
      *            J = Aenderung Geb{udeart

      ***  Datenfeld: Betriebsart LB FeldNr. 5708
                   05  SUH-AUS-521-CVGLBBTART          PIC X(50).
                   05  SUH-AUS-521-CVGLBBTART-UPD      PIC X.
      *            J = Aenderung Betriebsart LB

      ***  Datenfeld: Modell FeldNr. 5920
                   05  SUH-AUS-521-CMODELL             PIC X(18).
                   05  SUH-AUS-521-CMODELL-UPD         PIC X.
      *            J = Aenderung Modell

      ***  Datenfeld: Monopol (Ja/Nein) FeldNr. 5921
                   05  SUH-AUS-521-NMONOPOL            PIC XX.
                   05  SUH-AUS-521-NMONOPOL-UPD        PIC X.
      *            J = Aenderung Monopol Ja/Nein

      ***  Datenfeld: Auslandsrisiko (Ja/Nein) FeldNr. 5922
                   05  SUH-AUS-521-NAUSLRIS            PIC XX.
                   05  SUH-AUS-521-NAUSLRIS-UPD        PIC X.
      *            J = Aenderung Auslandsrisiko Ja/Nein

      ***  Datenfeld: Geb{udebezeichnung FeldNr. 5923
                   05  SUH-AUS-521-CGEBBEZ             PIC X(80).
                   05  SUH-AUS-521-CGEBBEZ-UPD         PIC X.
      *            J = Aenderung Geb{udebezeichnung

      ***  Datenfeld: Rohbauversicherung FeldNr. 5924
                   05  SUH-AUS-521-CROHBAUV            PIC 9(09).
                   05  SUH-AUS-521-CROHBAUV-UPD        PIC X.
      *            J = Aenderung Rohbauversicherung

      ***  Datenfeld: Wohnfl{che FeldNr. 5925
                   05  SUH-AUS-521-NWOHNFL             PIC 9(5).
                   05  SUH-AUS-521-NWOHNFL-UPD         PIC X.
      *            J = Aenderung Wohnfl{che

      ***  Datenfeld: Geschossanzahl FeldNr. 5926
                   05  SUH-AUS-521-NGESCHOSSE          PIC 99.
                   05  SUH-AUS-521-NGESCHOSSE-UPD      PIC X.
      *            J = Aenderung Geschossanzahl

      ***  Datenfeld: Keller vorhanden (Ja/Nein) FeldNr. 5927
                   05  SUH-AUS-521-NKELLER             PIC XX.
                   05  SUH-AUS-521-NKELLER-UPD         PIC X.
      *            J = Aenderung Keller vorhanden Ja/Nein

      ***  Datenfeld: Schwimmbad im Geb{ude (Ja/Nein)FeldNr. 5928
                   05  SUH-AUS-521-NSCHWIMM            PIC XX.
                   05  SUH-AUS-521-NSCHWIMM-UPD        PIC X.
      *            J = Aenderung Schwimmbad im Geb{ude (Ja/Nein)

      ***  Datenfeld: Nebengeb{ude Entsch{digungsgrenze FeldNr. 5930
                   05  SUH-AUS-521-NNEBENGEB           PIC 9(9).
                   05  SUH-AUS-521-NNEBENGEB-UPD       PIC X.
      *            J = Aenderung Nebengeb{ude Entsch{digungsgrenze

      ***  Datenfeld: Nebengeb{ude FeldNr. 5931
                   05  SUH-AUS-521-CNEBENGEB           PIC X(80).
                   05  SUH-AUS-521-CNEBENGEB-UPD       PIC X.
      *            J = Aenderung Nebengeb{ude

      ***  Datenfeld: Sonstiges FeldNr. 5932
                   05  SUH-AUS-521-CSONST              PIC X(80).
                   05  SUH-AUS-521-CSONST-UPD          PIC X.
      *            J = Aenderung Sonstiges

      ***  Datenfeld: Garagen/Carports FeldNr. 5933
                   05  SUH-AUS-521-NGARAGEN            PIC 99.
                   05  SUH-AUS-521-NGARAGEN-UPD        PIC X.
      *            J = Aenderung Garagen/Carports

      ***  Datenfeld: Zone !berschwemmung FeldNr. 5682
                   05  SUH-AUS-521-NVGUEBZONE          PIC 99.
                   05  SUH-AUS-521-NVGUEBZONE-UPD      PIC X.
      *            J = Aenderung Zone !berschwemmung

      ***  Datenfeld: Mailingaktion FeldNr. 5944
                   05  SUH-AUS-521-CMAILING            PIC X(70).
                   05  SUH-AUS-521-MAILING-UPD         PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5539
                   05  SUH-AUS-521-NDIFFDECKJN         PIC X(2).
                   05  SUH-AUS-521-NDIFFDECK-UPD       PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5540
                   05  SUH-AUS-521-CUMSTELLTERM        PIC 9(8).
                   05  SUH-AUS-521-UMSTELLTER-UPD      PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5542
                   05  SUH-AUS-521-CGESELL             PIC X(50).
                   05  SUH-AUS-521-CGESELL-UPD         PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5543
                   05  SUH-AUS-521-CDIFFVSNR           PIC X(40).
                   05  SUH-AUS-521-CDIFFVSNR-UPD       PIC X.
      *            J = Aenderung Versicherungsnummer

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein FeldNr. 5544
                   05  SUH-AUS-521-NKUENDBEST          PIC X(2).
                   05  SUH-AUS-521-NKUENDBEST-UPD      PIC X.
      *            J = Aenderung K}ndigungsbest{tigung Ja/Nein

      ***  Datenfeld: Elementar bei Diff-Vers. Ja/Nein FeldNr. 5545
                   05  SUH-AUS-521-NELEDIFFJN          PIC X(2).
                   05  SUH-AUS-521-NELEDIFFJN-UPD      PIC X.
      *            J = Aenderung Elementar bei Diff-Vers. Ja/Nein

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-521-INTLFDNR            PIC 9(4).

      ***  Datenfeld: sonstige Nutzflaeche FeldNr. 5709
310115             05 SUH-AUS-521-SNUTZFL           PIC 9(5).
310115             05 SUH-AUS-521-SNUTZFL-UPD       PIC X.
      *            J = Aenderung sonstige Nutzflaeche

<<    ***  Datenfeld: Leerstellen
                   05  SUH-AUS-521-LEERSTELLEN         PIC X(331).
090408*** Entität auf 1.600 Byte gekürzt.

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-521-INTLFDNR            PIC 9(4).
      *
310115******************************************************************
      *                                                                *
      *                522 - P K - P H O T O V O L T A I K             *
      *                                                                *
      *     LAENGE: 1600                        STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-522-PK-PV.
                   05  SUH-AUS-522-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
      ***  Schlüssel: Nummer der Satzart
                   05  SUH-AUS-522-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 8004
                   05  SUH-AUS-522-NGESELLST           PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 8001
                   05  SUH-AUS-522-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 8003
                   05  SUH-AUS-522-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Bedingungen FeldNr. 8011
           05  SUH-AUS-522-CPVBEDG          PIC X(30).
           05  SUH-AUS-522-CPVBEDG-UPD      PIC X.

      ***  Datenfeld: Mailingaktion FeldNr. 8012
                    05 SUH-AUS-522-CMAILING PIC X(70).
                    05 SUH-AUS-522-CMAILING-UPD      PIC X.

      ***  Datenfeld: Zusatzinfo FeldNr. 8013
                    05 SUH-AUS-522-CPVZUSINFO PIC X(100).
                    05 SUH-AUS-522-CPVZUSINFO-UPD      PIC X.

      ***  Datenfeld: Informationsquelle FeldNr. 8014
                    05 SUH-AUS-522-LVTQUELLE PIC X(9).
                    05 SUH-AUS-522-LVTQUELLE-UPD      PIC X.

      ***  Datenfeld: Baujahr FeldNr. 8015 Form JJJJ
                    05 SUH-AUS-522-CPVBAUJ PIC 9(4).
                    05 SUH-AUS-522-CPVBAUJ-UPD      PIC X.

      ***  Datenfeld: Anlagenleistung FeldNr. 8016
                    05 SUH-AUS-522-CPVLEIST PIC 9(4)V99.
                    05 SUH-AUS-522-CPVLEIST-UPD      PIC X.

      ***  Datenfeld: Abweichender Anlagenbetreiber FeldNr. 8017
                    05 SUH-AUS-522-CABWANLBTR PIC X(35).
                    05 SUH-AUS-522-CABWANLBTR-UPD      PIC X.

      ***  Datenfeld: Mitversicherung von Batteriesystemen
      ***         JA/NEIN    FeldNr. 8018
                    05 SUH-AUS-522-MVBATTSYS PIC XX.
                    05 SUH-AUS-522-MVBATTSYS-UPD      PIC X.

      ****  Datenfeld: Gebaeudeart FeldNr. 8019
310115              05 SUH-AUS-522-CPVGEBART PIC X(50).
                    05 SUH-AUS-522-CPVGEBART-UPD PIC X.

      **** Datenfeld: Leerstellen
                    05 SUH-AUS-522-LEERSTELLEN PIC X(1238).

                    05 SUH-AUS-522-INTLFDNR PIC 9(4).
      ******************************************************************
      *                         FirmenKunden                           *
      *                      F K - V E R T R A G                       *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-523-FK-VTRG.

                   05  SUH-AUS-523-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-523-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 5889
                   05  SUH-AUS-523-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 5890
                   05  SUH-AUS-523-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5892
                   05  SUH-AUS-523-CVEZHOSTNR          PIC X(15).

      ***  Datenfeld: Produkt/Tarif FeldNr. 5894
                   05  SUH-AUS-523-CFKPROD             PIC X(50).
                   05  SUH-AUS-523-CFKPROD-UPD         PIC X.
      *            J = Aenderung Produkt/Tarif

      ***  Datenfeld: Bedingungen FeldNr. 5895
                   05  SUH-AUS-523-CFKVBEDG            PIC X(20).
                   05  SUH-AUS-523-CFKVBEDG-UPD        PIC X.
      *            J = Aenderung Bedingungen

      ***  Datenfeld: Zusatzinfo FeldNr. 5896
                   05  SUH-AUS-523-CFKZUSINFO          PIC X(100).
                   05  SUH-AUS-523-CFKZUSINFO-UPD      PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Mailingaktion FeldNr. 5939
                   05  SUH-AUS-523-CMAILING            PIC X(70).
                   05  SUH-AUS-523-CMAILING-UPD        PIC X.
      *            J = Aenderung Mailingaktion

170209****  Datenfeld: Gesellschaft-Klartext FeldNr. 9152
                   05 SUH-AUS-523-CGESELL           PIC X(50).
                   05 SUH-AUS-523-CGESELL-UPD       PIC X.

      ****  Datenfeld: Versicherungsnummer FeldNr. 9153
                   05 SUH-AUS-523-CDIFFVSNR         PIC X(40).
                   05 SUH-AUS-523-CDIFFVSNR-UPD PIC X.

      ****  Datenfeld: Umstellungstermin FeldNr. 9154 i.d.F. YYYYMMDD
                   05 SUH-AUS-523-CUMSTELLTERM      PIC 9(8).
                   05 SUH-AUS-523-CUMSTELL-UPD      PIC X.

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9155
                   05 SUH-AUS-523-NKUENDBESTAET      PIC XX.
                   05 SUH-AUS-523-NKUENDBEST-UPD PIC X.

      ***  Datenfeld: Leerstellen
090408****         05  SUH-AUS-523-LEERSTELLEN         PIC X(1309).
170209             05  SUH-AUS-523-LEERSTELLEN         PIC X(1205).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-523-INTLFDNR            PIC 9(4).

      ***
      ******************************************************************
      *                                                                *
      *      S G W - B E S O N D E R E   V E R E I N B A R U N G       *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.1999      *
      *                                                      2007      *
      ******************************************************************
       01 SUH-AUS-613-SGW.

                   05  SUH-AUS-613-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-613-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6140
                   05  SUH-AUS-613-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6141
                   05  SUH-AUS-613-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6139
                   05  SUH-AUS-613-CVEZHOSTNR          PIC X(15).

      *** Schlüssel: Komponentengruppe FeldNr. 9317
030811             05  SUH-AUS-613-RGID              PIC 9(4).

      ***  Schlüssel: laufende Nummer FeldNr. 6136
                   05  SUH-AUS-613-PSGBLFDNR           PIC 999.

      ***  Datenfeld: Bedingung/Klausel-Nummer FeldNr. 6137
                   05  SUH-AUS-613-CSGBNRBVB           PIC X(20).
                   05  SUH-AUS-613-CSGBNRBVB-UPD       PIC X.
      *            J = Aenderung Bedingung/Klausel-Nummer

      ***  Datenfeld: Text besondere Vertragsbestandteile FeldNr. 6138
                   05  SUH-AUS-613-CSGBTXTBVB          PIC X(80).
                   05  SUH-AUS-613-CSGBTXTBVB-UPD      PIC X.
      *            J = Aenderung Text besondere Vertragsbestandteile

      ***  Datenfeld: Leerstellen
030811             05  SUH-AUS-613-LEERSTELLEN         PIC X(1444).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-613-INTLFDNR            PIC 9(4).

210108******************************************************************
>>    *                                                                *
      *       U N F A L L - V E R S I C H E R T E   P E R S O N        *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-614-UNFVP.

                   05  SUH-AUS-614-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-614-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6165
                   05  SUH-AUS-614-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6166
                   05  SUH-AUS-614-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6144
                   05  SUH-AUS-614-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: versicherte Person laufende Nummer (Schlüssel)
      ***             FeldNr. 6144
                   05  SUH-AUS-614-PUFVPLFDNR          PIC 999.

      ***  Datenfeld: versicherte Person laufende Nummer (Maske)
      ***             FeldNr. 6145
                   05  SUH-AUS-614-NUFVPVBNR           PIC 999.
                   05  SUH-AUS-614-NUFVPVBNR-UPD       PIC X.
      *            J = Aenderung versicherte Person laufende Nummer
      *                (Maske)

      ***  Datenfeld: VP-Name FeldNr. 6146
                   05  SUH-AUS-614-CUFVPNAME           PIC X(25).
                   05  SUH-AUS-614-CUFVPNAME-UPD       PIC X.
      *            J = Aenderung VP-Name

      ***  Datenfeld: VP-Beruf FeldNr. 6147
170717***          05  SUH-AUS-614-CUFVPBERUF          PIC X(21).
170717             05  SUH-AUS-614-CUFVPBERUF          PIC X(48).
                   05  SUH-AUS-614-CUFVPBERUF-UPD      PIC X.
      *            J = Aenderung VP-Beruf

      ***  Datenfeld: VP-Geburtsdatum Format: JJJJMMTT FeldNr. 6148
                   05  SUH-AUS-614-CUFVPGEBD           PIC 9(8).
                   05  SUH-AUS-614-CUFVPGEBD-UPD       PIC X.
      *            J = Aenderung VP-Beruf Gebdat

      ***  Datenfeld: VP-Gefahrengruppe FeldNr. 6149
                   05  SUH-AUS-614-CUFVPGEFGR          PIC X.
                   05  SUH-AUS-614-CUFVPGEFGR-UPD      PIC X.
      *            J = Aenderung VP-Gefahrengruppe

      ***  Datenfeld: VP-Beitrag FeldNr. 6150
                   05  SUH-AUS-614-NUFVPBTR            PIC 9(7)V99.
                   05  SUH-AUS-614-NUFVPBTR-UPD        PIC X.
      *            J = Aenderung VP-Beitrag

      ***  Datenfeld: Tarifart FeldNr. 6151
                   05  SUH-AUS-614-CUFTARIF            PIC X(30).
                   05  SUH-AUS-614-CUFTARIF-UPD        PIC X.
      *            J = Aenderung Tarifart

      ***  Datenfeld: Dynamik-Text FeldNr. 6152
                   05  SUH-AUS-614-CUFDYNTEXT          PIC X(5).
                   05  SUH-AUS-614-CUFDYNTEXT-UPD      PIC X.
      *            J = Aenderung Dynamik Text

      ***  Datenfeld: Dynamik-%-Satz FeldNr. 6153
                   05  SUH-AUS-614-NUFDYNPRZ           PIC 99V99.
                   05  SUH-AUS-614-NUFDYNPRZ-UPD       PIC X.
      *            J = Aenderung Dynamik-%-Satz

      ***  Datenfeld: Dynamik Ablehnungsjahr Format: JJJJMMTT
      ***             FeldNr. 6154
                   05  SUH-AUS-614-CUFDYNABLJ          PIC 9(8).
                   05  SUH-AUS-614-CUFDYNABLJ-UPD      PIC X.
      *            J = Aenderung Dynamik Ablehnungsjahr

      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 6155
                   05  SUH-AUS-614-NUFJNREGUL          PIC XX.
                   05  SUH-AUS-614-NUFJNREGUL-UPD      PIC X.
      *            J = Aenderung Regulierung Ja/Nein

      ***  Datenfeld: R}ckgew{hrsumme Ja/Nein FeldNr. 6156
                   05  SUH-AUS-614-NUFSUMRUEK          PIC XX.
                   05  SUH-AUS-614-NUFSUMRUEK-UPD      PIC X.
      *            J = Aenderung R}ckgew{hrsumme Ja/Nein

      ***  Datenfeld: Pr{mienr}ckgew{hr Ja/Nein FeldNr. 6157
                   05  SUH-AUS-614-NUFJNUPR            PIC XX.
                   05  SUH-AUS-614-NUFJNUPR-UPD        PIC X.
      *            J = Aenderung Pr{mienr}ckgew{hr Ja/Nein

      ***  Datenfeld: Freizeitdeckung Ja/Nein FeldNr. 6158
                   05  SUH-AUS-614-NUFJNFREIZ          PIC XX.
                   05  SUH-AUS-614-NUFJNFREIZ-UPD      PIC X.
      *            J = Aenderung Freizeitdeckung Ja/Nein

      ***  Datenfeld: Anzahl versicherte Personen je Personengruppe
      ***             FeldNr. 6159
                   05  SUH-AUS-614-NUFVPGRANZ          PIC 9(5).
                   05  SUH-AUS-614-NUFVPGRANZ-UPD      PIC X.
      *            J = Aenderung Anzahl versicherte Personen je
      *                Personengruppe

      ***  Datenfeld: Geschlecht versicherte Person FeldNr. 6160
                   05  SUH-AUS-614-NUFVPSEX            PIC 9(9).
                   05  SUH-AUS-614-NUFVPSEX-UPD        PIC X.
      *            J = Aenderung Geschlecht versicherte Person

      ***  Datenfeld: Pr{mienfreistellung KIZ Ja/Nein FeldNr. 6162
                   05  SUH-AUS-614-NUFJNKIZFR          PIC XX.
                   05  SUH-AUS-614-NUFJNKIZFR-UPD      PIC X.
      *            J = Aenderung Pr{mienfreistellung KIZ Ja/Nein

      ***  Datenfeld: Pr{mie KIZ FeldNr. 6163
                   05  SUH-AUS-614-NUFKIZPRM           PIC 9(7)V99.
                   05  SUH-AUS-614-NUFKIZPRM-UPD       PIC X.
      *            J = Aenderung Pr{mie KIZ

      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6168
                   05  SUH-AUS-614-CGPBHNR             PIC 9(15).
                   05  SUH-AUS-614-CGPBHNR-UPD         PIC X.
      *            J = Aenderung Partner-Host-ID der versich. Person

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5546
                   05  SUH-AUS-614-NDIFFDECKJN         PIC X(2).
                   05  SUH-AUS-614-DIFFDECKJN-UPD      PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Zielgruppe FeldNr. 5988
                   05  SUH-AUS-614-CZIELGRP            PIC X(30).
                   05  SUH-AUS-614-CZIELGRP-UPD        PIC X.
      *            J = Aenderung Zielgruppe

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 3235
                   05 SUH-AUS-614-CGESELL              PIC X(50).
                   05 SUH-AUS-614-CGESELL-UPD          PIC X.
      *            J = Aenderung Gesellschaft Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 3236
                   05 SUH-AUS-614-CDIFFVSNR            PIC X(40).
                   05 SUH-AUS-614-CDIFFVSNR-UPD        PIC X.
      *            J = Aenderung VersicherungsNummer

      ***  Datenfeld: Umstellungstermin FeldNr. 3237 i.d.F. YYYYMMDD
                   05 SUH-AUS-614-CUMSTELLTERM         PIC 9(08).
                   05 SUH-AUS-614-CUMSTELL-UPD         PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 3238
                   05 SUH-AUS-614-NKUENDBESTAET        PIC XX.
                   05 SUH-AUS-614-NKUENDBEST-UPD       PIC X.
      *            J = Aenderung Kuendigungsbestaetigung Ja/Nein

      ***  Datenfeld: Nachname   FeldNr. 6161
260716             05 SUH-AUS-614-NACHNAME             PIC X(40).
260716             05 SUH-AUS-614-NACHNAME-UPD         PIC X.
      *            J = Aenderung Nachname

      ***  Datenfeld: Vorname   FeldNr. 6161
260716             05 SUH-AUS-614-VORNAME              PIC X(30).
260716             05 SUH-AUS-614-VORNAME-UPD          PIC X.
      *            J = Aenderung Vorname

      ***  Datenfeld: Leerstellen
090408***          05  SUH-AUS-614-LEERSTELLEN         PIC X(1231).
260716***          05  SUH-AUS-614-LEERSTELLEN         PIC X(1159).
170717             05  SUH-AUS-614-LEERSTELLEN         PIC X(1132).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-614-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                 VerbundeneGeb{udeVersicherung                  *
      *           V G V - S E L B S T B E T E I L I G U N G            *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-615-VGVSB.

                   05  SUH-AUS-615-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-615-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6178
                   05  SUH-AUS-615-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6179
                   05  SUH-AUS-615-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6177
                   05  SUH-AUS-615-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Laufende Nummer der Selbstbeteiligung
      ***             FeldNr. 6170
                   05  SUH-AUS-615-PVGSBLFDNR          PIC 999.

      ***  Datenfeld: SelbstBeteiligung-Art FeldNr. 6171
                   05  SUH-AUS-615-NVGLBSBART          PIC 9(9).
                   05  SUH-AUS-615-NVGLBSBART-UPD      PIC X.
      *            J = Aenderung SelbstBeteiligung-Art

      ***  Datenfeld: SelbstBeteiligung-Bezeichnung FeldNr. 6172
                   05  SUH-AUS-615-CVGSBBEZ            PIC X(30).
                   05  SUH-AUS-615-CVGSBBEZ-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Bezeichnung

      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 6173
                   05  SUH-AUS-615-CVGSBPRZ            PIC X(14).
                   05  SUH-AUS-615-CVGSBPRZ-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Prz

      ***  Datenfeld: SelbstBeteiligung-Minimum FeldNr. 6174
                   05  SUH-AUS-615-NVGSBMIN            PIC 9(5)V99.
                   05  SUH-AUS-615-NVGSBMIN-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Minimum

      ***  Datenfeld: SelbstBeteiligung-Maximum FeldNr. 6175
                   05  SUH-AUS-615-NVGSBMAX            PIC 9(5)V99.
                   05  SUH-AUS-615-NVGSBMAX-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Maximum

      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 6176
                   05  SUH-AUS-615-NVGSBFES            PIC 9(5)V99.
                   05  SUH-AUS-615-NVGSBFES-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Fest

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 6243
                   05  SUH-AUS-615-CVGSBTXT            PIC X(30).
                   05  SUH-AUS-615-CVGSBTXT-UPD        PIC X.
      *            J = Aenderung SelbstBeteiligung-Text

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-615-LEERSTELLEN           PIC X(1439).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-615-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *    U N F A L L - M I T V E R S I C H E R T E   P E R S O N     *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-616-UNFMVP.

                   05  SUH-AUS-616-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-616-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6459
                   05  SUH-AUS-616-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6460
                   05  SUH-AUS-616-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6462
                   05  SUH-AUS-616-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: VP laufende Nummer FeldNr. 6445
                   05  SUH-AUS-616-PUFVPLFDNR          PIC 999.

      ***  Schlüssel: MVP laufende Nummer FeldNr. 6446
                   05  SUH-AUS-616-PUFMVPLFDNR             PIC 999.

      ***  Datenfeld: Name FeldNr. 6447
                   05  SUH-AUS-616-CNAME               PIC X(30).
                   05  SUH-AUS-616-CNAME-UPD           PIC X.
      *            J = Aenderung Name

      ***  Datenfeld: Vorname FeldNr. 6448
                   05  SUH-AUS-616-CVNAME              PIC X(30).
                   05  SUH-AUS-616-CVNAME-UPD          PIC X.
      *            J = Aenderung Vorname

      ***  Datenfeld: Geschlecht FeldNr. 6449
                   05  SUH-AUS-616-NSEX                PIC 9(9).
                   05  SUH-AUS-616-NSEX-UPD            PIC X.
      *            J = Aenderung Geschlecht

      ***  Datenfeld: Geburtsdatum Format: JJJJMMTT FeldNr. 6450
                   05  SUH-AUS-616-CGEBDAT             PIC 9(8).
                   05  SUH-AUS-616-CGEBDAT-UPD         PIC X.
      *            J = Aenderung Geburtsdatum

      ***  Datenfeld: Beginn Format: JJJJMMTT FeldNr. 6451
                   05  SUH-AUS-616-CBEGINN             PIC 9(8).
                   05  SUH-AUS-616-CBEGINN-UPD         PIC X.
      *            J = Aenderung Beginn

      ***  Datenfeld: Ende Format: JJJJMMTT FeldNr. 6452
                   05  SUH-AUS-616-CENDE               PIC 9(8).
                   05  SUH-AUS-616-CENDE-UPD           PIC X.
      *            J = Aenderung Ende

      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6457
                   05  SUH-AUS-616-CGPBHNR             PIC 9(15).
                   05  SUH-AUS-616-CGPBHNR-UPD         PIC X.
      *            J = Aenderung Ende

    <<***  Datenfeld: Leerstellen
210108             05  SUH-AUS-616-LEERSTELLEN           PIC X(1432).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
                   05  SUH-AUS-616-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *    U N F A L L - BEITRAGSRÜCKGEWÄHR KAPITALANTEIL              *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 15.11.2011      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-617-UBR.

                 05  SUH-AUS-617-VERARB-KNZ            PIC X(01).
      *          Verarbeitungskennzeichen
      *          I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                 05  SUH-AUS-617-SATZART               PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 138
                 05  SUH-AUS-617-NGESELLST             PIC 9(9).
      *          J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 139
                 05  SUH-AUS-617-CVNR                  PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 140
                 05  SUH-AUS-617-CVEZHOSTNR            PIC X(15).

      ***  Schlüssel: Komponentengruppennummer FeldNr. 141
                 05  SUH-AUS-617-PVRIGRP               PIC 9(04).

      ***  Schlüssel: laufendes Vertragsjahr FeldNr. 142
                 05  SUH-AUS-617-LFD-JAHR          PIC 99.

      ***  Datenfeld: Tabelle Kapitalwerte je Monat
                 05  SUH-AUS-617-UBR-KAPW      OCCURS 12.

      ***  Datenfeld: lfd. Nr. Datensatz FeldNr. 143
                   10  SUH-AUS-617-UBR-NR              PIC 9(03).
                   10  SUH-AUS-617-UBR-NR-UPD          PIC X.
      *            J = Aenderung lfd. Nr. Datensatz

      ***  Datenfeld: lfd. Versicherungsmonat im Vertragsjahr
      ***             FeldNr. 144
                   10  SUH-AUS-617-LFD-MONAT           PIC 9(02).
                   10  SUH-AUS-617-LFD-MONAT-UPD       PIC X.
      *            J = Aenderung lfd. Monat

      ***  Datenfeld: lfd. Kalenderjahr FeldNr. 145
                   10  SUH-AUS-617-JAHR                PIC 9(04).
                   10  SUH-AUS-617-JAHR-UPD            PIC X.
      *            J = Aenderung lfd. Jahr

      ***  Datenfeld: lfd. Kalendermonat FeldNr. 146
                   10  SUH-AUS-617-MONAT               PIC 9(02).
                   10  SUH-AUS-617-MONAT-UPD           PIC X.
      *            J = Aenderung Monat

      ***  Datenfeld: Status des Datensatzes  FeldNr. 147
                   10  SUH-AUS-617-STATUSCD            PIC X.
                   10  SUH-AUS-617-STATUSCD-UPD        PIC X.
      *            J = Aenderung Status

      ***  Datenfeld: Erstelldatum des Datensatzes FeldNr.148
                   10  SUH-AUS-617-STAT-DAT            PIC 9(08).
                   10  SUH-AUS-617-STAT-DAT-UPD        PIC X.
      *            J = Aenderung Erstelldatum Datensatz

      ***  Datenfeld: garantierter Rückkaufswert  FeldNr. 149
                   10  SUH-AUS-617-RUECKK-GAR          PIC 9(11)V9(04).
                   10  SUH-AUS-617-RUECKK-GAR-UPD      PIC X.
      *            J = Aenderung Rückkaufswert

      ***  Datenfeld: garant. Todesfallleistung   FeldNr. 150
                   10  SUH-AUS-617-LEITOD-GAR          PIC 9(11)V9(04).
                   10  SUH-AUS-617-LEITOD-GAR-UPD      PIC X.
      *            J = Aenderung garant. Todesfallleistung

      ***  Datenfeld: Rückkaufswert incl. Übersch.-Betlg.
      ***             FeldNr. 151
                   10  SUH-AUS-617-RUECK-UEBS          PIC 9(11)V9(04).
                   10  SUH-AUS-617-RUECK-UEBS-UPD      PIC X.
      *            J = Aenderung Rückkaufswert

      ***  Datenfeld: Todesfallleistung incl. Übersch.-Betlg.
      ***             FeldNr. 152
                   10  SUH-AUS-617-TOD-UEBS            PIC 9(11)V9(04).
                   10  SUH-AUS-617-TOD-UEBS-UPD        PIC X.
      *            J = Aenderung Todesfallleistung

      ***  Datenfeld: stille Reserve FeldNr. 153
                   10  SUH-AUS-617-WEANW               PIC 9(11)V9(04).
                   10  SUH-AUS-617-WEANW-UPD           PIC X.
      *            J = Aenderung stille Reserve

      ***  Datenfeld: Rechnungszins  FeldNr. 154
                   10  SUH-AUS-617-RECHZINS            PIC 9(02)V9(02).
                   10  SUH-AUS-617-RECHZINS-UPD        PIC X.
      *            J = Aenderung stille Reserve

      ***  Datenfeld: Leerstellen
                   05  SUH-AUS-617-LEERSTELLEN           PIC X(215).
      *** Entität auf 1.600 Byte gekürzt.

      ***  Datenfeld: Interne lfd. Nummer für die Datenbank
                   05  SUH-AUS-617-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *            V E R T R A G S R I S I K O G R U P P E             *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-628-VTRGRISGRP.

                   05  SUH-AUS-628-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-628-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6401
                   05  SUH-AUS-628-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6402
                   05  SUH-AUS-628-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6404
                   05  SUH-AUS-628-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Subjekt-Host-ID FeldNr. 6399
                   05  SUH-AUS-628-NVRGSUBHID          PIC 9(9).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6352
                   05  SUH-AUS-628-PVRGID              PIC 9(4).

      ***  Datenfeld: Risikogruppe-Typ FeldNr. 6353
                   05  SUH-AUS-628-NVRGTYP             PIC 9(9).
                   05  SUH-AUS-628-NVRGTYP-UPD         PIC X.
      *            J = Aenderung Risikogruppe-Typ

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6354
                   05  SUH-AUS-628-CVRGNAME            PIC X(50).
                   05  SUH-AUS-628-CVRGNAME-UPD        PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Beginndatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6355
                   05  SUH-AUS-628-CVRGBEG             PIC 9(8).
                   05  SUH-AUS-628-CVRGBEG-UPD         PIC X.
      *            J = Aenderung Beginndatum der Risikogruppe

      ***  Datenfeld: Ablaufdatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6356
                   05  SUH-AUS-628-CVRGABL             PIC 9(8).
                   05  SUH-AUS-628-CVRGABL-UPD         PIC X.
      *            J = Aenderung Ablaufdatum der Risikogruppe

      ***  Datenfeld: Nettobeitrag FeldNr. 6357
                   05  SUH-AUS-628-NVRGNBTRG           PIC 9(8)V99.
                   05  SUH-AUS-628-NVRGNBTRG-UPD       PIC X.
      *            J = Aenderung  Nettobeitrag

      ***  Datenfeld: ID übergeordnete Risikogruppe FeldNr. 6358
                   05  SUH-AUS-628-NVRGUEBID           PIC 9(4).
                   05  SUH-AUS-628-NVRGUEBID-UPD       PIC X.
      *            J = Aenderung ID übergeordnete Risikogruppe

170209***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9147
                   05 SUH-AUS-628-NDIFFDECKJN       PIC X(2).
                   05 SUH-AUS-628-NDIFFDECK-UPD PIC X.

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9148
                   05 SUH-AUS-628-CGESELL           PIC X(50).
                   05 SUH-AUS-628-CGESELL-UPD       PIC X.

      ***  Datenfeld: Versicherungsnummer FeldNr. 9149
                   05 SUH-AUS-628-CDIFFVSNR         PIC X(40).
                   05 SUH-AUS-628-CDIFFVSNR-UPD PIC X.

      ***  Datenfeld: Umstellungstermin FeldNr. 9150 i.d.F. YYYYMMDD
                   05 SUH-AUS-628-CUMSTELLTERM      PIC 9(8).
                   05 SUH-AUS-628-CUMSTELL-UPD      PIC X.

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9151
                   05 SUH-AUS-628-NKUENDBESTAET           PIC X(2).
                   05 SUH-AUS-628-NKUENDBEST-UPD          PIC X.

      ***  Datenfeld: Leerstellen
090408***          05  SUH-AUS-628-LEERSTELLEN           PIC X(1445).
170209             05  SUH-AUS-628-LEERSTELLEN           PIC X(1338).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-628-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *           V E R T R A G S R I S I K O M E R K M A L            *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-629-VTRGRISMKM.

                   05  SUH-AUS-629-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-629-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6408
                   05  SUH-AUS-629-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6409
                   05  SUH-AUS-629-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6411
                   05  SUH-AUS-629-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6361
                   05  SUH-AUS-629-PVRGID              PIC 9(4).

      ***  Schlüssel: Risikomerkmalwert-ID FeldNr. 6362
                   05  SUH-AUS-629-PVRISMID            PIC 999.

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6363
                   05  SUH-AUS-629-CVRISMNAME          PIC X(50).
                   05  SUH-AUS-629-CVRISMNAME-UPD      PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Risikomerkmalwert FeldNr. 6364
                   05  SUH-AUS-629-CVRISMWERT          PIC X(50).
                   05  SUH-AUS-629-CVRISMWERT-UPD      PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-629-LEERSTELLEN           PIC X(1444).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-629-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *       V E R T R A G S S E L B S T B E T E I L I G U N G        *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-630-VTRGSB.

                   05  SUH-AUS-630-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-630-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6415
                   05  SUH-AUS-630-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6416
                   05  SUH-AUS-630-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6418
                   05  SUH-AUS-630-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6372
                   05  SUH-AUS-630-PVRGID              PIC 9(4).

      ***  Schlüssel: Selbstbeteiligung-ID FeldNr. 6373
                   05  SUH-AUS-630-PVRSBID             PIC 999.

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6374
                   05  SUH-AUS-630-CVSBBEZ             PIC X(50).
                   05  SUH-AUS-630-CVSBBEZ-UPD         PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Wert in Prozent FeldNr. 6375
                   05  SUH-AUS-630-NVSBPRZ             PIC 99.
                   05  SUH-AUS-630-NVSBPRZ-UPD         PIC X.
      *            J = Aenderung Wert in Prozent

      ***  Datenfeld: Wert in DM/Euro FeldNr. 6376
                   05  SUH-AUS-630-NVSBBTRG            PIC 9(8)V99.
                   05  SUH-AUS-630-NVSBBTRG-UPD        PIC X.
      *            J = Aenderung Wert in DM/Euro

      ***  Datenfeld: Mindestbetrag in DM/Euro FeldNr. 6377
                   05  SUH-AUS-630-NVSBMIN             PIC 9(8)V99.
                   05  SUH-AUS-630-NVSBMIN-UPD         PIC X.
      *            J = Aenderung Mindestbetrag in DM/Euro

      ***  Datenfeld: Höchstbetrag in DM/Euro FeldNr. 6378
                   05  SUH-AUS-630-NVSBMAX             PIC 9(8)V99.
                   05  SUH-AUS-630-NVSBMAX-UPD         PIC X.
      *            J = Aenderung Höchstbetrag in DM/Euro

      ***  Datenfeld: Text zur Selbstbeteiligung FeldNr. 6379
                   05  SUH-AUS-630-CVSBTXT             PIC X(30).
                   05  SUH-AUS-630-CVSBTXT-UPD         PIC X.
      *            J = Aenderung Text zur Selbstbeteiligung

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508***          05  SUH-AUS-630-INTLFDNR            PIC 9(4).

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-630-LEERSTELLEN           PIC X(1428).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-630-INTLFDNR            PIC 9(4).

      *****************************************************************
      *                                                               *
      *                V E R T R A G S L E I S T U N G                *
      *                                                               *
      *     LAENGE: 1.600 Bytes                STAND: 26.06.2007      *
      *                                                               *
      *****************************************************************
       01 SUH-AUS-631-VTRGLSTG.

                   05  SUH-AUS-631-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-631-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 6391
                   05  SUH-AUS-631-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 6392
                   05  SUH-AUS-631-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6394
                   05  SUH-AUS-631-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6381
                   05  SUH-AUS-631-PVRGID              PIC 9(4).

      ***  Schlüssel: Leistung-ID FeldNr. 6382
                   05  SUH-AUS-631-PVLSTID             PIC 999.

      ***  Schlüssel: Themengebietsschlüssel FeldNr. 6443
                   05  SUH-AUS-631-PTGSCHL             PIC 9(15).

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6383
                   05  SUH-AUS-631-CVLSTBEZ            PIC X(50).
                   05  SUH-AUS-631-CVLSTBEZ-UPD        PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Wert DM/EUR FeldNr. 6384
                   05  SUH-AUS-631-NVLSTSUM            PIC 9(9)V99.
                   05  SUH-AUS-631-NVLSTSUM-UPD        PIC X.
      *            J = Aenderung Wert DM/EUR

      ***  Datenfeld: sonstiger Wert FeldNr. 6385
                   05  SUH-AUS-631-NVLSTWRT            PIC 9(8)V99.
                   05  SUH-AUS-631-NVLSTWRT-UPD        PIC X.
      *            J = Aenderung sonstiger Wert

      ***  Datenfeld: sonstiger Wert Einheit FeldNr. 6386
                   05  SUH-AUS-631-CVLSTWEH            PIC X(20).
                   05  SUH-AUS-631-CVLSTWEH-UPD        PIC X.
      *            J = Aenderung sonstiger Wert Einheit

      ***  Datenfeld: Text zur Leistung FeldNr. 6387
                   05  SUH-AUS-631-CVLSTTXT            PIC X(30).
                   05  SUH-AUS-631-CVLSTTXT-UPD        PIC X.
      *            J = Aenderung Text zur Leistung

      ***  Datenfeld: Leerstellen
090408             05  SUH-AUS-631-LEERSTELLEN           PIC X(1405).

270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-631-INTLFDNR            PIC 9(4).


      ******************************************************************
      *                                                                *
      *               Z U S C H L A G / A B S C H L A G                *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-701-ZU-ABSCHLAG.

                   05  SUH-AUS-701-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-701-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7062
                   05  SUH-AUS-701-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 7063
                   05  SUH-AUS-701-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7061
                   05  SUH-AUS-701-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Universelle Laufende Nummer FeldNr. 7055
                   05  SUH-AUS-701-PZUNILFDNR         PIC 999.

      ***  Datenfeld: Zu-/Abschlag in % FeldNr. 7057
                   05  SUH-AUS-701-NZSLGPROZ           PIC 99V99.
                   05  SUH-AUS-701-NZSLGPROZ-UPD       PIC X.
      *            J = Aenderung Zu-/Abschlag in %

      ***  Datenfeld: Zu-/Abschlag in WE FeldNr. 7058
                   05  SUH-AUS-701-NZSLGWE             PIC 9(7)V99.
                   05  SUH-AUS-701-NZSLGWE-UPD         PIC X.
      *            J = Aenderung Zu-/Abschlag in WE

      ***  Datenfeld: Kürzel zum Zu-/Abschlag FeldNr. 7059
                   05  SUH-AUS-701-CZSLGKRZL           PIC XXX.
                   05  SUH-AUS-701-CZSLGKRZL-UPD       PIC X.
      *            J = Aenderung Kürzel zum Zu-/Abschlag

      ***  Datenfeld: Erklärungstext zum Zu-/Abschlag FeldNr. 7060
                   05  SUH-AUS-701-CZSLGTXT            PIC X(30).
                   05  SUH-AUS-701-CZSLGTXT-UPD        PIC X.
      *            J = Aenderung Erklärungstext zum Zu-/Abschlag

270508***  Datenfeld: PZANR STAZ SL1 Zuschlags/Abschlagsnummer.
    >>***             FeldNr. 7065.
                   05 SUH-AUS-701-PZANR                PIC 9(09).
                   05 SUH-AUS-701-PZANR-UPD            PIC X.

      ***  Datenfeld: Leerstellen
090408***          05  SUH-AUS-701-LEERSTELLEN           PIC X(1500).
                   05  SUH-AUS-701-LEERSTELLEN           PIC X(1490).
    >>
270508***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-701-INTLFDNR            PIC 9(4).

      ******************************************************************
      *                                                                *
      *                  V E R T R A G S R I S I K O                   *
      *                                                                *
      *     LAENGE: 1.600 Bytes                 STAND: 26.06.2007      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-702-VTRGRISIKO.

                   05  SUH-AUS-702-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete

      ***  Schlüssel: Nummer der ALLFINANZ-Satzart
                   05  SUH-AUS-702-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7082
                   05  SUH-AUS-702-NGESELLST           PIC 9(9).
      *            J = Aenderung Versicherungsgesellschaft

      ***  Schlüssel: Vertragsnummer FeldNr. 7083
                   05  SUH-AUS-702-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7081
                   05  SUH-AUS-702-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 7066
                   05  SUH-AUS-702-PVRIGRP             PIC 9(4).

      ***  Schlüssel: Themengebietsschlüssel FeldNr. 7067
                   05  SUH-AUS-702-PTGSCHL             PIC 9(15).

      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7068
                   05  SUH-AUS-702-PVRILFDNR           PIC 999.

      ***  Datenfeld: Risikokennziffer FeldNr. 7069
                   05  SUH-AUS-702-CVRIKENNZ           PIC X(10).
                   05  SUH-AUS-702-CVRIKENNZ-UPD       PIC X.
      *            J = Aenderung Risikokennziffer

      ***  Datenfeld: Risikoname FeldNr. 7070
                   05  SUH-AUS-702-CVRINAME            PIC X(50).
                   05  SUH-AUS-702-CVRINAME-UPD        PIC X.
      *            J = Aenderung Risikoname

      ***  Datenfeld: Risikobeitrag FeldNr. 7071

                   05  SUH-AUS-702-NVRIBTRG            PIC 9(7)V99.
                   05  SUH-AUS-702-NVRIBTRG-UPD        PIC X.
      *            J = Aenderung Risikobeitrag

      ***  Datenfeld: Risikosumme FeldNr. 7072
                   05  SUH-AUS-702-NVRIVSUM            PIC 9(9)V99.
                   05  SUH-AUS-702-NVRIVSUM-UPD        PIC X.
      *            J = Aenderung Risikosumme

      ***  Datenfeld: Risiko-Entschädigungsgrenze FeldNr. 7073
                   05  SUH-AUS-702-NVRIEGR             PIC 9(08)V99.
                   05  SUH-AUS-702-NVRIEGR-UPD         PIC X.
      *            J = Aenderung Risiko-Entschädigungsgrenze

      ***  Datenfeld: Risiko-Entschädigungsgrenze-Einheit FeldNr. 7074
                   05  SUH-AUS-702-NVRIEGREH           PIC 9(9).
                   05  SUH-AUS-702-NVRIEGREH-UPD       PIC X.
      *            J = Aenderung Risiko-Entschädigungsgrenze

      ***  Datenfeld: Risikomenge FeldNr. 7075
                   05  SUH-AUS-702-NVRIMG              PIC 9(9).
                   05  SUH-AUS-702-NVRIMG-UPD          PIC X.
      *            J = Aenderung Risikomenge

      ***  Datenfeld: Risikomenge-Einheit FeldNr. 7076
                   05  SUH-AUS-702-CVRIMGEH            PIC X(25).
                   05  SUH-AUS-702-CVRIMGEH-UPD        PIC X.
      *            J = Aenderung Risikomenge-Einheit

      ***  Datenfeld: Risikosatz FeldNr. 7077
                   05  SUH-AUS-702-NVRISATZ            PIC 999V9(7).
                   05  SUH-AUS-702-NVRISATZ-UPD        PIC X.
      *            J = Aenderung Risikosatz

      ***  Datenfeld: Risikosatz-Einheit FeldNr. 7078
                   05  SUH-AUS-702-NVRISATZEH          PIC 9(9).
                   05  SUH-AUS-702-NVRISATZEH-UPD      PIC X.
      *            J = Aenderung Risikosatz-Einheit

      ***  Datenfeld: Risikotext FeldNr. 7079
                   05  SUH-AUS-702-CVRITXT             PIC X(30).
                   05  SUH-AUS-702-CVRITXT-UPD         PIC X.
      *            J = Aenderung Risikotest

      ***  Datenfeld: Risiko-Mitversichert (Ja/Nein)FeldNr. 7080
                   05  SUH-AUS-702-NVRIMITVJN          PIC XX.
                   05  SUH-AUS-702-NVRIMITVJN-UPD      PIC X.
      *            J = Aenderung Risiko-Mitversichert (Ja/Nein)

      ***  Datenfeld: Risikosumme-Einheit FeldNr. 7167
                   05  SUH-AUS-702-CVRIVSUMEH          PIC X(20).
                   05  SUH-AUS-702-CVRIVSUMEH-UPD      PIC X.
      *            J = Aenderung Risikosumme-Einheit

      ***  Datenfeld: Risikosumme unbegrenzt (Ja/Nein) FeldNr. 7168
                   05  SUH-AUS-702-NVRIUNBJN           PIC XX.
                   05  SUH-AUS-702-NVRIUNBJN-UPD       PIC X.
      *            J = Aenderung Risikosumme unbegrenzt (Ja/Nein)

270508***  Datenfeld: Risikomerkmal FeldNr. 7169
    >>             05  SUH-AUS-702-RISMNR              PIC 9(09).
                   05  SUH-AUS-702-RISMNR-UPD          PIC X.
      *
      ***  Datenfeld: Leerstellen
090408***          05  SUH-AUS-702-LEERSTELLEN         PIC X(1311).
                   05  SUH-AUS-702-LEERSTELLEN         PIC X(1301).

    >>***  Datenfeld: Interne lfd. Nummer für die Datenbank
270508             05  SUH-AUS-702-INTLFDNR            PIC 9(4).
      *                                                                *
      *
      *
310115******************************************************************
      *                                                                *
      *                705 - F R E M D V E R S I C H E R E R           *
      *                                                                *
      *     LAENGE: 1600                        STAND: 31.01.2015      *
      *                                                                *
      ******************************************************************
       01 SUH-AUS-705-FRV.
                   05  SUH-AUS-705-VERARB-KNZ          PIC X(01).
      *            Verarbeitungskennzeichen
      *            I = Insert U = Update D = Delete
      ***  Schlüssel: Nummer der Satzart
                   05  SUH-AUS-705-SATZART             PIC 999.

      ***  Schlüssel: Versicherungsgesellschaft FeldNr. 7503
                   05  SUH-AUS-705-NGESELLST           PIC 9(9).

      ***  Schlüssel: Vertragsnummer FeldNr. 7501
                   05  SUH-AUS-705-CVNR                PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7502
                   05  SUH-AUS-705-CVEZHOSTNR          PIC X(15).

      ***  Schlüssel: Risikogruppe FeldNr. 7504
                   05 SUH-AUS-705-PVRIGRP                PIC 9(4).

      ***  Schlüssel: Themengebietsschluessel FeldNr. 7505
                   05 SUH-AUS-705-PTGSCHL               PIC 9(15).

      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7506
                   05 SUH-AUS-705-PVRILFDNR             PIC 999.

      ***  Datenfeld: Differenzdeckungsgefahr FeldNr. 7511
                    05 SUH-AUS-705-NDIFFGEF PIC X(45).
                    05 SUH-AUS-705-NDIFFGEF-UPD PIC X.

      ***  Datenfeld: Differenzdeckungsgesellschaft FeldNr. 7512
                    05 SUH-AUS-705-CGESELL PIC X(50).
                    05 SUH-AUS-705-CGESELL-UPD PIC X.

      ***  Datenfeld: Versicherungsnummer FeldNr. 7513
                    05 SUH-AUS-705-CDIFFVSNR PIC X(40).
                    05 SUH-AUS-705-CDIFFVSNR-UPD PIC X.

      ***  Datenfeld: Umstellungstermin  FeldNr. 7514
                    05 SUH-AUS-705-CUMSTELLTERM PIC 9(8).
                    05 SUH-AUS-705-CUMSTELL-UPD      PIC X.

      ***  Datenfeld: Kuendigsunsbestaetigung JA/Nein FeldNr. 7515
                    05 SUH-AUS-705-NKUENDBEST        PIC XX.
                    05 SUH-AUS-705-NKUENDB-UPD          PIC X.

      ***  Datenfeld: Risikomerkmal FeldNr. 7529
                    05 SUH-AUS-705-RISMNR PIC 9(9).
                    05 SUH-AUS-705-RISMNR-UPD PIC X.

      ***  Datenfeld: Leerstellen
                    05 SUH-AUS-705-LEERSTELLEN PIC X(1371).

                    05 SUH-AUS-705-INTLFDNR PIC 9(4).
      *
      ******************************************************************
      *               E N D E    COPY-MEMBER DRKI2053                  *
      ******************************************************************





