      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle  VERTRAGINFO          *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *--------!--------------------------------------!---------!----- *
120416*12.04.16! NEUES FELD AKTIONERGEBNIS            ! HHU     !      *
      *--------!--------------------------------------!---------!----- *
120516*12.05.16! NEUES FELD Produktbezeichnung        ! HHU     !      *
      *--------!--------------------------------------!---------!----- *
080816*08.08.16! NEUES FELD KUNDENIDSMH               ! HHU     !      *
      *        !                                      !         !      *
010217*01.02.17! NEUE FELDER FLEXN                    ! HHU     !      *
      *        !                                      !         !      *
      *02.02.21! neues Feld VSPKZ                     ! HHU     !      *
      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 950 BYTES                 STAND: 12.05.2016        *
      **************************************************************** *

       01  DRKI2101.

                    05  REOB-AUS-400-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-400-VERTRAGS-ID        PIC X(13).

      ****  Schlüssel: Vertragsnummer FeldNr. 4002
                    05 REOB-AUS-400VI-CVNR             PIC 9(15).

      ***  Schlüssel: Herkunftssystem
                   05  REOB-AUS-400-HERKUNFT        PIC XXX.
                   05  REOB-AUS-400-HERKUNFT-UPD    PIC X.
      *            J = Änderung Herkunftssystem

      ***  Schlüssel: QuittungsNummer
                   05  REOB-AUS-400-QUITTNR         PIC 9(15).
                   05  REOB-AUS-400-QUITTNR-UPD     PIC X.
      *            J = Änderung QuittungsNr.

      ***  Schlüssel: Erstelldatum
                   05  REOB-AUS-400-ERSTDAT         PIC X(10).
                   05  REOB-AUS-400-ERSTDAT-UPD     PIC X.
      *            J = Änderung Erstell-Datum

      ***  Schlüssel: Erstellzeit (im Format HHMMSS)
                   05  REOB-AUS-400-ERSTZEIT        PIC 9(6).
                   05  REOB-AUS-400-ERSTZEIT-UPD    PIC X.
      *            J = Änderung Erstell-Datum

      ****  Datenfeld: Antragsnummer FeldNr. 4003
                    05  REOB-AUS-400-CANNR           PIC X(18).
                    05  REOB-AUS-400-CANNR-UPD        PIC X.
      **            J = Änderung Antragsnummer

      ****  Datenfeld: Merkmal Vermittler 1 zum Vertrag FeldNr. 4004
                    05  REOB-AUS-400-CV1MERKM        PIC X(20).
                    05  REOB-AUS-400-CV1MERKM-UPD    PIC X.
      **            J = Änderung Merkmal Vermittler 1

      ****  Datenfeld: Vermittlernummer 1 zum Vertrag FeldNr. 4005
                    05  REOB-AUS-400-CV1VTVMNR       PIC X(10).
                    05  REOB-AUS-400-CV1VTVMNR-UPD   PIC X.
     ***           J = Änderung Merkmal Vermittlernummer 1

     ***  Datenfeld: Merkmal Vermittler 2 zum Vertrag FeldNr. 4006
                  05  REOB-AUS-400-CV2MERKM        PIC X(20).
                  05  REOB-AUS-400-CV2MERKM-UPD     PIC X.
     ***           J = Änderung Merkmal Vermittler 2

     ***  Datenfeld: Vermittlernummer 2 zum Vertrag FeldNr. 4007
                  05  REOB-AUS-400-CV2VTVMNR       PIC X(10).
                  05  REOB-AUS-400-CV2VTVMNR-UPD   PIC X.
     ***           J = Änderung Vermittlernummer 2

     ***  Datenfeld: Tarif/Produkt FeldNr. 4008
                  05  REOB-AUS-400-CPBRDNR         PIC X(50).
                  05  REOB-AUS-400-CPBRDNR-UPD     PIC X.
     ***           J = Änderung Tarif/Produkt

     ***  Datenfeld: Vertragssparte FeldNr. 4010
                  05  REOB-AUS-400-NSPARTE         PIC 9(9).
                  05  REOB-AUS-400-NSPARTE-UPD     PIC X.
     ***           J = Änderung Vertragssparte

     ***  Datenfeld: Vertragsart FeldNr. 4011
                  05  REOB-AUS-400-NVTART          PIC 9(9).
                  05  REOB-AUS-400-NVTART-UPD      PIC X.
     ***           J = Änderung Vertragsart

     ***  Datenfeld: Versichertes Risiko FeldNr. 4015
                  05  REOB-AUS-400-CVERSRISK       PIC X(23).
                  05  REOB-AUS-400-CVERSRISK-UPD   PIC X.
     ***           J = Änderung Versichertes Risiko

      ***  Datenfeld: Änderungsgrund FeldNr. 4017
                   05  REOB-AUS-400-CVAEND1G        PIC X(50).
                   05  REOB-AUS-400-CVAEND1G-UPD    PIC X.
      **            J = Änderung Änderungsgrund

      ***  Datenfeld: AFB-Anteil in Prozent FeldNr. 4018
                   05  REOB-AUS-400-NAFBPROZ        PIC 99V999.
                   05  REOB-AUS-400-NAFBPROZ-UPD    PIC X.
      **            J = Änderung AFB-Anteil in Prozent

      ***  Datenfeld: Vertragsinfo FeldNr. 4022
                   05  REOB-AUS-400-CVTZINFO        PIC X(80).
                   05  REOB-AUS-400-CVTZINFO-UPD    PIC X.
      **            J = Änderung Vertragsinfo

      ****  Datenfeld: Vertragssumme FeldNr. 4027
                    05  REOB-AUS-400-NVSUMME         PIC 9(10).
                    05  REOB-AUS-400-NVSUMME-UPD     PIC X.
      **            J = Änderung Vertragssumme

      ***  Datenfeld: Jahresgesamtbeitrag brutto gemäss
      ***             Zahlungsweise. FeldNr. 4034
                   05  REOB-AUS-400-NINKJGSBTR      PIC 9(10)V99.
                   05  REOB-AUS-400-NINKJGSBTR-UPD  PIC X.
      **            J = Änderung Jahresgesamtbeitrag brutto gemäss
      **                Zahlungsweise.

      ***  Datenfeld: Fälligkeitsdatum  FeldNr. 4035
                   05  REOB-AUS-400-CINKFLGDAT      PIC X(10).
                   05  REOB-AUS-400-CINKFLGDAT-UPD  PIC X.
      **            J = Änderung Fälligkeitsdatum

      ***  Datenfeld: Jahresgesamtbeitrag netto FeldNr. 4036
                   05  REOB-AUS-400-NINKJGNBTR      PIC 9(8)V99.
                   05  REOB-AUS-400-NINKJGNBTR-UPD  PIC X.
      **            J = Änderung Jahresgesamtbeitrag netto

      ***  Datenfeld: Mahnstufe FeldNr. 4037
                   05  REOB-AUS-400-CINKMHNSTF      PIC X(25).
                   05  REOB-AUS-400-CINKMHNSTF-UPD  PIC X.
      **            J = Änderung Mahnstufe

      ***  Datenfeld: Mahndatum  FeldNr. 4038
                   05  REOB-AUS-400-CINKMHNDAT      PIC X(10).
                   05  REOB-AUS-400-CINKMHNDAT-UPD  PIC X.
      **            J = Änderung Mahnstufe

      ***  Datenfeld: Rückstandsbetrag FeldNr. 4039
                   05  REOB-AUS-400-NINKRSBTR       PIC 9(7)V99.
                   05  REOB-AUS-400-NINKRSBTR-UPD   PIC X.
      **            J = Änderung Rückstandsbetrag

      ***  Datenfeld: Laufzeitrabatt in % FeldNr. 4040
                   05  REOB-AUS-400-NINKLFZRB       PIC 99.
                   05  REOB-AUS-400-NINKLFZRB-UPD   PIC X.
      **            J = Änderung Laufzeitrabatt in Proz.

      ***  Datenfeld: Ratenzuschlag FeldNr. 4041
                   05  REOB-AUS-400-NINKRTZS        PIC 99.
                   05  REOB-AUS-400-NINKRTZS-UPD    PIC X.
      **            J = Änderung Ratenzuschlag

      ***  Datenfeld: Zeichnung FeldNr. 4042
                   05  REOB-AUS-400-CINKZCHNNG      PIC X(25).
                   05  REOB-AUS-400-CINKZCHNNG-UPD  PIC X.
      **            J = Änderung Zeichnung

      ***  Datenfeld: F/B-Beitrag brutto FeldNr. 4043
                   05  REOB-AUS-400-NINKFBBTR       PIC 9(8)V99.
                   05  REOB-AUS-400-NINKFBBTR-UPD   PIC X.
      **            J = Änderung F/B-Beitrag brutto

      ***  Datenfeld: F/B-Summe FeldNr. 4044
                   05  REOB-AUS-400-NINKFBSUM       PIC 9(12).
                   05  REOB-AUS-400-NINKFBSUM-UPD   PIC X.
      **            J = Änderung F/B-Summe

      ***  Datenfeld: Einlösungsbeitrag brutto FeldNr. 4045
                   05  REOB-AUS-400-NINKELBTBR      PIC 9(8)V99.
                   05  REOB-AUS-400-NINKELBTBR-UPD  PIC X.
      **            J = Aenderung Einlösungsbeitrag brutto

      ***  Datenfeld: Einlösungsbeitrag netto FeldNr. 4046
                   05  REOB-AUS-400-NINKELBTNT      PIC 9(8)V99.
                   05  REOB-AUS-400-NINKELBTNT-UPD  PIC X.
      **            J = Aenderung Einlösungsbeitrag netto

      ***  Datenfeld: Letzte Synchronisation Format: JJJJMMTT
      ***             FeldNr. 4047
                   05  REOB-AUS-400-CINKLSTSYN      PIC X(10).
                   05  REOB-AUS-400-CINKLSTSYN-UPD  PIC X.
      **            J = Aenderung Letzte Synchronisation

      ***  Datenfeld: Letzte Agenturbearbeitung Format: JJJJMMTT
      ***             FeldNr. 4048
                   05  REOB-AUS-400-CINKLSTAGT      PIC X(10).
                   05  REOB-AUS-400-CINKLSTAGT-UPD  PIC X.
      **            J = Aenderung Letzte Agenturbearbeitung

      ***  Datenfeld: Letzte Bewegung FeldNr. 4049
                   05  REOB-AUS-400-CINKLSTBWG      PIC X(61).
                   05  REOB-AUS-400-CINKLSTBWG-UPD  PIC X.
      **            J = Aenderung Letzte Bewegung

      ***  Datenfeld: Kundenbonus FeldNr. 4050
                   05  REOB-AUS-400-CINKKDBON       PIC X(26).
                   05  REOB-AUS-400-CINKKDBON-UPD   PIC X.
      **            J = Aenderung Kundenbonus

      ***  Datenfeld: Ostrabatt FeldNr. 4051
                   05  REOB-AUS-400-NINKOSTRBT      PIC 99.
                   05  REOB-AUS-400-NINKOSTRBT-UPD  PIC X.
      **            J = Aenderung Ostrabatt

      ***  Datenfeld: Kurztext Bestandssicherungsaktion FeldNr. 4094
                   05  REOB-AUS-400-CINKBSA         PIC X(6).
                   05  REOB-AUS-400-CINKBSA-UPD     PIC X.
      **            J = Aenderung Kurztext Bestandssicherungsaktion

      ***  Datenfeld: Langtext Bestandssicherungsaktion FeldNr. 4110
                   05  REOB-AUS-400-CVGBSALTXT      PIC X(50).
                   05  REOB-AUS-400-CVGBSALTXT-UPD  PIC X.
      **            J = Aenderung Langtext Bestandssicherungsaktion

      ***  Datenfeld: Versicherungsgesellschaft Text FeldNr. 4111
                   05  REOB-AUS-400-CGESELLST       PIC X(50).
                   05  REOB-AUS-400-CGESELLST-UPD   PIC X.
      **            J = Aenderung Versicherungsgesellschaft Text

      ***  Datenfeld: VD-Nr. FeldNr. 4112
                   05  REOB-AUS-400-NVDNR           PIC 9(5).
                   05  REOB-AUS-400-NVDNR-UPD       PIC X.
      **            J = Aenderung VD-Nr.

      ***  Datenfeld: VD-Name FeldNr. 4113
                   05  REOB-AUS-400-CVDNAME         PIC X(20).
                   05  REOB-AUS-400-CVDNAME-UPD     PIC X.
      **            J = Aenderung VD-Name

      ***  Datenfeld: Vor-Vertragsnummer FeldNr. 4114
                   05  REOB-AUS-400-CVORVNR         PIC X(17).
                   05  REOB-AUS-400-CVORVNR-UPD     PIC X.
      **            J = Aenderung Vor-Vertragsnummer

120416***  Datenfeld: Aktionsergebnis FeldNr. 4119
                   05  REOB-AUS-400-AKTIONERG       PIC X(03).
                   05  REOB-AUS-400-AKTIONERG-UPD   PIC X.
      **            J = Aenderung Vor-Vertragsnummer

120516***  Datenfeld: Produktkombibezeichnung  FeldNr. 4170
                   05  REOB-AUS-400-PRDKOMBIBEZ     PIC X(50).
                   05  REOB-AUS-400-PRDKOMBIBEZ-UPD PIC X.
      **            J = Aenderung Produktkombibezeichnung

080816***  Datenfeld: Produktkombibezeichnung  FeldNr. 4170
                   05  REOB-AUS-400-KUNDENIDSMH     PIC X(10).
                   05  REOB-AUS-400-KUNDENIDSMH-UPD PIC X.
      **            J = Aenderung KUNDENID SMART HOME

      ***  Datenfeld: VK-FLEX-VSP-Nachlass FeldNr. 4156
010217             05 REOB-AUS-400-FLEXNVSP         PIC 9(9)V9(2).
010217             05 REOB-AUS-400-FLEXNVSP-UPD     PIC X.

      ***  Datenfeld: VK-FLEX-VSP-Nachlass gewährt FeldNr. 4157
010217             05 REOB-AUS-400-FLEXNVSPGW       PIC X.
010217             05 REOB-AUS-400-FLEXNVSPGW-UPD   PIC X.

      ***  Datenfeld: VK-FLEX-VGV-Nachlass FeldNr. 4158
010217             05 REOB-AUS-400-FLEXNVGV         PIC 9(9)V9(2).
010217             05 REOB-AUS-400-FLEXNVGV-UPD     PIC X.

      ***  Datenfeld: VK-FLEX-VGV-Nachlass gewährt FeldNr. 4159
010217             05 REOB-AUS-400-FLEXNVGVGW       PIC X.
010217             05 REOB-AUS-400-FLEXNVGVGW-UPD   PIC X.

      ***  Datenfeld: VSPKZ aus FeldNr. 4152
010217             05 REOB-AUS-400-VSPKZ            PIC X.
010217             05 REOB-AUS-400-VSPKZ-UPD        PIC X.

                   05 REOB-AUS-400-BUENDEL          PIC 9(003).
                   05 REOB-AUS-400-BUENDEL-UPD      PIC X(001).
120416****         05 FILLER                        PIC X(38).
120416****         05 FILLER                        PIC X(34).
120516****         05 FILLER                        PIC X(73).
080816****         05 FILLER                        PIC X(62).
010217****         05 FILLER                        PIC X(34).
                   05 FILLER                        PIC X(28).



