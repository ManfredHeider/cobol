      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2083.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2083.CPv  $
      *
      *   Rev 2.6   Mar 19 2015 08:30:24   t13ae31
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 COPIES
      *+ FILES
      *+Jobs
      *
      *   Rev 2.5   Mar 28 2008 14:54:02   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.4   Mar 11 2008 10:41:36   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.3   20 Feb 2008 09:15:24   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:14   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.1   30 Oct 2007 12:19:18   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.0   Sep 17 2007 11:12:28   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VGVVERTRAG            *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! Beschreibungen überarbeitet          ! HHU     !      *
      *        ! Rohbauversicherung von X(5) auf 9(9) !         !      *
      *        ! Datumsfelder auf X(10)               !         !      *
      *        !                                      !         !      *
      *28.03.08!CVGBAUJ von X(10) auf 9(8)            ! HHU     !      *
      *        !                                      !         !      *
      *31.01.15!neues Feld sonstige Nutzflaeche       ! HHU     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 1.350 BYTES                 STAND: 10.10.2007      *
      **************************************************************** *
 
       01  DRKI2083.
 
                    05  REOB-AUS-521-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-521-VERTRAGS-ID        PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 5705
                   05  REOB-AUS-521-CVNR            PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5703
                   05  REOB-AUS-521-CVEZHOSTNR      PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5658
                   05  REOB-AUS-521-CVGRSPLZ        PIC X(6).
                   05  REOB-AUS-521-CVGRSPLZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5659
                   05  REOB-AUS-521-CVGRSSTR        PIC X(30).
                   05  REOB-AUS-521-CVGRSSTR-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Strasse
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5660
                   05  REOB-AUS-521-CVGRSHSNR       PIC X(15).
                   05  REOB-AUS-521-CVGRSHSNR-UPD   PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5661
                   05  REOB-AUS-521-CVGRSORT        PIC X(30).
                   05  REOB-AUS-521-CVGRSORT-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Ort
 
      ***  Datenfeld: Risikoanschrift: Länderkennzeichen FeldNr. 5662
                   05  REOB-AUS-521-NVGRSNKZ        PIC 9(9).
                   05  REOB-AUS-521-NVGRSNKZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Länderkennzeichen
 
      ***  Datenfeld: Bedingung FeldNr. 5663
                   05  REOB-AUS-521-CVGVBEDG        PIC X(16).
                   05  REOB-AUS-521-CVGVBEDG-UPD    PIC X.
      *            J = Aenderung Bedingung
 
      ***  Datenfeld: Risiko FeldNr. 5664
                   05  REOB-AUS-521-CVGRISIKO       PIC X(50).
                   05  REOB-AUS-521-CVGRISIKO-UPD   PIC X.
      *            J = Aenderung Risiko
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5665
                   05  REOB-AUS-521-CVGKLAUS        PIC X(80).
                   05  REOB-AUS-521-CVGKLAUS-UPD    PIC X.
      *            J = Aenderung Klausel/Tarif
 
      ***  Datenfeld: Beilagen FeldNr. 5666
                   05  REOB-AUS-521-CVGBEILGN       PIC X(19).
                   05  REOB-AUS-521-CVGBEILGN-UPD   PIC X.
      *            J = Aenderung Beilagen
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5668
                   05  REOB-AUS-521-CVGZUSINFO      PIC X(100).
                   05  REOB-AUS-521-CVGZUSINFO-UPD  PIC X.
      *            J = Aenderung Zusatzinfo
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5669
                   05  REOB-AUS-521-NVGJNFEUER      PIC XX.
                   05  REOB-AUS-521-NVGJNFEUER-UPD  PIC X.
      *            J = Aenderung Feuer Ja/Nein
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5670
                   05  REOB-AUS-521-NVGJNLW         PIC XX.
                   05  REOB-AUS-521-NVGJNLW-UPD     PIC X.
      *            J = Aenderung Leitungswasser Ja/Nein
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5671
                   05  REOB-AUS-521-NVGJNST         PIC XX.
                   05  REOB-AUS-521-NVGJNST-UPD     PIC X.
      *            J = Aenderung Sturm Ja/Nein
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5672
                   05  REOB-AUS-521-NVGJNHA         PIC XX.
                   05  REOB-AUS-521-NVGJNHA-UPD     PIC X.
      *            J = Aenderung Hagel Ja/Nein
 
      ***  Datenfeld: Elementarschäden Ja/Nein FeldNr. 5673
                   05  REOB-AUS-521-NVGJNSE         PIC XX.
                   05  REOB-AUS-521-NVGJNSE-UPD     PIC X.
      *            J = Aenderung Elementarschäden Ja/Nein
 
      ***  Datenfeld: Gebäudenutzung FeldNr. 5675
                   05  REOB-AUS-521-CVGGBNTZ        PIC X(26).
                   05  REOB-AUS-521-CVGGBNTZ-UPD    PIC X.
      *            J = Aenderung Gebäudenutzung Ja/Nein
 
      ***  Datenfeld: gewerbliche Fläche in Prozent FeldNr. 5677
                   05  REOB-AUS-521-NVGGEWFL        PIC 999.
                   05  REOB-AUS-521-NVGGEWFL-UPD    PIC X.
      *            J = Aenderung gewerbliche Fläche in Prozent
 
      ***  Datenfeld: Baujahr Format: JJJJ FeldNr. 5678
                   05  REOB-AUS-521-CVGBAUJ         PIC 9(8).
110208***            05  REOB-AUS-521-CVGBAUJ         PIC X(10).
                   05  REOB-AUS-521-CVGBAUJ-UPD     PIC X.
      *            J = Aenderung Baujahr
 
      ***  Datenfeld: Bauartklasse LB FeldNr. 5679
                   05  REOB-AUS-521-NVGLBBAUKL      PIC 9(9).
                   05  REOB-AUS-521-NVGLBBAUKL-UPD  PIC X.
      *            J = Aenderung Bauartklasse LB
 
      ***  Datenfeld: Rohbau bis Datum Format: JJJJMMTT FeldNr. 5680
      ***            05  REOB-AUS-521-CVGDTRBB        PIC 9(8).
110208             05  REOB-AUS-521-CVGDTRBB        PIC X(10).
                   05  REOB-AUS-521-CVGDTRBB-UPD    PIC X.
      *            J = Aenderung Rohbau bis Datum
 
      ***  Datenfeld: Leistungsart LB FeldNr. 5681
                   05  REOB-AUS-521-NVGLBLA         PIC 9(9).
                   05  REOB-AUS-521-NVGLBLA-UPD     PIC X.
      *            J = Aenderung Leistungsart LB
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5682
                   05  REOB-AUS-521-NVGJNUVZ        PIC XX.
                   05  REOB-AUS-521-NVGJNUVZ-UPD    PIC X.
      *            J = Aenderung Unterversicherungsverzicht Ja/Nein
 
      ***  Datenfeld: Grund für UVZ FeldNr. 5683
                   05  REOB-AUS-521-CVGUVZGRD       PIC X(40).
                   05  REOB-AUS-521-CVGUVZGRD-UPD   PIC X.
      *            J = Aenderung Grund für UnterVersicherungsverZicht
 
      ***  Datenfeld: Versicherungsumme 1914 FeldNr. 5684
                   05  REOB-AUS-521-NVGSUM1914      PIC 9(9).
                   05  REOB-AUS-521-NVGSUM1914-UPD  PIC X.
      *            J = Aenderung Versicherungsumme 1914
 
      ***  Datenfeld: Staffelprämie Ja/Nein FeldNr. 5685
                   05  REOB-AUS-521-NVGJNSTFPR      PIC XX.
                   05  REOB-AUS-521-NVGJNSTFPR-UPD  PIC X.
      *            J = Aenderung Staffelprämie Ja/Nein
 
      ***  Datenfeld: Zone Leitungswasser FeldNr. 5688
                   05  REOB-AUS-521-NVGLWZONE       PIC 9.
                   05  REOB-AUS-521-NVGLWZONE-UPD   PIC X.
      *            J = Aenderung Zone Leitungswasser
 
      ***  Datenfeld: Zone Sturm  FeldNr. 5689
                   05  REOB-AUS-521-NVGSTZONE       PIC 9.
                   05  REOB-AUS-521-NVGSTZONE-UPD   PIC X.
      *            J = Aenderung Zone Sturm
 
      ***  Datenfeld: Zone Erdbeben FeldNr. 5690
                   05  REOB-AUS-521-NVGEBZONE       PIC 9.
                   05  REOB-AUS-521-NVGEBZONE-UPD   PIC X.
      *            J = Aenderung Zone Erdbeben
 
      ***  Datenfeld: Zone sonstige Elementarschäden FeldNr. 5691
                   05  REOB-AUS-521-NVGSEZONE       PIC 9.
                   05  REOB-AUS-521-NVGSEZONE-UPD   PIC X.
      *            J = Aenderung Zone sonstige Elementarschäden
 
      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5692
                   05  REOB-AUS-521-NVGJNMBTR       PIC XX.
                   05  REOB-AUS-521-NVGJNMBTR-UPD   PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5693
      ***            05  REOB-AUS-521-CVGDTLSTFV      PIC 9(8).
110208             05  REOB-AUS-521-CVGDTLSTFV      PIC X(10).
                   05  REOB-AUS-521-CVGDTLSTFV-UPD  PIC X.
      *            J = Aenderung Leistungsfrei von Datum
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5694
      ***            05  REOB-AUS-521-CVGDTLSTFB      PIC 9(8).
110208             05  REOB-AUS-521-CVGDTLSTFB      PIC X(10).
                   05  REOB-AUS-521-CVGDTLSTFB-UPD  PIC X.
      *            J = Aenderung Leistungsfrei bis Datum
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5695
                   05  REOB-AUS-521-CVGTXTSB        PIC X(14).
                   05  REOB-AUS-521-CVGTXTSB-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Text
 
      ***  Datenfeld: PRM-Satz Hauptwagnis FeldNr. 5696
                   05  REOB-AUS-521-NVGHWPRMS       PIC 99V9(7).
                   05  REOB-AUS-521-NVGHWPRMS-UPD   PIC X.
      *            J = Aenderung PRM-Satz Hauptwagnis
 
      ***  Datenfeld: PRM-Satz weitere Elementargefahren FeldNr. 5697
                   05  REOB-AUS-521-NVGWEPRMS       PIC 99V9(4).
                   05  REOB-AUS-521-NVGWEPRMS-UPD   PIC X.
      *            J = Aenderung PRM-Satz weitere Elementargefahren
 
      ***  Datenfeld: Anzahl Vorschäden weitere Elementargefahren
      ***             FeldNr. 5698
                   05  REOB-AUS-521-NVGWEVSANZ      PIC 99.
                   05  REOB-AUS-521-NVGWEVSANZ-UPD  PIC X.
      *            J = Aenderung Anzahl Vorschäden weitere Elementar-
      *                gefahren
 
      ***  Datenfeld: Gefahrenklasse feuergefährlicher Betriebe/Lager
      ***             FeldNr. 5699
                   05  REOB-AUS-521-CVGGFKFBL       PIC X.
                   05  REOB-AUS-521-CVGGFKFBL-UPD   PIC X.
      *            J = Aenderung Gefahrenklasse feuergefährlicher
      *                Betriebe/Lager
 
      ***  Datenfeld: Fläche in Prozent Gefahrenklasse feuerge-
      ***             gefährlicher Betrag FeldNr. 5700
                   05  REOB-AUS-521-CVGFLFBL        PIC X(20).
                   05  REOB-AUS-521-CVGFLFBL-UPD    PIC X.
      *            J = Aenderung Fläche in Prozent Gefahrenklasse
      *                feuergefährlicher Betrag
 
      ***  Datenfeld: Produkt FeldNr. 5701
                   05  REOB-AUS-521-CVGPRD          PIC X(30).
                   05  REOB-AUS-521-CVGPRD-UPD      PIC X.
      *            J = Aenderung Produkt
 
      ***  Datenfeld: Gebäudeart FeldNr. 5707
                   05  REOB-AUS-521-CVGLBGEBAR      PIC X(50).
                   05  REOB-AUS-521-CVGLBGEBAR-UPD  PIC X.
      *            J = Aenderung Gebäudeart
 
      ***  Datenfeld: Betriebsart LB FeldNr. 5708
                   05  REOB-AUS-521-CVGLBBTART      PIC X(50).
                   05  REOB-AUS-521-CVGLBBTART-UPD  PIC X.
      *            J = Aenderung Betriebsart LB
 
      ***  Datenfeld: Modell FeldNr. 5920
                   05  REOB-AUS-521-CMODELL         PIC X(18).
                   05  REOB-AUS-521-CMODELL-UPD     PIC X.
      *            J = Aenderung Modell
 
      ***  Datenfeld: Monopol (Ja/Nein) FeldNr. 5921
                   05  REOB-AUS-521-NMONOPOL        PIC XX.
                   05  REOB-AUS-521-NMONOPOL-UPD    PIC X.
      *            J = Aenderung Monopol Ja/Nein
 
      ***  Datenfeld: Auslandsrisiko (Ja/Nein) FeldNr. 5922
                   05  REOB-AUS-521-NAUSLRIS        PIC XX.
                   05  REOB-AUS-521-NAUSLRIS-UPD    PIC X.
      *            J = Aenderung Auslandsrisiko Ja/Nein
 
      ***  Datenfeld: Gebäudebezeichnung FeldNr. 5923
                   05  REOB-AUS-521-CGEBBEZ         PIC X(80).
                   05  REOB-AUS-521-CGEBBEZ-UPD     PIC X.
      *            J = Aenderung Gebäudebezeichnung
 
      ***  Datenfeld: Rohbauversicherung FeldNr. 5924
     ****            05  REOB-AUS-521-CROHBAUV        PIC X(05).
110208             05  REOB-AUS-521-CROHBAUV        PIC 9(09).
                   05  REOB-AUS-521-CROHBAUV-UPD    PIC X.
      *            J = Aenderung Rohbauversicherung
 
      ***  Datenfeld: Wohnfläche FeldNr. 5925
                   05  REOB-AUS-521-NWOHNFL         PIC 9(5).
                   05  REOB-AUS-521-NWOHNFL-UPD     PIC X.
      *            J = Aenderung Wohnfläche
 
      ***  Datenfeld: Geschossanzahl FeldNr. 5926
                   05  REOB-AUS-521-NGESCHOSSE      PIC 99.
                   05  REOB-AUS-521-NGESCHOSSE-UPD  PIC X.
      *            J = Aenderung Geschossanzahl
 
      ***  Datenfeld: Keller vorhanden (Ja/Nein) FeldNr. 5927
                   05  REOB-AUS-521-NKELLER         PIC XX.
                   05  REOB-AUS-521-NKELLER-UPD     PIC X.
      *            J = Aenderung Keller vorhanden Ja/Nein
 
      ***  Datenfeld: Schwimmbad im Gebäude (Ja/Nein)FeldNr. 5928
                   05  REOB-AUS-521-NSCHWIMM        PIC XX.
                   05  REOB-AUS-521-NSCHWIMM-UPD    PIC X.
      *            J = Aenderung Schwimmbad im Gebäude (Ja/Nein)
 
      ***  Datenfeld: Nebengebäude Entschädigungsgrenze FeldNr. 5930
                   05  REOB-AUS-521-NNEBENGEB       PIC 9(9).
                   05  REOB-AUS-521-NNEBENGEB-UPD   PIC X.
      *            J = Aenderung Nebengebäude Entschädigungsgrenze
 
      ***  Datenfeld: Nebengebäude FeldNr. 5931
                   05  REOB-AUS-521-CNEBENGEB       PIC X(80).
                   05  REOB-AUS-521-CNEBENGEB-UPD   PIC X.
      *            J = Aenderung Nebengebäude
 
      ***  Datenfeld: Sonstiges FeldNr. 5932
                   05  REOB-AUS-521-CSONST          PIC X(80).
                   05  REOB-AUS-521-CSONST-UPD      PIC X.
      *            J = Aenderung Sonstiges
 
      ***  Datenfeld: Garagen/Carports FeldNr. 5933
                   05  REOB-AUS-521-NGARAGEN        PIC 99.
                   05  REOB-AUS-521-NGARAGEN-UPD    PIC X.
      *            J = Aenderung Garagen/Carports
 
      ***  Datenfeld: Zone Überschwemmung FeldNr. 5934
                   05  REOB-AUS-521-NVGUEBZONE      PIC 99.
                   05  REOB-AUS-521-NVGUEBZONE-UPD  PIC X.
      *            J = Aenderung Zone Überschwemmung
 
      ***  Datenfeld: Mailingaktion FeldNr. 5944
                   05  REOB-AUS-521-CMAILING        PIC X(70).
                   05  REOB-AUS-521-CMAILING-UPD     PIC X.
      *            J = Aenderung Mailingaktion
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5539
                   05  REOB-AUS-521-NDIFFDECKJN     PIC X(2).
                   05  REOB-AUS-521-NDIFFDECKJN-UPD   PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5540
      ***            05  REOB-AUS-521-CUMSTELLTERM    PIC 9(8).
110208             05  REOB-AUS-521-CUMSTELLTERM    PIC X(10).
                   05  REOB-AUS-521-CUMSTELLTERM-UPD  PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5542
                   05  REOB-AUS-521-CGESELL         PIC X(50).
                   05  REOB-AUS-521-CGESELL-UPD     PIC X.
      *            J = Aenderung Gesellschaft-Klartext
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5543
                   05  REOB-AUS-521-CDIFFVSNR       PIC X(40).
                   05  REOB-AUS-521-CDIFFVSNR-UPD   PIC X.
      *            J = Aenderung Versicherungsnummer
 
      ***  Datenfeld: Kündigungsbestätigung Ja/Nein FeldNr. 5544
                   05  REOB-AUS-521-NKUENDBEST      PIC X(2).
                   05  REOB-AUS-521-NKUENDBEST-UPD   PIC X.
      *            J = Aenderung Kündigungsbestätigung Ja/Nein
 
      ***  Datenfeld: Elementar bei Diff-Vers. Ja/Nein FeldNr. 5545
                   05  REOB-AUS-521-NELEDIFFJN      PIC X(2).
                   05  REOB-AUS-521-NELEDIFFJN-UPD  PIC X.
      *            J = Aenderung Elementar bei Diff-Vers. Ja/Nein
 
      ***  Datenfeld: Elementar bei Diff-Vers. Ja/Nein FeldNr. 5709
310115             05  REOB-AUS-521-SNUTZFL         PIC 9(5).
                   05  REOB-AUS-521-SNUTZFL-UPD     PIC X.
      *            J = Aenderung sonstige Nutzflaeche
 
      *** interne laufende Nummer
                   05  REOB-AUS-521-INTLFDNR       PIC 9(4).
 
    *****            05 FILLER                        PIC X(90).
                   05 FILLER                        PIC X(72).
 

