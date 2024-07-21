      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2081.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2081.CPv  $
      *
      *   Rev 2.4   Sep 19 2016 07:30:54   t13ae31
      *AKI01458: SUH Namensvorwort bei VP Copies
      *
      *   Rev 2.3   15 Feb 2008 15:33:02   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:00   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.1   30 Oct 2007 12:19:04   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.0   Sep 17 2007 11:12:00   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle HAUSRATVERTRAG        *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! Datumsfelder auf X(10)               ! HHU     !      *
      *        !                                      !         !      *
080816*08.08.16! neue Felder SMART-HOME               ! HHU     !      *
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
      *     LAENGE: 1100 BYTES                STAND: 08.08.2016        *
      **************************************************************** *
 
       01  DRKI2081.
 
                    05  REOB-AUS-510-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-510-VERTRAGS-ID        PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 5362
                   05  REOB-AUS-510-CVNR            PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5360
                   05  REOB-AUS-510-CVEZHOSTNR      PIC X(15).
 
      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5325
                   05  REOB-AUS-510-CHRRSPLZ        PIC X(6).
                   05  REOB-AUS-510-CHRRSPLZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl
 
      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5326
                   05  REOB-AUS-510-CHRRSSTR        PIC X(30).
                   05  REOB-AUS-510-CHRRSSTR-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Strasse
 
      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5327
                   05  REOB-AUS-510-CHRRSORT        PIC X(30).
                   05  REOB-AUS-510-CHRRSORT-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Ort
 
      ***  Datenfeld: Risikoanschrift: Länderkennzeichen FeldNr. 5328
                   05  REOB-AUS-510-NHRRSNKZ        PIC 9(9).
                   05  REOB-AUS-510-NHRRSNKZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Länderkennzeichen
 
      ***  Datenfeld: Bedingung FeldNr. 5329
                   05  REOB-AUS-510-CHRLBVBEDG      PIC X(16).
                   05  REOB-AUS-510-CHRLBVBEDG-UPD  PIC X.
      *            J = Aenderung Bedingung
 
      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5330
                   05  REOB-AUS-510-CHRRSHSNR       PIC X(15).
                   05  REOB-AUS-510-CHRRSHSNR-UPD   PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer
 
      ***  Datenfeld: Klausel/Tarif FeldNr. 5331
                   05  REOB-AUS-510-CHRLBKLAUS      PIC X(80).
                   05  REOB-AUS-510-CHRLBKLAUS-UPD  PIC X.
      *            J = Aenderung Klausel/Tarif
 
      ***  Datenfeld: Zusatzinfo FeldNr. 5333
                   05  REOB-AUS-510-CHRZUSINFO      PIC X(100).
                   05  REOB-AUS-510-CHRZUSINFO-UPD  PIC X.
      *            J = Aenderung Zusatzinfo
 
      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5334
      ***            05  REOB-AUS-510-CHRDTLSTFV      PIC 9(8).
110208             05  REOB-AUS-510-CHRDTLSTFV      PIC X(10).
                   05  REOB-AUS-510-CHRDTLSTFV-UPD  PIC X.
      *            J = Aenderung Leistungsfrei von Datum
 
      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5335
      ***            05  REOB-AUS-510-CHRDTLSTFB      PIC 9(8).
110208             05  REOB-AUS-510-CHRDTLSTFB      PIC X(10).
                   05  REOB-AUS-510-CHRDTLSTFB-UPD  PIC X.
      *            J = Aenderung Leistungsfrei bis Datum
 
      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5336
                   05  REOB-AUS-510-NHRJNSB         PIC XX.
                   05  REOB-AUS-510-NHRJNSB-UPD     PIC X.
      *            J = Aenderung SelbstBeteiligung Ja/Nein
 
      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5337
                   05  REOB-AUS-510-NHRSBPRZ        PIC 999.
                   05  REOB-AUS-510-NHRSBPRZ-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung Prozent
 
      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5338
                   05  REOB-AUS-510-NHRSBFES        PIC 9(5).
                   05  REOB-AUS-510-NHRSBFES-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung Fest
 
      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5339
                   05  REOB-AUS-510-CHRSBTXT        PIC X(50).
                   05  REOB-AUS-510-CHRSBTXT-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung Text
 
      ***  Datenfeld: Nutzung FeldNr. 5340
                   05  REOB-AUS-510-NHRLBNUTZ       PIC 9(9).
                   05  REOB-AUS-510-NHRLBNUTZ-UPD   PIC X.
      *            J = Aenderung Nutzung
 
      ***  Datenfeld: Versicherungssumme FeldNr. 5341
                   05  REOB-AUS-510-NHRVSUM         PIC 9(8).
                   05  REOB-AUS-510-NHRVSUM-UPD     PIC X.
      *            J = Aenderung Versicherungssumme
 
      ***  Datenfeld: Wohnfläche in qm FeldNr. 5342
                   05  REOB-AUS-510-NHRWOHNFL       PIC 9(5).
                   05  REOB-AUS-510-NHRWOHNFL-UPD   PIC X.
      *            J = Aenderung Wohnfläche im qm
 
      ***  Datenfeld: Unterversicherungsverzicht Ja/Nein FeldNr. 5343
                   05  REOB-AUS-510-NHRJNUVZ        PIC XX.
                   05  REOB-AUS-510-NHRJNUVZ-UPD    PIC X.
      *            J = Aenderung Unterversicherungsverzicht Ja/Nein
 
      ***  Datenfeld: Gebäudetyp FeldNr. 5344
                   05  REOB-AUS-510-NHRLBGEBTY      PIC 9(9).
                   05  REOB-AUS-510-NHRLBGEBTY-UPD  PIC X.
      *            J = Aenderung Gebäudetyp
 
      ***  Datenfeld: Eigentumsverhältnisse FeldNr. 5345
                   05  REOB-AUS-510-NHRLBEIGEN      PIC 9(9).
                   05  REOB-AUS-510-NHRLBEIGEN-UPD  PIC X.
      *            J = Aenderung Eigentumsverhältnisse
 
      ***  Datenfeld: Tarifzone FeldNr. 5346
                   05  REOB-AUS-510-CHRZONE         PIC X(4).
                   05  REOB-AUS-510-CHRZONE-UPD     PIC X.
      *            J = Aenderung Tarifzone
 
      ***  Datenfeld: Bauartklasse FeldNr. 5347
                   05  REOB-AUS-510-NHRLBBRTKL      PIC 9(9).
                   05  REOB-AUS-510-NHRLBBRTKL-UPD  PIC X.
      *            J = Aenderung Bauartklasse
 
      ***  Datenfeld: Alarmanlage Ja/Nein FeldNr. 5348
                   05  REOB-AUS-510-NHRJNALARM      PIC XX.
                   05  REOB-AUS-510-NHRJNALARM-UPD  PIC X.
      *            J = Aenderung Alarmanlage Ja/Nein
 
      ***  Datenfeld: Feuer Ja/Nein FeldNr. 5349
                   05  REOB-AUS-510-NHRJNFE         PIC XX.
                   05  REOB-AUS-510-NHRJNFE-UPD     PIC X.
      *            J = Aenderung Feuer Ja/Nein
 
      ***  Datenfeld: Sturm Ja/Nein FeldNr. 5350
                   05  REOB-AUS-510-NHRJNST         PIC XX.
                   05  REOB-AUS-510-NHRJNST-UPD     PIC X.
      *            J = Aenderung Sturm Ja/Nein
 
      ***  Datenfeld: Hagel Ja/Nein FeldNr. 5351
                   05  REOB-AUS-510-NHRJNHA         PIC XX.
                   05  REOB-AUS-510-NHRJNHA-UPD     PIC X.
      *            J = Aenderung Hagel Ja/Nein
 
      ***  Datenfeld: Einbruch/Diebstahl Ja/Nein FeldNr. 5352
                   05  REOB-AUS-510-NHRJNED         PIC XX.
                   05  REOB-AUS-510-NHRJNED-UPD     PIC X.
      *            J = Aenderung Einbruch/Diebstahl Ja/Nein
 
      ***  Datenfeld: Glas Ja/Nein FeldNr. 5353
                   05  REOB-AUS-510-NHRJNGL         PIC XX.
                   05  REOB-AUS-510-NHRJNGL-UPD     PIC X.
      *            J = Aenderung Glas Ja/Nein
 
      ***  Datenfeld: Leitungswasser Ja/Nein FeldNr. 5354
                   05  REOB-AUS-510-NHRJNLW         PIC XX.
                   05  REOB-AUS-510-NHRJNLW-UPD     PIC X.
      *            J = Aenderung Leitungswasser Ja/Nein
 
      ***  Datenfeld: Elementarschäden Ja/Nein FeldNr. 5355
 
                   05  REOB-AUS-510-NHRJNEL         PIC XX.
                   05  REOB-AUS-510-NHRJNEL-UPD     PIC X.
      *            J = Aenderung Elementarschäden
 
      ***  Datenfeld: Bezugsprämie FeldNr. 5356
                   05  REOB-AUS-510-NHRBZGPRM       PIC 999V999.
                   05  REOB-AUS-510-NHRBZGPRM-UPD   PIC X.
      *            J = Aenderung Bezugsprämie
 
      ***  Datenfeld: Leistungstext FeldNr. 5357
                   05  REOB-AUS-510-CHRTXLEIST      PIC X(40).
                   05  REOB-AUS-510-CHRTXLEIST-UPD  PIC X.
      *            J = Aenderung Leistungstext
 
      ***  Datenfeld: Summenanpassungsklausel FeldNr. 5358
                   05  REOB-AUS-510-CHRTXSUAK       PIC X(32).
                   05  REOB-AUS-510-CHRTXSUAK-UPD   PIC X.
      *            J = Aenderung Summenanpassungsklausel
 
      ***  Datenfeld: Produkt FeldNr. 5359
                   05  REOB-AUS-510-CHRPRD          PIC X(30).
                   05  REOB-AUS-510-CHRPRD-UPD      PIC X.
      *            J = Aenderung Produkt
 
      ***  Datenfeld: Mailingaktion FeldNr. 5942
                   05  REOB-AUS-510-CMAILING        PIC X(70).
                   05  REOB-AUS-510-CMAILING-UPD    PIC X.
      *            J = Aenderung Mailingaktion
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5521
                   05  REOB-AUS-510-NDIFFDECKJN     PIC X(2).
                   05  REOB-AUS-510-NDIFFDECKJN-UPD  PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5522
      ***            05  REOB-AUS-510-CUMSTELLTERM    PIC 9(8).
110208             05  REOB-AUS-510-CUMSTELLTERM    PIC X(10).
                   05  REOB-AUS-510-CUMSTELLTER-UPD  PIC X.
      *            J = Aenderung Umstellungstermin
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5524
                   05  REOB-AUS-510-CGESELL         PIC X(50).
                   05  REOB-AUS-510-CGESELL-UPD     PIC X.
      *            J = Aenderung Gesellschaft-Klartext
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 5525
                   05  REOB-AUS-510-CDIFFVSNR       PIC X(40).
                   05  REOB-AUS-510-CDIFFVSNR-UPD   PIC X.
      *            J = Aenderung Versicherungsnummer
 
      ***  Datenfeld: Kündigungsbestätigung Ja/Nein FeldNr. 5526
                   05  REOB-AUS-510-NKUENDBEST      PIC X(2).
                   05  REOB-AUS-510-NKUENDBEST-UPD  PIC X.
      *            J = Aenderung Kündigungsbestätigung Ja/Nein
 
      ***  Datenfeld: Zielgruppe FeldNr. 5986
                   05  REOB-AUS-510-CZIELGRP        PIC X(30).
                   05  REOB-AUS-510-CZIELGRP-UPD    PIC X.
      *            J = Aenderung Zielgruppe
 
080816***  Datenfeld: SMART-HOME Paket FeldNr. 5730
080816             05  REOB-AUS-510-SMHPAKET        PIC X(02).
080816             05  REOB-AUS-510-SMHPAKET-UPD    PIC X.
080816*            J = Aenderung SMART-HOME PAKET
 
080816***  Datenfeld: SMART-HOME DATUM FeldNr. 5731
080816             05  REOB-AUS-510-SMHDATUM        PIC X(10).
080816             05  REOB-AUS-510-SMHDATUM-UPD    PIC X.
080816*            J = Aenderung SMART-HOME DATUM
 
080816***  Datenfeld: SMART-HOME TRACKID FeldNr. 5732
080816             05  REOB-AUS-510-SMHTRACKID      PIC X(60).
080816             05  REOB-AUS-510-SMHTRACKID-UPD  PIC X.
080816*            J = Aenderung SMART-HOME TRACKID
 
080816***  Datenfeld: SMART-HOME CONTROL FeldNr. 5733
080816             05  REOB-AUS-510-SMHCONTROLZ        PIC X(60).
080816             05  REOB-AUS-510-SMHCONTROLZ-UPD    PIC X.
080816*            J = Aenderung SMART-HOME PAKET
 
      *** interne laufende Nummer
                   05  REOB-AUS-510-INTLFDNR       PIC 9(4).
 
     ****            05 FILLER                        PIC X(28).
110208***          05 FILLER                        PIC X(22).
080816             05 FILLER                        PIC X(126).
 

