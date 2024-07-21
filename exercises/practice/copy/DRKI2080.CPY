      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2080.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2080.CPv  $
      *
      *   Rev 2.3   15 Feb 2008 15:32:54   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:15:52   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:18:56   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:11:48   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle HAFTPFLICHTVERTRAG    *
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
      *     LAENGE: 950 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2080.

                    05  REOB-AUS-509-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-509-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 5322
                   05  REOB-AUS-509-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5320
                   05  REOB-AUS-509-CVEZHOSTNR      PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 5297
                   05  REOB-AUS-509-CHFVBEDG        PIC X(16).
                   05  REOB-AUS-509-CHFVBEDG-UPD    PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risiko FeldNr. 5298
                   05  REOB-AUS-509-CHFLBRIS        PIC X(50).
                   05  REOB-AUS-509-CHFLBRIS-UPD    PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Klausel/Tarif FeldNr. 5299
                   05  REOB-AUS-509-CHFKLAUS        PIC X(80).
                   05  REOB-AUS-509-CHFKLAUS-UPD    PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Zusatzinfo FeldNr. 5301
                   05  REOB-AUS-509-CHFZUSINFO      PIC X(100).
                   05  REOB-AUS-509-CHFZUSINFO-UPD  PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5302
                   05  REOB-AUS-509-CHFRSPLZ        PIC X(6).
                   05  REOB-AUS-509-CHFRSPLZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5303
                   05  REOB-AUS-509-CHFRSSTR        PIC X(30).
                   05  REOB-AUS-509-CHFRSSTR-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Strasse

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5304
                   05  REOB-AUS-509-CHFRSORT        PIC X(30).
                   05  REOB-AUS-509-CHFRSORT-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Ort

      ***  Datenfeld: Risikoanschrift: Länderkennzeichen FeldNr. 5305
                   05  REOB-AUS-509-NHFRSNKZ        PIC 9(9).
                   05  REOB-AUS-509-NHFRSNKZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Länderkennzeichen

      ***  Datenfeld: Mitversicherte Personen FeldNr. 5306
                   05  REOB-AUS-509-CHFMVPERS       PIC X(35).
                   05  REOB-AUS-509-CHFMVPERS-UPD   PIC X.
      *            J = Aenderung Mitversicherte Person

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5307
                   05  REOB-AUS-509-CHFRSHSNR       PIC X(15).
                   05  REOB-AUS-509-CHFRSHSNR-UPD   PIC X.
      *            J = Aenderung Risikoanschrift: Hausnummer

      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 5308
                   05  REOB-AUS-509-NHFJNREGUL      PIC XX.
                   05  REOB-AUS-509-NHFJNREGUL-UPD  PIC X.
      *            J = Aenderung Regulierung Ja/Nein

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5309
                   05  REOB-AUS-509-NHFJNMBTR       PIC XX.
                   05  REOB-AUS-509-NHFJNMBTR-UPD   PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5310
      ***            05  REOB-AUS-509-CHFDTLSTFV      PIC 9(8).
110208             05  REOB-AUS-509-CHFDTLSTFV      PIC X(10).
                   05  REOB-AUS-509-CHFDTLSTFV-UPD  PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum FeldNr. 5311
      ***            05  REOB-AUS-509-CHFDTLSTFB      PIC 9(8).
110208             05  REOB-AUS-509-CHFDTLSTFB      PIC X(10).
                   05  REOB-AUS-509-CHFDTLSTFB-UPD  PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

     ***  Datenfeld: SB Ja/Nein FeldNr. 5312
                  05  REOB-AUS-509-NHFJNSB         PIC XX.
                  05  REOB-AUS-509-NHFJNSB-UPD     PIC X.
     **           J = Aenderung SB ja/Nein

     ***  Datenfeld: SB-Prz FeldNr. 5313
                  05  REOB-AUS-509-NHFSBPRZ        PIC 999.
                  05  REOB-AUS-509-NHFSBPRZ-UPD    PIC X.
     **           J = Aenderung SB Prz

     ***  Datenfeld: SB-Fest FeldNr. 5314
                  05  REOB-AUS-509-NHFSBFES        PIC 9(5).
                  05  REOB-AUS-509-NHFSBFES-UPD    PIC X.
     **           J = Aenderung SB Fest

     ***  Datenfeld: SB-Text FeldNr. 5315
                  05  REOB-AUS-509-CHFSBTXT        PIC X(50).
                  05  REOB-AUS-509-CHFSBTXT-UPD    PIC X.
     **           J = Aenderung SB Test

     ***  Datenfeld: Auslandschäden Ja/Nein FeldNr. 5316
                  05  REOB-AUS-509-NHFJNAUSL       PIC XX.
                  05  REOB-AUS-509-NHFJNAUSL-UPD   PIC X.
     **           J = Aenderung Auslandschäden

      ***  Datenfeld: Verwandschaftsschäden Ja/Nein FeldNr. 5317
                   05 REOB-AUS-509-NHFJNVERW       PIC XX.
                   05 REOB-AUS-509-NHFJNVERW-UPD   PIC X.
      *            J = Aenderung Verwandschaftsschäden Ja/Nein

      ***  Datenfeld: Produkt FeldNr. 5318
                   05 REOB-AUS-509-CHFPRD          PIC X(30).
                   05 REOB-AUS-509-CHFPRD-UPD      PIC X.
      *            J = Aenderung Produkt

      ***  Datenfeld: Namen Elternteil/volljährige Kinder FeldNr. 5319
                   05  REOB-AUS-509-CHNAMENEK       PIC X(45).
                   05  REOB-AUS-509-CHNAMENEK-UPD   PIC X.
      *            J = Aenderung Namen Elternteil/volljährige Kinder

      ***  Datenfeld: Namen Elternteil / volljährige Kinder FeldNr. 5859
                   05  REOB-AUS-509-CHNAMEKIND      PIC X(45).
                   05  REOB-AUS-509-CHNAMEKIND-UPD  PIC X.
      *            J = Aenderung Namen Elternteil/volljährige Kinder

      ***  Datenfeld: abweichende VP FeldNr. 5860
                   05  REOB-AUS-509-CHNAMEABVP      PIC X(35).
                   05  REOB-AUS-509-CHNAMEABVP-UPD  PIC X.
      *            J = Aenderung abweichende VP

      ***  Datenfeld: Mailingaktion FeldNr. 5941
                   05  REOB-AUS-509-CMAILING        PIC X(70).
                   05  REOB-AUS-509-CMAILING-UPD    PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5527
                   05  REOB-AUS-509-NDIFFDECKJN     PIC X(2).
                   05  REOB-AUS-509-NDIFFDECKJN-UPD  PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5528
      ***            05  REOB-AUS-509-CUMSTELLTERM    PIC 9(8).
110208             05  REOB-AUS-509-CUMSTELLTERM    PIC X(10).
                   05  REOB-AUS-509-CUMSTELLTRM-UPD  PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5530
                   05  REOB-AUS-509-CGESELL         PIC X(50).
                   05  REOB-AUS-509-CGESELL-UPD     PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5531
                   05  REOB-AUS-509-CDIFFVSNR       PIC X(40).
                   05  REOB-AUS-509-CDIFFVSNR-UPD   PIC X.
      *            J = Aenderung  Versicherungsnummer

      ***  Datenfeld: Kündigungsbestätigung Ja/Nein FeldNr. 5532
                   05  REOB-AUS-509-NKUENDBEST      PIC X(2).
                   05  REOB-AUS-509-NKUENDBEST-UPD  PIC X.
      *            J = Aenderung Kündigungsbestätigung Ja/Nein

      ***  Datenfeld: Zielgruppe FeldNr. 5987
                   05  REOB-AUS-509-CZIELGRP        PIC X(30).
                   05  REOB-AUS-509-CZIELGRP-UPD    PIC X.
      *            J = Aenderung Zielgruppe

      *** interne laufende Nummer
                   05  REOB-AUS-509-INTLFDNR       PIC 9(4).

     ***            05 FILLER                        PIC X(59).
110208             05 FILLER                        PIC X(53).

