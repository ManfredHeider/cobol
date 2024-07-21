      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2082.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2082.CPv  $
      *
      *   Rev 2.3   15 Feb 2008 15:32:30   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:08   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:19:10   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:12:16   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle UNFALLVERTRAG         *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! Satzart rausnehmen                   !         !      *
      *        ! Datumsfelder auf X(10)               ! HHU     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
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
      *     LAENGE: 650 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2082.

                    05  REOB-AUS-520-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-520-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 5655
                   05  REOB-AUS-520-CVNR            PIC 9(15).

110208***  Schlüssel: Nummer der ALLFINANZ-Satzart
110208****            05  REOB-AUS-520-SATZART         PIC 999.

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5653
                   05  REOB-AUS-520-CVEZHOSTNR      PIC X(15).

      ***  Datenfeld: Bedingung FeldNr. 5639
                   05  REOB-AUS-520-CUFVBEDG        PIC X(16).
                   05  REOB-AUS-520-CUFVBEDG-UPD    PIC X.
      *            J = Aenderung Bedingung

      ***  Datenfeld: Risiko FeldNr. 5640
                   05  REOB-AUS-520-CUFLBRIS        PIC X(50).
                   05  REOB-AUS-520-CUFLBRIS-UPD    PIC X.
      *            J = Aenderung Risiko

      ***  Datenfeld: Klausel/Tarif FeldNr. 5641
                   05  REOB-AUS-520-CUFKLAUS        PIC X(80).
                   05  REOB-AUS-520-CUFKLAUS-UPD    PIC X.
      *            J = Aenderung Klausel/Tarif

      ***  Datenfeld: Zusatzinfo FeldNr. 5643
                   05  REOB-AUS-520-CUFZUSINFO      PIC X(100).
                   05  REOB-AUS-520-CUFZUSINFO-UPD  PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Anzahl versicherte Personen FeldNr. 5644
                   05  REOB-AUS-520-NUFVPANZ        PIC 9(5).
                   05  REOB-AUS-520-NUFVPANZ-UPD    PIC X.
      *            J = Aenderung Anzahl versicherte Personen

      ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
      ***             FeldNr. 5645
      ***            05  REOB-AUS-520-CUFDTLSTFV      PIC 9(8).
110208             05  REOB-AUS-520-CUFDTLSTFV      PIC X(10).
                   05  REOB-AUS-520-CUFDTLSTFV-UPD  PIC X.
      *            J = Aenderung Leistungsfrei von Datum

      ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
      ***             FeldNr. 5646
      ***            05  REOB-AUS-520-CUFDTLSTFB      PIC 9(8).
110208             05  REOB-AUS-520-CUFDTLSTFB      PIC X(10).
                   05  REOB-AUS-520-CUFDTLSTFB-UPD  PIC X.
      *            J = Aenderung Leistungsfrei bis Datum

      ***  Datenfeld: SelbstBeteiligung Ja/Nein FeldNr. 5647
                   05  REOB-AUS-520-NUFJNSB         PIC XX.
                   05  REOB-AUS-520-NUFJNSB-UPD     PIC X.
      *            J = Aenderung SelbstBeteiligung Ja/Nein

      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 5648
                   05  REOB-AUS-520-NUFSBPRZ        PIC 999.
                   05  REOB-AUS-520-NUFSBPRZ-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Prz

      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 5649
                   05  REOB-AUS-520-NUFSBFES        PIC 9(5).
                   05  REOB-AUS-520-NUFSBFES-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Fest

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 5650
                   05  REOB-AUS-520-CUFSBTXT        PIC X(50).
                   05  REOB-AUS-520-CUFSBTXT-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Text

      ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5651
                   05  REOB-AUS-520-NUFJNMBTR       PIC XX.
                   05  REOB-AUS-520-NUFJNMBTR-UPD   PIC X.
      *            J = Aenderung Mindestbeitrag Ja/Nein

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5652
      ***            05  REOB-AUS-520-CUFDTUMST       PIC 9(8).
110208             05  REOB-AUS-520-CUFDTUMST       PIC X(10).
                   05  REOB-AUS-520-CUFDTUMST-UPD   PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Mailingaktion FeldNr. 5919
                   05  REOB-AUS-520-CMAILING        PIC X(70).
                   05  REOB-AUS-520-CMAILING-UPD    PIC X.
      *            J = Aenderung Mailingaktion

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5547
      ***            05  REOB-AUS-520-CUMSTELLTERM    PIC 9(8).
110208             05  REOB-AUS-520-CUMSTELLTERM    PIC X(10).
                   05  REOB-AUS-520-CUMSTELLTER-UPD  PIC X.
      *            J = Aenderung Umstellungstermin

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5549
                   05  REOB-AUS-520-CGESELL         PIC X(50).
                   05  REOB-AUS-520-CGESELL-UPD     PIC X.
      *            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 5550
                   05  REOB-AUS-520-CDIFFVSNR       PIC X(40).
                   05  REOB-AUS-520-CDIFFVSNR-UPD   PIC X.
      *            J = Aenderung Versicherungsnummer

      ***  Datenfeld: Kündigungsbestätigung Ja/Nein
      ***             NKUENDBESTAET in Bestlief FeldNr. 5551
                   05  REOB-AUS-520-NKUENDBEST      PIC X(2).
                   05  REOB-AUS-520-NKUENDBEST-UPD  PIC X.
      *            J = Aenderung Kündigungsbestätigung Ja/Nein

      *** interne laufende Nummer
                   05  REOB-AUS-520-INTLFDNR       PIC 9(4).

    ******            05 FILLER                        PIC X(74).
110208             05 FILLER                        PIC X(69).

