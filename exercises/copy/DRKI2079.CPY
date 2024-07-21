      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2079.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2079.CPv  $
      *
      *   Rev 2.3   15 Feb 2008 15:32:46   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:15:42   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:18:50   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:11:34   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle GLASVERTRAG           *
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
      *     LAENGE: 650 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2079.

                    05  REOB-AUS-508-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-508-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 5294
                   05  REOB-AUS-508-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5292
                   05  REOB-AUS-508-CVEZHOSTNR      PIC X(15).

      ***  Datenfeld: Risikoanschrift: Postleitzahl FeldNr. 5276
                   05  REOB-AUS-508-CGLRSPLZ        PIC X(6).
                   05  REOB-AUS-508-CGLRSPLZ-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: Postleitzahl

      ***  Datenfeld: Risikoanschrift: Strasse FeldNr. 5277
                   05  REOB-AUS-508-CGLRSSTR        PIC X(30).
                   05  REOB-AUS-508-CGLRSSTR-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: STRASSE

      ***  Datenfeld: Risikoanschrift: Ort FeldNr. 5278
                   05  REOB-AUS-508-CGLRSORT        PIC X(30).
                   05  REOB-AUS-508-CGLRSORT-UPD    PIC X.
      *            J = Aenderung Risikoanschrift: ORT

      ***  Datenfeld: Risikoanschrift: Hausnummer FeldNr. 5279
                   05  REOB-AUS-508-CGLRSHSNR       PIC X(15).
                   05  REOB-AUS-508-CGLRSHSNR-UPD   PIC X.
      **            J = Aenderung Risikoanschrift: Hausnummer

      ****  Datenfeld: Risikoanschrift: Länderkennzeichen FeldNr. 5280
                    05  REOB-AUS-508-NGLRSNKZ        PIC 9(9).
                    05  REOB-AUS-508-NGLRSNKZ-UPD    PIC X.
      **            J = Aenderung Risikoanschrift: Länderkennzeichen

      ****  Datenfeld: Klausel/Tarif FeldNr. 5281
                    05  REOB-AUS-508-CGLKLAUS        PIC X(80).
                    05  REOB-AUS-508-CGLKLAUS-UPD    PIC X.
      **            J = Aenderung Klausel/Tarif

      ****  Datenfeld: Bedingung FeldNr. 5282
                    05  REOB-AUS-508-CGLLBVBEDG      PIC X(16).
                    05  REOB-AUS-508-CGLLBVBEDG-UPD  PIC X.
      **            J = Aenderung Bedingung

      ****  Datenfeld: Gebäudetyp FeldNr. 5283
                    05  REOB-AUS-508-NGLLBGEBTY      PIC 9(9).
                    05  REOB-AUS-508-NGLLBGEBTY-UPD  PIC X.
      **            J = Aenderung Gebäudetyp

      ****  Datenfeld: Risiko FeldNr. 5285
                    05  REOB-AUS-508-CGLLBRIS        PIC X(50).
                    05  REOB-AUS-508-CGLLBRIS-UPD    PIC X.
      **            J = Aenderung Risiko
     ***  Datenfeld: Mindestbeitrag Ja/Nein FeldNr. 5286
                  05  REOB-AUS-508-NGLJNMBTR       PIC XX.
                  05  REOB-AUS-508-NGLJNMBTR-UPD   PIC X.
     **           J = Aenderung Mindestbeitrag Ja/Nein

     ***  Datenfeld: SB-Prz FeldNr. 5287
                  05  REOB-AUS-508-NGLSBPRZ        PIC 99V99.
                  05  REOB-AUS-508-NGLSBPRZ-UPD    PIC X.
     **           J = Aenderung SB-Prz.

     ***  Datenfeld: SB-Fest FeldNr. 5288
                  05  REOB-AUS-508-NGLSBFES        PIC 9(5).
                  05  REOB-AUS-508-NGLSBFES-UPD    PIC X.
     **           J = Aenderung SB-Fest

     ***  Datenfeld: Zusatzinfo FeldNr. 5289
                  05  REOB-AUS-508-CGLZUSINFO      PIC X(100).
                  05  REOB-AUS-508-CGLZUSINFO-UPD  PIC X.
     **           J = Aenderung Zusatzinfo

     ***  Datenfeld: Leistungsfrei von Datum Format: JJJJMMTT
     ***             FeldNr. 5290
     ****           05  REOB-AUS-508-CGLDTLSTFV      PIC 9(8).
110208            05  REOB-AUS-508-CGLDTLSTFV      PIC X(10).
                  05  REOB-AUS-508-CGLDTLSTFV-UPD  PIC X.

     **           J = Aenderung Leistungsfrei von Datum
     ***  Datenfeld: Leistungsfrei bis Datum Format: JJJJMMTT
     ***             FeldNr. 5291
     ***           05  REOB-AUS-508-CGLDTLSTFB      PIC 9(8).
110208            05  REOB-AUS-508-CGLDTLSTFB      PIC X(10).
                  05  REOB-AUS-508-CGLDTLSTFB-UPD  PIC X.
     **           J = Aenderung Leistungsfrei bis Datum

     ***  Datenfeld: Mailingaktion FeldNr. 5940
                  05  REOB-AUS-508-CMAILING        PIC X(70).
                  05  REOB-AUS-508-CMAILING-UPD    PIC X.
     **           J = Aenderung Mailingaktion

     ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5533
                  05  REOB-AUS-508-NDIFFDECKJN     PIC X(2).
                  05  REOB-AUS-508-DIFFDECKJN-UPD  PIC X.
     **           J = Aenderung Differenzdeckung Ja/Nein

     ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 5534
     ***           05  REOB-AUS-508-CUMSTELLTERM    PIC 9(8).
110208            05  REOB-AUS-508-CUMSTELLTERM    PIC X(10).
                  05  REOB-AUS-508-CUMSTTERM-UPD   PIC X.
     **           J = Aenderung Umstellungstermin

     ***  Datenfeld: Gesellschaft-Klartext FeldNr. 5536
                  05  REOB-AUS-508-CGESELL         PIC X(50).
                  05  REOB-AUS-508-CGESELL-UPD     PIC X.
     **           J = Aenderung Gesellschaft-Klartext
     ***  Datenfeld: Versicherungsnummer FeldNr. 5537
                  05  REOB-AUS-508-CDIFFVSNR       PIC X(40).
                  05  REOB-AUS-508-CDIFFVSNR-UPD   PIC X.
     **           J = Aenderung V dersicherungsNr.

     ***  Datenfeld: Kündigungsbestätigung Ja/Nein FeldNr. 5538
                  05  REOB-AUS-508-NKUENDBEST      PIC X(2).
                  05  REOB-AUS-508-NKUENDBEST-UPD  PIC X.
     **           J = Aenderung V dersicherungsNr.

      *** interne laufende Nummer
                   05  REOB-AUS-508-INTLFDNR       PIC 9(4).

     ****            05 FILLER                        PIC X(37).
110208             05 FILLER                        PIC X(31).

