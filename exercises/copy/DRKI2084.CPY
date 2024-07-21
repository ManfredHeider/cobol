      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2084.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2084.CPv  $
      *
      *   Rev 2.3   Feb 18 2009 14:15:34   t13ae31
      *AKI00795: Differ.deck in FK Best.vertr. Aufber.
      *
      *   Rev 2.2   13 Nov 2007 11:16:22   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:19:24   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:12:42   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle FKVERTRAG             *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
170209*17.02.09! Differenzdeckung FK eingebaut        ! HHU     !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 450 BYTES                 STAND: 17.02.2009        *
      **************************************************************** *

       01  DRKI2084.

                    05  REOB-AUS-523-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-523-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 5890
                   05  REOB-AUS-523-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 5892
                   05  REOB-AUS-523-CVEZHOSTNR      PIC X(15).

      ***  Datenfeld: Produkt/Tarif FeldNr. 5894
                   05  REOB-AUS-523-CFKPROD         PIC X(50).
                   05  REOB-AUS-523-CFKPROD-UPD     PIC X.
      *            J = Aenderung Produkt/Tarif

      ***  Datenfeld: Bedingungen FeldNr. 5895
                   05  REOB-AUS-523-CFKVBEDG        PIC X(20).
                   05  REOB-AUS-523-CFKVBEDG-UPD    PIC X.
      *            J = Aenderung Bedingungen

      ***  Datenfeld: Zusatzinfo FeldNr. 5896
                   05  REOB-AUS-523-CFKZUSINFO      PIC X(100).
                   05  REOB-AUS-523-CFKZUSINFO-UPD  PIC X.
      *            J = Aenderung Zusatzinfo

      ***  Datenfeld: Mailingaktion FeldNr. 5939
                   05  REOB-AUS-523-CMAILING        PIC X(70).
                   05  REOB-AUS-523-CMAILING-UPD    PIC X.
      *            J = Aenderung Mailingaktion

170209****  Datenfeld: Gesellschaft-Klartext FeldNr. 9152
                   05 REOB-AUS-523-CGESELL       PIC X(50).
                   05 REOB-AUS-523-CGESELL-UPD   PIC X.

      ****  Datenfeld: Versicherungsnummer FeldNr. 9153
                   05 REOB-AUS-523-CDIFFVSNR     PIC X(40).
                   05 REOB-AUS-523-CDIFFVSNR-UPD PIC X.

      ****  Datenfeld: Umstellungstermin FeldNr. 9154 i.d.F. YYYYMMDD
                   05 REOB-AUS-523-CUMSTELLTERM  PIC X(10).
                   05 REOB-AUS-523-CUMSTELL-UPD  PIC X.

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9155
                   05 REOB-AUS-523-NKUENDBESTAET  PIC XX.
                   05 REOB-AUS-523-NKUENDBEST-UPD PIC X.

      *** interne laufende Nummer
                   05  REOB-AUS-523-INTLFDNR       PIC 9(4).

     ****            05 FILLER                        PIC X(58).
170209             05 FILLER                        PIC X(52).

