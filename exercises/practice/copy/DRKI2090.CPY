      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2090.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2090.CPv  $
      *
      *   Rev 2.3   Feb 18 2009 14:15:38   t13ae31
      *AKI00795: Differ.deck in FK Best.vertr. Aufber.
      *
      *   Rev 2.2   13 Nov 2007 11:17:06   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:06   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:14:02   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGRISIKOGRUPPE   *
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
      *     LAENGE: 300 BYTES                 STAND: 17.02.2009        *
      **************************************************************** *

       01  DRKI2090.

                    05  REOB-AUS-628-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-628-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6402
                   05  REOB-AUS-628-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6404
                   05  REOB-AUS-628-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6352
                   05  REOB-AUS-628-PVRGID          PIC 9(4).

      ***  Schlüssel: Subjekt-Host-ID FeldNr. 6399
      ***    Schl. nicht in der Datenbank aber in der KSDS
      ***     Datenbank = RISIKOHOSTID Verbindung zu 403
                   05  REOB-AUS-628-NVRGSUBHID      PIC 9(9).

      ***  Datenfeld: Risikogruppe-Typ FeldNr. 6353
                   05  REOB-AUS-628-NVRGTYP         PIC 9(9).
                   05  REOB-AUS-628-NVRGTYP-UPD     PIC X.
      *            J = Aenderung Risikogruppe-Typ

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6354
                   05  REOB-AUS-628-CVRGNAME        PIC X(50).
                   05  REOB-AUS-628-CVRGNAME-UPD    PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Beginndatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6355
                   05  REOB-AUS-628-CVRGBEG         PIC X(10).
                   05  REOB-AUS-628-CVRGBEG-UPD     PIC X.
      *            J = Aenderung Beginndatum der Risikogruppe

      ***  Datenfeld: Ablaufdatum der Risikogruppe Format: JJJJMMTT
      ***             FeldNr. 6356
                   05  REOB-AUS-628-CVRGABL         PIC X(10).
                   05  REOB-AUS-628-CVRGABL-UPD     PIC X.
      *            J = Aenderung Ablaufdatum der Risikogruppe

      ***  Datenfeld: Nettobeitrag FeldNr. 6357
                   05  REOB-AUS-628-NVRGNBTRG       PIC 9(8)V99.
                   05  REOB-AUS-628-NVRGNBTRG-UPD   PIC X.
      *            J = Aenderung  Nettobeitrag

     ***  Datenfeld: ID übergeordnete Risikogruppe FeldNr. 6358
                  05  REOB-AUS-628-NVRGUEBID       PIC 9(4).
                  05  REOB-AUS-628-NVRGUEBID-UPD   PIC X.
     **           J = Aenderung ID übergeordnete Risikogruppe

170209***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9147
                   05 REOB-AUS-628-NDIFFDECKJN   PIC X(2).
                   05 REOB-AUS-628-NDIFFDECK-UPD PIC X.

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9148
                   05 REOB-AUS-628-CGESELL       PIC X(50).
                   05 REOB-AUS-628-CGESELL-UPD   PIC X.

      ***  Datenfeld: Versicherungsnummer FeldNr. 9149
                   05 REOB-AUS-628-CDIFFVSNR     PIC X(40).
                   05 REOB-AUS-628-CDIFFVSNR-UPD PIC X.

      ***  Datenfeld: Umstellungstermin FeldNr. 9150 i.d.F. YYYYMMDD
                   05 REOB-AUS-628-CUMSTELLTERM  PIC X(10).
                   05 REOB-AUS-628-CUMSTELL-UPD  PIC X.

      ***  Datenfeld: Kuendigungsbestaetigung Ja/Nein FeldNr. 9151
                   05 REOB-AUS-628-NKUENDBESTAET       PIC X(2).
                   05 REOB-AUS-628-NKUENDBEST-UPD      PIC X.

      *** interne laufende Nummer
                   05  REOB-AUS-628-INTLFDNR       PIC 9(4).

      ****            05 FILLER                        PIC X(40).
170209             05 FILLER                        PIC X(31).

