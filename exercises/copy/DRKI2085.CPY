      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2085.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2085.CPv  $
      *
      *   Rev 2.3   Dec 23 2008 11:42:24   t13ae31
      *AKI00772: Reisegepäckvers. Aufbereitung
      *
      *   Rev 2.2   13 Nov 2007 11:16:30   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:19:32   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:12:56   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle PKVERTRAG             *
      *         Aenderungskennzeichen auf Feldebene                    *
      *   ab 15.12.2008 für REISEGEPÄCK geändert                       *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *15.12.08! da 524 nicht benötigt wird für neue  ! HHU     !      *
      *        ! 504 Reisegepäck verwendet            !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 400 BYTES                 STAND: 15.12.2008        *
      **************************************************************** *

       01  DRKI2085.

                    05  REOB-AUS-504-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-504-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 9189
                   05  REOB-AUS-504-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9191
                   05  REOB-AUS-504-CVEZHOSTNR      PIC X(15).

      ***  Datenfeld: Bedingungen FeldNr. 9177
                   05  REOB-AUS-504-BEDINGUNG        PIC X(30).
                   05  REOB-AUS-504-BEDINGUNG-UPD    PIC X.
      **            J = Aenderung Bedingungen

      ***  Datenfeld: Versicherungssumme FeldNr. 9178
                   05  REOB-AUS-504-VERSSUMME      PIC 9(8).
                   05  REOB-AUS-504-VERSSUMME-UPD  PIC X.
      **            J = Aenderung Versicherungssumme

      ****  Datenfeld: Mailingaktion FeldNr. 9179
                    05  REOB-AUS-504-MAILING        PIC X(70).
                    05  REOB-AUS-504-MAILING-UPD    PIC X.
     ***            J = Aenderung Mailingaktion

      ***  Datenfeld: Zusatzinfo FeldNr. 9180
                   05  REOB-AUS-504-ZUSATZINFO      PIC X(100).
                   05  REOB-AUS-504-ZUSATZINFO-UPD  PIC X.
      **            J = Aenderung Zusatzinfo

      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9181
                   05  REOB-AUS-504-DIFFDECK     PIC X(2).
                   05  REOB-AUS-504-DIFFDECK-UPD  PIC X.
      **            J = Aenderung Differenzdeckung Ja/Nein

      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9182
                   05  REOB-AUS-504-GESELLSCHAFT         PIC X(50).
                   05  REOB-AUS-504-GESELLSCH-UPD     PIC X.
      **            J = Aenderung Gesellschaft-Klartext

      ***  Datenfeld: Versicherungsnummer FeldNr. 9183
                   05  REOB-AUS-504-DDVERSNR       PIC X(40).
                   05  REOB-AUS-504-DDVERSNR-UPD   PIC X.
      **            J = Aenderung  Versicherungsnummer

      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 9184
                   05  REOB-AUS-504-UMSTELLTERMIN      PIC X(10).
                   05  REOB-AUS-504-UMSTELLTRM-UPD     PIC X.
      **            J = Aenderung Umstellungstermin

      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein
      ***             NKUENDBESTAET in Bestlief FeldNr. 9185
                   05  REOB-AUS-504-DDKUENDBEST      PIC X(2).
                   05  REOB-AUS-504-DDKUENDBEST-UPD  PIC X.
      **            J = Aenderung K}ndigungsbest{tigung Ja/Nein

      *** interne laufende Nummer
                   05  REOB-AUS-504-INTLFDNR       PIC 9(4).

                   05 FILLER                        PIC X(31).

