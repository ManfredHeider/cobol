      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3074.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3074.CPv  $
      *
      *   Rev 2.1   Jun 23 2016 08:37:20   t13ae31
      *AKI01426: G10 SUH VSP Reiseversicherung Pgme
      *
      *   Rev 2.0   Apr 21 2016 07:39:56   t13ae31
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
210416*21.04.16! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           S U H                                *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 420 BYTES                 STAND: 21.04.2016        *
      **************************************************************** *
 
       01  DRKI3074.
 
                    05  REOB-AUS-505-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-505-VERTRAGS-ID      PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 9250
                   05  REOB-AUS-505-CVNR             PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 9251
                   05  REOB-AUS-505-CVEZHOSTNR       PIC X(15).
 
      ***  Datenfeld: Bedingungen FeldNr. 9260
                   05  REOB-AUS-505-BEDINGUNG        PIC X(30).
                   05  REOB-AUS-505-BEDINGUNG-UPD    PIC X.
      **            J = Aenderung Bedingungen
 
      ***  Datenfeld: Versicherungssumme Reisegepäck FeldNr. 9261
                   05  REOB-AUS-505-VSUMRGEPCK       PIC 9(8).
                   05  REOB-AUS-505-VSUMRGEPCK-UPD   PIC X.
      **            J = Aenderung Versicherungssumme Reisegepäck
 
      ***  Datenfeld: Versicherungssumme Reiserücktritt FeldNr. 9262
                   05  REOB-AUS-505-VSUMRRUECK       PIC 9(8).
                   05  REOB-AUS-505-VSUMRRUECK-UPD   PIC X.
      **            J = Aenderung Versicherungssumme Reiserücktritt
 
      ****  Datenfeld: Mailingaktion FeldNr. 9663
                    05  REOB-AUS-505-MAILING         PIC X(70).
                    05  REOB-AUS-505-MAILING-UPD     PIC X.
     ***            J = Aenderung Mailingaktion
 
      ***  Datenfeld: Zusatzinfo FeldNr. 9664
                   05  REOB-AUS-505-ZUSATZINFO       PIC X(100).
                   05  REOB-AUS-505-ZUSATZINFO-UPD   PIC X.
      **            J = Aenderung Zusatzinfo
 
      ***  Datenfeld: Informationsquelle FeldNr. 9665
                   05 REOB-AUS-505-LVTQUELLP         PIC 9(9).
                   05 REOB-AUS-505-LVTQUELLP-UPD     PIC X.
      *      J = Aenderung Informationsquelle
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 9181
                   05  REOB-AUS-505-DIFFDECK         PIC X(2).
                   05  REOB-AUS-505-DIFFDECK-UPD     PIC X.
      **            J = Aenderung Differenzdeckung Ja/Nein
 
      ***  Datenfeld: Gesellschaft-Klartext FeldNr. 9182
                   05  REOB-AUS-505-GESELLSCHAFT      PIC X(50).
                   05  REOB-AUS-505-GESELLSCH-UPD     PIC X.
      **            J = Aenderung Gesellschaft-Klartext
 
      ***  Datenfeld: Versicherungsnummer FeldNr. 9183
                   05  REOB-AUS-505-DDVERSNR          PIC X(40).
                   05  REOB-AUS-505-DDVERSNR-UPD      PIC X.
      **            J = Aenderung  Versicherungsnummer
 
      ***  Datenfeld: Umstellungstermin Format: JJJJMMTT FeldNr. 9184
                   05  REOB-AUS-505-UMSTELLTERMIN     PIC X(10).
                   05  REOB-AUS-505-UMSTELLTRM-UPD    PIC X.
      **            J = Aenderung Umstellungstermin
 
      ***  Datenfeld: K}ndigungsbest{tigung Ja/Nein
      ***             NKUENDBESTAET in Bestlief FeldNr. 9185
                   05  REOB-AUS-505-DDKUENDBEST       PIC X(2).
                   05  REOB-AUS-505-DDKUENDBEST-UPD   PIC X.
      **            J = Aenderung K}ndigungsbest{tigung Ja/Nein
 
      *** interne laufende Nummer
                   05  REOB-AUS-505-INTLFDNR          PIC 9(4).
 
                   05 FILLER                          PIC X(32).
 

