      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2087.CPY  $                                 0001000
 
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2087.CPv  $
      *
      *   Rev 2.6   Aug 10 2017 09:42:12   t13ae31
      *AKI01530: G10 SUH SA614 Beruf auf 48 St. Copies
      *
      *   Rev 2.5   Sep 19 2016 07:30:40   t13ae31
      *AKI01458: SUH Namensvorwort bei VP Copies
      *
      *   Rev 2.4   Mar 11 2008 10:41:44   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.3   20 Feb 2008 09:15:34   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:44   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.1   30 Oct 2007 12:19:46   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.0   Sep 17 2007 11:13:22   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle UNFALLVERSPERSON      *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! neue Felder 3235 - 3238              ! HHU     !      *
      *        ! Datumsfelder auf X(10)               !         !      *
      *        !                                      !         !      *
260716*AKI01458! neue Felder Namensvorwort            ! HHU     !      *
      *        !                                      !         !      *
170717*AKI01530! SA614 Beruf auf 48 Stellen           !P. Hübner!      *
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
      *     LAENGE: 300 BYTES                 STAND: 10.10.2007        *
      *     LAENGE: 400 BYTES                 STAND: 11.02.2008        *
260716*     LAENGE: 500 BYTES                 STAND: 26.07.2016        *
      **************************************************************** *
 
       01  DRKI2087.
 
                    05  REOB-AUS-614-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete
 
                    05 REOB-AUS-614-VERTRAGS-ID        PIC X(13).
 
      ***  Schlüssel: Vertragsnummer FeldNr. 6166
                   05  REOB-AUS-614-CVNR            PIC 9(15).
 
      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6164
                   05  REOB-AUS-614-CVEZHOSTNR      PIC X(15).
 
      ***  Schlüssel: versicherte Person laufende Nummer (Schlüssel)
      ***             FeldNr. 6144
                   05  REOB-AUS-614-PUFVPLFDNR      PIC 999.
 
      ***  Datenfeld: versicherte Person laufende Nummer (Maske)
      ***             FeldNr. 6145
                   05  REOB-AUS-614-NUFVPVBNR       PIC 999.
                   05  REOB-AUS-614-NUFVPVBNR-UPD   PIC X.
      *            J = Aenderung versicherte Person laufende Nummer
      *                (Maske)
 
      ***  Datenfeld: VP-Name FeldNr. 6146
                   05  REOB-AUS-614-CUFVPNAME       PIC X(25).
                   05  REOB-AUS-614-CUFVPNAME-UPD   PIC X.
      *            J = Aenderung VP-Name
 
      ***  Datenfeld: VP-Beruf FeldNr. 6147
170717***          05  REOB-AUS-614-CUFVPBERUF      PIC X(21).
170717             05  REOB-AUS-614-CUFVPBERUF      PIC X(48).
                   05  REOB-AUS-614-CUFVPBERUF-UPD  PIC X.
      *            J = Aenderung VP-Beruf
 
      ***  Datenfeld: VP-Geburtsdatum Format: JJJJMMTT FeldNr. 6148
      ***            05  REOB-AUS-614-CUFVPGEBD       PIC 9(8).
110208             05  REOB-AUS-614-CUFVPGEBD       PIC X(10).
                   05  REOB-AUS-614-CUFVPGEBD-UPD   PIC X.
      *            J = Aenderung VP-Beruf Gebdat
 
      ***  Datenfeld: VP-Gefahrengruppe FeldNr. 6149
                   05  REOB-AUS-614-CUFVPGEFGR      PIC X.
                   05  REOB-AUS-614-CUFVPGEFGR-UPD  PIC X.
      *            J = Aenderung VP-Gefahrengruppe
 
      ***  Datenfeld: VP-Beitrag FeldNr. 6150
                   05  REOB-AUS-614-NUFVPBTR        PIC 9(7)V99.
                   05  REOB-AUS-614-NUFVPBTR-UPD    PIC X.
      *            J = Aenderung VP-Beitrag
 
      ***  Datenfeld: Tarifart FeldNr. 6151
                   05  REOB-AUS-614-CUFTARIF        PIC X(30).
                   05  REOB-AUS-614-CUFTARIF-UPD    PIC X.
      *            J = Aenderung Tarifart
 
      ***  Datenfeld: Dynamik-Text FeldNr. 6152
                   05  REOB-AUS-614-CUFDYNTEXT      PIC X(5).
                   05  REOB-AUS-614-CUFDYNTEXT-UPD  PIC X.
      *            J = Aenderung Dynamik Text
 
      ***  Datenfeld: Dynamik-%-Satz FeldNr. 6153
                   05  REOB-AUS-614-NUFDYNPRZ       PIC 99V99.
                   05  REOB-AUS-614-NUFDYNPRZ-UPD   PIC X.
      *            J = Aenderung Dynamik-%-Satz
 
      ***  Datenfeld: Dynamik Ablehnungsjahr Format: JJJJ
      ***             FeldNr. 6154
      ***            05  REOB-AUS-614-CUFDYNABLJ      PIC 9(8).
110208             05  REOB-AUS-614-CUFDYNABLJ      PIC X(10).
                   05  REOB-AUS-614-CUFDYNABLJ-UPD  PIC X.
      *            J = Aenderung Dynamik Ablehnungsjahr
 
      ***  Datenfeld: Regulierung Ja/Nein FeldNr. 6155
                   05  REOB-AUS-614-NUFJNREGUL      PIC XX.
                   05  REOB-AUS-614-NUFJNREGUL-UPD  PIC X.
      *            J = Aenderung Regulierung Ja/Nein
 
      ***  Datenfeld: Rückgewährsumme Ja/Nein FeldNr. 6156
                   05  REOB-AUS-614-NUFSUMRUEK      PIC XX.
                   05  REOB-AUS-614-NUFSUMRUEK-UPD  PIC X.
      *            J = Aenderung Rückgewährsumme Ja/Nein
 
      ***  Datenfeld: Prämienrückgewähr Ja/Nein FeldNr. 6157
                   05  REOB-AUS-614-NUFJNUPR        PIC XX.
                   05  REOB-AUS-614-NUFJNUPR-UPD    PIC X.
      *            J = Aenderung Prämienrückgewähr Ja/Nein
 
      ***  Datenfeld: Freizeitdeckung Ja/Nein FeldNr. 6158
                   05  REOB-AUS-614-NUFJNFREIZ      PIC XX.
                   05  REOB-AUS-614-NUFJNFREIZ-UPD  PIC X.
      *            J = Aenderung Freizeitdeckung Ja/Nein
 
      ***  Datenfeld: Anzahl versicherte Personen je Personengruppe
      ***             FeldNr. 6159
                   05  REOB-AUS-614-NUFVPGRANZ      PIC 9(5).
                   05  REOB-AUS-614-NUFVPGRANZ-UPD  PIC X.
      *            J = Aenderung Anzahl versicherte Personen je
      *                Personengruppe
 
      ***  Datenfeld: Geschlecht versicherte Person FeldNr. 6160
                   05  REOB-AUS-614-NUFVPSEX        PIC 9(9).
                   05  REOB-AUS-614-NUFVPSEX-UPD    PIC X.
      *            J = Aenderung Geschlecht versicherte Person
 
      ***  Datenfeld: Prämienfreistellung KIZ Ja/Nein FeldNr. 6162
                   05  REOB-AUS-614-NUFJNKIZFR      PIC XX.
                   05  REOB-AUS-614-NUFJNKIZFR-UPD  PIC X.
      *            J = Aenderung Prämienfreistellung KIZ Ja/Nein
 
      ***  Datenfeld: Prämie KIZ FeldNr. 6163
                   05  REOB-AUS-614-NUFKIZPRM       PIC 9(7)V99.
                   05  REOB-AUS-614-NUFKIZPRM-UPD   PIC X.
      *            J = Aenderung Prämie KIZ
 
      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6168
                   05  REOB-AUS-614-CGPBHNR         PIC X(17).
                   05  REOB-AUS-614-CGPBHNR-UPD     PIC X.
      *            J = Aenderung Partner-Host-ID der versich. Person
 
      ***  Datenfeld: Differenzdeckung Ja/Nein FeldNr. 5546
                   05  REOB-AUS-614-NDIFFDECKJN     PIC X(2).
                   05  REOB-AUS-614-NDIFFDECKJN-UPD  PIC X.
      *            J = Aenderung Differenzdeckung Ja/Nein
 
      ***  Datenfeld: Zielgruppe FeldNr. 5988
                   05  REOB-AUS-614-CZIELGRP        PIC X(30).
                   05  REOB-AUS-614-CZIELGRP-UPD    PIC X.
      *            J = Aenderung Zielgruppe
 
110208***  Datenfeld: Gesellschaft-Klartext Feld Nr. 3235
                   05  REOB-AUS-614-CGESELL       PIC X(50).
                   05  REOB-AUS-614-CGESELL-UPD    PIC X.
 
      ***  Datenfeld: Versicherungsnummer Feld Nr. 3236
                   05  REOB-AUS-614-CDIFFVSNR     PIC X(40).
                   05    REOB-AUS-614-CDIFFVSNR-UPD    PIC X.
 
      ***  Datenfeld: Umstellungstermin Feld Nr. 3237
      ***            05    REOB-AUS-614-CUMSTELLTERM  PIC 9(08).
110208             05    REOB-AUS-614-CUMSTELLTERM  PIC X(10).
                   05    REOB-AUS-614-CUMSTELLTERM-UPD    PIC X.
 
      ***  Datenfeld: Kündigungsbestätigung Ja/Nein Feld Nr. 3238
                   05  REOB-AUS-614-NKUENDBESTAET PIC X(02).
                   05  REOB-AUS-614-NKUENDBEST-UPD    PIC X.
 
      ***  Datenfeld: Nachname  Feld Nr. 6161
260716             05  REOB-AUS-614-NACHNAME            PIC X(40).
260716             05  REOB-AUS-614-NACHNAME-UPD        PIC X.
 
      ***  Datenfeld: Vorname  Feld Nr. 6169
260716             05  REOB-AUS-614-VORNAME             PIC X(30).
260716             05  REOB-AUS-614-VORNAME-UPD         PIC X.
 
      *** interne laufende Nummer
                   05  REOB-AUS-614-INTLFDNR       PIC 9(4).
 
    *****            05 FILLER                        PIC X(32).
110208****         05 FILLER                        PIC X(22).
260716***          05 FILLER                        PIC X(50).
170717             05 FILLER                        PIC X(23).
 
 

