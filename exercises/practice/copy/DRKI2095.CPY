      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2095.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2095.CPv  $
      *
      *   Rev 2.3   Jun 21 2008 12:52:06   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:17:40   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:42   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:15:14   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGSRISIKO        *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *280508  ! Integration neues Feld RISMNR        ! HSL     !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 350 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2095.

                    05  REOB-AUS-702-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-702-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 7083
                   05  REOB-AUS-702-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7081
                   05  REOB-AUS-702-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Risikogruppe FeldNr. 7066
                   05  REOB-AUS-702-PVRIGRP         PIC 9(4).

      ***  Schlüssel: Themengebietsschlüssel FeldNr. 7067
                   05  REOB-AUS-702-PTGSCHL         PIC 9(15).

      ***  Schlüssel: Risiko-Lfdnr FeldNr. 7068
                   05  REOB-AUS-702-PVRILFDNR       PIC 999.

      ***  Datenfeld: Risikokennziffer FeldNr. 7069
                   05  REOB-AUS-702-CVRIKENNZ       PIC X(10).
                   05  REOB-AUS-702-CVRIKENNZ-UPD   PIC X.
      *            J = Aenderung Risikokennziffer

      ***  Datenfeld: Risikoname FeldNr. 7070
                   05  REOB-AUS-702-CVRINAME        PIC X(50).
                   05  REOB-AUS-702-CVRINAME-UPD    PIC X.
      *            J = Aenderung Risikoname

      ***  Datenfeld: Risikobeitrag FeldNr. 7071

                   05  REOB-AUS-702-NVRIBTRG        PIC 9(7)V99.
                   05  REOB-AUS-702-NVRIBTRG-UPD    PIC X.
      *            J = Aenderung Risikobeitrag

      ***  Datenfeld: Risikosumme FeldNr. 7072
                   05  REOB-AUS-702-NVRIVSUM        PIC 9(9)V99.
                   05  REOB-AUS-702-NVRIVSUM-UPD    PIC X.
      *            J = Aenderung Risikosumme

      ***  Datenfeld: Risiko-Entschädigungsgrenze FeldNr. 7073
                   05  REOB-AUS-702-NVRIEGR         PIC 9(08)V99.
                   05  REOB-AUS-702-NVRIEGR-UPD     PIC X.
      *            J = Aenderung Risiko-Entschädigungsgrenze

      ***  Datenfeld: Risiko-Entschädigungsgrenze-Einheit FeldNr. 7074
                   05  REOB-AUS-702-NVRIEGREH       PIC 9(9).
                   05  REOB-AUS-702-NVRIEGREH-UPD   PIC X.
      *            J = Aenderung Risiko-Entschädigungsgrenze

      ***  Datenfeld: Risikomenge FeldNr. 7075
                   05  REOB-AUS-702-NVRIMG          PIC 9(9).
                   05  REOB-AUS-702-NVRIMG-UPD      PIC X.
      *            J = Aenderung Risikomenge

      ***  Datenfeld: Risikomenge-Einheit FeldNr. 7076
                   05  REOB-AUS-702-CVRIMGEH        PIC X(25).
                   05  REOB-AUS-702-CVRIMGEH-UPD    PIC X.
      *            J = Aenderung Risikomenge-Einheit

      ***  Datenfeld: Risikosatz FeldNr. 7077
                   05  REOB-AUS-702-NVRISATZ        PIC 999V9(7).
                   05  REOB-AUS-702-NVRISATZ-UPD    PIC X.
      *            J = Aenderung Risikosatz

      ***  Datenfeld: Risikosatz-Einheit FeldNr. 7078
                   05  REOB-AUS-702-NVRISATZEH      PIC 9(9).
                   05  REOB-AUS-702-NVRISATZEH-UPD  PIC X.
      *            J = Aenderung Risikosatz-Einheit

      ***  Datenfeld: Risikotext FeldNr. 7079
                   05  REOB-AUS-702-CVRITXT         PIC X(30).
                   05  REOB-AUS-702-CVRITXT-UPD     PIC X.
      *            J = Aenderung Risikotest

      ***  Datenfeld: Risiko-Mitversichert (Ja/Nein)FeldNr. 7080
                   05  REOB-AUS-702-NVRIMITVJN      PIC XX.
                   05  REOB-AUS-702-NVRIMITVJN-UPD  PIC X.
      *            J = Aenderung Risiko-Mitversichert (Ja/Nein)

      ***  Datenfeld: Risikosumme-Einheit FeldNr. 7167
                   05  REOB-AUS-702-CVRIVSUMEH      PIC X(20).
                   05  REOB-AUS-702-CVRIVSUMEH-UPD  PIC X.
      *            J = Aenderung Risikosumme-Einheit

      ***  Datenfeld: Risikosumme unbegrenzt (Ja/Nein) FeldNr. 7168
                   05  REOB-AUS-702-NVRIUNBJN       PIC XX.
                   05  REOB-AUS-702-NVRIUNBJN-UPD   PIC X.
      *            J = Aenderung Risikosumme unbegrenzt (Ja/Nein)

280508***  Datenfeld: Risikomerkmal FeldNr. 7169
    >>             05  REOB-AUS-702-RISMNR          PIC 9(9).
    <<             05  REOB-AUS-702-RISMNR-UPD      PIC X.
280508*            J = Aenderung Risikomerkmal

      *** interne laufende Nummer
                   05  REOB-AUS-702-INTLFDNR       PIC 9(4).

280508***          05 FILLER                       PIC X(60).
                   05 FILLER                       PIC X(50).
