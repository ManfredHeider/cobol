      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2078.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2078.CPv  $
      *
      *   Rev 2.2   13 Nov 2007 11:15:34   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:18:42   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:11:14   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGSRISIKOADRESSE *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 200 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2078.

                    05  REOB-AUS-403-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-403-VERTRAGS-ID        PIC X(13).

     ***  Schlüssel: Vertragsnummer FeldNr. 9999
                  05  REOB-AUS-403-CVNR            PIC 9(15).

     ***  Schlüssel: Risikoadresse-HostID FeldNr. 6366
                  05  REOB-AUS-403-NRISADRHID      PIC 9(9).

     ***  Datenfeld: Strasse FeldNr. 6367
                  05  REOB-AUS-403-CRISADRSTR      PIC X(40).
                  05  REOB-AUS-403-CRISADRSTR-UPD  PIC X.
     ***           J = Aenderung Strasse

     ***  Datenfeld: PLZ FeldNr. 6368
                  05  REOB-AUS-403-CRISADRPLZ      PIC X(6).
                  05  REOB-AUS-403-CRISADRPLZ-UPD  PIC X.
     ***           J = Aenderung PLZ

     ***  Datenfeld: Ort FeldNr. 6369
                  05  REOB-AUS-403-CRISADRORT      PIC X(40).
                  05  REOB-AUS-403-CRISADRORT-UPD  PIC X.
     ***           J = Aenderung Ort

     ***  Datenfeld: Länderkennzeichen FeldNr. 6370
                  05  REOB-AUS-403-CRISADRLKZ      PIC 9(9).
                  05  REOB-AUS-403-CRISADRLKZ-UPD  PIC X.
     ***           J = Aenderung Länderkennzeichen

                   05 FILLER                        PIC X(63).
