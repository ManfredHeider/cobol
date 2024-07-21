      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2089.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2089.CPv  $
      *
      *   Rev 2.4   Dec 15 2020 12:16:56   t13ae31
      *FKI01676: G10 SUH Fehler Ermittlung SA616
      *
      *   Rev 2.3   15 Feb 2008 15:33:10   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:58   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.1   30 Oct 2007 12:19:58   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag
      *
      *   Rev 2.0   Sep 17 2007 11:13:48   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle UNFALLMITVP           *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! Datumsfelder auf X(10)               ! HHU     !      *
      *--------!--------------------------------------!         !      *
      *15.12.20! Feldnamen angleichen                 !         !      *
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
      *     LAENGE: 200 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2089.

                    05  REOB-AUS-616-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-616-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6460
                   05  REOB-AUS-616-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6462
                   05  REOB-AUS-616-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: VP laufende Nummer FeldNr. 6445
                   05  REOB-AUS-616-PUFVPLFDNR      PIC 999.

      ***  Schlüssel: MVP laufende Nummer FeldNr. 6446
                   05  REOB-AUS-616-PUFMVPLFDNR         PIC 999.

      ***  Datenfeld: Name FeldNr. 6447
                   05  REOB-AUS-616-CNAME           PIC X(30).
                   05  REOB-AUS-616-CNAME-UPD       PIC X.
      *            J = Aenderung Name

      ***  Datenfeld: Vorname FeldNr. 6448
                   05  REOB-AUS-616-CVNAME          PIC X(30).
                   05  REOB-AUS-616-CVNAME-UPD      PIC X.
      *            J = Aenderung Vorname

      ***  Datenfeld: Geschlecht FeldNr. 6449
                   05  REOB-AUS-616-NSEX            PIC 9(9).
                   05  REOB-AUS-616-NSEX-UPD        PIC X.
      *            J = Aenderung Geschlecht

      ***  Datenfeld: Geburtsdatum Format: JJJJMMTT FeldNr. 6450
      ***            05  REOB-AUS-616-CGEBDAT         PIC 9(8).
110208             05  REOB-AUS-616-CGEBDAT         PIC X(10).
                   05  REOB-AUS-616-CGEBDAT-UPD     PIC X.
      *            J = Aenderung Geburtsdatum

      ***  Datenfeld: Beginn Format: JJJJMMTT FeldNr. 6451
      ***            05  REOB-AUS-616-CBEGINN         PIC 9(8).
110208             05  REOB-AUS-616-CBEGINN         PIC X(10).
                   05  REOB-AUS-616-CBEGINN-UPD     PIC X.
      *            J = Aenderung Beginn

      ***  Datenfeld: Ende Format: JJJJMMTT FeldNr. 6452
     ****            05  REOB-AUS-616-CENDE           PIC 9(8).
110208             05  REOB-AUS-616-CENDE           PIC X(10).
                   05  REOB-AUS-616-CENDE-UPD       PIC X.
      *            J = Aenderung Ende

      ***  Datenfeld: Partner-Host-ID der versicherten Person
      ***             FeldNr. 6457
                   05  REOB-AUS-616-CGPBHNR         PIC X(17).
                   05  REOB-AUS-616-CGPBHNR-UPD     PIC X.
      *            J = Aenderung Ende

      *** interne laufende Nummer
                   05  REOB-AUS-616-INTLFDNR       PIC 9(4).

     ***            05 FILLER                        PIC X(29).
110208             05 FILLER                        PIC X(23).


