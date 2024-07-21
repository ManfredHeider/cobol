      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2091.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2091.CPv  $
      *
      *   Rev 2.2   13 Nov 2007 11:17:12   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:14   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:14:16   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGRISIKOMERKMAL  *
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

       01  DRKI2091.

                    05  REOB-AUS-629-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-629-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6409
                   05  REOB-AUS-629-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6411
                   05  REOB-AUS-629-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6361
                   05  REOB-AUS-629-PVRGID          PIC 9(4).

      ***  Schlüssel: Risikomerkmalwert-ID FeldNr. 6362
                   05  REOB-AUS-629-PVRISMID        PIC 999.

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6363
                   05  REOB-AUS-629-CVRISMNAME      PIC X(50).
                   05  REOB-AUS-629-CVRISMNAME-UPD  PIC X.
      *            J = Aenderung Name/Bezeichnung

     ***  Datenfeld: Risikomerkmalwert FeldNr. 6364
                  05  REOB-AUS-629-CVRISMWERT      PIC X(50).
                  05  REOB-AUS-629-CVRISMWERT-UPD  PIC X.
     **           J = Aenderung Name/Bezeichnung

      *** interne laufende Nummer
                   05  REOB-AUS-629-INTLFDNR       PIC 9(4).

                   05 FILLER                        PIC X(43).

