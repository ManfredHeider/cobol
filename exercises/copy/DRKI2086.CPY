      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2086.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2086.CPv  $
      *
      *   Rev 2.3   Aug 05 2011 11:13:56   t13ae31
      *AKI01037: SUH - FK-And. 613 Erm+Aufb+Akt 
      *
      *   Rev 2.2   13 Nov 2007 11:16:36   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:19:38   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:13:08   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle SGWBESONDVEREINB      *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
030811*AKI01037! Änd. SA 613                          !P. Hübner!      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 200 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2086.

                    05  REOB-AUS-613-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-613-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6141
                   05  REOB-AUS-613-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6139
                   05  REOB-AUS-613-CVEZHOSTNR      PIC X(15).

      *** Schlüssel: Komponentengruppe FeldNr. 9317
030811            05  REOB-AUS-613-RGID          PIC 9(4).

      ***  Schlüssel: laufende Nummer FeldNr. 6136
                   05  REOB-AUS-613-PSGBLFDNR       PIC 999.

      ***  Datenfeld: Bedingung/Klausel-Nummer FeldNr. 6137
                   05  REOB-AUS-613-CSGBNRBVB       PIC X(20).
                   05  REOB-AUS-613-CSGBNRBVB-UPD   PIC X.
      *            J = Aenderung Bedingung/Klausel-Nummer

      ***  Datenfeld: Text besondere Vertragsbestandteile FeldNr. 6138
                   05  REOB-AUS-613-CSGBTXTBVB      PIC X(80).
                   05  REOB-AUS-613-CSGBTXTBVB-UPD  PIC X.
      *            J = Aenderung Text besondere Vertragsbestandteile

      *** interne laufende Nummer
                   05  REOB-AUS-613-INTLFDNR       PIC 9(4).

030811             05 FILLER                        PIC X(43).


