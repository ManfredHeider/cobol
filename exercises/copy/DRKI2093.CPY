      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2093.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2093.CPv  $
      *
      *   Rev 2.2   13 Nov 2007 11:17:26   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:28   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:14:46   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGSLEISTUNG      *
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
      *     LAENGE: 250 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2093.

                    05  REOB-AUS-631-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-631-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6392
                   05  REOB-AUS-631-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6394
                   05  REOB-AUS-631-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6381
                   05  REOB-AUS-631-PVRGID          PIC 9(4).

      ***  Schlüssel: Leistung-ID FeldNr. 6382
                   05  REOB-AUS-631-PVLSTID         PIC 999.

      ***  Schlüssel: Themengebietsschlüssel FeldNr. 6443
                   05  REOB-AUS-631-PTGSCHL         PIC 9(15).

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6383
                   05  REOB-AUS-631-CVLSTBEZ        PIC X(50).
                   05  REOB-AUS-631-CVLSTBEZ-UPD    PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Wert DM/EUR FeldNr. 6384
                   05  REOB-AUS-631-NVLSTSUM        PIC 9(9)V99.
                   05  REOB-AUS-631-NVLSTSUM-UPD    PIC X.
      *            J = Aenderung Wert DM/EUR

      ***  Datenfeld: sonstiger Wert FeldNr. 6385
                   05  REOB-AUS-631-NVLSTWRT        PIC 9(8)V99.
                   05  REOB-AUS-631-NVLSTWRT-UPD    PIC X.
      *            J = Aenderung sonstiger Wert

      ***  Datenfeld: sonstiger Wert Einheit FeldNr. 6386
                   05  REOB-AUS-631-CVLSTWEH        PIC X(20).
                   05  REOB-AUS-631-CVLSTWEH-UPD    PIC X.
      *            J = Aenderung sonstiger Wert Einheit

      ***  Datenfeld: Text zur Leistung FeldNr. 6387
                   05  REOB-AUS-631-CVLSTTXT        PIC X(30).
                   05  REOB-AUS-631-CVLSTTXT-UPD    PIC X.
     **           J = Aenderung Text zur Leistung
      *** interne laufende Nummer
                   05  REOB-AUS-631-INTLFDNR       PIC 9(4).

                   05 FILLER                        PIC X(54).

