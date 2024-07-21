      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2092.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2092.CPv  $
      *
      *   Rev 2.2   13 Nov 2007 11:17:20   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:20   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:14:32   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VERTRAGSSELBSTBET     *
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

       01  DRKI2092.

                    05  REOB-AUS-630-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-630-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6416
                   05  REOB-AUS-630-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6418
                   05  REOB-AUS-630-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Risikogruppe-ID FeldNr. 6372
                   05  REOB-AUS-630-PVRGID          PIC 9(4).

      ***  Schlüssel: Selbstbeteiligung-ID FeldNr. 6373
                   05  REOB-AUS-630-PVRSBID         PIC 999.

      ***  Datenfeld: Name/Bezeichnung FeldNr. 6374
                   05  REOB-AUS-630-CVSBBEZ         PIC X(50).
                   05  REOB-AUS-630-CVSBBEZ-UPD     PIC X.
      *            J = Aenderung Name/Bezeichnung

      ***  Datenfeld: Wert in Prozent FeldNr. 6375
                   05  REOB-AUS-630-NVSBPRZ         PIC 99.
                   05  REOB-AUS-630-NVSBPRZ-UPD     PIC X.
      *            J = Aenderung Wert in Prozent

      ***  Datenfeld: Wert in DM/Euro FeldNr. 6376
                   05  REOB-AUS-630-NVSBBTRG        PIC 9(8)V99.
                   05  REOB-AUS-630-NVSBBTRG-UPD    PIC X.
      *            J = Aenderung Wert in DM/Euro

      ***  Datenfeld: Mindestbetrag in DM/Euro FeldNr. 6377
                   05  REOB-AUS-630-NVSBMIN         PIC 9(8)V99.
                   05  REOB-AUS-630-NVSBMIN-UPD     PIC X.
      *            J = Aenderung Mindestbetrag in DM/Euro

      ***  Datenfeld: Höchstbetrag in DM/Euro FeldNr. 6378
                   05  REOB-AUS-630-NVSBMAX         PIC 9(8)V99.
                   05  REOB-AUS-630-NVSBMAX-UPD     PIC X.
      *            J = Aenderung Höchstbetrag in DM/Euro

      ***  Datenfeld: Text zur Selbstbeteiligung FeldNr. 6379
                   05  REOB-AUS-630-CVSBTXT         PIC X(30).
                   05  REOB-AUS-630-CVSBTXT-UPD     PIC X.
      *            J = Aenderung Text zur Selbstbeteiligung

      *** interne laufende Nummer
                   05  REOB-AUS-630-INTLFDNR       PIC 9(4).

                   05 FILLER                        PIC X(27).

