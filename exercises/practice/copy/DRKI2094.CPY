      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2094.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2094.CPv  $
      *
      *   Rev 2.3   Jun 21 2008 12:51:56   t13ex52
      *AKL00665: Allfinanz; Firmenkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:17:34   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:20:34   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:15:00   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle ZUSCHLAG/ABSCHLAG     *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *28.05.08! Integration neues Datenfeld PZANR    ! HSL     !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *                           R E O B                              *
      *                A U S G A B E - D A T E N S A T Z               *
      *                                                                *
      *                       (BetaVersion 1.0)                        *
      *                                                                *
      *     LAENGE: 150 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2094.

                    05  REOB-AUS-701-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-701-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 7063
                   05  REOB-AUS-701-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 7061
                   05  REOB-AUS-701-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Universelle Laufende Nummer FeldNr. 7055
                   05  REOB-AUS-701-PZUNILFDNR     PIC 999.

      ***  Datenfeld: Zu-/Abschlag in % FeldNr. 7057
                   05  REOB-AUS-701-NZSLGPROZ       PIC 99V99.
                   05  REOB-AUS-701-NZSLGPROZ-UPD   PIC X.
      *            J = Aenderung Zu-/Abschlag in %

      ***  Datenfeld: Zu-/Abschlag in WE FeldNr. 7058
                   05  REOB-AUS-701-NZSLGWE         PIC 9(7)V99.
                   05  REOB-AUS-701-NZSLGWE-UPD     PIC X.
      *            J = Aenderung Zu-/Abschlag in WE

      ***  Datenfeld: Kürzel zum Zu-/Abschlag FeldNr. 7059
                   05  REOB-AUS-701-CZSLGKRZL       PIC XXX.
                   05  REOB-AUS-701-CZSLGKRZL-UPD   PIC X.
      *            J = Aenderung Kürzel zum Zu-/Abschlag

      ***  Datenfeld: Erklärungstext zum Zu-/Abschlag FeldNr. 7060
                   05  REOB-AUS-701-CZSLGTXT        PIC X(30).
                   05  REOB-AUS-701-CZSLGTXT-UPD    PIC X.
      *            J = Aenderung Erklärungstext zum Zu-/Abschlag

280508***  Datenfeld: PZANR STAZ SL1 Zuschlags-/AbschlagsNr.
    >>***             FeldNr. 7065
                   05  REOB-AUS-701-PZANR           PIC 9(9).
    <<             05  REOB-AUS-701-PZANR-UPD       PIC X.
280508*            J = Aenderung Zuschlags-/AbschlagsNr.


      *** interne laufende Nummer
                   05  REOB-AUS-701-INTLFDNR       PIC 9(4).

280505***          05 FILLER                       PIC X(49).
280508             05 FILLER                       PIC x(39).

