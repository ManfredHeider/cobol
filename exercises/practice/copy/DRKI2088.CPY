      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2088.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2088.CPv  $
      *
      *   Rev 2.3   20 Feb 2008 09:15:42   T13ae31
      *AKL00665: Allfinanz; Pivatkunden AMV
      *
      *   Rev 2.2   13 Nov 2007 11:16:52   T13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.1   30 Oct 2007 12:19:52   t13ae31
      *AKL00664: Allfinanz: Bestandsdaten AMV Aufb. Vertrag 
      *
      *   Rev 2.0   Sep 17 2007 11:13:34   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle VGVSELBSTBET          *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.09.07! NEU                                  ! HHU     !      *
      *        !                                      !         !      *
      *11.02.08! INTLFDNR geändert                    ! HHU     !      *
      *        !                                      !         !      *
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
      *     LAENGE: 200 BYTES                 STAND: 10.10.2007        *
      **************************************************************** *

       01  DRKI2088.

                    05  REOB-AUS-615-VERARB-KNZ      PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                    05 REOB-AUS-615-VERTRAGS-ID        PIC X(13).

      ***  Schlüssel: Vertragsnummer FeldNr. 6179
                   05  REOB-AUS-615-CVNR            PIC 9(15).

      ***  Schlüssel: Einzelvertrag-Host-ID FeldNr. 6177
                   05  REOB-AUS-615-CVEZHOSTNR      PIC X(15).

      ***  Schlüssel: Laufende Nummer der Selbstbeteiligung
      ***             FeldNr. 6170
                   05  REOB-AUS-615-PVGSBLFDNR      PIC 999.

      ***  Datenfeld: SelbstBeteiligung-Art FeldNr. 6171
                   05  REOB-AUS-615-NVGLBSBART      PIC 9(9).
                   05  REOB-AUS-615-NVGLBSBART-UPD  PIC X.
      *            J = Aenderung SelbstBeteiligung-Art

      ***  Datenfeld: SelbstBeteiligung-Bezeichnung FeldNr. 6172
                   05  REOB-AUS-615-CVGSBBEZ        PIC X(30).
                   05  REOB-AUS-615-CVGSBBEZ-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Bezeichnung

      ***  Datenfeld: SelbstBeteiligung-Prz FeldNr. 6173
                   05  REOB-AUS-615-CVGSBPRZ        PIC X(14).
                   05  REOB-AUS-615-CVGSBPRZ-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Prz

      ***  Datenfeld: SelbstBeteiligung-Minimum FeldNr. 6174
                   05  REOB-AUS-615-NVGSBMIN        PIC 9(5)V99.
                   05  REOB-AUS-615-NVGSBMIN-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Minimum

      ***  Datenfeld: SelbstBeteiligung-Maximum FeldNr. 6175
                   05  REOB-AUS-615-NVGSBMAX        PIC 9(5)V99.
                   05  REOB-AUS-615-NVGSBMAX-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Maximum

      ***  Datenfeld: SelbstBeteiligung-Fest FeldNr. 6176
                   05  REOB-AUS-615-NVGSBFES        PIC 9(5)V99.
                   05  REOB-AUS-615-NVGSBFES-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Fest

      ***  Datenfeld: SelbstBeteiligung-Text FeldNr. 6243
                   05  REOB-AUS-615-CVGSBTXT        PIC X(30).
                   05  REOB-AUS-615-CVGSBTXT-UPD    PIC X.
      *            J = Aenderung SelbstBeteiligung-Text

      *** interne laufende Nummer
      ***            05  REOB-AUS-524-INTLFDNR       PIC 9(4).
110208             05  REOB-AUS-615-INTLFDNR       PIC 9(4).

                   05 FILLER                        PIC X(38).

