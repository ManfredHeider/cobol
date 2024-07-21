      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.06.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0393.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRAT0393.CPv  $
      *
      *   Rev 2.5   Feb 02 2012 11:22:22   t13ae14
      *ASL00420: Partnerbonus - Ermittlungsprogramm
      *
      *   Rev 2.4   Nov 15 2010 17:06:12   t13ae14
      *ASL00368: BOZ-Ermittlung anpassen für VB mit Vorquartalsumsa
      *tz < =0
      *
      *   Rev 2.3   Sep 07 2010 09:25:38   t13ae35
      *ASL00352: BOZ DVAG - Ermittlungsprogramm
      *
      *   Rev 2.2   Sep 01 2010 10:55:10   t13ae35
      *ASL00352: BOZ DVAG -> Ermittlungsprogramm
      *
      *   Rev 2.1   Jun 11 2010 08:50:26   t13ex43
      *ASL00353: PGM - Nichterfüller Columbus-Bonus
      *
      *   Rev 2.0   Jun 08 2010 11:05:02   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: KNR-KONSTANTEN FÜR NICHTERFÜLLER-GRUND        *
      *                 KTY-NR = 32                                   *
      *                                                               *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.06.2010                                       *
      *                                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * ASL00353!08.06.10!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00352!23.07.10!2.2 ! BOZ-Nichterf-Gründe       ! J.Hering  *
      * ASL00420!02.02.12!2.4 ! PABO-Nichterf-Gründe      ! K.Ober    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *
       01  K-KNR-BETR-AUSTRITT              PIC S9(04) COMP VALUE 1.
       01  K-KNR-BETR-BES                   PIC S9(04) COMP VALUE 2.
       01  K-KNR-PART-AUSTRITT              PIC S9(04) COMP VALUE 3.
       01  K-KNR-PART-BES                   PIC S9(04) COMP VALUE 4.
       01  K-KNR-ALLF-BES                   PIC S9(04) COMP VALUE 5.
       01  K-KNR-PART-RUECKSTF              PIC S9(04) COMP VALUE 6.
       01  K-KNR-NICHT-BEDI-BEFOE           PIC S9(04) COMP VALUE 7.
       01  K-KNR-450-EH-GRENZE              PIC S9(04) COMP VALUE 8.
       01  K-KNR-MINDEST-EH                 PIC S9(04) COMP VALUE 9.
       01  K-KNR-300-EEG-REGEL              PIC S9(04) COMP VALUE 10.
020212 01  K-KNR-FEHLENDE-PARTNER           PIC S9(04) COMP VALUE 11.
020212 01  K-KNR-FEHLENDE-ERFPRO            PIC S9(04) COMP VALUE 12.
       01  K-KNR-BOZ-AUSTRITT               PIC S9(04) COMP VALUE 100.
       01  K-KNR-BOZ-BES                    PIC S9(04) COMP VALUE 101.
       01  K-KNR-12-MON-UMS                 PIC S9(04) COMP VALUE 102.
       01  K-KNR-03-MON-UMS                 PIC S9(04) COMP VALUE 103.
       01  K-KNR-EQF-100                    PIC S9(04) COMP VALUE 104.
       01  K-KNR-BOZ-0                      PIC S9(04) COMP VALUE 105.

      *
      ******** ENDE COPY-MEMBER KNR-NICHTERFÜLLER-GRUND (DRAT0393) ****
      *
