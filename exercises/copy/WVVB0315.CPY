      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0315.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0315.CPv  $
      *
      *   Rev 2.1   Apr 13 2012 17:02:48   t13ae36
      *AAS00996: Allfinanz Wettbewerbe 2012:Bestandsübertragungen i
      *n akt. Bewertung
      *
      *   Rev 2.0   Mar 30 2012 15:39:46   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: WVVB0315                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0315 (BESTUEB )                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    30.03.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * -       !30.03.12!2.1 ! NEU                       ! -         *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0315                           *
      *****************************************************************

        05 WB-NI-VIVB0315-TAB.
           10 WB-NI-VIVB0315-ELE   PIC S9(04)  BINARY OCCURS   5.
        05 WB-NI-VIVB0315          REDEFINES   WB-NI-VIVB0315-TAB.
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.
           10 WB-NI-BESTUEB-MONAT         PIC S9(04)       BINARY.
           10 WB-NI-BESTUEB-BEZ-MON       PIC S9(04)       BINARY.
           10 WB-NI-BESTUEB-HIN           PIC S9(04)       BINARY.
           10 WB-NI-BESTUEB-WEG           PIC S9(04)       BINARY.

        05 WB-VIVB0315.
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.
           10 WB-BESTUEB-MONAT            PIC  X(10).
           10 WB-BESTUEB-BEZ-MON          PIC  X(10).
           10 WB-BESTUEB-HIN              PIC S9(09)V9(02) COMP-3.
           10 WB-BESTUEB-WEG              PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   5                    *
      *****************************************************************

