      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1843.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1843.CPv  $
      *
      *   Rev 2.1   Dec 22 2015 10:35:22   t13ex44
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.0   Dec 15 2015 15:34:52   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1843                                      *
      *    --------------------------------------------------------   *
      *    STEUERSTATUS DER VBS                                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    15.12.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!15.12.15!2.0 ! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
             30 STEUERSTATUSTABELLE.
                35 STEUSTATAB-KZ              PIC  X.
                   88 S-STEUSTATAB-AUS-VORB        VALUE 'V'.
                   88 S-STEUSTATAB-UEBERARB        VALUE 'U'.
                   88 S-STEUSTATAB-LEER            VALUE 'L'.
                35 IX-STEUSTA-MAX             PIC S9(04)       BINARY.
                35 FILLER                     OCCURS 20
                                              INDEXED BY IX-STEUSTA.
                   40 STEUSTA-VBNR            PIC S9(09)       BINARY.
                   40 STEUSTA-KNR-STATUS      PIC S9(04)       BINARY.
                   40 STEUSTA-KZ              PIC  X.
                      88 S-STEUSTA-AUS-VORB        VALUE 'V'.
                      88 S-STEUSTA-UEBERARB        VALUE 'U'.

