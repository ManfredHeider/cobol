      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1055.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1055.CPv  $
      *
      *   Rev 2.1   Apr 13 2012 17:02:26   t13ae36
      *AAS00996: Allfinanz Wettbewerbe 2012:Bestandsübertragungen i
      *n akt. Bewertung
      *
      *   Rev 2.0   Mar 30 2012 14:17:24   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1055                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0315                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: -                                                *
      *    DATUM:    30.03.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00996!30.03.12!2.1 ! NEU                       ! R.Franke  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1055 - ERMITTELN SATZ AUS TABELLE BESTUEB     *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1055.
          05 PI-DRVB1055-DATEN.
             10 PI-DRVB1055-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1055-BESTUEB-MONAT      PIC  X(10).
             10 PI-DRVB1055-BESTUEB-BEZ-MON    PIC  X(10).

       01 PO-DRVB1055.
         03 PO-DRVB1055-RC                     PIC  X(03).
             88 PO-DRVB1055-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1055-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1055-DATEN.
          05 PO-DRVB1055-VIVB0315.
             10 PO-DRVB1055-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1055-BESTUEB-MONAT      PIC  X(10).
             10 PO-DRVB1055-BST-BESTH-SUM-GRP  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1055-BST-BESTW-SUM-GRP  PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1055   --------------------------*
      *---------------------------------------------------------------*
