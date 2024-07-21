      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1056.CPv  $
      *
      *   Rev 2.1   Apr 13 2012 17:02:34   t13ae36
      *AAS00996: Allfinanz Wettbewerbe 2012:Bestandsübertragungen i
      *n akt. Bewertung
      *
      *   Rev 2.0   Mar 30 2012 14:17:32   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1056                                      *
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
      *    FUNKTION DRVB1056 - ERMITTELN SATZ AUS TABELLE BESTUEB     *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1056.
          05 PI-DRVB1056-DATEN.
             10 PI-DRVB1056-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1056-BESTUEB-MONAT      PIC  X(10).
             10 PI-DRVB1056-BESTUEB-BEZ-MON    PIC  X(10).

       01 PO-DRVB1056.
         03 PO-DRVB1056-RC                     PIC  X(03).
             88 PO-DRVB1056-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1056-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1056-DATEN.
          05 PO-DRVB1056-VIVB0315.
             10 PO-DRVB1056-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1056-BESTUEB-MONAT      PIC  X(10).
             10 PO-DRVB1056-BST-BESTHIN-SUM    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1056-BST-BESTWEG-SUM    PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1056   --------------------------*
      *---------------------------------------------------------------*

