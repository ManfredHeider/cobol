      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1012.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1012.CPv  $
      *
      *   Rev 2.1   Mar 28 2011 10:40:34   t13ae36
      *AAS00952: Allfinanz Wettbewerbe 2011:DHL ; Erf.Eig; Erf.Gr
      *
      *   Rev 2.0   Mar 10 2011 08:18:02   t13ae36
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB1012                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0207                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    10.03.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00952!10.03.11!2.1 ! NEU                       ! FRANKE    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1012 - ERMITTELN Summe Bestandsübertragungen  *
      *                        und Bestandsabgaben innerhalb          *
      *                        PI-DRVB1012-LZA-MONAT-VON              *
      *                        PI-DRVB1012-LZA-MONAT-BIS              *
      *****************************************************************

       01 PI-DRVB1012.
          05 PI-DRVB1012-DATEN.
             10 PI-DRVB1012-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1012-LZA-MONAT-VON      PIC  X(10).
             10 PI-DRVB1012-LZA-MONAT-BIS      PIC  X(10).

       01 PO-DRVB1012.
         03 PO-DRVB1012-RC                     PIC  X(03).
             88 PO-DRVB1012-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1012-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1012-DATEN.
          05 PO-DRVB1012-VIVBA207.
             10 PO-DRVB1012-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1012-LZA-MONAT-VON      PIC  X(10).
             10 PO-DRVB1012-LZA-MONAT-BIS      PIC  X(10).
             10 PO-DRVB1012-LZA-BESTHIN-SUM    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1012-LZA-BESTWEG-SUM    PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1012   --------------------------*
      *---------------------------------------------------------------*
