      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1023.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1023.CPv  $
      *
      *   Rev 2.2   Sep 14 2011 15:45:40   t13ae36
      *AAS00969: Ermittlung der Gewinner in 01/2012 bzw. 12/2011
      *
      *   Rev 2.1   Jul 21 2011 16:12:52   t13ae36
      *AAS00969: Ermittlung der Gewinner in 01/2012 bzw. 12/2011
      *
      *   Rev 2.0   Jun 29 2011 15:59:56   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1023 - ERMITTELN Summe Bestandsübertragungen  *
      *                        und Bestandsabgaben innerhalb          *
      *                        PI-DRVB1012-LZA-MONAT-VON              *
      *                        PI-DRVB1012-LZA-MONAT-BIS              *
      *                        für die Gruppe
      *****************************************************************

       01 PI-DRVB1023.
          05 PI-DRVB1023-DATEN.
             10 PI-DRVB1023-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1023-LZA-MONAT-VON      PIC  X(10).
             10 PI-DRVB1023-LZA-MONAT-BIS      PIC  X(10).

       01 PO-DRVB1023.
         03 PO-DRVB1023-RC                     PIC  X(03).
             88 PO-DRVB1023-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1023-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1023-DATEN.
          05 PO-DRVB1023-VIVBA207.
             10 PO-DRVB1023-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1023-LZA-MONAT-VON      PIC  X(10).
             10 PO-DRVB1023-LZA-MONAT-BIS      PIC  X(10).
             10 PO-DRVB1023-LZA-BESTH-SUM-GRP  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1023-LZA-BESTW-SUM-GRP  PIC S9(09)V9(02) COMP-3.

