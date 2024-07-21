      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1128.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1128.CPv  $
      *
      *   Rev 2.1   Feb 26 2016 13:37:10   t13ex30
      *FVB00793: SV-Nr in eigenem DB-Feld
      *
      *   Rev 2.0   Feb 26 2016 09:20:04   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    Copy-Member: DRVB1128                                      *
      *    --------------------------------------------------------   *
      *    Schnittstelle für das Upro ZWVB0004                        *
      *    - Ermittlung der Sozialversicherungsnnummer (Österreich)   *
      *    --------------------------------------------------------   *
      *    Erstellt: Heidi Wölflik                                    *
      *    Datum   : 26.02.2016                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * FVB00793!26.02.16!2.0 ! NEU                       !H. Wölflik *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    Dieses Copy enthält die Ein- und Ausgabeparameter          *
      *    für die Funktion DRVB1128                                  *
      *    ausser: den Funktionsnamen,                                *
      *            dem Error-Bereich und                              *
      *            den Standard-Status-Bereich                        *
      *            (jeweils separate Copys)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1128.
          05 PI-DRVB1128-MA-ST-NR             PIC S9(009) BINARY.
      *
       01 PO-DRVB1128.
          05 PO-DRVB1128-RC                   PIC  X(003).
             88 PO-DRVB1128-RC-OK                  VALUE 'OK '.
             88 PO-DRVB1128-RC-NOT-FOUND           VALUE 'NFD'.
          05 PO-DRVB1128-DATEN.
             10 PO-DRVB1128-MA-SOZ-VSNR-WIEN  PIC  X(015).
      *
      ***** Ende Copy-Member DRVB1128 *****************************
      *

