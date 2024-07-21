      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0843.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0843.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:08:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    Das nachfolgende Copy-Member ist von der DVAG übernommen.  *
      *    Das Datenfeld Z-DWVB0001-EQF-NR ist auf drei Nachkomma-    *
      *    stellen erweitert (von ursprünglich zwei).                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Wolff                                         *
      *    DATUM:    03.11.1994                                       *
      *****************************************************************
      *
      *
      *****************************************************************
      *    COPY-MEMBER: DWVB0001       EQF-Berechung                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN          (DVAG)                          *
      *    DATUM:    11.02.1993                                       *
      *****************************************************************
      *    VERSION   1    28.10.94 10:11:08
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      *         !16.09.93! 01 ! NEU                       ! Kuhn     *  
      * AFVB0142!19.10.94! 02 ! Ausgabe auf 1 Nachkommast.! Priggen  *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      *---------------------------------------------------------------
      * Übergabe vor Aufruf:                                         *
      *                                                              *
      *    Einheiten müssen als Zugang in Z-DWVB0001-Zugang und als  *
      *    Abgang in Z-DWVB0001-Abgang zur Verfügung gestellt werden.*
      *    Beide Felder sind als  PIC S9(014)V9  PACKED-DECIMAL      *
      *    definiert.                                                *
      *                                                              *
      *    Der ermittelte EQF wird in zwei unterschiedlichen         *
      *    Formaten bereitgestellt:                                  *
      *    1. Z-DWVB0001-EQF-NR enthält den auf 3 Stellen ermittelten*
      *       nicht gerundeten EQF im Format S9(03)V999              *
      *    2. Z-DWVB0001-EQF enthält den auf eine Stelle gerundeten  *
      *       EQF im Format S9(03)V9                                 *
      *    beide Werte werden PACKED-DECIMAL dargestellt             *
      *                                                              *

       01  Z-DWVB0001.
           05  Z-DWVB0001-Zugang       PIC S9(014)V9   PACKED-DECIMAL.
           05  Z-DWVB0001-Abgang       PIC S9(014)V9   PACKED-DECIMAL.
           05  Z-DWVB0001-EQF-NR       PIC S9(003)V999 PACKED-DECIMAL.
           05  Z-DWVB0001-EQF          PIC S9(003)V9   PACKED-DECIMAL.

