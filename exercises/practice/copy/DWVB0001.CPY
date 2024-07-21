      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DWVB0001.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DWVB0001.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:33:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DWVB0001       EQF-Berechung                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M. KUHN          (DVAG)                          *
      *    DATUM:    11.02.1993                                       *
      *****************************************************************
      *    VERSION   1    28.10.94 10:11:08
      ****************************************************************  00091000
      * AENDERUNGEN:                                                 *  00092000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00093000
      * -------------------------------------------------------------*  00094000
      *         !16.09.93! 01 ! NEU                       ! Kuhn     *  00095002
      * AFVB0142!19.10.94! 02 ! Ausgabe auf 1 Nachkommast.! Priggen  *  00096001
      *         !        !    !                           !          *  00097001
      *         !        !    !                           !          *  00098001
      *         !        !    !                           !          *  00099000
      *         !        !    !                           !          *  00099100
      *         !        !    !                           !          *  00099200
      *         !        !    !                           !          *  00099300
      *                                                              *  00099400
      * ENDE-LOGBUCH                                                 *  00099500
      ****************************************************************  00099600
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
      *    1. Z-DWVB0001-EQF-NR enthält den auf 2 Stellen ermittelten*
      *       nicht gerundeten EQF im Format S9(03)V99               *
      *    2. Z-DWVB0001-EQF enthält den auf eine Stelle gerundeten  *
      *       EQF im Format S9(03)V99                                *
      *    beide Werte werden PACKED-DECIMAL dargestellt             *
      *                                                              *

       01  Z-DWVB0001.
           05  Z-DWVB0001-Zugang       PIC S9(014)V9  PACKED-DECIMAL.
           05  Z-DWVB0001-Abgang       PIC S9(014)V9  PACKED-DECIMAL.
           05  Z-DWVB0001-EQF-NR       PIC S9(003)V99 PACKED-DECIMAL.
           05  Z-DWVB0001-EQF          PIC S9(003)V9  PACKED-DECIMAL.

