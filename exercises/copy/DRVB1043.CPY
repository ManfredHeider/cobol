      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.01.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1043.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1043.CPv  $
      *
      *   Rev 2.1   Jan 27 2012 08:22:02   t13ae36
      *AAS01000: Jahreswettbewerb 2011 - Erfolge im Eigen-und Grupp
      *engeschäft
      *
      *   Rev 2.0   Jan 25 2012 13:17:48   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1043                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0207                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FRANKE                                           *
      *    DATUM:    26.01.2012                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS01000!26.01.12!2.1 ! NEU                       ! FRANKE    *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1043 - ERMITTELN Summe Bestandsübertragungen  *
      *                        und Bestandsabgaben innerhalb          *
      *                        PI-DRVB1043-LZA-MONAT                  *
      *****************************************************************

       01 PI-DRVB1043.
          05 PI-DRVB1043-DATEN.
             10 PI-DRVB1043-MA-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1043-LZA-MONAT          PIC  X(10).

       01 PO-DRVB1043.
         03 PO-DRVB1043-RC                     PIC  X(03).
             88 PO-DRVB1043-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1043-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1043-DATEN.
          05 PO-DRVB1043-VIVBA207.
             10 PO-DRVB1043-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1043-LZA-MONAT          PIC  X(10).
             10 PO-DRVB1043-LZA-BESTHIN-12M    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1043-LZA-BESTWEG-12M    PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1043   --------------------------*
      *---------------------------------------------------------------*

