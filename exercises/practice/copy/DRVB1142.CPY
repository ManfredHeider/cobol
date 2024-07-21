      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1142.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1142.CPv  $
      *
      *   Rev 2.1   Jun 30 2016 11:51:26   t13ex71
      *AVB01400: Projekt Lucy2 - Neue Funktion in UPVB0189 aufnehme
      *n (BPR)
      *
      *   Rev 2.0   May 25 2016 18:07:30   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1142                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.Pretsch                                        *
      *    DATUM:    24.05.20016                                      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01400!24.05.16! 01 ! NEU                       !B.PRETSCH *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB1142                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1142.
          05 PI-DRVB1142-KNR-STEUER-INFO    PIC S9(004) COMP.
 
      *
       01 PO-DRVB1142.
 
          05 PO-DRVB1142-RC                 PIC  X(003).
             88 PO-DRVB1142-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1142-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1142-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1142-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB1142-DATEN.
             10 PO-DRVB1142-ZEITPUNKT        PIC X(026).
 
      *
      ***** ENDE COPY-MEMBER DRVB1142 *****************************

