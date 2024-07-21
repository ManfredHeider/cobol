      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1102.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1102.CPv  $
      *
      *   Rev 2.1   Jun 02 2015 15:01:24   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher 
      *Status für Daisy
      *
      *   Rev 2.0   May 26 2015 13:22:22   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1102                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0005                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: B.PRETSCH                                        *
      *    DATUM:    26.05.2015                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!26.05.15! 01 ! NEU                       !B.PRETSCH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1102                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1102.
          05 PI-DRVB1102-RFT-ST-NR          PIC S9(009) COMP.
      *
       01 PO-DRVB1102.
 
          05 PO-DRVB1102-RC                 PIC  X(003).
             88 PO-DRVB1102-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1102-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB1102-DATEN.
             10 PO-DRVB1102-MA-ST-NR        PIC S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB1102 *****************************

