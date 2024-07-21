      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1103.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1103.CPv  $
      *
      *   Rev 2.2   Sep 02 2015 10:45:40   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher 
      *Status für Daisy
      *
      *   Rev 2.1   Jun 02 2015 15:01:28   t13ex71
      *AVB01347: Unterprogramm UPVB0189 zum Ermitteln steuerlicher
      *Status für Daisy
      *
      *   Rev 2.0   May 26 2015 13:22:28   t13ex71
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1103                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0040                        *
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
      *    FÜR DIE FUNKTION DRVB1103                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1103.
          05 PI-DRVB1103-KL-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB1103.
 
          05 PO-DRVB1103-RC                 PIC  X(003).
             88 PO-DRVB1103-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1103-RC-NOT-FOUND                 VALUE 'NFD'.
 
          05 PO-DRVB1103-DATEN.
             10 PO-DRVB1103-MA-ST-NR        PIC S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB1103 *****************************
 

