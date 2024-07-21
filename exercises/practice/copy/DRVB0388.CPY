      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0388.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRVB0388.CPv  $
      *
      *   Rev 2.3   Jun 25 1999 08:20:48   T13AE30
      *AAS00051 - Stammdaten -DTA ---> Aachen
      *
      *   Rev 2.2   Jun 25 1999 08:11:38   T13AE30
      * 
      *
      *   Rev 2.1   Jun 02 1999 14:25:42   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0388                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS ZWVB0021                             *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    28.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!28.04.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0388                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0388.
          05 PI-DRVB0388-ST-NR                 PIC S9(009) COMP.
      *
       01 PO-DRVB0388.

          05 PO-DRVB0388-RC                    PIC  X(003).
             88 PO-DRVB0388-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0388-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0388-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0388-DATEN.
             10 PO-DRVB0388-BV-KTO-INHABER     PIC  X(040).
             10 PO-DRVB0388-BV-KTO-NR          PIC  X(020).
             10 PO-DRVB0388-BV-BANKNAME        PIC  X(040).
             10 PO-DRVB0388-BV-BLZ             PIC  X(011).
             10 PO-DRVB0388-BV-SPERRE          PIC S9(004) COMP.

      *
      ***** ENDE COPY-MEMBER DRVB0388 *****************************

