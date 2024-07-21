      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0551.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0551.CPv  $
      *
      *   Rev 2.2   24 Aug 2001 15:18:36   t13ex57
      *AVB00534 - ERWEITERTE F&A-SCHNITT-
      *STELLE FÜR AVBG, WIEN
      *
      *   Rev 2.1   02 Aug 2001 12:05:14   t13ae30
      *AAS00270 - ERWEITERTE F&A-SCHNITTSTELLE FÜR AVBG, WIEN
      *
      *   Rev 2.0   15 May 2001 13:33:30   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0551                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0055                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    04.07.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00270!04.07.01! 01 ! NEUERSTELLUNG             !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0551                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0551.
          05 PI-DRVB0551-MA-ST-NR                PIC  9(007) COMP-3.
      *
       01 PO-DRVB0551.
          05 PO-DRVB0551-RC                      PIC  X(003).
             88 PO-DRVB0551-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0551-RC-NFD                       VALUE 'NFD'.

          05 PO-DRVB0551-DATEN.
             10 PO-DRVB0551-AL-ZUSATZ            PIC  X(004).

      ***************** Ende Copy-Member DRVB0551 ********************
