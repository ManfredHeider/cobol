      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0543.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0543.CPv  $
      *
      *   Rev 2.1   31 May 2001 10:40:28   t13ae27
      *neue Funktion
      *
      *   Rev 2.0   15 May 2001 13:33:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0543                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.DUERR                                          *
      *    DATUM:    14.05.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00274!14.05.01!2.1 ! NEUERSTELLUNG             !P.DUERR   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0543                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0543.
          05 PI-DRVB0543-KNR-STEUER-INFO    PIC S9(004) COMP.
          05 PI-DRVB0543-MONAT              PIC  X(006).
      *
       01 PO-DRVB0543.

          05 PO-DRVB0543-RC                 PIC  X(003).
             88 PO-DRVB0543-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0543-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0543-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0543-DATEN.
             10 PO-DRVB0543-ZEITPUNKT       PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0524 *****************************

