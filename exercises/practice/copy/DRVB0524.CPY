      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0524.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0524.CPv  $
      *
      *   Rev 2.1   19 Mar 2001 17:20:48   T13AE30
      *AAS00264- LEISTUNGSPLAN / WETTBEWERB 2001
      *
      *   Rev 2.0   22 Feb 2001 16:59:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0524                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    28.02.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!28.02.01!2.1 ! NEUERSTELLUNG             !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0524                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0524.
          05 PI-DRVB0524-KNR-STEUER-INFO    PIC S9(004) COMP.
          05 PI-DRVB0524-MONAT              PIC  X(006).
      *
       01 PO-DRVB0524.

          05 PO-DRVB0524-RC                 PIC  X(003).
             88 PO-DRVB0524-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0524-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0524-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0524-DATEN.
             10 PO-DRVB0524-ZEITPUNKT       PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0524 *****************************
