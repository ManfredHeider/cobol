      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0527.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0527.CPv  $
      *
      *   Rev 2.1   20 Mar 2001 10:06:02   T13AE13
      *AAS000264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.0   22 Feb 2001 16:59:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0527                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    07.03.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!07.03.01!2.1 ! NEUERSTELLUNG             ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0527                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0527.
          05 PI-DRVB0527-ZEITRAUM-VON       PIC  X(010).
          05 PI-DRVB0527-ZEITRAUM-BIS       PIC  X(010).
          05 PI-DRVB0527-ABZUG-MONATE       PIC  9(004).
          05 PI-DRVB0527-MS-NR-AB           PIC S9(004) COMP.
          05 PI-DRVB0527-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0527.
          05 PO-DRVB0527-RC                 PIC X(003).
             88 PO-DRVB0527-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0527-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****************** ENDE COPY-MEMBER DRVB0527*********************
