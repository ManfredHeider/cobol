      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0382.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0382.CPv  $
      *
      *   Rev 2.2   Jan 11 2000 16:24:08   T13AE30
      *AAS00051 - Stammdaten-DTA -> Aachen
      *
      *   Rev 2.1   Apr 21 1999 10:04:52   T13AE30
      *AAS00051 - Neuerstellung PMVB0212
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0382                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0014                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    20.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!20.04.99! 01 ! NEUERSTELLUNG             !WILLIG    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0382                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0382.
           05  PI-DRVB0382-MAN-KEY.
               10 PI-DRVB0382-MAN-BEZUGS-NR   PIC S9(007) BINARY.
      *
       01  PO-DRVB0382.
           05  PO-DRVB0382-RC                 PIC  X(003).
               88 PO-DRVB0382-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0382-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0382-MAN-VORNAME        PIC  X(020).
           05  PO-DRVB0382-MAN-NAME           PIC  X(040).
           05  PO-DRVB0382-ANREDE             PIC  s9(4) usage comp.
           05  PO-DRVB0382-GEBURTSDAT         PIC  X(010).
           05  PO-DRVB0382-MAN-TITEL-AKAD     PIC  X(010).
           05  PO-DRVB0382-MAN-TITEL-ADEL     PIC  X(010).
           05  PO-DRVB0382-NI-MAN-GEBURTSDAT  PIC S9(004) COMP.

      *
      ***** Ende Copy-Member DRVB0382 *****************************

