      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0503.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0503.CPv  $
      *
      *   Rev 2.1   06 Oct 2000 10:40:34   T13AE30
      *ASL00067 - FÖBO2
      *
      *   Rev 2.0   07 Sep 2000 11:32:02   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0503                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0072                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    20.09.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00067!20.09.00!2.1 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0503                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0503.
          05 PI-DRVB0503-KNR-STEUER-INFO    PIC S9(004) COMP.
          05 PI-DRVB0503-ZEITPUNKT          PIC  X(010).
          05 PI-DRVB0503-MONAT              PIC  X(006).
      *
       01 PO-DRVB0503.
          05 PO-DRVB0503-RC                 PIC X(003).
             88 PO-DRVB0503-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0503-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0503-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0503-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0503-DATEN.
             10 PO-DRVB0503-ANZAHL-MONATE   PIC  9(004).
      *
      ****************** ENDE COPY-MEMBER DRVB0503 *********************
