      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1114.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB1114.CPv  $
      *
      *   Rev 2.4   Sep 04 2015 14:00:54   t13ae30
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - TWI
      *
      *   Rev 2.3   Sep 04 2015 12:45:30   t13ae30
      *AVB01347: USt.pfl. Provisionen / VB-System -TWI-
      *
      *   Rev 2.2   Jul 08 2015 15:17:56   t13ae30
      *AVB01347: Upro - USTAT-Stat. letzten Abr. nicht vorhanden
      *
      *   Rev 2.1   Jul 07 2015 17:17:38   t13ae30
      *AVB01347: Upro - USTAT-Stat. letzten Abr. nicht vorhanden
      *
      *   Rev 2.0   Jul 01 2015 14:28:52   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1114                                     *
      *    FUNKTION ERSTER USTAT-STATUS NACH LETZTER ABRECHNUNG      *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    01.07.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!01.07.15! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1148                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB1114.
          05 PI-DRVB1114-ST-NR              PIC  9(007).
      *
       01 PO-DRVB1114.
          05 PO-DRVB1114-RC                 PIC  X(003).
             88 PO-DRVB1114-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1114-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB1114-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1114-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB1114-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB1114-DATEN.
             10 PO-DRVB1114-USTAT-STATUS    PIC S9(004).
      *
      ******** Ende Copy-Member DRVB1114 *****************************




