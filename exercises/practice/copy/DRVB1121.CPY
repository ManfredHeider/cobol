      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1121.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1121.CPv  $
      *
      *   Rev 2.1   Oct 16 2015 11:39:40   t13ae13
      *AVB01347: Lucy - USt.pfl. Provisionen / VB-System - ZWVB0065
      *
      *   Rev 2.0   Oct 14 2015 09:44:04   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1121                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1121                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    14.10.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01347!14.10.15!2.1 ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1121
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1121.
           05  PI-DRVB1121-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB1121-DATUM-VON          PIC  X(010).
           05  PI-DRVB1121-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB1121.
           05  PO-DRVB1121-RC                 PIC  X(003).
               88 PO-DRVB1121-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1121-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1121-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB1121-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB1121-EH-GS-GR-EG         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB1121-NI-EH-GS-GR-EG      PIC  S9(004)   COMP.
      *
      ******* ENDE COPY-MEMBER **************************************


