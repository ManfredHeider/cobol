      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0532.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0532.CPv  $
      *
      *   Rev 2.2   25 Jun 2001 07:49:42   t13ae13
      *AVB00534: Einführung PIN für VB-Portal 
      *
      *   Rev 2.1   09 May 2001 14:43:10   t13ae30
      * 
      *
      *   Rev 2.0   22 Feb 2001 16:59:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0532                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0532                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    09.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!09.05.01!    ! NEU                       !TH.WILLIG *
      *         !18.06.01!    ! PI-MPW vergrößert         !S. Walb   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0532
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0532.
180601*    05  PI-DRVB0532-MPW                PIC  S9(004)    COMP.
180601     05  PI-DRVB0532-MPW                PIC  X(010).
      *
       01  PO-DRVB0532.
           05  PO-DRVB0532-RC                 PIC  X(003).
               88 PO-DRVB0532-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0532-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0532-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0532-RC-ERR                       VALUE 'ERR'.
      *
180601*    05  PO-DRVB0532-MPW-TUPEL-GESAMT   PIC X(048).
180601     05  PO-DRVB0532-MPW-TUPEL-GESAMT   PIC X(040).
      *
      *********************** ENDE COPY-MEMBER *************************
