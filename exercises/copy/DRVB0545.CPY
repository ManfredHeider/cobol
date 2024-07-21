      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0545.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRVB0545.CPv  $
      *
      *   Rev 2.3   05 Jul 2001 11:32:08   t13ae13
      *AVB00534: Einführung PIN
      *
      *   Rev 2.2   25 Jun 2001 07:56:32   t13ae13
      *AVB00534: Einführung PIN für VB-Portal
      *
      *   Rev 2.1   19 Jun 2001 10:04:50   t13ae13
      *AVB00534: Einführung Masterpaßwort für VB-Portal
      *
      *   Rev 2.0   15 May 2001 13:33:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0545                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0545                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    28.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!28.05.01!2.1 ! NEU                       !S. Walb   *
      *         !        !2.2 ! neues PI-Feld             !S. Walb   *
      *         !05.07.01!2.3 ! neues PI-Feld ausgesternt !S. Walb   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0545
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0545.
           05  PI-DRVB0545-FUNKTION           PIC  S9(004)    COMP.
           05  PI-DRVB0545-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0545-VERARB-TIMESTAMP   PIC  X(026).
050701*    05  PI-DRVB0545-PROGRAMM-KZ        PIC  X(001).
      *
       01  PO-DRVB0545.
           05  PO-DRVB0545-RC                 PIC  X(003).
               88 PO-DRVB0545-RC-OK                VALUE 'OK '.
               88 PO-DRVB0545-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0545-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0545-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRVB0545-PIN                PIC  X(010).
      *
      *********************** ENDE COPY-MEMBER *************************
