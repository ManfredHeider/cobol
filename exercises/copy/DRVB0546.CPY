      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0546.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0546.CPv  $
      *
      *   Rev 2.1   25 Jun 2001 07:59:16   t13ae13
      *AVB00534: Einführung PIN für VB-Portal
      *
      *   Rev 2.0   15 May 2001 13:33:26   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0546                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0546                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    28.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!28.05.01!    ! NEU                       !S. Walb   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0546
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0546.
           05  PI-DRVB0546-MA-ST-NR           PIC  S9(009)    COMP.
      *
       01  PO-DRVB0546.
           05  PO-DRVB0546-RC                 PIC  X(003).
               88 PO-DRVB0546-RC-OK                VALUE 'OK '.
               88 PO-DRVB0546-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0546-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0546-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRVB0546-PIN                PIC  X(010).
      *
      *********************** ENDE COPY-MEMBER *************************
