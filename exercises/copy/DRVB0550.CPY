      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0550.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0550.CPv  $
      *
      *   Rev 2.1   10 Jul 2001 11:13:32   t13ae13
      *AVB00534: Einführung PIN
      *
      *   Rev 2.0   15 May 2001 13:33:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0550                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0550                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    10.07.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!10.07.01!2.1 ! NEU                       !S. Walb   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0550
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0550.
           05  PI-DRVB0550-VBNR               PIC  S9(009)    COMP.
      *
       01  PO-DRVB0550.
           05  PO-DRVB0550-RC                 PIC  X(003).
               88 PO-DRVB0550-RC-OK                VALUE 'OK '.
               88 PO-DRVB0550-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0550-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0550-RC-ERR               VALUE 'ERR'.
      *
           05  PO-DRVB0550-AKT-INAKT          PIC  9(001).
               88 PO-DRVB0550-AKTIV                VALUE 1.
               88 PO-DRVB0550-INAKTIV              VALUE 2.
      *
      *********************** ENDE COPY-MEMBER *************************
