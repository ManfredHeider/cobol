      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0695.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0695.CPv  $
      *
      *   Rev 2.1   Dec 18 2003 15:37:22   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.0   Feb 17 2003 11:01:34   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0695                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0695                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    17.12.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00443!17.12.03! 01 ! NEU                       ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0695
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0695.
           05  PI-DRVB0695-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0695-DATUM-VON          PIC  X(010).
           05  PI-DRVB0695-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0695.
           05  PO-DRVB0695-RC                 PIC  X(003).
               88 PO-DRVB0695-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0695-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0695-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0695-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0695-EH-GS-EG           PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


