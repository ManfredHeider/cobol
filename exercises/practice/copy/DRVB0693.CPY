      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0693.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0693.CPv  $
      *
      *   Rev 2.1   Dec 18 2003 15:26:02   t13ae27
      *NEU: Jahres-WBW-2004
      *
      *   Rev 2.0   Feb 17 2003 11:01:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0693                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0693                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    17.12.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00443!17.12.03!    ! NEU                       ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0693
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0693.
           05  PI-DRVB0693-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0693-DATUM-VON          PIC  X(010).
           05  PI-DRVB0693-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0693.
           05  PO-DRVB0693-RC                 PIC  X(003).
               88 PO-DRVB0693-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0693-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0693-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0693-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0693-EH-GS-GR-EG-PRG     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0693-EH-GS-GR-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0693-EH-GS-EG-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0693-EH-NEU-GS-EG-PRG    PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

