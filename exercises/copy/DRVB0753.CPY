      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0753.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0753.CPv  $
      *
      *   Rev 2.1   Mar 14 2005 09:41:18   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Feb 01 2005 16:47:26   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0753                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0753                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    01.02.2005                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00531!01.02.05!    ! NEU                       ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0753
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0753.
           05  PI-DRVB0753-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0753-DATUM-VON          PIC  X(010).
           05  PI-DRVB0753-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0753.
           05  PO-DRVB0753-RC                 PIC  X(003).
               88 PO-DRVB0753-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0753-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0753-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0753-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0753-EH-GS-GR-EG-PRG     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0753-EH-GS-GR-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0753-EH-GS-EG-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0753-EH-NEU-GS-EG-PRG    PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
