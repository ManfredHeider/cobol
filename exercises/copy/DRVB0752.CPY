      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.02.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0752.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0752.CPv  $
      *
      *   Rev 2.1   Mar 14 2005 09:38:44   T13AE27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Feb 01 2005 16:47:06   T13AE27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0752                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0752                    *
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
      *    FÜR DIE FUNKTION DRVB0752
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0752.
           05  PI-DRVB0752-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0752-DATUM-VON          PIC  X(010).
           05  PI-DRVB0752-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0752.
           05  PO-DRVB0752-RC                 PIC  X(003).
               88 PO-DRVB0752-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0752-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0752-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0752-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0752-EH-GS-GR-EG-PRG     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0752-EH-GS-GR-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0752-EH-GS-EG-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0752-EH-NEU-GS-EG-PRG    PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
