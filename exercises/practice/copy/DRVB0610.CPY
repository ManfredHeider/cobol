      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0610.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0610.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:30:10   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:35:42   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0610                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0610                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    07.11.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00149!07.11.02!    ! NEU                       ! P.Duerr  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0610
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0610.
           05  PI-DRVB0610-UFKT-CODE          PIC  9(002).
           05  PI-DRVB0610-UNTST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0610-DATUM-VON          PIC  X(010).
           05  PI-DRVB0610-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0610.
           05  PO-DRVB0610-RC                 PIC  X(003).
               88 PO-DRVB0610-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0610-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0610-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0610-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0610-BVH-BETR-NR        PIC  S9(009) COMP.
           05  PO-DRVB0610-BVH-DAT-VON        PIC  X(010).
           05  PO-DRVB0610-BVH-DAT-BIS        PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************



