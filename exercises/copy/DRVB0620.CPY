      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0620.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0620.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:32:04   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:36:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0620                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0620                    *
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
      *    FÜR DIE FUNKTION DRVB0620
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0620.
           05  PI-DRVB0620-UFKT-CODE          PIC  9(002).
           05  PI-DRVB0620-BETR-NR            PIC  S9(009)    COMP.
           05  PI-DRVB0620-UNTST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0620-DATUM-VON          PIC  X(010).
           05  PI-DRVB0620-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0620.
           05  PO-DRVB0620-RC                 PIC  X(003).
               88 PO-DRVB0620-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0620-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0620-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0620-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0620-BVH-DAT-VON        PIC  X(010).
           05  PO-DRVB0620-BVH-DAT-BIS        PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************



