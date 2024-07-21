      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0615.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0615.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 10:37:42   t13ae27
      *neu
      *
      *   Rev 2.0   Nov 05 2002 11:36:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0615                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0615                    *
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
      *    FÜR DIE FUNKTION DRVB0615
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0615.
           05  PI-DRVB0615-UFKT-CODE          PIC  9(002).
           05  PI-DRVB0615-BBON-DAT-ANSPRUCH  PIC  X(010).
      *
       01  PO-DRVB0615.
           05  PO-DRVB0615-RC                 PIC  X(003).
               88 PO-DRVB0615-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0615-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0615-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0615-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0615-TUPEL              PIC  X(190).
      *
      ******* ENDE COPY-MEMBER **************************************
