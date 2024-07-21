      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0612.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0612.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:30:42   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:35:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0612                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0612                    *
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
      *    FÜR DIE FUNKTION DRVB0612
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0612.
           05  PI-DRVB0612-BETR-NR            PIC  S9(009)    COMP.
           05  PI-DRVB0612-UNTST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0612-DAT-ANSPRUCH       PIC  X(010).
      *
       01  PO-DRVB0612.
           05  PO-DRVB0612-RC                 PIC  X(003).
               88 PO-DRVB0612-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0612-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0612-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0612-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0612-TUPEL              PIC  X(190).
      *
      ******* ENDE COPY-MEMBER **************************************
