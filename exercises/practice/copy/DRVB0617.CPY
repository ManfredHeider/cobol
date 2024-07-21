      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0617.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0617.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:31:12   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:36:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0617                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0617                    *
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
      *    FÜR DIE FUNKTION DRVB0617
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0617.
           05  PI-DRVB0617-UNTST-NR           PIC  S9(009)    COMP.
      *
       01  PO-DRVB0617.
           05  PO-DRVB0617-RC                 PIC  X(003).
               88 PO-DRVB0617-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0617-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0617-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0617-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0617-BBON-DAT-VON       PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************
