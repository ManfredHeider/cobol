      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0621.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0621.CPv  $
      *
      *   Rev 2.1   Dec 19 2002 14:41:26   t13ae30
      *FVB00513 - Kapitänsclub abgebrochen
      *
      *   Rev 2.0   Dec 04 2002 12:20:44   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0621                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0621                    *
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
      *    FÜR DIE FUNKTION DRVB0621
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0621.
           05  PI-DRVB0621-MA-ST-NR           PIC  9(009) COMP.
           05  PI-DRVB0621-ZEITPUNKT          PIC  X(010).
      *
       01  PO-DRVB0621.
           05  PO-DRVB0621-RC                 PIC  X(003).
               88 PO-DRVB0621-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0621-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0621-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0621-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0621-AL-ZUSATZ          PIC  X(004).
      *
      ******* ENDE COPY-MEMBER **************************************
