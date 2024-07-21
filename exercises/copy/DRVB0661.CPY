      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0661.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0661.CPv  $
      *
      *   Rev 2.1   Jun 10 2003 14:21:58   t13ex22
      *AVB00666: Einheitenberechnung Basis-/Steigerungszeitraum
      *
      *   Rev 2.0   Feb 17 2003 10:58:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0661                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0661                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T.Mund                                           *
      *    DATUM:    15.05.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!15.05.03!    ! NEU                       ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0558
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01 PI-DRVB0661.
          05 PI-DRVB0661-PROGRAMM           PIC  X(008).
          05 PI-DRVB0661-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0661-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0661-ZEITPUNKT-VON      PIC  X(010).
          05 PI-DRVB0661-ZEITPUNKT-BIS      PIC  X(010).
      *
       01 PO-DRVB0661.
          05 PO-DRVB0661-RC                 PIC  X(003).
             88 PO-DRVB0661-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0661-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0661-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0661-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0661-DATEN.
             10 PO-DRVB0661-ED-KONTO         PIC  S9(009) COMP.
             10 PO-DRVB0661-EDD-BEZUGS-NR    PIC  S9(009) COMP.
             10 PO-DRVB0661-EDD-DAT-VON      PIC  X(010).
      *
      ******* ENDE COPY-MEMBER DRVB0661 ********************************

