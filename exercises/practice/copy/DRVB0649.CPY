      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0649.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0649.CPv  $
      *
      *   Rev 2.1   Apr 30 2003 09:12:34   t13ae27
      *LP2003
      *
      *   Rev 2.0   Feb 17 2003 10:57:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0649                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0649                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    02.04.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00400!02.04.03!    ! NEU                       !P. Duerr  *
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
       01 PI-DRVB0649.
          05 PI-DRVB0649-PROGRAMM           PIC  X(008).
          05 PI-DRVB0649-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0649-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0649-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0649.
          05 PO-DRVB0649-RC                 PIC  X(003).
             88 PO-DRVB0649-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0649-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0649-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0649-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0649-DATEN.
             10 PO-DRVB0649-ED-KONTO         PIC  S9(009) COMP.
             10 PO-DRVB0649-EDD-BEZUGS-NR    PIC  S9(009) COMP.
             10 PO-DRVB0649-EDD-DAT-VON      PIC  X(010).
      *
      ******* ENDE COPY-MEMBER DRVB0649 ********************************
