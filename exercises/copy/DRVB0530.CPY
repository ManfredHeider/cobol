      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0530.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0530.CPv  $
      *
      *   Rev 2.1   20 Apr 2001 14:21:44   t13ae30
      *FAS00063 - Fehler beim Ermitteln Leistungplan / Wettbewerb
      *
      *   Rev 2.0   22 Feb 2001 16:59:50   p13sp16
      *Archiverstellung
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0530                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0530                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    06.04.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAS00063!06.04.01!    ! NEU                       !TH.WILLIG *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0530
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01 PI-DRVB0530.
          05 PI-DRVB0530-PROGRAMM           PIC  X(008).
          05 PI-DRVB0530-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0530-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0530-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0530.
          05 PO-DRVB0530-RC                 PIC  X(003).
             88 PO-DRVB0530-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0530-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0530-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0530-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0530-DATEN.
             10 PO-DRVB0530-ED-KONTO         PIC  S9(009) COMP.
      *
      ******* ENDE COPY-MEMBER DRVB0530 ********************************
