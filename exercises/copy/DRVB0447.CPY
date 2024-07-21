      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0447.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0447.CPv  $
      *
      *   Rev 2.1   Nov 18 1999 15:11:42   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Aug 04 1999 10:30:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0447                                     *
      *    FUNKTION AS-CLUB KANDIDATEN ERMITTELN                     *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    14.10.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00158!14.10.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0447                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0447.
          05 PI-DRVB0447-PROGRAMM           PIC  X(008).
          05 PI-DRVB0447-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0447.
          05 PO-DRVB0447-RC                 PIC  X(003).
             88 PO-DRVB0447-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0447-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0447-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0447-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0447-DATEN.
             10 PO-DRVB0447-MA-ST-NR        PIC  9(007)  COMP-3.

      *
      ******** Ende Copy-Member DRVB0447 *****************************
