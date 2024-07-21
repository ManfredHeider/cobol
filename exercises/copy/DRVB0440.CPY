      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0440.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0440.CPv  $
      *
      *   Rev 2.2   Jan 11 2000 16:25:54   T13AE30
      *AAS00051 - Stammdaten-DTA -> Aachen
      *
      *   Rev 2.1   Sep 02 1999 12:09:58   T13AE30
      * 
      *
      *   Rev 2.0   Aug 04 1999 10:30:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0440  FUNKTION Austrittsdatum ermitteln  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    23.08.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!23.08.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      * AAS00051!25.11.99! 02 ! EINBAU NULL-INDIKATOR     ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0440                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0440.
          05 PI-DRVB0440-DATEN.
             10 PI-DRVB0440-KL-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0440.
          05 PO-DRVB0440-RC                  PIC  X(003).
             88 PO-DRVB0440-RC-OK            VALUE 'OK '.
             88 PO-DRVB0440-RC-NFD           VALUE 'NFD'.

          05 PO-DRVB0440-DATEN.
             10 PO-DRVB0440-KL-DAT-VON       PIC x(010).
             10 PO-DRVB0440-KL-DAT-BIS       PIC x(010).
             10 PO-DRVB0440-NI-KL-DAT-BIS    PIC S9(004) COMP.
      *
      ******** Ende Copy-Member **************************************

