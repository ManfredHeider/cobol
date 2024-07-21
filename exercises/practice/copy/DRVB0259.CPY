      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0259.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0259.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:34   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:23:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0259  FUNKTION Austrittsdatum ermitteln  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: Zgur-Vujic                                      *
      *    DATUM:    13.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!13.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0259                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0259.
          05 PI-DRVB0259-DATEN.
             10 PI-DRVB0259-KL-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0259.
          05 PO-DRVB0259-RC                  PIC  X(003).
             88 PO-DRVB0259-RC-OK            VALUE 'OK '.
             88 PO-DRVB0259-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0259-DATEN.
             10 PO-DRVB0259-KL-DAT-BIS       PIC x(010).
             10 PO-DRVB0259-NI-KL-DAT-BIS    PIC s9(4).
      *
      ******** Ende Copy-Member **************************************
