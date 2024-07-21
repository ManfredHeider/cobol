      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0441.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0441.cpv  $
      *
      *   Rev 2.1   Sep 02 1999 12:10:00   T13AE30
      * 
      *
      *   Rev 2.0   Aug 04 1999 10:30:12   P13SP20
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0441                                     *
      *    FUNKTION: ERMITTELT DAS LAST-UPDATE ZU EINER SDNR, UM     *
      *    ES ALS DAT_VON ZU NUTZEN (SIEHE PMVB0212 STAMMDATEN-DTA)  *
      *    DIES IST EIN NOTBEHELF, DA FÜR SDNR KEIN DAT_VON VORHANDEN*
      *    IST.                                                      *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    24.08.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!24.08.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0441                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0441.
          05 PI-DRVB0441-DATEN.
             10 PI-DRVB0441-ST-NR            PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0441.
          05 PO-DRVB0441-RC                  PIC  X(003).
             88 PO-DRVB0441-RC-OK            VALUE 'OK '.
             88 PO-DRVB0441-RC-NFD           VALUE 'NFD'.

          05 PO-DRVB0441-DATEN.
             10 PO-DRVB0441-DATE-LAST-UPDATE PIC x(010).
      *
      ******** Ende Copy-Member **************************************

