      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0263.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0263.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:40   T13EX19
      *VB-SCHNITTSTELLE
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0263  FUNKTION Austrittsdatum ermitteln  *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: Zgur-Vujic                                      *
      *    DATUM:    16.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!16.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0263                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0263.
          05 PI-DRVB0263-DATEN.
             10 PI-DRVB0263-JUR-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0263.
          05 PO-DRVB0263-RC                  PIC  X(003).
             88 PO-DRVB0263-RC-OK            VALUE 'OK '.
             88 PO-DRVB0263-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0263-DATEN.
             10 PO-DRVB0263-NI-JUR-DAT-BIS    PIC s9(4).
             10 PO-DRVB0263-JUR-DAT-BIS       PIC x(010).
             10 PO-DRVB0263-JUR-DAT-VON       PIC x(010).
             10 PO-DRVB0263-JUR-BEZ           PIC x(040).
      *
      ******** Ende Copy-Member **************************************
