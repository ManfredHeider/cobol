      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0363.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0363.CPv  $
      *
      *   Rev 2.0   May 18 1999 10:29:00   T13AE42
      *NT-Version
      *
      *   Rev 1.1   May 17 1999 16:01:16   T13AE42
      *AAS00117 - Delta-Rueckkonvertierung
      *
      *   Rev 1.0   22 Jan 1999 13:40:56   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    --------------------------------------------------------- *
      *    ERSTELLT: SCHMIDT, B.                                     *
      *    DATUM:    22.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00117!22.02.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0363                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0363.
          05 PI-DRVB0363-DATEN.
             10 PI-DRVB0363-BVH-UNTST-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0363.
          05 PO-DRVB0363-RC                  PIC  X(003).
             88 PO-DRVB0363-RC-OK            VALUE 'OK '.
             88 PO-DRVB0363-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0363-DATEN.
             10 PO-DRVB0363-BVH-BETR-NR      PIC S9(009) COMP.
             10 PO-DRVB0363-BVH-LAST-UPDATE  PIC  X(026).
      *
      ******** ENDE COPY-MEMBER  DRVB0363 ****************************
