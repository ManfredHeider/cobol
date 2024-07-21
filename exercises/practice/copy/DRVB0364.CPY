      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0364.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0364.CPv  $
      *
      *   Rev 2.0   May 18 1999 17:02:30   T13AE42
      *NT-Version
      *
      *   Rev 1.1   May 17 1999 16:37:10   T13AE42
      *AAS00096 - Delta-Rueckkonvertierung
      *
      *   Rev 1.0   22 Jan 1999 13:40:58   P13SP20
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
      *    FÜR DIE FUNKTION DRVB0364                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0364.
          05 PI-DRVB0364-DATEN.
             10 PI-DRVB0364-EDED-UNTST-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0364.
          05 PO-DRVB0364-RC                  PIC  X(003).
             88 PO-DRVB0364-RC-OK            VALUE 'OK '.
             88 PO-DRVB0364-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0364-DATEN.
             10 PO-DRVB0364-EDED-BETR-NR      PIC S9(009) COMP.
             10 PO-DRVB0364-EDED-LAST-UPDATE  PIC  X(026).
      *
      ******** ENDE COPY-MEMBER  DRVB0364 ****************************
