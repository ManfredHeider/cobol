      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0154.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0154.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:12   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   Feb 19 1997 09:59:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0154  FUNKTION Rückstellungs-Satz für    *
      *                           ED ermitteln                       *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    08.08.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !08.08.97! 01 ! NEUERSTELLUNG             ! JÄGER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0154                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0154.
          05 PI-DRVB0154-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRVB0154-DATEN.
             10 PI-DRVB0154-JUR-ST-NR        PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0154.
          05 PO-DRVB0154-RC                  PIC  X(003).
             88 PO-DRVB0154-RC-OK            VALUE 'OK '.
             88 PO-DRVB0154-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0154-DATEN.
             10 PO-DRVB0154-JUR-RCK-STZ      PIC S9(003)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
