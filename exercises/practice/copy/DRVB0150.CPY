      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0150.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0150.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:02   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   Feb 19 1997 09:58:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0150  FUNKTION Rückstellungs-Satz für    *
      *                           RFT ermitteln                      *
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
      *    FÜR DIE FUNKTION DRVB0150                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0150.
          05 PI-DRVB0150-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRVB0150-DATEN.
             10 PI-DRVB0150-RFT-ST-NR        PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0150.
          05 PO-DRVB0150-RC                  PIC  X(003).
             88 PO-DRVB0150-RC-OK            VALUE 'OK '.
             88 PO-DRVB0150-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0150-DATEN.
             10 PO-DRVB0150-RFT-RCK-STZ      PIC S9(003)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
