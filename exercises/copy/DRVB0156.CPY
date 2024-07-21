      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0156.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drvb0156.cpv  $
      *
      *   Rev 2.0   May 28 1999 13:24:24   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   Feb 19 1997 09:59:18   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0156  FUNKTION Rückstellungs ermitteln   *
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
      *    FÜR DIE FUNKTION DRVB0156                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0156.
          05 PI-DRVB0156-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRVB0156-DATEN.
             10 PI-DRVB0156-ST-NR            PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0156.
          05 PO-DRVB0156-RC                  PIC  X(003).
             88 PO-DRVB0156-RC-OK            VALUE 'OK '.
             88 PO-DRVB0156-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0156-DATEN.
             10 PO-DRVB0156-RCK-STZ      PIC S9(003)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
