      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0449.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0449.CPv  $
      *
      *   Rev 2.1   Nov 18 1999 15:11:46   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Aug 04 1999 10:30:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0449                                     *
      *    FUNKTION AS-CLUB KRITERIEN PRÜFEN                         *
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
      *    FÜR DIE FUNKTION DRVB0449                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0449.
          05 PI-DRVB0449-PROGRAMM           PIC  X(008).
          05 PI-DRVB0449-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0449-MA-ST-NR           PIC  9(007).
          05 PI-DRVB0449-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVB0449.
          05 PO-DRVB0449-RC                 PIC  X(003).
             88 PO-DRVB0449-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0449-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0449-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0449-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0449-RC-ERR                       VALUE 'ERR'.

      *
      ******** Ende Copy-Member DRVB0449 *****************************
      *---------------------------------------------------------------*
