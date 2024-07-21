      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0257.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0257.CPv  $
      *
      *   Rev 2.1   Nov 18 1999 15:11:04   T13AE30
      *AAS00158 - ERMITTLUNG AS-CLUB-MITGLIEDER
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:22:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0257                                     *
      *    FUNKTION AS-CLUB-MITGLIEDER ERMITTELN, DEREN MITGLIED-    *
      *    SCHAFT IM AS-CLUB ERNEUT ÜBERPRÜFT WERDEN MUß.            *
      *    (NACH ABLAUF DER MINDESTMITGLIEDSDAUER VON 3-MONATEN)     *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    08.11.1999                                      *
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
      *    FÜR DIE FUNKTION DRVB0257                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0257.
          05 PI-DRVB0257-PROGRAMM                PIC  X(008).
          05 PI-DRVB0257-UFKT-CODE               PIC  9(002).
      *
       01 PO-DRVB0257.
          05 PO-DRVB0257-RC                      PIC  X(003).
             88 PO-DRVB0257-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0257-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0257-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0257-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0257-DATEN.
             10 PO-DRVB0257-MA-ST-NR             PIC  S9(009)  COMP.
             10 PO-DRVB0257-MAST-DAT-VON         PIC  X(026).
             10 PO-DRVB0257-KTY-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0257-KNR-MAST-TYP         PIC S9(004) USAGE COMP.
             10 PO-DRVB0257-MAST-LAST-UPDATE     PIC  X(026).
             10 PO-DRVB0257-MAST-DAT-BIS         PIC  X(010).
      *
      ******** Ende Copy-Member DRVB0257 *****************************
