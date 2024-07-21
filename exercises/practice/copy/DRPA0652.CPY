      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0652.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0652.CPv  $
      *
      *   Rev 2.0   May 27 1999 11:44:58   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:57:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0652                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST-OZP PEBD       >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0652                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0652.
          05 PI-DRPA0652-PROGRAMM                   PIC  X(008).
          05 PI-DRPA0652-UFKT                       PIC  9(002).
          05 PI-DRPA0652-DATEN.
             10 PI-DRPA0652-PEST-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0652.
          05 PO-DRPA0652-RC                         PIC  X(003).
             88 PO-DRPA0652-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0652-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA0652-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA0652-DATEN.
             10 PO-DRPA0652-PEBD.
                COPY DRPA0414.
      *
      ******* ENDE COPY-MEMBER **************************************
