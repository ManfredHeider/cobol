      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0168.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0168.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 15 1997 13:47:28   T13AE31
      *Neuerstellung
      *
      *   Rev 1.0   Feb 19 1997 10:22:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0168  FUNKTION LIST-OZP FÜR TABELLE PVST *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.01.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0005!31.01.97! 01 ! NEUERSTELLUNG             ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0168                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0168.
          05 PI-DRPA0168-PROGRAMM           PIC  X(008).
          05 PI-DRPA0168-UFKT-CODE          PIC  9(002).
          05 PI-DRPA0168-AUSWAHL-KRITERIEN.
             10 PI-DRPA0168-VSTAT-MIN       PIC  X(004).
             10 PI-DRPA0168-VSTAT-MAX       PIC  X(004).
      *
       01 PO-DRPA0168.
          05 PO-DRPA0168-RC                 PIC  X(003).
             88 PO-DRPA0168-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0168-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA0168-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA0168-DATEN.
             10 PO-DRPA0168-PVST-VSTAT       PIC  X(004).
             10 PO-DRPA0168-PVST-TEXT        PIC  X(050).
             10 PO-DRPA0168-PVST-LAST-UPDATE PIC  X(026).
             10 PO-DRPA0168-PVST-KENNUNG     PIC  X(008).
      *
      ***** Ende-Copy-Member *****************************************
