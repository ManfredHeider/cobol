      * T01JM2E   Stand: 05/01/99    14:59:41
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1209.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1209.CPv  $
      *
      *   Rev 2.3   Jun 18 1999 10:52:36   T01JM2E
      *APA00012 - Erweitern Satzzähler für RESTART
      *
      *   Rev 2.2   May 27 1999 13:06:08   T01JM2E
      *Übergabe nach Produktion APA00042
      *
      *   Rev 2.1   Apr 16 1999 16:15:24   T01JM2E
      * 
      *
      *   Rev 1.0   01 Apr 1998 14:19:00   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1209                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  ARBEITSSATZPAKET ERMITTELN               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    05.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.04.99! 01 ! NEU                       !D.WILHELM *
      *         !16.06.99!2.7 ! ANZ-READ AUF S9(8) BINARY !D.WILHELM *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1209                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1209.
          05 PI-DRPA1209-UFKT-CODE               PIC  9(002).
          05 PI-DRPA1209-PROGRAMM                PIC  X(008).
160699    05 PI-DRPA1209-ANZ-READ                PIC S9(008) BINARY.
      *
       01 PO-DRPA1209.
          05 PO-DRPA1209-RC                      PIC  X(003).
             88 PO-DRPA1209-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1209-RC-NOT-FOUND              VALUE 'NFD'.
             88 PO-DRPA1209-RC-EOF                    VALUE 'EOF'.
