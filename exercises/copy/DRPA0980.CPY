      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0980.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0980.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:25:24   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   01 Apr 1998 14:20:16   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPAO980  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: ERB                                             *
      *    DATUM:    03.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.07.98!01 ! NEUERSTELLUNG             ! ERB      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPAO980                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PAMS TBPA0129              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0980.
          05 PI-DRPA0980-PDPT-KEY.
             10 PI-DRPA0980-PDPT-ID              PIC S9(15)V COMP-3.

       01 PO-DRPA0980.
          05 PO-DRPA0980-RC                      PIC  X(003).
             88 PO-DRPA0980-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0980-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0980-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0980-PAMS-DATEN.
             COPY DRPA0989.

