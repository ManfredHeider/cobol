      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0982.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0982.cpv  $
      *
      *   Rev 2.0   May 27 1999 11:27:56   T13EX25
      *APA00042, Erstellung V2.0
      *
      *   Rev 1.0   01 Apr 1998 14:20:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0982  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: ERB                                             *
      *    DATUM:    03.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!03.07.98! 01 ! NEUERSTELLUNG             ! ERB      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0982                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PAMS TBPA0129              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0982.
          05 PI-DRPA0982-PROGRAMM                PIC  X(008).
          05 PI-DRPA0982-UFKT                    PIC  9(002).
          05 PI-DRPA0982-DATEN.
             COPY DRPA0989.
      *
       01 PO-DRPA0982.
          05 PO-DRPA0982-RC                      PIC  X(003).
             88 PO-DRPA0982-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0982-RC-DUPLICATE-VALUE            VALUE 'DVA'.

