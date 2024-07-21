      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1181.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1181.cpv  $
      *
      *   Rev 2.1   Mar 16 1999 14:03:50   T01JM2S
      * 
      *
      *   Rev 2.0   Mar 05 1999 10:32:06   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   21 Oct 1998 14:04:00   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1181                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN FÜR DRPA1181  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    08.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.03.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1181                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1181.
          05 PI-DRPA1181-DATEN.
             10 PI-DRPA1181-PDPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1181.
          05 PO-DRPA1181-RC                         PIC  X(003).
             88 PO-DRPA1181-RC-OK                         VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
