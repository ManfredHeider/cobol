      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1188.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1188.cpv  $
      *
      *   Rev 2.1   Mar 16 1999 14:06:26   T01JM2S
      * 
      *
      *   Rev 2.0   Mar 05 1999 10:35:22   T01JM2S
      * 
      *
      *   Rev 1.0   21 Oct 1998 14:04:38   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1188                                      *
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
      *    FÜR DIE FUNKTION DRPA1188                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1188.
          05 PI-DRPA1188-DATEN.
             10 PI-DRPA1188-PVPT-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1188-PVPT-VERSION            PIC S9(004) BINARY.
      *
       01 PO-DRPA1188.
          05 PO-DRPA1188-RC                         PIC  X(003).
             88 PO-DRPA1188-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1188-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1188-DATEN.
             10 PO-DRPA1188-PVPT-VT-PRIO            PIC S9(004) BINARY.
             10 PO-DRPA1188-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PO-DRPA1188-PVER-VERSION            PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
