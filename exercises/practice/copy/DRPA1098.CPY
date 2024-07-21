      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1098.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1098.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:48:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   20 Jul 1998 10:21:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1098 FUNKTION PUT VM                     *
      *                          FüR VERTRAGSDATENMODUL   UGPA0001   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    13.10.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.10.98! 01 !NEU                        ! STORCH   *
      *         !02.11.98!    !ÄNDERUNGEN IM PI-BEREICH   ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1098                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1098.
             10 PI-DRPA1098-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1098-VM                 PIC S9(009) BINARY.
021198       10 PI-DRPA1098-PEST-ID            PIC S9(015)
021198                                         PACKED-DECIMAL.

       01 PO-DRPA1098.
          05 PO-DRPA1098-RC                    PIC  X(003).
             88 PO-DRPA1098-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1098-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1098-MELDUNG.
             10 PO-DRPA1098-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1098-MELDUNG-NR         PIC S9(004) BINARY.

          05 PO-DRPA1098-VSTATUS               PIC  X(0004).
      *
