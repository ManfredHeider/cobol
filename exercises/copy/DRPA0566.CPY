      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0566.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0566.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:04:24   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 11:55:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0566                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPR AUS VORH. PMPR >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0566                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0566.
          05 PI-DRPA0566-DATEN.
             10 PI-DRPA0566-PGV                     PIC S9(004) BINARY.
             10 PI-DRPA0566-PMPR.
                COPY DRPA0412.
             10 PI-DRPA0566-PMBE.
                COPY DRPA0411.
      *
       01 PO-DRPA0566.
          05 PO-DRPA0566-RC                         PIC  X(003).
             88 PO-DRPA0566-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0566-DATEN.
             10 PO-DRPA0566-PMPR.
                COPY DRPA0412.
      *
      ******* ENDE COPY-MEMBER **************************************
