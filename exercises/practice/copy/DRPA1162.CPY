      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1162.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1162.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:30:36   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 26 1999 16:02:06   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:02:16   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1162                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<FKPAV041- FREIGABE EINER BUCHUNG>>         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: LÜTTGES                                          *
      *    DATUM:    08.02.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.02.98! 01 ! NEU                       ! LÜTTGES  *
      * APA02908!17.02.20!2.1 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1162                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1162.
170220    05 PI-DRPA1162-PGEB-GES      PIC S9(004) BINARY.
          05 PI-DRPA1162-PMPT-ID       PIC S9(015) PACKED-DECIMAL.
          05 PI-DRPA1162-PMPT-VERSION  PIC S9(004) BINARY.
      *
       01 PO-DRPA1162.
          05 PO-DRPA1162-RC            PIC  X(003).
             88 PO-DRPA1162-RC-OK           VALUE 'OK '.
             88 PO-DRPA1162-RC-NOK          VALUE 'NOK'.
             88 PO-DRPA1162-RC-CHA          VALUE 'CHA'.
             88 PO-DRPA1162-RC-VST-IN-VGR-NFD
                                            VALUE 'VNF'.

      ******* ENDE COPY-MEMBER **************************************

