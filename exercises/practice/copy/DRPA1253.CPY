      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1253.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1253.CPv  $
      *
      *   Rev 2.4   Jun 19 2012 09:28:44   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.3   May 21 2012 08:53:32   t13ex67
      *APA01101: Restrukturierung (Mandant,SW-Überarbeitung)
      *
      *   Rev 2.2   Feb 20 2008 12:19:18   t13ex21
      *APA00622: Allfinanz Stufe 1 im abgerechneten Geschäft
      *
      *   Rev 2.1   Jun 22 1999 23:16:00   T13AE18
      *apa00012
      *
      *   Rev 2.0   Mar 30 1999 08:44:14   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1253   FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.06.99! 01 ! NEUERSTELLUNG             !BOHLINGER *
      * APA00622!12.02.08!2.02! KNR-VGES HINZU            ! STORCH   *
      * APA01101!23.02.12!2.03! VGES DURCH MANDANT ERSETZT! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1253                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PRUB TBPA0025                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1253.
          05 PI-DRPA1253-PRUB-KEY.
             10 PI-DRPA1253-PRUB-ID             PIC S9(015)V
                                                    PACKED-DECIMAL.
             10 PI-DRPA1253-PDPT-ID             PIC S9(015)V
                                                    PACKED-DECIMAL.

       01 PO-DRPA1253.
          05 PO-DRPA1253-RC                     PIC  X(003).
             88 PO-DRPA1253-RC-OK                    VALUE 'OK '.
             88 PO-DRPA1253-RC-NOT-FOUND             VALUE 'NFD'.

          05 PO-DRPA1253-PRUB-DATEN.
             10 PO-DRPA1253-PGEB-GES            PIC S9(004) BINARY.
             10 PO-DRPA1253-PRINTE              PIC S9(015)V
                                                PACKED-DECIMAL.
             10 PO-DRPA1253-PVER-VERSION        PIC S9(004) BINARY.
             10 PO-DRPA1253-PMPT-ID             PIC S9(015)V
                                                PACKED-DECIMAL.
             10 PO-DRPA1253-PMPT-VERSION        PIC S9(004) BINARY.
             10 PO-DRPA1253-KNR-BUCH-GRUND-GRP  PIC S9(004) BINARY.
             10 PO-DRPA1253-PRUB-UMBUCH-DAT     PIC  X(010).
             10 PO-DRPA1253-PRUB-KENNUNG        PIC  X(008).
             10 PO-DRPA1253-KNR-MANDANT         PIC S9(004) BINARY.
      *
      ****** ENDE COPY-MEMBER ****************************************
      *---------------------------------------------------------------*
