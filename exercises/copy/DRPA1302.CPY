      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1302.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRPA1302.CPv  $
      *
      *   Rev 2.11   Jan 30 2019 09:14:10   t13ex67
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.10   Jul 16 2015 15:12:04   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.9   Jan 13 2014 07:22:32   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.8   Mar 20 2006 08:38:18   t13ex44
      *APA00414: Erweiterung der Felder für Haftungszeitangaben bei
      * Central
      *
      *   Rev 2.7   Mar 19 2003 09:22:26   t13ex21
      *APA00223 - Vergrößerung Prov-Multi
      *
      *   Rev 2.6   Feb 03 2003 09:25:38   t13ex25
      *FPA00521; Printe und PGV-KZ weitergeben
      *
      *   Rev 2.5   Nov 21 2002 17:49:44   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.4   Mar 20 2002 10:23:14   t13ex21
      *FPA00368 - Erweiterung PI-Bereich
      *
      *   Rev 2.3   31 Aug 2001 09:48:30   t13ex21
      *APA00133 - Erweiterung um Laufzeit
      *
      *   Rev 2.2   15 May 2001 13:19:40   t13ex21
      *APA00132 - PI-Bereich erweitert
      *
      *   Rev 2.1   Jan 18 2000 12:02:34   T13AE18
      *APA00051 'Umbuchen einer PMPT'
      *
      *   Rev 2.0   Sep 02 1999 10:39:50   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1302                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<FKPAV043 - UMBUCHUNG EINER PMPT          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    17.09.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00051!17.09.99! 01 ! NEU                       ! BOHLINGER*
      * APA00132!07.05.01!2.02! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA00133!22.08.01!2.02! PI-BEREICH UM LAUFZEIT ERW! STORCH   *
      * FPA00368!18.03.02!2.03! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA00219!13.11.02!2.04! PROV-MULTI-NEU            ! MUND     *
      * FPA00521!31.01.03!2.05! PRINTE + PGV-KZ NEU       ! JÄGER    *
      * APA00223!12.03.03!2.06! PROV-MULTI VERGRÖSSERT    ! STORCH   *
      * APA00414!12.01.06!2.08! HAFTZEITDAUER UND BELAS-  ! SCHEIDEL *
      *         !        !    ! TUNGSDAUER VIERSTELLIG    !          *
      * APA01526!21.08.13!2.09! KZ NEUTRAL HINZU          ! STORCH   *
      *         !        !    ! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA01932!17.06.15!2.10! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA02548!23.01.19!2.11! DSGVO:                    ! STORCH   *
      *         !        !    ! PO-BEREICH ERWEITERT      !          *
      * DSY04126!04.01.22!    !PI-DRPA1302-NEUST-KZ NEU   ! WANG     *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1302                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1302.
          05 PI-DRPA1302-BUCHUNG-ALT.
             COPY DRPA0451.
          05 PI-DRPA1302-BA-PEST-PEDB.
             COPY DRPA0433.
      *
          05 PI-DRPA1302-BUCHUNG-NEU.
             10 PI-DRPA1302-KNR-BUCHUNGS-GRUND     PIC S9(004) BINARY.
             10 PI-DRPA1302-PGEB-GES               PIC S9(004) BINARY.
             10 PI-DRPA1302-PRODSL                 PIC S9(004) BINARY.
             10 PI-DRPA1302-GES-PROV               PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1302-BEITRAG                PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1302-SUMME                  PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1302-DGV                    PIC S9(004) BINARY.
             10 PI-DRPA1302-PRODART                PIC S9(004) BINARY.
             10 PI-DRPA1302-STICHTAG               PIC  X(010).
             10 PI-DRPA1302-BER-WAEHRUNG           PIC S9(004) BINARY.

          05 PI-DRPA1302-BN-PEST-PEDB.
             COPY DRPA0433.
      *
          05 PI-DRPA1302-S-GEGENBUCHUNG-ALT        PIC  X(001).
             88 PI-DRPA1302-S-GBA-NICHT-NOETIG          VALUE  '1'.
             88 PI-DRPA1302-S-GBA-VORHANDEN             VALUE  '2'.
             88 PI-DRPA1302-S-GBA-NICHT-VORH            VALUE  '3'.
      *
          05 PI-DRPA1302-GEGENBUCHUNG-ALT.
             COPY DRPA0451.
      *
          05 PI-DRPA1302-GBA-PEST-PEDB.
             COPY DRPA0433.

          05 PI-DRPA1302-HAFTZEITDAUER          PIC  X(004).
          05 PI-DRPA1302-BELASTUNGSDAUER        PIC  X(004).
          05 PI-DRPA1302-LAUFZEIT               PIC S9(004) BINARY.
          05 PI-DRPA1302-PMBE-ID                PIC S9(015)
                                                    PACKED-DECIMAL.
          05 PI-DRPA1302-PROV-MULTI             PIC  9(003)V999999.

          05 PI-DRPA1302-PMBE-GSMT-PROV-DAT     PIC  X(010).
          05 PI-DRPA1302-PGV-KZ                 PIC X.
          05 PI-DRPA1302-PRINTE                 PIC S9(015)
                                                    PACKED-DECIMAL.
210813    05 PI-DRPA1302-KZ-NEUTRAL             PIC X.
210813    05 PI-DRPA1302-VVG-KZ                 PIC X.
210813    05 PI-DRPA1302-UMB-36-MONATE          PIC X.
210813    05 PI-DRPA1302-TARIFSCHL-GRP          PIC S9(009) BINARY.
210813    05 PI-DRPA1302-TARIFSCHL              PIC S9(009) BINARY.
210813    05 PI-DRPA1302-KNR-TARIF-KZ           PIC S9(004) BINARY.
210813    05 PI-DRPA1302-VERM-GES               PIC S9(004) BINARY.
210813    05 PI-DRPA1302-KNR-MANDANT            PIC S9(004) BINARY.
210813    05 PI-DRPA1302-SAM-NR                 PIC S9(009) BINARY.
210813    05 PI-DRPA1302-KNR-RAB-ST             PIC S9(004) BINARY.
210813    05 PI-DRPA1302-ZAHLW                  PIC S9(004) BINARY.
210813    05 PI-DRPA1302-URSPR-POL-DAT          PIC X(010).
210813    05 PI-DRPA1302-FAELLIG-VON            PIC X(010).

170615    05 PI-DRPA1302-PG-UST-SATZ            PIC S9(003)V999 COMP-3.
170615    05 PI-DRPA1302-PG-UST-BETRAG          PIC S9(009)V99  COMP-3.


          05 PI-DRPA1302-PMPT-ALT.
             COPY DRPA0413.
             35   PMPT-LAST-UPDATE              PIC X(026).

          05 PI-DRPA1302-KONDITION-KDB          PIC S9(004) BINARY.
          05 PI-DRPA1302-S-KONDITION-KDB        PIC  X(001).
040122    05 PI-DRPA1302-NEUST-KZ               PIC  X(001).

      *
       01 PO-DRPA1302.
          05 PO-DRPA1302-RC                      PIC  X(003).
             88 PO-DRPA1302-RC-UMB-OK            VALUE 'UOK'.
             88 PO-DRPA1302-RC-UMB-OK-O-NEUT     VALUE 'UON'.
             88 PO-DRPA1302-RC-UMB-OK-NEUT-GBA   VALUE 'UNG'.
             88 PO-DRPA1302-RC-UMB-OK-NUR-NEU    VALUE 'UNN'.
             88 PO-DRPA1302-RC-UMB-NOK-O-AEND    VALUE 'UOA'.
             88 PO-DRPA1302-RC-UMB-NOK           VALUE 'UNO'.
             88 PO-DRPA1302-RC-UMB-UNZULAESSIG   VALUE 'UUZ'.
      *
          05 PO-DRPA1302-GEGENBUCHUNG.
             COPY DRPA0451.
          05 PO-DRPA1302-BUCHUNG-NEU.
             COPY DRPA0451.
          05 PO-DRPA1302-GEGENBUCHUNG-MOD.
             COPY DRPA0451.

          05 PO-DRPA1302-PMPT-NEU.
             COPY DRPA0413.
             35   PMPT-LAST-UPDATE               PIC X(026).

          05 PO-DRPA1302-S-PMPT-AEND-KZ          PIC X(001).

230119    05 PO-DRPA1302-KZ-GB-PEST-NEU          PIC X(001).
230119    05 PO-DRPA1302-GB-PEST-PEDB.
230119        COPY DRPA0433.

      ******* ENDE COPY-MEMBER **************************************

