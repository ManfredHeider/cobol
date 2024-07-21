      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1030.CPY  $
      *    $Revision:   2.12  $
      *
      *    $Log:   L:/mf/CPY/DRPA1030.CPv  $
      *
      *   Rev 2.12   Jan 30 2019 09:13:54   t13ex67
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.11   Jul 16 2015 15:11:54   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.10   Jan 13 2014 07:22:20   t13ex67
      *APA01539: Kalypso TP5: Anpassungen Datenmodell: TBPA0023-TBP
      *A0033
      *
      *   Rev 2.9   Nov 08 2006 15:58:16   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.8   Mar 20 2006 08:38:06   t13ex44
      *APA00414: Erweiterung der Felder für Haftungszeitangaben bei
      * Central
      *
      *   Rev 2.7   Mar 19 2003 09:22:12   t13ex21
      *APA00223 - Vergrößerung Prov-Multi
      *
      *   Rev 2.6   Feb 03 2003 09:24:32   t13ex25
      *FPA00521; Printe und PGV-KZ weitergeben
      *
      *   Rev 2.5   Nov 21 2002 17:49:24   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.4   Mar 20 2002 10:23:26   t13ex21
      *FPA00368 - Erweiterung PI-Bereich
      *
      *   Rev 2.3   31 Aug 2001 09:48:16   t13ex21
      *APA00133 - Erweiterung um Laufzeit
      *
      *   Rev 2.2   15 May 2001 13:18:44   t13ex21
      *APA0096 - PI-Bereich erweitert
      *
      *   Rev 2.1   Jan 18 2000 12:02:30   T13AE18
      *APA00051 'Umbuchen einer PMPT'
      *
      *   Rev 2.0   May 26 1999 13:42:32   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jul 1998 13:59:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1030                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<FKPAV031 - BUCHUNG UMBUCHEN>>            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                            *
      *    DATUM:    28.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.07.98! 01 ! NEU                       ! FOLTZ    *
      * AFPA0012!16.10.98!1.01! PO-BUCHUNGSGRUND-NEU RAUS ! LÜTTGES  *
      * APA00051!17.09.99!2.02! PO-BEREICH UM KONDITIONEN ! BOHLINGER*
      *         !        !    ! ERWEITERT                 !          *
      * APA00132!07.05.01!2.03! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA00133!22.08.01!2.04! PI-BEREICH UM LAUFZEIT ERW! STORCH   *
      * FPA00368!18.03.02!2.05! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA00219!13.11.02!2.04! PROV-MULTI-NEU            ! MUND     *
      * FPA00521!31.01.03!2.05! PRINTE + PGV-KZ NEU       ! JÄGER    *
      * APA00223!12.03.03!2.06! PROV-MULTI VERGRÖSSERT    ! STORCH   *
      * APA00414!12.01.06!2.07! HAFTZEITDAUER UND BELAS-  ! SCHEIDEL *
      *         !        !    ! TUNGSDAUER VIERSTELLIG    !          *
      * APA00452!08.11.06!2.08! UMSTELLUNG 3-STELL. PROD. ! JÄGER    *
      * APA01526!21.08.13!2.09! KZ NEUTRAL HINZU          ! STORCH   *
      * APA01932!17.06.15!2.10! PI-BEREICH ERWEITERT      ! STORCH   *
      * APA02548!23.01.19!2.11! DSGVO:                    ! STORCH   *
      *         !        !    ! PO-BEREICH ERWEITERT      !          *
      * DSY04126!04.01.22!    ! PI-DRPA1030-NEUST-KZ NEU  ! WANG     * 
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1030                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1030.
          05 PI-DRPA1030-BUCHUNG-ALT.
             COPY DRPA0451.
          05 PI-DRPA1030-BA-PEST-PEDB.
             COPY DRPA0433.
      *
          05 PI-DRPA1030-BUCHUNG-NEU.
             10 PI-DRPA1030-KNR-BUCHUNGS-GRUND     PIC S9(004) BINARY.
             10 PI-DRPA1030-PGEB-GES               PIC S9(004) BINARY.
             10 PI-DRPA1030-PROD-PROD-SCHL         PIC S9(004) BINARY.
             10 PI-DRPA1030-GES-PROV               PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1030-BEITRAG                PIC S9(09)V9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1030-SUMME                  PIC S9(09)v9(2)
                                                       PACKED-DECIMAL.
             10 PI-DRPA1030-DGV                    PIC S9(004) BINARY.
             10 PI-DRPA1030-PRODART                PIC S9(004) BINARY.
             10 PI-DRPA1030-STICHTAG               PIC  X(010).
             10 PI-DRPA1030-BER-WAEHRUNG           PIC S9(004) BINARY.

          05 PI-DRPA1030-BN-PEST-PEDB.
             COPY DRPA0433.
      *
          05 PI-DRPA1030-S-GEGENBUCHUNG-ALT        PIC  X(001).
             88 PI-DRPA1030-S-GBA-NICHT-NOETIG          VALUE  '1'.
             88 PI-DRPA1030-S-GBA-VORHANDEN             VALUE  '2'.
             88 PI-DRPA1030-S-GBA-NICHT-VORH            VALUE  '3'.
      *
          05 PI-DRPA1030-HAFTZEITDAUER          PIC  X(004).
          05 PI-DRPA1030-BELASTUNGSDAUER        PIC  X(004).
          05 PI-DRPA1030-LAUFZEIT               PIC S9(004) BINARY.
          05 PI-DRPA1030-PMBE-ID                PIC S9(015)
                                                    PACKED-DECIMAL.
          05 PI-DRPA1030-PROV-MULTI             PIC  9(003)V999999.

          05 PI-DRPA1030-PMBE-GSMT-PROV-DAT     PIC  X(010).
          05 PI-DRPA1030-PGV-KZ                 PIC  X.
          05 PI-DRPA1030-PRINTE                 PIC S9(015)
                                                    PACKED-DECIMAL.
210813    05 PI-DRPA1030-KZ-NEUTRAL             PIC X.
210813    05 PI-DRPA1030-VVG-KZ                 PIC X.
210813    05 PI-DRPA1030-UMB-36-MONATE          PIC X.
210813    05 PI-DRPA1030-TARIFSCHL-GRP          PIC S9(009) BINARY.
210813    05 PI-DRPA1030-TARIFSCHL              PIC S9(009) BINARY.
210813    05 PI-DRPA1030-KNR-TARIF-KZ           PIC S9(004) BINARY.
210813    05 PI-DRPA1030-VERM-GES               PIC S9(004) BINARY.
210813    05 PI-DRPA1030-KNR-MANDANT            PIC S9(004) BINARY.
210813    05 PI-DRPA1030-SAM-NR                 PIC S9(009) BINARY.
210813    05 PI-DRPA1030-KNR-RAB-ST             PIC S9(004) BINARY.
210813    05 PI-DRPA1030-ZAHLW                  PIC S9(004) BINARY.
210813    05 PI-DRPA1030-URSPR-POL-DAT          PIC X(010).
210813    05 PI-DRPA1030-FAELLIG-VON            PIC X(010).

170615    05 PI-DRPA1030-PG-UST-SATZ            PIC S9(003)V999 COMP-3.
170615    05 PI-DRPA1030-PG-UST-BETRAG          PIC S9(009)V99  COMP-3.
040122    05 PI-DRPA1030-NEUST-KZ               PIC X(001).

          05 PI-DRPA1030-GEGENBUCHUNG-ALT.
             COPY DRPA0451.
      *
          05 PI-DRPA1030-GBA-PEST-PEDB.
             COPY DRPA0433.
      *
       01 PO-DRPA1030.
          05 PO-DRPA1030-RC                      PIC  X(003).

             88 PO-DRPA1030-RC-UMB-OK            VALUE 'UOK'.
             88 PO-DRPA1030-RC-UMB-OK-O-NEUT     VALUE 'UON'.
             88 PO-DRPA1030-RC-UMB-OK-NEUT-GBA   VALUE 'UNG'.
             88 PO-DRPA1030-RC-UMB-OK-NUR-NEU    VALUE 'UNN'.
             88 PO-DRPA1030-RC-UMB-NOK-O-AEND    VALUE 'UOA'.
             88 PO-DRPA1030-RC-UMB-NOK           VALUE 'UNO'.
             88 PO-DRPA1030-RC-UMB-UNZULAESSIG   VALUE 'UUZ'.
      *
          05 PO-DRPA1030-GEGENBUCHUNG.
             COPY DRPA0451.
          05 PO-DRPA1030-BUCHUNG-NEU.
             COPY DRPA0451.
          05 PO-DRPA1030-GEGENBUCHUNG-MOD.
             COPY DRPA0451.
          05 PO-DRPA1030-KONDITIONEN.
             10 PO-DRPA1030-KONDITION-GRP    PIC S9(004) BINARY.
             10 PO-DRPA1030-GRUND-PROV       PIC S9(009)V99
                                                      PACKED-DECIMAL.
             10 PO-DRPA1030-KONDITION-EINH   PIC S9(004) BINARY.

             10 PO-DRPA1030-EINH-PMPT    PIC S9(006)V99 PACKED-DECIMAL.

230119    05 PO-DRPA1030-KZ-GB-PEST-NEU      PIC  X(001).
230119    05 PO-DRPA1030-GB-PEST-PEDB.
230119        COPY DRPA0433.

      ******* ENDE COPY-MEMBER **************************************


