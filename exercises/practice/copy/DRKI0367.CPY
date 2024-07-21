      * T13AE38   Stand: 06/10/98    08:26:33
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0367.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0367.CPv  $
      *
      *   Rev 2.4   Feb 02 2021 15:11:20   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Db2
      *
      *   Rev 2.3   Mar 12 2019 07:31:10   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.2   Jun 23 2016 10:41:38   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.1   Sep 12 2011 10:04:50   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.0   Mar 23 1999 15:52:44   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0367                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0367                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.02.2021 / 11:45:22 / T13AC20
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0367
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0367.
           05  PI-DRKI0367-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0367-LTTTTNR   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI0367-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0367-N-LTTBSCHLSSVRM     PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBSCHRT-KNR     PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBSCHRT-KTY     PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBSCHVR-KNR     PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBSCHVR-KTY     PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBTRZHLDR       PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBZBRFGRP-KNR   PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTBZBRFGRP-KTY   PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTDYNAMIKKZ     PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTDYNLTGRVID    PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTDYNPROZ       PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTDYNWAHL-KNR   PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTLSTERHPROZ    PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTPROVHAFTZBIS  PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTPROVHAFTZVON  PIC S9(4) COMP.
             15 PI-DRKI0367-NI-LTTRZINS         PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTVRTRGSBLF      PIC S9(4) COMP.
             15 PI-DRKI0367-N-LTTVRTRGSBGNN     PIC S9(4) COMP.
END   *
 
           05  PI-DRKI0367-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI0367-LTTABSCHLUSSVERM   PIC S9(9) COMP.
             10 PI-DRKI0367-LTTABSICHART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0367-LTTABSICHART-KTY   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTABSICHVAR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0367-LTTABSICHVAR-KTY   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTBEITRZAHLDAUER   PIC  X(010).
             10 PI-DRKI0367-LTTBUZBERUFGRP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0367-LTTBUZBERUFGRP-KTY   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTDYNAMIKKZ   PIC  X(0001).
             10 PI-DRKI0367-LTTDYNLTGRVID   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTDYNPROZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0367-LTTDYNWAHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0367-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             10 PI-DRKI0367-LTTLSTERHPROZ   PIC S9(2)V9(1) COMP-3.
             10 PI-DRKI0367-LTTPROVHAFTZBIS   PIC  X(010).
             10 PI-DRKI0367-LTTPROVHAFTZVON   PIC  X(010).
             10 PI-DRKI0367-LTTRZINS   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0367-LTTVERTRAGSABLAUF   PIC  X(010).
             10 PI-DRKI0367-LTTVERTRAGSBEGINN   PIC  X(010).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0367-NI1.
           05 PI-DRKI0367-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 19.
       01  PI-DRKI0367-NI7.
           05 PI-DRKI0367-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 19.
      *
       01  PO-DRKI0367.
           05  PO-DRKI0367-RC                     PIC  X(003).
               88 PO-DRKI0367-RC-OK               VALUE 'OK '.
               88 PO-DRKI0367-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

