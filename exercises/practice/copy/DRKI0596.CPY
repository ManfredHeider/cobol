      * T13AE38   Stand: 06/10/98    08:26:51
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0596.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0596.CPv  $
      *
      *   Rev 2.4   Feb 02 2021 15:12:06   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Db2
      *
      *   Rev 2.3   Mar 12 2019 07:31:42   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.2   Sep 21 2017 12:07:08   t13ac20
      *AKI01503: DCLGEN und OZPs bereinigen
      *
      *   Rev 2.1   Sep 12 2011 10:05:18   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.0   Mar 23 1999 15:53:18   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   07 Aug 1998 14:50:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0596                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0596                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         02.02.2021 / 08:03:25 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0596
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0596.
           05  PI-DRKI0596-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0596-LTTTTNR   PIC S9(4) COMP.
             10 PI-DRKI0596-LTTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0596-LTTVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI0596.
           05  PO-DRKI0596-RC                     PIC  X(003).
               88 PO-DRKI0596-RC-OK               VALUE 'OK '.
               88 PO-DRKI0596-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI0596-NI.
               10 NI-DRKI0596-TAB.
                  15 DRKI0596-ELE
                     PIC S9(04) COMP OCCURS 18 TIMES.
               10 NI-DRKI0596 REDEFINES NI-DRKI0596-TAB.
 
ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI0596-N-LTTBSCHLSSVRM   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBSCHRT-KNR   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBSCHRT-KTY   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBSCHVR-KNR   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBSCHVR-KTY   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBTRZHLDR   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBZBRFGRP-KNR   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTBZBRFGRP-KTY   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTDYNLTGRVID   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTDYNPROZ   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTDYNWAHL-KNR   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTLSTERHPROZ   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTPROVHAFTZBIS   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTPROVHAFTZVON   PIC S9(4) COMP.
             15 PO-DRKI0596-NI-LTTRZINS   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTVRTRGSBLF   PIC S9(4) COMP.
             15 PO-DRKI0596-N-LTTVRTRGSBGNN   PIC S9(4) COMP.
END   *
 
           05  PO-DRKI0596-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI0596-LTTABSCHLUSSVERM   PIC S9(9) COMP.
             10 PO-DRKI0596-LTTABSICHART-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI0596-LTTABSICHART-KTY   PIC S9(4) COMP.
             10 PO-DRKI0596-LTTABSICHVAR-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI0596-LTTABSICHVAR-KTY   PIC S9(4) COMP.
             10 PO-DRKI0596-LTTBEITRZAHLDAUER   PIC  X(010).
             10 PO-DRKI0596-LTTBUZBERUFGRP-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI0596-LTTBUZBERUFGRP-KTY   PIC S9(4) COMP.
             10 PO-DRKI0596-LTTDYNAMIKKZ   PIC  X(0001).
             10 PO-DRKI0596-LTTDYNLTGRVID   PIC S9(4) COMP.
             10 PO-DRKI0596-LTTDYNPROZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI0596-LTTDYNWAHL-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI0596-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             10 PO-DRKI0596-LTTLSTERHPROZ   PIC S9(2)V9(1) COMP-3.
             10 PO-DRKI0596-LTTPROVHAFTZBIS   PIC  X(010).
             10 PO-DRKI0596-LTTPROVHAFTZVON   PIC  X(010).
             10 PO-DRKI0596-LTTRZINS   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI0596-LTTVERTRAGSABLAUF   PIC  X(010).
             10 PO-DRKI0596-LTTVERTRAGSBEGINN   PIC  X(010).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

