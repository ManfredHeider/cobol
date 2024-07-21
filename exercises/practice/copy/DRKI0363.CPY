      * T13EX33   Stand: 08/02/99    10:28:36
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0363.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0363.CPv  $
      *
      *   Rev 2.4   Feb 02 2021 15:10:34   t13ac20
      *AKI01825: Leben: Schüler BU Erweiterung und Anpassung im DBO
      *: Sourcen Db2
      *
      *   Rev 2.3   Mar 12 2019 07:30:40   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw.
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.2   Jun 23 2016 10:41:30   t13ex27
      *AKI01434: AML-Neue Produkte zum RT 06/16
      *
      *   Rev 2.1   Sep 12 2011 10:04:42   t13ex27
      *AKI01065: AMLeben - Einführung Impulsprodukt - VASP-Vertrags
      *daten
      *
      *   Rev 2.0   Mar 23 1999 15:52:38   T13EX33
      *aki00019 Produktionsübergabe
      *
      *   Rev 1.0   14 Jul 1998 17:02:22   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0363                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0363                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         02.02.2021 / 08:03:18 / T13AC20
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
      *    FÜR DIE FUNKTION DRKI0363
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0363.
           05  PI-DRKI0363-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI0363-LTTTTNR   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI0363-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0363-N-LTTBSCHRT-KNR     PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBSCHRT-KTY     PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBSCHVR-KNR     PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBSCHVR-KTY     PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBTRZHLDR       PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBZBRFGRP-KNR   PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTBZBRFGRP-KTY   PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTDYNLTGRVID    PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTDYNPROZ       PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTDYNWAHL-KNR   PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTLSTERHPROZ    PIC S9(4) COMP.
             15 PI-DRKI0363-NI-LTTRZINS         PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTVRTRGSBLF      PIC S9(4) COMP.
             15 PI-DRKI0363-N-LTTVRTRGSBGNN     PIC S9(4) COMP.
END   *
 
           05  PI-DRKI0363-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI0363-LTTABSICHART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0363-LTTABSICHART-KTY   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTABSICHVAR-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0363-LTTABSICHVAR-KTY   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTBEITRZAHLDAUER   PIC  X(010).
             10 PI-DRKI0363-LTTBUZBERUFGRP-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0363-LTTBUZBERUFGRP-KTY   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTDYNAMIKKZ   PIC  X(0001).
             10 PI-DRKI0363-LTTDYNLTGRVID   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTDYNPROZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0363-LTTDYNWAHL-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI0363-LTTDYNWAHL-KTY   PIC S9(4) COMP.
             10 PI-DRKI0363-LTTLSTERHPROZ   PIC S9(2)V9(1) COMP-3.
             10 PI-DRKI0363-LTTRZINS   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0363-LTTVERTRAGSABLAUF   PIC  X(010).
             10 PI-DRKI0363-LTTVERTRAGSBEGINN   PIC  X(010).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0363-NI1.
           05 PI-DRKI0363-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 15.
       01  PI-DRKI0363-NI7.
           05 PI-DRKI0363-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 15.
 
      *
       01  PO-DRKI0363.
           05  PO-DRKI0363-RC                     PIC  X(003).
               88 PO-DRKI0363-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0363-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

