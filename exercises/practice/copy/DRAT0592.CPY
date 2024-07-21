      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0592.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0592.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:28:14   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.1   Jul 07 2015 11:13:38   t13ex27
      *AAT00049: Fehlerverfahren mit Prozentkriterien-OZPs
      *
      *   Rev 2.0   Jun 26 2015 13:02:55 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0592                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0592                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 09:59:10 / T13AE52
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
      *    FÜR DIE FUNKTION DRAT0592
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0592.
           05  PI-DRAT0592-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0592-FAB-JOBNAME   PIC  X(0008).
             10 PI-DRAT0592-FAB-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0592-FAB-RELEVANZ   PIC  X(0001).
             10 PI-DRAT0592-FAB-SYSCODE   PIC  X(0002).
END   *

      *
       01  PO-DRAT0592.
           05  PO-DRAT0592-RC                     PIC  X(003).
               88 PO-DRAT0592-RC-OK               VALUE 'OK '.
               88 PO-DRAT0592-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRAT0592-NI.
               10 NI-DRAT0592-TAB.
                  15 DRAT0592-ELE
                     PIC S9(04) COMP OCCURS 2 TIMES.
               10 NI-DRAT0592 REDEFINES NI-DRAT0592-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRAT0592-NI-FAB-PROZENT   PIC S9(4) COMP.
             15 PO-DRAT0592-NI-FAB-USERID   PIC S9(4) COMP.
END   *

           05  PO-DRAT0592-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRAT0592-FAB-FAKTOR   PIC S9(4) COMP.
             10 PO-DRAT0592-FAB-GRENZE   PIC S9(9) COMP.
             10 PO-DRAT0592-FAB-PROZENT   PIC S9(2)V9(2) COMP-3.
             10 PO-DRAT0592-FAB-USERID   PIC  X(0007).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

