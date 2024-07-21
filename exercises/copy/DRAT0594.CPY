      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0594.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0594.CPv  $
      *
      *   Rev 2.2   Aug 23 2019 10:29:02   t13ae52
      *AAT00073: Erweiterung Fehlerverfahren/Fehlertoleranz - NEU
      *
      *   Rev 2.1   Jul 07 2015 11:13:56   t13ex27
      *AAT00049: Fehlerverfahren mit Prozentkriterien-OZPs
      *
      *   Rev 2.0   Jun 26 2015 13:02:59 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0594                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0594                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         01.07.2019 / 09:59:13 / T13AE52
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
      *    FÜR DIE FUNKTION DRAT0594
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRAT0594.
           05  PI-DRAT0594-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRAT0594-FAB-JOBNAME   PIC  X(0008).
             10 PI-DRAT0594-FAB-PROGRAMMNAME   PIC  X(0008).
             10 PI-DRAT0594-FAB-RELEVANZ   PIC  X(0001).
             10 PI-DRAT0594-FAB-SYSCODE   PIC  X(0002).
END   *

           05  PI-DRAT0594-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRAT0594-NI-FAB-ANZAHL       PIC S9(4) COMP.
             15 PI-DRAT0594-NI-FAB-PROZENT      PIC S9(4) COMP.
             15 PI-DRAT0594-NI-FAB-USERID       PIC S9(4) COMP.
END   *

           05  PI-DRAT0594-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRAT0594-FAB-ANZAHL   PIC S9(4) COMP.
             10 PI-DRAT0594-FAB-FAKTOR   PIC S9(4) COMP.
             10 PI-DRAT0594-FAB-GRENZE   PIC S9(9) COMP.
             10 PI-DRAT0594-FAB-PROZENT   PIC S9(2)V9(2) COMP-3.
             10 PI-DRAT0594-FAB-USERID   PIC  X(0007).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRAT0594-NI1.
           05 PI-DRAT0594-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 3.
       01  PI-DRAT0594-NI7.
           05 PI-DRAT0594-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 3.
      *
       01  PO-DRAT0594.
           05  PO-DRAT0594-RC                     PIC  X(003).
               88 PO-DRAT0594-RC-OK               VALUE 'OK '.
               88 PO-DRAT0594-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

