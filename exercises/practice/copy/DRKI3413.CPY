      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3413.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3413.CPv  $
      *
      *   Rev 2.4   Nov 21 2019 08:26:52   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.3   Nov 28 2018 17:28:24   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.2   Sep 27 2018 13:14:32   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.1   Sep 11 2018 12:15:10   t13ac20
      *AKI01600: Sperren & Löschen von Verträgen - DB2 neue Tabellen
      *
      *   Rev 2.0   Sep 6 2018 08:09:13 T13AC20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3413                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3413                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.09.2019 / 16:41:06 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3413
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3413.
           05  PI-DRKI3413-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3413-SPALFDNR   PIC S9(4) COMP.
             10 PI-DRKI3413-SPAPARTGESELLID   PIC S9(4) COMP.
             10 PI-DRKI3413-SPAVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI3413-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI3413-NI-SPAARCHIVTST     PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPAENTSPERRTST   PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPAGRUND         PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPALOESCHTST     PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPASPERRDAT      PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPASTATUS-KNR    PIC S9(4) COMP.
             15 PI-DRKI3413-NI-SPAVERTRAGSID    PIC S9(4) COMP.
END   *

           05  PI-DRKI3413-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3413-SPAARCHIVTST   PIC  X(0026).
             10 PI-DRKI3413-SPAENTSPERRTST   PIC  X(0026).
             10 PI-DRKI3413-SPAGRUND   PIC  X(0001).
             10 PI-DRKI3413-SPALOESCHTST   PIC  X(0026).
             10 PI-DRKI3413-SPASPERRDAT   PIC  X(010).
             10 PI-DRKI3413-SPASTATUS-KNR   PIC S9(4) COMP.
             10 PI-DRKI3413-SPAVERTRAGSID   PIC  X(0013).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3413-NI1.
           05 PI-DRKI3413-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 7.
       01  PI-DRKI3413-NI7.
           05 PI-DRKI3413-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 7.
      *
       01  PO-DRKI3413.
           05  PO-DRKI3413-RC                     PIC  X(003).
               88 PO-DRKI3413-RC-OK               VALUE 'OK '.
               88 PO-DRKI3413-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

