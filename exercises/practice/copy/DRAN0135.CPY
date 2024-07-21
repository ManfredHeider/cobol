      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0135.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0135.CPv  $
      *
      *   Rev 2.4   Dec 14 2011 16:58:52   t13ae23
      *AAN01033: DVAG-Wien: Erweiterung Header-Datensatz Ges. 16 u.
      * 26 - TBAN0008 -
      *
      *   Rev 2.3   26 Oct 2006 08:54:32   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   24 Oct 2006 13:54:08   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Sep 26 2006 10:54:16   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Sep 14 2006 16:22:52   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAN0135                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ANTK ANTRAGSTATUS, GES.        >>   *
      *    --------------------------------------------------------   *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!25.09.06!2.1 ! NEU                       ! HEIDER   *
      * AAN01033!        !2.4 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0135                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAN0135.
          05 PI-DRAN0135-PROGRAMM               PIC  X(008).
          05 PI-DRAN0135-UFKT                   PIC  9(002).
          05 ANTK-KEY.
             10 ANTA-ANTR-STATUS-1              PIC S9(4) COMP.
             10 PGEB-GES                        PIC S9(4) COMP.
      *
       01 PO-DRAN0135.
          05 PO-DRAN0135-RC                     PIC  X(003).
             88 PO-DRAN0135-RC-OK                    VALUE 'OK '.
             88 PO-DRAN0135-RC-EOF                   VALUE 'EOF'.
             88 PO-DRAN0135-RC-LEER                  VALUE 'LER'.
          05 PO-DRAN0135-DATEN.
             15 PO-DRAN0135-ANTRAG-KOPF.
                COPY DRAN0141.
             15 ANTA-LAST-UPDATE                PIC  X(026).
             15 ANTA-KENNUNG                    PIC  X(008).
             15 PO-DRAN0135-ANTRAG-PE-STRUKTUR.
                COPY DRAN0142.
      *
      ******* ENDE COPY-MEMBER **************************************
