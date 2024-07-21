      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0043.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0043.CPv  $
      *
      *   Rev 2.1   24 Sep 2008 10:05:12   t13ex27
      *AKI00743: DTA-Kontrolle Teil 1
      *
      *   Rev 2.0   Oct 19 2006 15:02:44   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0043                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0043                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00473!14.08.08!2.1 ! DTA-Kontrolle für OKI     !I. Frenzel*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0043
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0043.
           05  PI-DROK0043-KEY.

             10 PI-DROK0043-AK-KEY   PIC X(26).
      *
       01  PO-DROK0043.
           05  PO-DROK0043-RC                 PIC  X(003).
               88 PO-DROK0043-RC-OK                        VALUE 'OK '.
               88 PO-DROK0043-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0043-DATEN.

             10 PO-DROK0043-AK-LOGMAS-DATEI      PIC X(36).
             10 PO-DROK0043-AK-LOGMAS-GEN        PIC X(8).
             10 PO-DROK0043-AK-LOGMAS-ANZ        PIC S9(9) COMP.
             10 PO-DROK0043-AK-LOGMAS-JOB        PIC X(8).
             10 PO-DROK0043-AK-LOGMAS-STATUS     PIC X(1).
             10 PO-DROK0043-AK-EINARB-DATEI      PIC X(36).
             10 PO-DROK0043-AK-EINARB-GEN        PIC X(8).
             10 PO-DROK0043-AK-EINARB-ANZ        PIC S9(9) COMP.
             10 PO-DROK0043-AK-EINARB-JOB        PIC X(8).
             10 PO-DROK0043-AK-EINARB-STATUS     PIC X(1).
             10 PO-DROK0043-AK-LOGMAS-TSTP       PIC X(26).
             10 PO-DROK0043-AK-EINARB-TSTP       PIC X(26).
      *
      ******* ENDE COPY-MEMBER ***************************************
