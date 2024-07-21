      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2908.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2908.CPv  $
      *
      *   Rev 2.4   Jun 17 2020 10:04:52   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.3   Jun 23 2016 08:43:58   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.2   Oct 08 2014 13:43:52   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:34:00   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Standard-OZP für TCKI0469 G10KS
      *DSSUHSA401
      *
      *   Rev 2.0   Jun 4 2014 08:14:19 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2908                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2908                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:40:39 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2908
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2908.
           05  PI-DRKI2908-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2908-G10EZVHOSTNR   PIC  X(0015).
             10 PI-DRKI2908-G10EZVVERTRAGSNR   PIC  X(0015).
END   *

      *
       01  PO-DRKI2908.
           05  PO-DRKI2908-RC                     PIC  X(003).
               88 PO-DRKI2908-RC-OK               VALUE 'OK '.
               88 PO-DRKI2908-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2908-NI.
               10 NI-DRKI2908-TAB.
                  15 DRKI2908-ELE
                     PIC S9(04) COMP OCCURS 1 TIMES.
               10 NI-DRKI2908 REDEFINES NI-DRKI2908-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2908-NI-G10EZVPRODBEZ   PIC S9(4) COMP.
END   *

           05  PO-DRKI2908-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2908-G10EZVABLAUF   PIC  X(010).
             10 PO-DRKI2908-G10EZVAENDDAT   PIC  X(010).
             10 PO-DRKI2908-G10EZVAENDGRD   PIC  X(0050).
             10 PO-DRKI2908-G10EZVBEGINN   PIC  X(010).
             10 PO-DRKI2908-G10EZVBRUTJBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVFLEXNSP   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVFLEXNSPGW   PIC  X(0001).
             10 PO-DRKI2908-G10EZVLETBEARB   PIC  X(010).
             10 PO-DRKI2908-G10EZVNBTRZW   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVNETJBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVNVEZBTRG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVPBRDNR   PIC S9(9) COMP.
             10 PO-DRKI2908-G10EZVPRODBEZ   PIC  X(0050).
             10 PO-DRKI2908-G10EZVRABNETTO   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVREOBPRDNR   PIC  X(0005).
             10 PO-DRKI2908-G10EZVSPARTE   PIC S9(9) COMP.
             10 PO-DRKI2908-G10EZVSTATUS   PIC  X(0009).
             10 PO-DRKI2908-G10EZVSUAKTBTR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2908-G10EZVSUMME   PIC S9(9) COMP.
             10 PO-DRKI2908-G10EZVVART   PIC S9(9) COMP.
             10 PO-DRKI2908-G10EZVZWEIG   PIC  X(0002).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

