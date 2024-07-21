      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.06.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2909.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2909.CPv  $
      *
      *   Rev 2.4   Jun 17 2020 10:06:32   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.3   Jun 23 2016 08:44:04   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP ZT/ZM
      *
      *   Rev 2.2   Oct 08 2014 13:43:58   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:34:04   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Standard-OZP für TCKI0469 G10KS
      *DSSUHSA401
      *
      *   Rev 2.0   Jun 4 2014 08:14:22 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2909                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2909                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:40:42 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2909
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2909.
           05  PI-DRKI2909-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2909-G10EZVHOSTNR   PIC  X(0015).
             10 PI-DRKI2909-G10EZVVERTRAGSNR   PIC  X(0015).
END   *

           05  PI-DRKI2909-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2909-NI-G10EZVPRODBEZ    PIC S9(4) COMP.
END   *

           05  PI-DRKI2909-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2909-G10EZVABLAUF   PIC  X(010).
             10 PI-DRKI2909-G10EZVAENDDAT   PIC  X(010).
             10 PI-DRKI2909-G10EZVAENDGRD   PIC  X(0050).
             10 PI-DRKI2909-G10EZVBEGINN   PIC  X(010).
             10 PI-DRKI2909-G10EZVBRUTJBTR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVFLEXNSP   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVFLEXNSPGW   PIC  X(0001).
             10 PI-DRKI2909-G10EZVLETBEARB   PIC  X(010).
             10 PI-DRKI2909-G10EZVNBTRZW   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVNETJBTR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVNVEZBTRG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVPBRDNR   PIC S9(9) COMP.
             10 PI-DRKI2909-G10EZVPRODBEZ   PIC  X(0050).
             10 PI-DRKI2909-G10EZVRABNETTO   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVREOBPRDNR   PIC  X(0005).
             10 PI-DRKI2909-G10EZVSPARTE   PIC S9(9) COMP.
             10 PI-DRKI2909-G10EZVSTATUS   PIC  X(0009).
             10 PI-DRKI2909-G10EZVSUAKTBTR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2909-G10EZVSUMME   PIC S9(9) COMP.
             10 PI-DRKI2909-G10EZVVART   PIC S9(9) COMP.
             10 PI-DRKI2909-G10EZVZWEIG   PIC  X(0002).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2909-NI1.
           05 PI-DRKI2909-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2909-NI7.
           05 PI-DRKI2909-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI2909.
           05  PO-DRKI2909-RC                     PIC  X(003).
               88 PO-DRKI2909-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2909-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

