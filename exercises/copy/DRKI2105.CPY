      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2105.CPY  $

      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2105.CPv  $
      *
      *   Rev 2.6   Jun 17 2020 10:11:20   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.5   Jun 23 2016 09:22:10   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP
      *
      *   Rev 2.4   Mar 01 2011 14:13:56   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.3   Feb 28 2011 12:07:28   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.2   Jun 24 2010 14:37:30   t13ex51
      *AKI00974: TEXT-SCHL. für Tab. Einzelvertrag
      *
      *   Rev 2.1   17 Oct 2007 11:17:16   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   27 Sep 2007 14:23:20   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2105                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2105                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:46:49 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2105
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2105.
           05  PI-DRKI2105-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2105-VEZVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2105-VEZVETLFDNR   PIC S9(4) COMP.
END   *

           05  PI-DRKI2105-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2105-NI-VEZABLAUF        PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZAENDDAT       PIC S9(4) COMP.
             15 PI-DRKI2105-N-VZNDRNGSGRD       PIC S9(4) COMP.
             15 PI-DRKI2105-N-VZBBTRGZHLW       PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZBEARBEITDAT   PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZBEGINN        PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZBJBEITRAG     PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZGESCHAEFTSZW  PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZHOSTID        PIC S9(4) COMP.
             15 PI-DRKI2105-N-VZNBTRGZHLW       PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZNJBEITRAG     PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZPRODBEZ-KNR   PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZPRODBEZ-KTY   PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZPRODUKTID     PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZRABNJBEITRAG  PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZREOBPRODNR    PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZSPARTE-KNR    PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZSTATUS        PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZSUMKOMPBEITR  PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZSUMME         PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZTARPROD-KNR   PIC S9(4) COMP.
             15 PI-DRKI2105-NI-VEZVERTRARTEZV   PIC S9(4) COMP.
END   *

           05  PI-DRKI2105-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2105-VEZABLAUF   PIC  X(010).
             10 PI-DRKI2105-VEZAENDDAT   PIC  X(010).
             10 PI-DRKI2105-VEZAENDERUNGSGRD   PIC  X(0050).
             10 PI-DRKI2105-VEZBBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZBEARBEITDAT   PIC  X(010).
             10 PI-DRKI2105-VEZBEGINN   PIC  X(010).
             10 PI-DRKI2105-VEZBJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZFLEXNSP   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2105-VEZFLEXNSPGW   PIC  X(0001).
             10 PI-DRKI2105-VEZGESCHAEFTSZW   PIC  X(0002).
             10 PI-DRKI2105-VEZHOSTID   PIC  X(0015).
             10 PI-DRKI2105-VEZNBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZPRODBEZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2105-VEZPRODBEZ-KTY   PIC S9(4) COMP.
             10 PI-DRKI2105-VEZPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2105-VEZRABNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZREOBPRODNR   PIC S9(9) COMP.
             10 PI-DRKI2105-VEZSPARTE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2105-VEZSTATUS   PIC S9(4) COMP.
             10 PI-DRKI2105-VEZSUMKOMPBEITR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2105-VEZSUMME   PIC S9(10)V COMP-3.
             10 PI-DRKI2105-VEZTARPROD-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2105-VEZVERTRARTEZV   PIC S9(9) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2105-NI1.
           05 PI-DRKI2105-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 22.
       01  PI-DRKI2105-NI7.
           05 PI-DRKI2105-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 22.
      *
       01  PO-DRKI2105.
           05  PO-DRKI2105-RC                     PIC  X(003).
               88 PO-DRKI2105-RC-OK               VALUE 'OK '.
               88 PO-DRKI2105-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

