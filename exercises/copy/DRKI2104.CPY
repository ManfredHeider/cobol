      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2104.CPY  $

      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2104.CPv  $
      *
      *   Rev 2.6   Jun 17 2020 10:09:42   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.5   Jun 23 2016 09:22:04   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP
      *
      *   Rev 2.4   Mar 01 2011 14:13:50   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.3   Feb 28 2011 12:07:22   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.2   Jun 24 2010 14:37:22   t13ex51
      *AKI00974: TEXT-SCHL. für Tab. Einzelvertrag
      *
      *   Rev 2.1   17 Oct 2007 11:17:04   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   27 Sep 2007 14:23:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2104                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2104                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:46:47 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2104
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2104.
           05  PI-DRKI2104-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2104-VEZVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2104-VEZVETLFDNR   PIC S9(4) COMP.
END   *

           05  PI-DRKI2104-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2104-NI-VEZABLAUF        PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZAENDDAT       PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZBEARBEITDAT   PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZBEGINN        PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZPRODBEZ-KNR   PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZRABNJBEITRAG  PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZREOBPRODNR    PIC S9(4) COMP.
             15 PI-DRKI2104-NI-VEZSUMKOMPBEITR  PIC S9(4) COMP.
END   *

           05  PI-DRKI2104-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2104-VEZABLAUF   PIC  X(010).
             10 PI-DRKI2104-VEZAENDDAT   PIC  X(010).
             10 PI-DRKI2104-VEZAENDERUNGSGRD   PIC  X(0050).
             10 PI-DRKI2104-VEZBBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZBEARBEITDAT   PIC  X(010).
             10 PI-DRKI2104-VEZBEGINN   PIC  X(010).
             10 PI-DRKI2104-VEZBJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZFLEXNSP   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2104-VEZFLEXNSPGW   PIC  X(0001).
             10 PI-DRKI2104-VEZGESCHAEFTSZW   PIC  X(0002).
             10 PI-DRKI2104-VEZHOSTID   PIC  X(0015).
             10 PI-DRKI2104-VEZNBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZPRODBEZ-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2104-VEZPRODBEZ-KTY   PIC S9(4) COMP.
             10 PI-DRKI2104-VEZPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2104-VEZRABNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZREOBPRODNR   PIC S9(9) COMP.
             10 PI-DRKI2104-VEZSPARTE-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2104-VEZSTATUS   PIC S9(4) COMP.
             10 PI-DRKI2104-VEZSUMKOMPBEITR   PIC S9(8)V9(2) COMP-3.
             10 PI-DRKI2104-VEZSUMME   PIC S9(10)V COMP-3.
             10 PI-DRKI2104-VEZTARPROD-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2104-VEZVERTRARTEZV   PIC S9(9) COMP.
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2104-NI1.
           05 PI-DRKI2104-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 8.
       01  PI-DRKI2104-NI7.
           05 PI-DRKI2104-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 8.

      *
       01  PO-DRKI2104.
           05  PO-DRKI2104-RC                     PIC  X(003).
               88 PO-DRKI2104-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2104-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

