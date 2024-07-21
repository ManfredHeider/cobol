      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2107.CPY  $

      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2107.CPv  $
      *
      *   Rev 2.6   Jun 17 2020 10:13:02   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.5   Jun 23 2016 09:22:16   t13ae31
      *AKI01438: G10 SUH Angleichung DTA OZP
      *
      *   Rev 2.4   Mar 01 2011 14:14:02   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.3   Feb 28 2011 12:07:34   t13ex27
      *AKI01012: VSP Vorteilsargumentation-OZP
      *
      *   Rev 2.2   Jun 24 2010 14:37:42   t13ex51
      *AKI00974: TEXT-SCHL. für Tab. Einzelvertrag
      *
      *   Rev 2.1   17 Oct 2007 11:17:38   t13ex27
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs
      *
      *   Rev 2.0   27 Sep 2007 14:23:40   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2107                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2107                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         16.06.2020 / 12:46:53 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2107
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2107.
           05  PI-DRKI2107-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2107-VEZVERTRAGSID   PIC  X(0013).
             10 PI-DRKI2107-VEZVETLFDNR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI2107.
           05  PO-DRKI2107-RC                     PIC  X(003).
               88 PO-DRKI2107-RC-OK               VALUE 'OK '.
               88 PO-DRKI2107-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2107-NI.
               10 NI-DRKI2107-TAB.
                  15 DRKI2107-ELE
                     PIC S9(04) COMP OCCURS 9 TIMES.
               10 NI-DRKI2107 REDEFINES NI-DRKI2107-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2107-NI-VEZABLAUF   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZAENDDAT   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZBEARBEITDAT   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZBEGINN   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZPRODBEZ-KNR   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZPRODBEZ-KTY   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZRABNJBEITRAG   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZREOBPRODNR   PIC S9(4) COMP.
             15 PO-DRKI2107-NI-VEZSUMKOMPBEITR   PIC S9(4) COMP.
END   *

           05  PO-DRKI2107-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2107-LETZTEAENDERUNG   PIC  X(0026).
             10 PO-DRKI2107-VEZABLAUF   PIC  X(010).
             10 PO-DRKI2107-VEZAENDDAT   PIC  X(010).
             10 PO-DRKI2107-VEZAENDERUNGSGRD   PIC  X(0050).
             10 PO-DRKI2107-VEZBBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZBEARBEITDAT   PIC  X(010).
             10 PO-DRKI2107-VEZBEGINN   PIC  X(010).
             10 PO-DRKI2107-VEZBJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZFLEXNSP   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2107-VEZFLEXNSPGW   PIC  X(0001).
             10 PO-DRKI2107-VEZGESCHAEFTSZW   PIC  X(0002).
             10 PO-DRKI2107-VEZHOSTID   PIC  X(0015).
             10 PO-DRKI2107-VEZNBEITRAGZAHLW   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZPRODBEZ-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2107-VEZPRODBEZ-KTY   PIC S9(4) COMP.
             10 PO-DRKI2107-VEZPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2107-VEZRABNJBEITRAG   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZREOBPRODNR   PIC S9(9) COMP.
             10 PO-DRKI2107-VEZSPARTE-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2107-VEZSTATUS   PIC S9(4) COMP.
             10 PO-DRKI2107-VEZSUMKOMPBEITR   PIC S9(8)V9(2) COMP-3.
             10 PO-DRKI2107-VEZSUMME   PIC S9(10)V COMP-3.
             10 PO-DRKI2107-VEZTARPROD-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2107-VEZVERTRARTEZV   PIC S9(9) COMP.
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

